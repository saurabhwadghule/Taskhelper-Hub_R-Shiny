# modules/pdf_compiler.R
# PDF Compiler Module using qpdf::pdf_combine
# Pattern supported:
#   0.pdf (cover) + for i in 1..N: i.pdf then i p.pdf (if present)
# Inputs:
#   - Folder path (server filesystem) OR upload a ZIP of PDFs
#   - Max index (N), output filename, and a "Preview order" button
# Output:
#   - Ordered file list
#   - Downloadable compiled PDF

pdf_compiler_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("PDF Compiler"),
    radioButtons(
      ns("source_mode"), "Choose source",
      c("Server folder" = "folder", "Upload ZIP" = "zip"),
      inline = TRUE
    ),
    fluidRow(
      conditionalPanel(
        condition = sprintf("input['%s'] == 'folder'", ns("source_mode")),
        column(
          12,
          textInput(
            ns("pdf_dir"),
            "Absolute folder path (server):",
            placeholder = "e.g., /srv/data/T3ERESA"
          ),
          helpText("Folder must contain files like 0.pdf, 1.pdf, 1p.pdf, 2.pdf, 2p.pdf, ...")
        )
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'zip'", ns("source_mode")),
        column(
          12,
          fileInput(
            ns("zipfile"),
            "Upload a ZIP containing your PDFs (filenames like 0.pdf, 1.pdf, 1p.pdf, ...)",
            accept = ".zip"
          )
        )
      )
    ),
    numericInput(ns("max_i"), "Max index (N)", value = 9, min = 1, step = 1),
    textInput(ns("outfile"), "Output filename", value = "TH3RESA_combined.pdf"),
    actionButton(ns("preview"), "Preview order"),
    br(), br(),
    verbatimTextOutput(ns("order_preview")),
    hr(),
    withSpinner(downloadButton(ns("download"), "Download compiled PDF"))
  )
}

pdf_compiler_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    shiny::validate(need(requireNamespace("qpdf", quietly = TRUE), "Package 'qpdf' is required."))
    
    # Working dir for ZIP uploads
    workdir <- reactiveVal(NULL)
    
    observeEvent(input$zipfile, {
      req(input$zipfile)
      tmpdir <- tempfile("pdfzip_")
      dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
      utils::unzip(input$zipfile$datapath, exdir = tmpdir)
      workdir(tmpdir)
    }, ignoreInit = TRUE)
    
    # Resolve actual source directory
    source_dir <- reactive({
      if (input$source_mode == "folder") {
        req(nzchar(input$pdf_dir))
        return(normalizePath(input$pdf_dir, winslash = "/",
                             mustWork = FALSE)) # may reside on server
      } else {
        req(workdir())
        return(workdir())
      }
    })
    
    # Build ordered file list
    build_order <- function(dir_path, N) {
      stopifnot(is.numeric(N), N >= 1)
      ordered_files <- character(0)
      
      # Cover
      cover <- file.path(dir_path, "0.pdf")
      if (file.exists(cover)) ordered_files <- c(ordered_files, cover)
      
      # 1..N (i.pdf then i p.pdf)
      for (i in seq_len(N)) {
        one_pager  <- file.path(dir_path, sprintf("%d.pdf", i))
        publication <- file.path(dir_path, sprintf("%dp.pdf", i))
        if (file.exists(one_pager))   ordered_files <- c(ordered_files, one_pager)
        if (file.exists(publication)) ordered_files <- c(ordered_files, publication)
      }
      
      unique(ordered_files)
    }
    
    files_ordered <- eventReactive(input$preview, {
      dir_path <- source_dir()
      shiny::validate(need(dir.exists(dir_path), sprintf("Directory not found: %s", dir_path)))
      ord <- build_order(dir_path, input$max_i)
      shiny::validate(need(length(ord) > 0, "No matching PDFs found."))
      ord
    }, ignoreInit = TRUE)
    
    output$order_preview <- renderText({
      req(files_ordered())
      rel <- gsub(paste0("^", gsub("\\\\", "\\\\\\\\", source_dir())), ".", files_ordered())
      paste0("File count: ", length(rel), "\n\nOrder:\n", paste(rel, collapse = "\n"))
    })
    
    # Compile and provide download
    output$download <- downloadHandler(
      filename = function() {
        nm <- input$outfile
        if (!grepl("\\.pdf$", nm, ignore.case = TRUE)) nm <- paste0(nm, ".pdf")
        nm
      },
      content = function(file) {
        ord <- if (!is.null(isolate(files_ordered()))) {
          isolate(files_ordered())
        } else {
          # compile without preview if user clicks download first
          dir_path <- source_dir()
          shiny::validate(need(dir.exists(dir_path), sprintf("Directory not found: %s", dir_path)))
          tmp <- build_order(dir_path, input$max_i)
          shiny::validate(need(length(tmp) > 0, "No matching PDFs found."))
          tmp
        }
        
        # qpdf combine needs readable file list
        withProgress(message = "Combining PDFs...", value = 0, {
          incProgress(0.6)
          qpdf::pdf_combine(input = ord, output = file)
          incProgress(1)
        })
      }
    )
  })
}
