# modules/reference_folder.R
# Requires: shiny, qpdf, shinycssloaders (optional for spinners)

message("Loaded nct_update.R")  # ✅ This confirms the file is sourced

reference_folder_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Reference folder"),
    # Reference source
    textInput(
      ns("ref_dir"),
      "Reference folder (where PDFs are stored):",
      value = "C:/Users/Saurabh/OneDrive - Innomagine Consulting Private Limited/00 References full text articles",
      placeholder = "Absolute path, e.g. C:/path/to/reference"
    ),
    helpText("Files are expected like 2901.pdf, 2903.pdf. If present, paired 2901p.pdf, 2903p.pdf will also be used."),
    # IDs
    textInput(
      ns("ids"),
      "IDs (comma or space separated):",
      placeholder = "e.g., 2901, 2903, 2910"
    ),
    checkboxInput(ns("include_paired"), "Include paired 'p' files (e.g., 2901p.pdf)", value = TRUE),
    actionButton(ns("preview"), "Preview found / missing"),
    
    br(), br(),
    uiOutput(ns("preview_summary")),
    tableOutput(ns("preview_table")),
    tags$hr(),
    
    # Option 1: Copy to folder
    h4("Option 1 — Copy into a single folder"),
    textInput(
      ns("target_dir"),
      "Target folder (will be created if it doesn't exist):",
      placeholder = "Absolute path, e.g. C:/path/to/output/folder"
    ),
    checkboxInput(ns("overwrite"), "Overwrite if file already exists in target", value = FALSE),
    withSpinner(actionButton(ns("copy_btn"), "Copy PDFs")),
    
    br(), uiOutput(ns("copy_result")),
    
    tags$hr(),
    
    # Option 2: Combine and download
    h4("Option 2 — Combine into a single PDF"),
    textInput(ns("outfile_name"), "Output filename:", value = "combined.pdf"),
    withSpinner(downloadButton(ns("download_pdf"), "Download combined PDF"))
  )
}

reference_folder_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Ensure qpdf is available for combine
    shiny::validate(need(requireNamespace("qpdf", quietly = TRUE),
                         "Package 'qpdf' is required. Install with install.packages('qpdf')."))
    
    # Helpers ----
    normalize_win <- function(path) {
      if (!nzchar(path)) return(path)
      # Accept both backslashes and slashes; normalize for R
      tryCatch(normalizePath(path, winslash = "/", mustWork = FALSE),
               error = function(e) path)
    }
    
    parse_ids <- function(txt) {
      if (!nzchar(txt)) return(integer(0))
      # keep digits, commas, spaces
      clean <- gsub("[^0-9, ]", "", txt)
      ids <- as.integer(unlist(strsplit(clean, "[, ]+")))
      ids[!is.na(ids)]
    }
    
    build_candidates <- function(dir_path, ids, include_p = TRUE) {
      # Return data.frame with expected filenames and status (exists or not)
      if (!dir.exists(dir_path)) {
        return(data.frame(
          id = integer(0),
          filename = character(0),
          full_path = character(0),
          exists = logical(0),
          stringsAsFactors = FALSE
        ))
      }
      out <- list()
      for (i in ids) {
        main <- file.path(dir_path, sprintf("%d.pdf", i))
        out[[length(out) + 1]] <- data.frame(
          id = i,
          filename = sprintf("%d.pdf", i),
          full_path = main,
          exists = file.exists(main),
          stringsAsFactors = FALSE
        )
        if (isTRUE(include_p)) {
          paired <- file.path(dir_path, sprintf("%dp.pdf", i))
          out[[length(out) + 1]] <- data.frame(
            id = i,
            filename = sprintf("%dp.pdf", i),
            full_path = paired,
            exists = file.exists(paired),
            stringsAsFactors = FALSE
          )
        }
      }
      do.call(rbind, out)
    }
    
    # Reactive: preview table of found/missing ----
    preview_tbl <- eventReactive(input$preview, {
      ref_dir <- normalize_win(input$ref_dir)
      ids <- parse_ids(input$ids)
      validate(
        need(nzchar(ref_dir), "Please provide a Reference folder path."),
        need(length(ids) > 0, "Please enter at least one ID.")
      )
      if (!dir.exists(ref_dir)) {
        return(data.frame(
          id = NA_integer_, filename = NA_character_, full_path = ref_dir,
          exists = FALSE, stringsAsFactors = FALSE
        ))
      }
      build_candidates(ref_dir, ids, include_p = isTRUE(input$include_paired))
    }, ignoreInit = TRUE)
    
    output$preview_table <- renderTable({
      tbl <- preview_tbl()
      req(nrow(tbl) > 0)
      data.frame(
        ID = tbl$id,
        File = tbl$filename,
        Exists = ifelse(tbl$exists, "Available", "Not available"),
        stringsAsFactors = FALSE
      )
    })
    
    output$preview_summary <- renderUI({
      tbl <- preview_tbl()
      req(nrow(tbl) > 0)
      if (!all(is.finite(tbl$id))) {
        return(tags$div(class = "text-danger",
                        "Directory not found: ", strong(tbl$full_path[1])))
      }
      n_total <- nrow(tbl)
      n_found <- sum(tbl$exists)
      n_missing <- n_total - n_found
      tags$div(
        sprintf("Total files requested: %d | Available: %d | Missing: %d", n_total, n_found, n_missing),
        if (n_missing > 0) tags$div(class = "text-warning",
                                    "Missing files will be skipped; operations continue for available files.")
      )
    })
    
    # Option 1: Copy files ----
    observeEvent(input$copy_btn, {
      req(preview_tbl())
      ref_dir <- normalize_win(input$ref_dir)
      target <- normalize_win(input$target_dir)
      validate(
        need(nzchar(target), "Please enter a Target folder path.")
      )
      if (!dir.exists(target)) {
        dir.create(target, recursive = TRUE, showWarnings = FALSE)
      }
      
      tbl <- preview_tbl()
      existing <- tbl[tbl$exists, , drop = FALSE]
      missing  <- tbl[!tbl$exists, , drop = FALSE]
      
      copied <- logical(0)
      msgs <- character(0)
      
      if (nrow(existing) > 0) {
        for (i in seq_len(nrow(existing))) {
          src <- existing$full_path[i]
          dst <- file.path(target, basename(src))
          ok <- file.copy(src, dst, overwrite = isTRUE(input$overwrite))
          copied <- c(copied, ok)
          msgs   <- c(msgs, sprintf("%s → %s [%s]",
                                    basename(src), target, ifelse(ok, "copied", "skipped")))
        }
      }
      
      out_html <- tagList(
        tags$p(strong("Copy summary")),
        tags$ul(lapply(msgs, tags$li))
      )
      if (nrow(missing) > 0) {
        miss_list <- lapply(seq_len(nrow(missing)), function(i) {
          tags$li(sprintf("%s — Not available", missing$filename[i]))
        })
        out_html <- tagAppendChildren(
          out_html,
          tags$p(class = "text-warning", strong("Missing files")),
          tags$ul(miss_list)
        )
      }
      output$copy_result <- renderUI(out_html)
    }, ignoreInit = TRUE)
    
    # Option 2: Combine & download ----
    # Build ordered list of existing files at time of download
    build_existing_order <- reactive({
      tbl <- preview_tbl()
      req(nrow(tbl) > 0)
      # Keep only existing ones, preserve order in preview_tbl()
      tbl$full_path[tbl$exists]
    })
    
    output$download_pdf <- downloadHandler(
      filename = function() {
        nm <- input$outfile_name
        if (!grepl("\\.pdf$", nm, ignore.case = TRUE)) nm <- paste0(nm, ".pdf")
        nm
      },
      content = function(file) {
        ord <- build_existing_order()
        validate(need(length(ord) > 0, "No available PDFs to combine."))
        # Combine
        qpdf::pdf_combine(input = ord, output = file)
      }
    )
  })
}
