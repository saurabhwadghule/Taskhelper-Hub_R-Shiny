# app.R
# install.packages(c("shiny","readxl","dplyr","tidyr","DT","janitor","writexl"))

library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(DT)
library(janitor)
library(writexl)

message("Loaded nct_update.R")  # ✅ This confirms the file is sourced

veeva_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    theme = bslib::bs_theme(bootswatch = "cosmo"),
    titlePanel("Veeva IDs"),
    fluidRow(
      column(
        3,
        wellPanel(
          fileInput(ns("file1"), "Upload First File (Earlier)", accept = c(".xlsx",".xls",".csv")),
          fileInput(ns("file2"), "Upload Second File (Recent)", accept = c(".xlsx",".xls",".csv")),
          checkboxInput(ns("trim_ws"), "Trim whitespace before comparing", TRUE),
          checkboxInput(ns("blank_eq_na"), "Treat blanks and NA as equal", TRUE),
          downloadButton(ns("download_new"),     "Download New IDs"),
          downloadButton(ns("download_updated"), "Download Updated IDs (diffs)"),
          downloadButton(ns("download_expired"), "Download Expired IDs")
        )
      ),
      column(
        9,
        h4("Summary"),
        verbatimTextOutput(ns("summary")),
        tags$hr(),
        fluidRow(
          column(
            6,
            h4("New IDs"),
            DTOutput(ns("new_entries"))
          ),
          column(
            6,
            h4("Updated IDs (old vs new by column)"),
            DTOutput(ns("updated_entries"))
          )
        ),
        tags$hr(),
        h4("Expired IDs (Expiration Date < Today)"),
        DTOutput(ns("expired_entries"))
      )
    )
  )
}

veeva_server <- function(id) {
    moduleServer(id, function(input, output, session) {
  
  # ---- read & normalize ----
  read_any <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx","xls")) {
    suppressMessages(read_excel(path))
  } else if (ext == "csv") {
    read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  } else {
    stop("Upload .xlsx, .xls or .csv")
  }
}
  
  normalize_df <- function(df, trim_ws = TRUE, blank_eq_na = TRUE) {
    df <- df %>% clean_names()              # "ID" -> "id", "Expiration Date" -> "expiration_date", etc.
    df <- df %>% mutate(across(everything(), as.character))
    if (trim_ws) df <- df %>% mutate(across(everything(), ~ trimws(.)))
    if (blank_eq_na) df <- df %>% mutate(across(everything(), ~ ifelse(. == "", NA, .)))
    df
  }
  
  data <- reactive({
    req(input$file1, input$file2)
    df1 <- normalize_df(read_any(input$file1$datapath), input$trim_ws, input$blank_eq_na)
    df2 <- normalize_df(read_any(input$file2$datapath), input$trim_ws, input$blank_eq_na)
    validate(need("document_number" %in% names(df1) && "document_number" %in% names(df2),
                  "Both files must contain a column named 'Document Number' (any case)."))
    list(df1 = df1, df2 = df2)
  })
  
  # ---- new IDs ----
  new_trials <- reactive({
    anti_join(data()$df2, data()$df1 %>% select(document_number), by = "document_number") %>%
      mutate(entry_type = "New")
  })
  
  # ---- updated IDs: cell-wise diffs for common columns ----
  updated_trials_long <- reactive({
    df1 <- data()$df1
    df2 <- data()$df2
    
    # keep only columns present in BOTH files
    common_cols <- intersect(names(df1), names(df2))
    common_cols <- setdiff(common_cols, "document_number")
    
    # join by document_number to align rows, add .old/.new suffixes
    jn <- inner_join(
      df1 %>% select(document_number, all_of(common_cols)),
      df2 %>% select(document_number, all_of(common_cols)),
      by = "document_number", suffix = c(".old", ".new")
    )
    
    # build a long table of diffs (one row per ID x Column where a value changed)
    diffs <- bind_rows(lapply(common_cols, function(col) {
      tibble(
        document_number = jn$document_number,
        field = col,
        old   = jn[[paste0(col, ".old")]],
        new   = jn[[paste0(col, ".new")]]
      )
    })) %>%
      mutate(
        old_comp = ifelse(is.na(old),  NA_character_, old),
        new_comp = ifelse(is.na(new),  NA_character_, new)
      ) %>%
      filter( is.na(old_comp) != is.na(new_comp) | (!is.na(old_comp) & old_comp != new_comp) ) %>%
      select(document_number, field, old, new) %>%
      arrange(document_number, field) %>%
      mutate(entry_type = "Updated")
    
    diffs
  })
  
  # ---- expired IDs (from recent file) ----
  expired_ids <- reactive({
    df2 <- data()$df2
    validate(need("expiration_date" %in% names(df2),
                  "Recent file must contain a column named 'Expiration Date'"))
    
    # try to coerce expiration_date into Date
    d <- suppressWarnings(as.Date(df2$expiration_date, origin = "1899-12-30"))
    need <- is.na(d)
    if (any(need)) {
      d[need] <- suppressWarnings(as.Date(
        df2$expiration_date[need],
        tryFormats = c("%Y-%m-%d","%m/%d/%Y","%d/%m/%Y")
      ))
    }
    
    df2 %>%
      mutate(expiration_date = d) %>%
      filter(!is.na(expiration_date) & expiration_date < Sys.Date())
  })
  
  # ---- summary ----
  output$summary <- renderPrint({
    df1 <- data()$df1; df2 <- data()$df2
    cat("Total in Earlier file:", nrow(df1), "\n")
    cat("Total in Recent file :", nrow(df2), "\n")
    cat("New IDs             :", nrow(new_trials()), "\n")
    cat("Updated IDs         :", updated_trials_long() |> pull(document_number) |> unique() |> length(), "\n")
    if ("expiration_date" %in% names(df2)) {
      cat("Expired (in Recent) :", nrow(expired_ids()), "\n")
    }
  })
  
  # ---- tables ----
  output$new_entries <- DT::renderDT({
    DT::datatable(new_trials(),
                  options = list(scrollX = TRUE, scrollY = "300px", paging = TRUE),
                  class = 'compact stripe', rownames = FALSE)
  })
  
  output$updated_entries <- DT::renderDT({
    DT::datatable(updated_trials_long(),
                  options = list(scrollX = TRUE, scrollY = "300px", paging = TRUE),
                  class = 'compact stripe', rownames = FALSE)
  })
  
  output$expired_entries <- DT::renderDT({
    df <- expired_ids() %>%
      mutate(across(everything(),
                    \(x) if (inherits(x,"Date")) format(x,"%Y-%m-%d") else as.character(x)))
    datatable(df,
              options = list(scrollX = TRUE, scrollY = "300px", paging = TRUE),
              class = 'compact stripe', rownames = FALSE)
  })
  
  # ---- downloads ----
  output$download_new <- downloadHandler(
    filename = function() "new_ids.xlsx",
    content  = function(file) writexl::write_xlsx(new_trials(), path = file)
  )
  output$download_updated <- downloadHandler(
    filename = function() "updated_ids_diffs.xlsx",
    content  = function(file) writexl::write_xlsx(updated_trials_long(), path = file)
  )
  output$download_expired <- downloadHandler(
    filename = function() "expired_ids.xlsx",
    content  = function(file) writexl::write_xlsx(expired_ids(), path = file)
  )
})
}

shinyApp(veeva_ui, veeva_server)