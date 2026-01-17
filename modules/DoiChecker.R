# Module/DoiChecker.R
# ─────────────────────────────────────────────────────────────────────────────
# DOI Checker module (STRICT): fast HEAD check → deep GET scan requiring markers
# Uses {curl} (no system curl dependency). Hosting-safe (shinyapps.io/Posit Connect).
# UI: Left upload + Start; Right summary cards + non-working table + downloads.
# ─────────────────────────────────────────────────────────────────────────────

message("Loaded DoiChecker.R")

# Required packages (ensure installed in Deploy.R or before app launch):
# shiny, readxl, writexl, curl, DT, dplyr

# DoiChecker.R — standalone Shiny app (STRICT)
# Module/DoiChecker.R
# Uses your working code as-is; only wrapped for module use.

library(shiny)
library(readxl)
library(dplyr)
library(DT)
library(writexl)
library(curl)

# ---- Core checker (STRICT) -- UNCHANGED ----
DoiScanner <- function(url) {
  if (is.na(url) || !nzchar(url)) return("not working")
  u <- trimws(as.character(url))
  if (!grepl("^https?://", u, ignore.case = TRUE)) {
    u <- paste0("https://doi.org/", gsub("^/*", "", u))
  }
  
  # Fast HEAD-like
  h <- curl::new_handle()
  curl::handle_setopt(h, followlocation = 1L, nobody = 1L,
                      connecttimeout = 15L, timeout = 15L,
                      ssl_verifyhost = 2L, ssl_verifypeer = 1L)
  head_ok <- FALSE
  try({
    res <- curl::curl_fetch_memory(u, handle = h)
    if (res$status_code >= 200 && res$status_code < 400) head_ok <- TRUE
  }, silent = TRUE)
  if (head_ok) return("working")
  
  # Deep GET (require tokens)
  h2 <- curl::new_handle()
  curl::handle_setopt(h2, followlocation = 1L, connecttimeout = 15L, timeout = 15L)
  ok <- FALSE
  try({
    res2 <- curl::curl_fetch_memory(u, handle = h2)
    if (res2$status_code >= 200 && res2$status_code < 400) {
      rawtxt <- tryCatch(rawToChar(res2$content), error = function(e) "")
      ok <- any(grepl("(<!DOCTYPE|<html|<meta|<title|article|pdf)", rawtxt, ignore.case = TRUE))
    }
  }, silent = TRUE)
  if (ok) "working" else "not working"
}

# ---- UI (namespaced) ----
doiCheckerUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("DOI checker (strict)"),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h3("Upload & Run"),
        fileInput(ns("file"), "Upload Excel (columns: PID, Link)", accept = c(".xlsx", ".xls")),
        actionButton(ns("start"), "Start checking", class = "btn btn-primary"),
        tags$hr(),
        p("Expected columns:"),
        tags$ul(
          tags$li(tags$code("PID")),
          tags$li(tags$code("Link"))
        ),
        tags$small("Bare DOIs (e.g., 10.xxxx/yyy) are auto-resolved via doi.org")
      ),
      mainPanel(
        width = 8,
        fluidRow(
          column(
            4,
            div(style="background:#f8f9fa;border-radius:12px;padding:14px;border:1px solid #eee;",
                h5("Total links", style="margin-top:0;"),
                h3(textOutput(ns("tot_links")), style="margin:0;"))
          ),
          column(
            4,
            div(style="background:#f6fff8;border-radius:12px;padding:14px;border:1px solid #e6f3ea;",
                h5("Working", style="margin-top:0;"),
                h3(textOutput(ns("ok_links")), style="margin:0;"))
          ),
          column(
            4,
            div(style="background:#fff6f6;border-radius:12px;padding:14px;border:1px solid #f3e6e6;",
                h5("Not working", style="margin-top:0;"),
                h3(textOutput(ns("bad_links")), style="margin:0;"))
          )
        ),
        tags$hr(),
        h4("Not working links"),
        DTOutput(ns("bad_table")),
        br(),
        fluidRow(
          column(6, downloadButton(ns("dl_bad"),  "Download non-working (.xlsx)", class = "btn btn-danger")),
          column(6, downloadButton(ns("dl_full"), "Download full results (.xlsx)",  class = "btn btn-secondary"))
        )
      )
    )
  )
}

# ---- Server (moduleServer wrapper; logic UNCHANGED) ----
doiCheckerServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    results <- reactiveVal(NULL)
    
    observeEvent(input$start, {
      req(input$file)
      dat <- tryCatch(readxl::read_excel(input$file$datapath), error = function(e) e)
      validate(
        need(!inherits(dat, "error"), "Could not read the Excel file."),
        need(all(c("PID", "Link") %in% names(dat)),
             "Input must have columns named 'PID' and 'Link'.")
      )
      
      dat <- dat %>% mutate(PID = as.character(PID), Link = as.character(Link))
      n <- nrow(dat); if (n == 0) { results(NULL); return() }
      
      withProgress(message = "Checking links…", value = 0, {
        chk <- character(n)
        for (i in seq_len(n)) {
          incProgress(1/n, detail = paste0("Checking ", i, " of ", n))
          chk[i] <- tryCatch(DoiScanner(dat$Link[i]), error = function(e) "not working")
        }
        dat$link_check_status <- chk
      })
      
      results(dat)
    }, ignoreInit = TRUE)
    
    output$tot_links <- renderText({ dat <- results(); if (is.null(dat)) "0" else nrow(dat) })
    output$ok_links  <- renderText({ dat <- results(); if (is.null(dat)) "0" else sum(dat$link_check_status == "working", na.rm = TRUE) })
    output$bad_links <- renderText({ dat <- results(); if (is.null(dat)) "0" else sum(dat$link_check_status != "working", na.rm = TRUE) })
    
    output$bad_table <- DT::renderDT({
      dat <- results(); if (is.null(dat)) return(NULL)
      bad <- dat %>% dplyr::filter(is.na(link_check_status) | link_check_status != "working") %>%
        dplyr::select(PID, Link, link_check_status)
      DT::datatable(bad, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
    })
    
    output$dl_bad <- downloadHandler(
      filename = function() paste0("doi_non_working_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
      content  = function(file) {
        dat <- results(); validate(need(!is.null(dat), "No results to download."))
        bad <- dat %>% dplyr::filter(is.na(link_check_status) | link_check_status != "working") %>%
          dplyr::select(PID, Link, link_check_status)
        writexl::write_xlsx(bad, path = file)
      }
    )
    output$dl_full <- downloadHandler(
      filename = function() paste0("doi_full_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
      content  = function(file) {
        dat <- results(); validate(need(!is.null(dat), "No results to download."))
        writexl::write_xlsx(dat, path = file)
      }
    )
  })
}
