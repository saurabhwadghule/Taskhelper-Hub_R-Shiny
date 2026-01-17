# Load required libraries
library(shiny)
library(readxl)
library(dplyr)
library(DT)
library(writexl)
library(janitor)
library(purrr)
library(tibble)


message("Loaded nct_update.R")  # ✅ This confirms the file is sourced

# User interface layout
nct_ui <- function(id) {
  ns <- NS(id)  # namespace for input/output IDs
  fluidPage(
    theme = bslib::bs_theme(bootswatch = "cosmo"),
    titlePanel("New or Updated NCT IDs"),
    fluidRow(
      column(3,
             wellPanel(
               fileInput(ns("file1"), "Upload First File (Earlier)", accept = ".xlsx"),
               fileInput(ns("file2"), "Upload Second File (Recent)", accept = ".xlsx"),
               downloadButton(ns("download_new"), "Download New Entries"),
               downloadButton(ns("download_updated"), "Download Updated Entries")
             )
      ),
      column(9,
             h4("Summary"),
             verbatimTextOutput(ns("summary")),
             fluidRow(
               column(6, h4("New Entries"), DTOutput(ns("new_entries"))),
               column(6, h4("Updated Entries"), DTOutput(ns("updated_entries")))
             )
      )
    )
  )
}

#server logic
nct_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    data <- reactive({
      req(input$file1, input$file2)
      df1 <- read_excel(input$file1$datapath) %>% clean_names()
      df2 <- read_excel(input$file2$datapath) %>% clean_names()
      key <- "nct_number"
      if (!(key %in% colnames(df1)) || !(key %in% colnames(df2))) {
        stop("Both files must contain a column named 'NCT Number' (case-insensitive).")
      }
      list(df1 = df1, df2 = df2)
    })
  
  new_trials <- reactive({
    anti_join(data()$df2, data()$df1, by = "nct_number") %>% mutate(entry_type = "New")
  })
  
  updated_trials_long <- reactive({
    df1 <- data()$df1
    df2 <- data()$df2
    common_ids <- intersect(df1$nct_number, df2$nct_number)
    df1_common <- df1 %>% filter(nct_number %in% common_ids)
    df2_common <- df2 %>% filter(nct_number %in% common_ids)
    df1_common <- df1_common %>% select(sort(names(.)))
    df2_common <- df2_common %>% select(sort(names(.)))
    differences <- map2_dfr(1:nrow(df1_common), 1:nrow(df2_common), function(i, j) {
      row1 <- df1_common[i, ]
      row2 <- df2_common[j, ]
      diffs <- which(row1 != row2)
      if (length(diffs) == 0) return(NULL)
      tibble(
        nct_number = row1$nct_number,
        field = names(row1)[diffs],
        old = as.character(row1[1, diffs]),
        new = as.character(row2[1, diffs])
      )
    })
    differences %>% distinct() %>% mutate(entry_type = "Updated")
  })
  
  output$summary <- renderPrint({
    df1 <- data()$df1
    df2 <- data()$df2
    cat("Total in File 1:", nrow(df1), "\n")
    cat("Total in File 2:", nrow(df2), "\n")
    cat("New Entries:", nrow(new_trials()), "\n")
    cat("Updated Entries:", updated_trials_long() %>% pull(nct_number) %>% unique() %>% length(), "\n")
  })
  
  output$new_entries <- renderDT({
    datatable(new_trials(), options = list(scrollX = TRUE, scrollY = "300px", paging = TRUE), class = 'compact stripe')
  })
  
  output$updated_entries <- renderDT({
    datatable(updated_trials_long(), options = list(scrollX = TRUE, scrollY = "300px", paging = TRUE), class = 'compact stripe')
  })
  
  output$download_new <- downloadHandler(
    filename = function() { "new_nct_entries.xlsx" },
    content = function(file) {
      write_xlsx(new_trials(), file)
    }
  )
  
  output$download_updated <- downloadHandler(
    filename = function() { "updated_nct_entries.xlsx" },
    content = function(file) {
      write_xlsx(updated_trials_long(), file)
    }
  )
})
}



