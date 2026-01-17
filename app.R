library(shiny)
library(bslib)
library(here)
library(shinycssloaders)

# Load modular UIs and servers
source("modules/nct_update.R")
source("modules/veeva_update.R")
source("modules/coming_soon.R")
source("Modules/DoiChecker.R")
source("modules/pdf_compiler.R")
source("modules/reference_folder.R")

# UI with tabs
ui <- fluidPage(
  theme = bs_theme(bootswatch = "cosmo"),
  titlePanel("Clinical Tools Portal"),
  tabsetPanel(
    type = "tabs",
    tabPanel("NCT Update", nct_ui("nct")),
    tabPanel("Veeva Resource Update", veeva_ui("veeva")),
    tabPanel("DOI Checker", doiCheckerUI("doi")), 
    tabPanel("PDF Compiler", pdf_compiler_ui("pdfc")),
    tabPanel("Reference folder", reference_folder_ui("refmod")),
    tabPanel("Coming Soon",   coming_soon_ui("coming"))
  )
)

# Server
server <- function(input, output, session) {
  nct_server("nct")
  veeva_server("veeva")
  doiCheckerServer("doi") 
  coming_soon_server("coming")
  pdf_compiler_server("pdfc")
  reference_folder_server("refmod")
}

# Run app
shinyApp(ui, server)
