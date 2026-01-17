# coming_soon.R
message("Loaded coming_soon.R")

# Server module for Coming Soon tab (no logic needed)
# modules/coming_soon.R
coming_soon_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-top: 50px; text-align: center;",
      h2("🚧 Coming Soon 🚧"),
      p("More features are under development and will appear here.")
    )
  )
}

coming_soon_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No logic yet
  })
}
