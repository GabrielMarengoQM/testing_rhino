## Imports
# Packages
box::use(
  shiny[...],
  bslib[...],
  shinyWidgets[...]
)

# Modules
box::use(
  app/view/table_sidebar,
  app/view/meta_data_table,
)

# Rda data
box::use(
  app/logic/import_rda_data
  )

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  page_sidebar(
    # Sidebar
    sidebar = sidebar(
      width = 340,
      open = c('open'),
      table_sidebar$ui(ns("options_sidebar"))
    ),
    # Main content
    div(
      meta_data_table$ui(ns("meta_data_table"))
    )
  )

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    table_data <- import_rda_data$meta_data_table_data()
    selected_columns <- table_sidebar$server("options_sidebar")
    
    meta_data_table$server("meta_data_table", table_data, selected_columns)
  })
}