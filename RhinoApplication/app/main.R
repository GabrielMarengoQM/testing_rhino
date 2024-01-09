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
  page_navbar(
    title = "MorPhiC",
    bg = "#0062cc",
    nav_panel(
      title = "Gene List Browser", 
      page_sidebar(
        # Sidebar
        sidebar = sidebar(
          width = "340px",
          open = c('open'),
          table_sidebar$ui(ns("options_sidebar"))
        ),
        # Main content
        meta_data_table$ui(ns("meta_data_table"))
      )
    ),
    nav_panel(
      title = "Visualisations", 
      h3("vis")),
    nav_panel(
      title = "Metadata Information",
      h3("metadata")
    ),
    nav_spacer(),
    nav_menu(
      title = "Links",
      align = "right",
      nav_item(tags$a(href = 'https://morphic.bio/', "MorPhiC Home Page"))
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    table_data <- import_rda_data$meta_data_table_data()
    sidebar_data <- table_sidebar$server("options_sidebar")
    
    meta_data_table$server("meta_data_table", table_data, sidebar_data)
    
  })
}