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
  app/view/visuals_sidebar,
  app/view/visuals_layout
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
    # Gene List Browser tab
    nav_panel(
      title = "Gene List Browser", 
      page_sidebar(
        # Sidebar
        sidebar = sidebar(
          width = "340px",
          open = c('open'),
          table_sidebar$ui(ns("table_sidebar"))
        ),
        # Main content
        meta_data_table$ui(ns("meta_data_table"))
      )
    ),
    # Visualisations tab
    nav_panel(
      title = "Visualisations", 
      visuals_layout$ui(ns("visuals_layout"))
      ),
    # Metadata Information tab
    nav_panel(
      title = "Metadata Information",
      h3("metadata")
    ),
    nav_spacer(),
    # Links
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
    
    # Gene List Browser tab
    table_data <- import_rda_data$meta_data_table_data()
    sidebar_data <- table_sidebar$server("table_sidebar")
    meta_data_table$server("meta_data_table", table_data, sidebar_data)
    
    # Visualisations tab
    visuals_sidebar_data1 <- visuals_sidebar$server("visuals_sidebar1")
    # visuals_sidebar_data2 <- visuals_sidebar$server("visuals_sidebar2")
    # visuals_sidebar_data3 <- visuals_sidebar$server("visuals_sidebar3")
    # visuals_sidebar_data4 <- visuals_sidebar$server("visuals_sidebar4")
    visuals_layout$server("visuals_layout", visuals_sidebar_data1)
  })
}