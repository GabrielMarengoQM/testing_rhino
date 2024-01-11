## Imports
# Packages
box::use(
  shiny[...],
  bslib[...],
  shinyWidgets[...],
  glue[glue],
  plotly[...],
  dplyr[...],
  stats[...]
)

# Modules
box::use(
  app/view/visuals_sidebar,
  app/logic/generate_visuals
)

# Rda data
box::use(
  app/logic/import_rda_data,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  layout_column_wrap(
    width = 1/2, 
    navset_card_tab(
      height = 450,
      full_screen = TRUE,
      title = "Mouse models data",
      nav_panel(
        "IMPC",
        page_sidebar(
          # Sidebar
          sidebar = sidebar(
            width = "340px",
            open = c('open'),
            visuals_sidebar$ui(ns("sidebar_data_impc"))
          ),
          # Main content
          plotlyOutput(ns("impc_chart"))
        )
      ),
      nav_panel(
        "MGI",
        page_sidebar(
          # Sidebar
          sidebar = sidebar(
            width = "340px",
            open = c('open'),
            visuals_sidebar$ui(ns("visuals_sidebar2"))
          ),
          # Main content
          h3("mgi plots")
        )
      ),
      nav_panel(
        shiny::icon("circle-info"),
        markdown("Learn more about [htmlwidgets](http://www.htmlwidgets.org/)")
      )
    ),
    navset_card_tab(
      height = 450,
      full_screen = TRUE,
      title = "Disease Data",
      nav_panel(
        "OMIM",
        page_sidebar(
          # Sidebar
          sidebar = sidebar(
            width = "340px",
            open = c('open'),
            visuals_sidebar$ui(ns("visuals_sidebar3"))
          ),
          # Main content
          h3("omim plots")
        )
      ),
      nav_panel(
        "DDG2P",
        page_sidebar(
          # Sidebar
          sidebar = sidebar(
            width = "340px",
            open = c('open'),
            visuals_sidebar$ui(ns("visuals_sidebar4"))
          ),
          # Main content
          h3("DDG2P plots")
        )
      ),
      nav_panel(
        shiny::icon("circle-info"),
        markdown("Learn more about [htmlwidgets](http://www.htmlwidgets.org/)")
      )
    ),
    navset_card_tab(
      height = 450,
      full_screen = TRUE,
      title = "HTML Widgets",
      nav_panel(
        "Plotly",
        card_title("A plotly plot"),
        h3("info")
      ),
      nav_panel(
        "Leaflet",
        card_title("A leaflet plot"),
        h3("plot")
      ),
      nav_panel(
        shiny::icon("circle-info"),
        markdown("Learn more about [htmlwidgets](http://www.htmlwidgets.org/)")
      )
    ),
    navset_card_tab(
      height = 450,
      full_screen = TRUE,
      title = "HTML Widgets",
      nav_panel(
        "Plotly",
        card_title("A plotly plot"),
        h3("info")
      ),
      nav_panel(
        "Leaflet",
        card_title("A leaflet plot"),
        h3("plot")
      ),
      nav_panel(
        shiny::icon("circle-info"),
        markdown("Learn more about [htmlwidgets](http://www.htmlwidgets.org/)")
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Get sidebar inputs
    sidebar_input_impc_barchart <- visuals_sidebar$server("sidebar_data_impc")
    
    # Get gene list data and meta data
    gene_list_data <- import_rda_data$dpc_gene_list_data()
    meta_data <- import_rda_data$visuals_data()
    
    output$impc_chart <- renderPlotly({
      # Generate plot data frames
      gene_lists_for_plots <- generate_visuals$getDataFromUserSelect(sidebar_input_impc_barchart(), gene_list_data)
      plots <- generate_visuals$getImpcPlotData(gene_lists_for_plots, meta_data)
      
      # Generate bar charts
      if (length(plots) == 1) {
        generate_visuals$generateImpcPlot(plots[[1]])
      } else if (length(plots) > 1) {
        generate_visuals$generateMultipleTracesImpcPlot(plots, gene_lists_for_plots)
      }
    }) 

  })
}