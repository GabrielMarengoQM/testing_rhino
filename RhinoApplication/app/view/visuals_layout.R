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
  app/view[visuals_sidebar, visuals_violinplots_sidebar],
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
            visuals_sidebar$ui(ns("sidebar_data_mgi"))
          ),
          # Main content
          plotlyOutput(ns("mgi_chart"))
        )
      ),
      nav_panel(
        shiny::icon("circle-info"),
        markdown("Learn more about [IMPC](https://www.mousephenotype.org/)"),
        markdown("Learn more about [MGI](https://www.informatics.jax.org/)")
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
            visuals_sidebar$ui(ns("sidebar_data_omim"))
          ),
          # Main content
          layout_column_wrap(
            width = 1/2, 
            plotlyOutput(ns("omim_chart")),
            plotlyOutput(ns("omim_lethality_chart"))
          )
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
        markdown("Learn more about [OMIM](https://www.omim.org/help/about)")
      )
    ),
    navset_card_tab(
      height = 450,
      full_screen = TRUE,
      title = "Gene constraint metrics",
      nav_panel(
        "Sequencing",
        page_sidebar(
          # Sidebar
          sidebar = sidebar(
            width = "340px",
            open = c('open'),
            visuals_violinplots_sidebar$ui(ns("sidebar_data_sequencing"))
          ),
          # Main content
          layout_column_wrap(
            width = 1/2,
            plotlyOutput(ns("gnomad_lof"))
          )
        ),
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
    sidebar_input_mgi_barchart <- visuals_sidebar$server("sidebar_data_mgi")
    sidebar_input_omim_barchart <- visuals_sidebar$server("sidebar_data_omim")
    sidebar_input_sequencing_violinplots <- visuals_violinplots_sidebar$server("sidebar_data_sequencing")
    
    # Get gene list data and meta data
    gene_list_data <- import_rda_data$dpc_gene_list_data()
    meta_data <- import_rda_data$visuals_data()
    constraint_metrics_plot_data <- import_rda_data$constraint_metrics()
    
    output$impc_chart <- renderPlotly({
      req(!is.null(sidebar_input_impc_barchart()))
      
      # Generate plot data frames
      gene_lists_for_plots <- generate_visuals$getDataFromUserSelect(sidebar_input_impc_barchart(), gene_list_data)
      plots <- generate_visuals$getImpcPlotData(gene_lists_for_plots, meta_data)
      
      # Generate bar charts
      if (length(plots) == 1) {
        generate_visuals$generateImpcPlot(plots[[1]])
      } else {
        generate_visuals$generateMultipleTracesImpcPlot(plots, gene_lists_for_plots)
      }
    }) %>%
      bindCache(sidebar_input_impc_barchart())
    
    output$mgi_chart <- renderPlotly({
      req(!is.null(sidebar_input_mgi_barchart()))
      
      # Generate plot data frames
      gene_lists_for_plots <- generate_visuals$getDataFromUserSelect(sidebar_input_mgi_barchart(), gene_list_data)
      plots <- generate_visuals$getMgiPlotData(gene_lists_for_plots, meta_data)
      
      # Generate bar charts
      if (length(plots) == 1) {
        generate_visuals$generateMgiPlot(plots[[1]])
      } else {
        generate_visuals$generateMultipleTracesMgiPlot(plots, gene_lists_for_plots)
      }
    }) %>%
      bindCache(sidebar_input_mgi_barchart())
    
    output$omim_chart <- renderPlotly({
      req(!is.null(sidebar_input_omim_barchart()))
      
      # Generate plot data frames
      gene_lists_for_plots <- generate_visuals$getDataFromUserSelect(sidebar_input_omim_barchart(), gene_list_data)
      plots_data <- generate_visuals$getHasOmimPlotData(gene_lists_for_plots, meta_data)

      # Generate bar charts
      if (length(plots_data) == 1) {
        generate_visuals$generateHasOmimPlot(plots_data[[1]])
      } else {
        generate_visuals$generateMultipleTracesHasOmimPlot(plots_data, gene_lists_for_plots)
      }
    }) %>%
      bindCache(sidebar_input_omim_barchart())
    
    output$omim_lethality_chart <- renderPlotly({
      req(!is.null(sidebar_input_omim_barchart()))
      
      # Generate plot data frames
      gene_lists_for_plots <- generate_visuals$getDataFromUserSelect(sidebar_input_omim_barchart(), gene_list_data)
      plots_data <- generate_visuals$getOmimLethalityPlotData(gene_lists_for_plots, meta_data)

      # Generate bar charts
      if (length(plots_data) == 1) {
        generate_visuals$generateOmimLethalityPlot(plots_data[[1]])
      } else {
        generate_visuals$generateMultipleTracesOmimLethalityPlot(plots_data, gene_lists_for_plots)
      }
    }) %>%
      bindCache(sidebar_input_omim_barchart())
    
    output$gnomad_lof <- renderPlotly({
      req(!is.null(sidebar_input_sequencing_violinplots$selected_dpc()))
      data <- constraint_metrics_plot_data
      column <- 'lof_oe'
      gene_lists <- generate_visuals$getDataFromUserSelect(sidebar_input_sequencing_violinplots$selected_dpc(), gene_list_data)
      plot_data <- generate_visuals$getViolinPlotData(data, column, gene_lists)
      genes_to_highlight <- unlist(strsplit(sidebar_input_sequencing_violinplots$gene_search_input(), ","))

      generate_visuals$renderViolinPlot(plot_data, column, genes_to_highlight)
      
    }) %>%
      bindCache(c(sidebar_input_sequencing_violinplots$selected_dpc(), sidebar_input_sequencing_violinplots$gene_search_input()))
 
    

  })
}