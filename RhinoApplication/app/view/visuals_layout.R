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
            visuals_sidebar$ui(ns("visuals_sidebar1"))
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
server <- function(id, visuals_sidebar_data1) {
  moduleServer(id, function(input, output, session) {
    
    visuals_sidebar_data1 <- visuals_sidebar$server("visuals_sidebar1")
    # visuals_sidebar$server("visuals_sidebar2")
    # visuals_sidebar$server("visuals_sidebar3")
    # visuals_sidebar$server("visuals_sidebar4")
    
    getMousePlotsDataImpc <- reactive({
      
      gene_lists_for_plots <- generate_visuals$getGeneListsFromSelect_list(visuals_sidebar_data1())
      
      mouse_data_list <- list()
      for (i in gene_lists_for_plots) {
        mouse_data <- generate_visuals$getImpcPlotData(i)
        mouse_data_list <- c(mouse_data_list, list(mouse_data))
      }
      return(mouse_data_list)
    })
    
    output$impc_chart <- renderPlotly({
      plots <- getMousePlotsDataImpc()
      if (length(plots) == 1) {
        generate_visuals$generateImpcPlot(plots[[1]])
      } else if (length(plots) > 1) {
        #generateImpcPlot(plots[[2]])
        plots
        percentage_cols <- lapply(plots, function(plot) plot$percentage)
        
        # Bind the percentage column
        df <- data.frame(x_axis = c("lethal", "subviable", "viable"))
        df <- bind_cols(df, !!!percentage_cols)
        # Rename the columns
        #colnames(df) <- c("x_axis", paste0("percentage_set", 1:length(percentage_cols)))
        # Extract the second elements (list names) from gene_lists_for_plots
        gene_lists_for_plots <- generate_visuals$subsetGeneListsWithNames(visuals_sidebar_data1())
        
        list_names <- sapply(gene_lists_for_plots, function(x) x[[2]])
        
        # Create column names for the dataframe
        col_names <- c("x_axis", list_names)
        
        # Assign column names to your dataframe (replace df with your actual dataframe)
        colnames(df) <- col_names
        # set y_col as first value name for initial plotly obj
        y_col <- names(df)[2] # first value after xaxis column
        y_col
        p <- plot_ly(df, x = ~x_axis, y = as.formula(paste0("~", y_col)),
                     type = 'bar', name = y_col, textposition = 'outside', text = ~get(y_col)) %>%
          plotly::layout(yaxis = list(title = '% of genes'), xaxis = list(title = 'IMPC preweaning viability assessment'))
        p
        # set y_cols2 for rest of value names for traces
        y_cols1<- names(df)[-1]
        y_cols2 <- y_cols1[-1]
        # Add traces
        for (i in y_cols2) {
          text_col <- paste0("text_", i)  # New variable for dynamic text
          df[[text_col]] <- df[[i]]
          
          p <- p %>%
            add_trace(data = df, y = as.formula(paste0("~", i)), name = i, text = as.formula(paste0("~", text_col)))
        }
        # Print the resulting plot
        p
      }
    }) 

  })
}