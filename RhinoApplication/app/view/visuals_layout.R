## Imports
# Packages
box::use(
  shiny[...],
  bslib[...],
  shinyWidgets[...],
  glue[glue],
  plotly[...],
  dplyr[...],
  stats[...],
  DT[...],
  fst[read.fst]
)

# Modules
box::use(
  app/view[visuals_sidebar, visuals_violinplots_sidebar, gene_ontology_sidebar, reactome_sidebar],
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
      # nav_panel(
      #   "DDG2P",
      #   page_sidebar(
      #     # Sidebar
      #     sidebar = sidebar(
      #       width = "340px",
      #       open = c('open'),
      #       visuals_sidebar$ui(ns("visuals_sidebar4"))
      #     ),
      #     # Main content
      #     h3("DDG2P plots")
      #   )
      # ),
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
          fillable = FALSE,
          # Sidebar
          sidebar = sidebar(
            width = "340px",
            open = c('open'),
            visuals_violinplots_sidebar$ui(ns("sidebar_data_sequencing"))
          ),
          # Main content
          layout_column_wrap(
            width = 1/2,
            plotlyOutput(ns("gnomad_lof"), height = "400px"),
            plotlyOutput(ns("gnomad_mis"), height = "400px"),
            plotlyOutput(ns("shet_rgcme"), height = "400px"),
            plotlyOutput(ns("shet_posterior"), height = "400px"),
            plotlyOutput(ns("domino"), height = "400px"),
            plotlyOutput(ns("scones"), height = "400px"),
            plotlyOutput(ns("alpha_missense"), height = "400px")
          )
        ),
      ),
      nav_panel(
        "Cell Lines",
        page_sidebar(
          fillable = FALSE,
          # Sidebar
          sidebar = sidebar(
            width = "340px",
            open = c('open'),
            visuals_violinplots_sidebar$ui(ns("sidebar_data_cell_lines"))
          ),
          # Main content
          layout_column_wrap(
            width = 1/2,
            plotlyOutput(ns("depmap"), height = "400px"),
            plotlyOutput(ns("bf_mef"), height = "400px"),
            plotlyOutput(ns("bf_lam"), height = "400px")
          )
        ),
      ),
      nav_panel(
        shiny::icon("circle-info"),
        markdown("Learn more about [gnomad_lof & gnomad_mis](https://gnomad.broadinstitute.org/downloads#v4-constraint)")
      )
    ),
    navset_card_tab(
      height = 450,
      full_screen = TRUE,
      title = "Proteins",
      nav_panel(
        "Panther Protein Classes",
        page_sidebar(
          # Sidebar
          sidebar = sidebar(
            width = "340px",
            open = c('open'),
            visuals_sidebar$ui(ns("sidebar_data_panther_classes"))
          ),
          # Main content
          plotlyOutput(ns("panther_classes"))
        ),
      ),
      nav_panel(
        shiny::icon("circle-info"),
        markdown("Learn more about [PANTHERTdb](https://www.pantherdb.org/about.jsp)")
      )
    ),
    navset_card_tab(
      height = 450,
      full_screen = TRUE,
      title = "Upset Plot",
      nav_panel(
        "Upset",
        page_sidebar(
          # Sidebar
          sidebar = sidebar(
            width = "340px",
            open = c('open'),
            visuals_sidebar$ui(ns("sidebar_data_upset"))
          ),
          # Main content
          htmlOutput(ns("upsetPlot"))
        ),
      )
    ),
    navset_card_tab(
      height = 450,
      full_screen = TRUE,
      title = "Enrichment and Semantic Similarity Analysis",
      nav_panel(
        "Gene Ontology",
        page_sidebar(
          fillable = FALSE,
          # Sidebar
          sidebar = sidebar(
            width = "340px",
            open = c('open'),
            # NEW SIDEBAR WITHOUT ALL PROTEIN CODING GENES?
            gene_ontology_sidebar$ui(ns("sidebar_data_go"))
          ),
          # Main content

          h5("Semantic Similarity Analysis of Enriched Terms"),
          plotlyOutput(ns("go_semantic_similarity_plot"), height = "500px"),
          h5("Top 10 Enriched Terms"),
          DTOutput(ns("top_enriched_go_terms"))

        ),
      ),
      nav_panel(
        "Reactome",
        page_sidebar(
          fillable = FALSE,
          # Sidebar
          sidebar = sidebar(
            width = "340px",
            open = c('open'),
            reactome_sidebar$ui(ns("sidebar_data_reactome"))
          ),
          # Main content
          layout_column_wrap(
            width = 1,
            plotOutput(ns("reactome_enrichment_plot")),
            DTOutput(ns("top_enriched_reactome_terms"))
          )
        ),
      ),
      nav_panel(
        shiny::icon("circle-info"),
        markdown("")
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
    sidebar_input_panther_classes_barchart <- visuals_sidebar$server("sidebar_data_panther_classes")
    sidebar_input_upset <- visuals_sidebar$server("sidebar_data_upset")
    sidebar_input_go <- gene_ontology_sidebar$server("sidebar_data_go")
    sidebar_input_reactome <- reactome_sidebar$server("sidebar_data_reactome")
    sidebar_input_sequencing_violinplots <- visuals_violinplots_sidebar$server("sidebar_data_sequencing")
    sidebar_input_cell_lines_violinplots <- visuals_violinplots_sidebar$server("sidebar_data_cell_lines")

    # Get gene list data and meta data
    gene_list_data <- import_rda_data$morphic_gene_list_data()
    meta_data <- import_rda_data$visuals_data()
    constraint_metrics_plot_data <- import_rda_data$constraint_metrics()
    go_scatter_plots <- import_rda_data$go_scatter_plots()
    go_top_enriched_terms_tables <- import_rda_data$go_top_enriched_terms_tables()
    # reactome_enrichment_plots <- import_rda_data$reactome_enrichment_plots()
    # reactome_enrichment_tables <- import_rda_data$reactome_enrichment_tables()
    reactome_enrichment_all <- import_rda_data$reactome_enrichment_all()

    output$impc_chart <- renderPlotly({
      req(!is.null(sidebar_input_impc_barchart()))

      gene_lists_for_plots <- generate_visuals$getDataFromUserSelect(sidebar_input_impc_barchart(), gene_list_data)
      generate_visuals$generateImpcBarchart(gene_lists_for_plots, meta_data)

    }) %>%
      bindCache(sidebar_input_impc_barchart())

    output$mgi_chart <- renderPlotly({
      req(!is.null(sidebar_input_mgi_barchart()))

      # Generate plot data frames
      gene_lists_for_plots <- generate_visuals$getDataFromUserSelect(sidebar_input_mgi_barchart(), gene_list_data)
      generate_visuals$generateMgiBarchart(gene_lists_for_plots, meta_data)

    }) %>%
      bindCache(sidebar_input_mgi_barchart())

    output$omim_chart <- renderPlotly({
      req(!is.null(sidebar_input_omim_barchart()))

      # Generate plot data frames
      gene_lists_for_plots <- generate_visuals$getDataFromUserSelect(sidebar_input_omim_barchart(), gene_list_data)
      generate_visuals$generateHasOmimPlot(gene_lists_for_plots, meta_data)

    }) %>%
      bindCache(sidebar_input_omim_barchart())

    output$omim_lethality_chart <- renderPlotly({
      req(!is.null(sidebar_input_omim_barchart()))

      # Generate plot data frames
      gene_lists_for_plots <- generate_visuals$getDataFromUserSelect(sidebar_input_omim_barchart(), gene_list_data)
      generate_visuals$generateOmimLethalityPlot(gene_lists_for_plots, meta_data)

    }) %>%
      bindCache(sidebar_input_omim_barchart())

    #  Reactive variables for Violin plot options
    # Sequencing
    toggle_data_points_sequencing <- reactive({
      sidebar_input_sequencing_violinplots$show_all_data_points()
    })

    gene_list_selection_sequencing <- reactive({
      sidebar_input_sequencing_violinplots$selected_dpc()
    })

    highlighted_genes_sequencing <- reactive({
      sidebar_input_sequencing_violinplots$gene_search_input()
    })

    # Cell lines
    toggle_data_points_cell_lines <- reactive({
      sidebar_input_cell_lines_violinplots$show_all_data_points()
    })

    gene_list_selection_cell_lines <- reactive({
      sidebar_input_cell_lines_violinplots$selected_dpc()
    })

    highlighted_genes_cell_lines <- reactive({
      sidebar_input_cell_lines_violinplots$gene_search_input()
    })

    # Sequencing metrics ----
    output$gnomad_lof <- renderPlotly({
      req(!is.null(gene_list_selection_sequencing()))
      column <- 'lof_oe'

      data <- constraint_metrics_plot_data
      gene_lists <- generate_visuals$getDataFromUserSelect(gene_list_selection_sequencing(), gene_list_data)
      plot_data <- generate_visuals$getViolinPlotData(data, column, gene_lists)
      genes_to_highlight <- unlist(strsplit(highlighted_genes_sequencing(), ","))

      generate_visuals$renderViolinPlot(plot_data, column, genes_to_highlight, 0.35, toggle_data_points_sequencing(),
                                        "LOEUF score (gnomAD)")

    }) %>%
      bindCache(
        c(
          gene_list_selection_sequencing(),
          highlighted_genes_sequencing(),
          toggle_data_points_sequencing()
          )
        )

    output$gnomad_mis <- renderPlotly({
      req(!is.null(gene_list_selection_sequencing()))
      data <- constraint_metrics_plot_data
      column <- 'mis_oe'
      gene_lists <- generate_visuals$getDataFromUserSelect(gene_list_selection_sequencing(), gene_list_data)
      plot_data <- generate_visuals$getViolinPlotData(data, column, gene_lists)
      genes_to_highlight <- unlist(strsplit(highlighted_genes_sequencing(), ","))

      generate_visuals$renderViolinPlot(plot_data, column, genes_to_highlight, NULL, toggle_data_points_sequencing(),
                                        "Missense observed/expected (gnomAD)")

    }) %>%
      bindCache(
        c(
          gene_list_selection_sequencing(),
          highlighted_genes_sequencing(),
          toggle_data_points_sequencing()
        )
      )

    output$shet_rgcme <- renderPlotly({
      req(!is.null(gene_list_selection_sequencing()))
      data <- constraint_metrics_plot_data
      column <- 'mean'
      gene_lists <- generate_visuals$getDataFromUserSelect(gene_list_selection_sequencing(), gene_list_data)
      plot_data <- generate_visuals$getViolinPlotData(data, column, gene_lists)
      genes_to_highlight <- unlist(strsplit(highlighted_genes_sequencing(), ","))

      generate_visuals$renderViolinPlot(plot_data, column, genes_to_highlight, 0.075, toggle_data_points_sequencing(),
                                        "Shet (RGC-ME dataset)")

    }) %>%
      bindCache(
        c(
          gene_list_selection_sequencing(),
          highlighted_genes_sequencing(),
          toggle_data_points_sequencing()
        )
      )

    output$shet_posterior <- renderPlotly({
      req(!is.null(gene_list_selection_sequencing()))
      data <- constraint_metrics_plot_data
      column <- 'post_mean'
      gene_lists <- generate_visuals$getDataFromUserSelect(gene_list_selection_sequencing(), gene_list_data)
      plot_data <- generate_visuals$getViolinPlotData(data, column, gene_lists)
      genes_to_highlight <- unlist(strsplit(highlighted_genes_sequencing(), ","))

      generate_visuals$renderViolinPlot(plot_data, column, genes_to_highlight, 0.1, toggle_data_points_sequencing(),
                                        "Shet (gnomAD dataset)")

    }) %>%
      bindCache(
        c(
          gene_list_selection_sequencing(),
          highlighted_genes_sequencing(),
          toggle_data_points_sequencing()
        )
      )

    output$domino <- renderPlotly({
      req(!is.null(gene_list_selection_sequencing()))
      data <- constraint_metrics_plot_data
      column <- 'DOMINO'
      gene_lists <- generate_visuals$getDataFromUserSelect(gene_list_selection_sequencing(), gene_list_data)
      plot_data <- generate_visuals$getViolinPlotData(data, column, gene_lists)
      genes_to_highlight <- unlist(strsplit(highlighted_genes_sequencing(), ","))

      generate_visuals$renderViolinPlot(plot_data, column, genes_to_highlight, NULL, toggle_data_points_sequencing(),
                                        "DOMINO score")

    }) %>%
      bindCache(
        c(
          gene_list_selection_sequencing(),
          highlighted_genes_sequencing(),
          toggle_data_points_sequencing()
        )
      )

    output$scones <- renderPlotly({
      req(!is.null(gene_list_selection_sequencing()))
      data <- constraint_metrics_plot_data
      column <- 'SCoNeS'
      gene_lists <- generate_visuals$getDataFromUserSelect(gene_list_selection_sequencing(), gene_list_data)
      plot_data <- generate_visuals$getViolinPlotData(data, column, gene_lists)
      genes_to_highlight <- unlist(strsplit(highlighted_genes_sequencing(), ","))

      generate_visuals$renderViolinPlot(plot_data, column, genes_to_highlight, NULL, toggle_data_points_sequencing(),
                                        "SCoNeS score")

    }) %>%
      bindCache(
        c(
          gene_list_selection_sequencing(),
          highlighted_genes_sequencing(),
          toggle_data_points_sequencing()
        )
      )

    output$alpha_missense <- renderPlotly({
      req(!is.null(gene_list_selection_sequencing()))
      data <- constraint_metrics_plot_data
      column <- 'mean_am_pathogenicity'
      gene_lists <- generate_visuals$getDataFromUserSelect(gene_list_selection_sequencing(), gene_list_data)
      plot_data <- generate_visuals$getViolinPlotData(data, column, gene_lists)
      genes_to_highlight <- unlist(strsplit(highlighted_genes_sequencing(), ","))

      generate_visuals$renderViolinPlot(plot_data, column, genes_to_highlight, NULL, toggle_data_points_sequencing(),
                                        "Mean Alpha Missense pathogenicity score")

    }) %>%
      bindCache(
        c(
          gene_list_selection_sequencing(),
          highlighted_genes_sequencing(),
          toggle_data_points_sequencing()
        )
      )

    # Cell line metrics ----
    output$depmap <- renderPlotly({
      req(!is.null(gene_list_selection_cell_lines()))
      data <- constraint_metrics_plot_data
      column <- 'mean_score_all'
      gene_lists <- generate_visuals$getDataFromUserSelect(gene_list_selection_cell_lines(), gene_list_data)
      plot_data <- generate_visuals$getViolinPlotData(data, column, gene_lists)
      genes_to_highlight <- unlist(strsplit(highlighted_genes_cell_lines(), ","))

      generate_visuals$renderViolinPlot(plot_data, column, genes_to_highlight, NULL, toggle_data_points_cell_lines(),
                                        "Mean gene effect score (DepMap)")

    }) %>%
      bindCache(
        c(
          gene_list_selection_cell_lines(),
          highlighted_genes_cell_lines(),
          toggle_data_points_cell_lines()
        )
      )

    output$bf_mef <- renderPlotly({
      req(!is.null(gene_list_selection_cell_lines()))
      data <- constraint_metrics_plot_data
      column <- 'bf_mef'
      gene_lists <- generate_visuals$getDataFromUserSelect(gene_list_selection_cell_lines(), gene_list_data)
      plot_data <- generate_visuals$getViolinPlotData(data, column, gene_lists)
      genes_to_highlight <- unlist(strsplit(highlighted_genes_cell_lines(), ","))

      generate_visuals$renderViolinPlot(plot_data, column, genes_to_highlight, NULL, toggle_data_points_cell_lines(),
                                        "Bayes Factor (Mouse Embryonic Feeder cells growth substrate)")

    }) %>%
      bindCache(
        c(
          gene_list_selection_cell_lines(),
          highlighted_genes_cell_lines(),
          toggle_data_points_cell_lines()
        )
      )

    output$bf_lam <- renderPlotly({
      req(!is.null(gene_list_selection_cell_lines()))
      data <- constraint_metrics_plot_data
      column <- 'bf_lam'
      gene_lists <- generate_visuals$getDataFromUserSelect(gene_list_selection_cell_lines(), gene_list_data)
      plot_data <- generate_visuals$getViolinPlotData(data, column, gene_lists)
      genes_to_highlight <- unlist(strsplit(highlighted_genes_cell_lines(), ","))

      generate_visuals$renderViolinPlot(plot_data, column, genes_to_highlight, NULL, toggle_data_points_cell_lines(),
                                        "Bayes Factor (Laminin growth substrate)")

    }) %>%
      bindCache(
        c(
          gene_list_selection_cell_lines(),
          highlighted_genes_cell_lines(),
          toggle_data_points_cell_lines()
        )
      )

    output$panther_classes <- renderPlotly({
      req(!is.null(sidebar_input_panther_classes_barchart()))
      gene_lists <- generate_visuals$getDataFromUserSelect(sidebar_input_panther_classes_barchart(), gene_list_data)
      generate_visuals$getPantherPlots(meta_data, gene_lists)
    }) %>%
      bindCache(
        sidebar_input_panther_classes_barchart()
      )

    output$upsetPlot <- renderUI({
      req(!is.null(sidebar_input_upset()))
      gene_lists <- generate_visuals$getDataFromUserSelect(sidebar_input_upset(), gene_list_data)
      htmltools::browsable(generate_visuals$generateUpsetR(gene_lists))
    }) %>%
      bindCache(
        sidebar_input_upset()
      )

    output$go_semantic_similarity_plot <- renderPlotly({
      if (sidebar_input_go$ontology() == "CC" && sidebar_input_go$dpc_selected() == "JAX") {
        NULL
      } else {
        plot <- generate_visuals$renderGoScatterPlot(go_scatter_plots, sidebar_input_go$ontology(), sidebar_input_go$dpc_selected(), sidebar_input_go$show_legend())
        plotly::ggplotly(plot)
      }
    }) %>%
      bindCache(
        c(
          sidebar_input_go$ontology(),
          sidebar_input_go$dpc_selected(),
          sidebar_input_go$show_legend()
        )
      )

    # How to go about this - only single choice? instead of multiple options
    output$top_enriched_go_terms <- renderDT({
      table <- generate_visuals$renderGoTable(go_top_enriched_terms_tables, sidebar_input_go$ontology(), sidebar_input_go$dpc_selected())

      datatable(
        table,
        options = list(
          searching = FALSE,  # Remove search bar
          lengthChange = FALSE,  # Remove number of results per page
          paging = FALSE  # Remove pagination controls
        )
      )

    })

    output$reactome_enrichment_plot <- renderPlot({
      generate_visuals$renderReactomeEnrichmentPlot(reactome_enrichment_all, sidebar_input_reactome$dpc_selected_reactome())
    }) %>%
      bindCache(
        sidebar_input_reactome$dpc_selected_reactome()
      )


    output$top_enriched_reactome_terms <- renderDT({
      table <- generate_visuals$renderReactomeTable(reactome_enrichment_all, sidebar_input_reactome$dpc_selected_reactome())

      datatable(
        table,
        options = list(
          searching = FALSE,  # Remove search bar
          lengthChange = FALSE,  # Remove number of results per page
          paging = FALSE  # Remove pagination controls
        )
      )
      })



  })
}
