box::use(
  shiny[...],
  rhino,
  DT,
  htmltools[...],
  bslib[...],
  shinyWidgets[...],
  glue[glue],
  fst[read.fst],
  utils[...],
  dplyr[...],
  shinyjs[...]
  )

box::use(
  app/logic/import_rda_data[...]
)

genesMetaDataDf_data <- meta_data_table_data()

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    # uiOutput(
    #   ns("current_table_view")
    # ),
    # hr(),
    # textOutput(
    #   ns("select_data_table")
    # ),
    useShinyjs(),
    selectInput(
      ns("select_dpcs"),
      "Select Data Production Center",
      c("JAX", "MSK", "NWU", "UCSF"),
      multiple = TRUE,
      selected = c("JAX", "MSK", "NWU", "UCSF")
      # options = pickerOptions(
      #   actionsBox = TRUE,
      #   showTick = TRUE,
      #   width = "300px"
      # ),
      # inline = FALSE
    ),
    awesomeRadio(
      inputId = ns("unify_or_intersect_gene_lists"),
      label = "Show in table:",
      choices = c("Union",
                  "Intersection"),
      selected = "Union",
      inline = TRUE
    ),
    materialSwitch(
      inputId = ns("show_apcg"),
      label = "Display all protein coding genes",
      status = "primary",
      inline = FALSE
    ),
    pickerInput(
      ns("show_cols"),
      "Select meta data to display:",
      c('DPCs studying Gene', 'Gene IDs', 'Mouse data', 'Disease data', 'Cell line data',
        'Sequencing data', 'Pantherdb protein data', 'Gene Ontology data', 'Pathway data'),
      selected = c('DPCs studying Gene', 'Gene IDs', 'Mouse data', 'Disease data'),
      multiple = TRUE,
      options = pickerOptions(
        actionsBox = TRUE,
        showTick = TRUE,
        width = "300px"
      ),
      inline = FALSE
    ),
    downloadButton(ns("downloadData"), "Download all genes and metadata")
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {



    # output$current_table_view <- renderUI({
    #   header <- 'Current view: '
    #   dpc_selected <- input$select_dpcs
    #   body <- dpc_selected
    #   body <- paste(body, collapse = ", ")
    #
    #   HTML(glue("<p>{header} <br> {body}</p>"))
    # })

    observeEvent(input$show_apcg, {
      if (input$show_apcg == TRUE) {
        shinyjs::disable("select_dpcs")
        shinyjs::disable("unify_or_intersect_gene_lists")
      } else if (input$show_apcg == FALSE) {
        shinyjs::enable("select_dpcs")
        shinyjs::enable("unify_or_intersect_gene_lists")
      }
    })


    output$downloadData <- downloadHandler(
      filename = function() {
        paste("morphic-gene-list-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        # Example: Write a CSV file
        genesMetaDataDf_data2 <- genesMetaDataDf_data %>%
          filter(!is.na(DPCs_studying_gene))
        write.csv(genesMetaDataDf_data2, file)  # Replace 'mtcars' with your dataset
      }
    )
    outputOptions(output, "downloadData", suspendWhenHidden = FALSE)

    output$select_data_table <- renderText('Select Gene list:')

    list(
      selected_columns = reactive(input$show_cols),
      selected_dpc_gene_list = reactive(input$select_dpcs),
      show_union_or_intersection = reactive(input$unify_or_intersect_gene_lists),
      show_allpcg = reactive(input$show_apcg)
    )
  })
}


