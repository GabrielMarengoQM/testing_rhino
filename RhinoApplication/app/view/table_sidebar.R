box::use(
  shiny[...],
  rhino,
  DT,
  htmltools[...],
  bslib[...],
  shinyWidgets[...],
  glue[glue]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(
      ns("current_table_view")
    ),
    hr(),
    textOutput(
      ns("select_data_table")
    ),
    dropdown(label = "MorPhiC Gene lists",
             icon = icon("table"),
             style = "bordered", 
             status = "primary", 
             width = "300px",
             size =  "sm",
             animate = animateOptions(
               enter = animations$fading_entrances$fadeInLeftBig,
               exit = animations$fading_exits$fadeOutLeft
             ),
             selectInput(
               ns("select_dpcs"), 
               "Select Data Production Center", 
               c("JAX", "MSK", "NWU", "UCSF"),
               multiple = TRUE,
               selected = "JAX"
             )
    ),
    hr(),
    checkboxGroupInput(
      ns("show_cols"),
      "Select meta data to display:",
      c('DPCs studying Gene', 'Gene IDs', 'Mouse data', 'Disease data', 'Cell line data - gene constraint metrics', 
        'Sequencing data - gene constraint metrics', 'Pantherdb protein data', 'Gene Ontology data', 'Pathway data'), 
      selected = c('DPCs studying Gene', 'Gene IDs', 'Mouse data', 'Disease data'),
      inline = FALSE
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$current_table_view_header <- renderUI({
      header <- 'Current view:'
    })
    
    output$select_data_table <- renderText('Select Gene list:')

    output$current_table_view <- renderUI({
      header <- 'Current view: '
      dpc_selected <- input$select_dpcs
      body <- dpc_selected
      body <- paste(body, collapse = ", ")
      
      HTML(glue("<p>{header} <br> {body}</p>"))
    })
    
    list(
      selected_columns = reactive(input$show_cols),
      selected_dpc_gene_list = reactive(input$select_dpcs)
    )
  })
}


