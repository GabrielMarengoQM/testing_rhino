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
    
    # dpc_selected <- reactive({"JAX"}) # Initialize with a default value
    # show_cols <- reactive({c('DPCs studying Gene', 'Gene IDs', 'Mouse data', 'Disease data')})
    
    output$current_table_view <- renderUI({
      header <- 'Current view: '
      dpc_selected <- input$select_dpcs
      body <- dpc_selected
      body <- paste(body, collapse = ", ")
      
      HTML(glue("<p>{header} <br> {body}</p>"))
    })
    
    # observeEvent(input$select_dpcs, {
    #   dpc_selected <- input$select_dpcs
    # })
    # 
    # observeEvent(input$show_cols, {
    #   show_cols <- input$show_cols
    # })
    
    # list(
    #   dpc_selected = dpc_selected,
    #   show_cols = show_cols
    # )
    
    ###
    data_to_display1 <- reactiveVal()
    #example_data <- utils$read_data_from_database()
    
    observeEvent(input$select_dpcs, {
      # Updates the data_to_display reactive variable
      data_to_display1(
        input$select_dpcs
      )
    })
    
    # Returns the reactive
   
    #
    data_to_display2 <- reactiveVal()
    #example_data <- utils$read_data_from_database()
    
    observeEvent(input$show_cols, {
      # Updates the data_to_display reactive variable
      data_to_display2(
        input$show_cols
      )
    })
    
    # Returns the reactive
    list(
      data_to_display1,
      data_to_display2
    )
  })
}


