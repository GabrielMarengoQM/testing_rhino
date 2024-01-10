## Imports
# Packages
box::use(
  shiny[...],
  bslib[...],
  shinyWidgets[...],
  glue[glue]
)

# Modules
box::use(
)

# Rda data
box::use(
  app/logic/import_rda_data
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(
      ns("current_visuals_view")
    ),
    hr(),
    textOutput(
      ns("select_visuals_view")
    ),
    dropdown(label = "MorPhiC Gene lists",
             width = "300px",
             animate = animateOptions(
               enter = animations$fading_entrances$fadeInLeftBig,
               exit = animations$fading_exits$fadeOutLeft
             ),
             selectInput(
               ns("select_dpcs_vis"), 
               "Select Data Production Center", 
               c("JAX", "MSK", "NWU", "UCSF"),
               multiple = TRUE,
               selected = "JAX"
             )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$current_visuals_view <- renderUI({
      header <- 'Current view: '
      dpc_selected <- input$select_dpcs_vis
      body <- dpc_selected
      body <- paste(body, collapse = ", ")
      
      HTML(glue("<p>{header} <br> {body}</p>"))
    })
    
    output$select_visuals_view <- renderText('Select Gene list:')
    
    visuals_sidebar_data <- reactive(input$select_dpcs_vis)
  })
}