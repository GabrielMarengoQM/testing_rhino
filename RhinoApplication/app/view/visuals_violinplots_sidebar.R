# visuals_violinplots_sidebar.R

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
    # GENE SEARCH
    searchInput(
      ns("search_gene"),
      label = "Search Gene Symbol",
      placeholder = "DLX1",
      btnSearch = icon("search"),
      btnReset = icon("remove"),
      width = "100%",
      value = "",
      resetValue = ""
    ),
    pickerInput(
      ns("select_dpcs_vis"),
      "Select Data Production Center",
      c("JAX", "MSK", "NWU", "UCSF"),
      multiple = TRUE,
      selected = "JAX",
      options = pickerOptions(
        actionsBox = TRUE,
        showTick = TRUE,
        width = "100%"
      ),
      inline = FALSE
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # reactive_value <- reactiveVal("")
    # 
    # observeEvent(input$search_gene, {
    #   reactive_value(input$search_gene)
    # })
    
    list(
      selected_dpc = reactive(input$select_dpcs_vis),
      # gene_search_input = reactive(reactive_value())
      #gene_search_input = reactive(input$search_gene)
      gene_search_input = reactive(input$search_gene)
    )
  })
}