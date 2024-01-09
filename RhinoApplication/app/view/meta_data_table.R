box::use(
  shiny[...],
  rhino,
  DT[...],
  shinycssloaders[...],
  htmltools[...],
)

box::use(
  app/view/table_sidebar,
  app/logic/table_manage,
  app/logic/import_rda_data,
  app/logic/create_table
)


#' @export
ui <- function(id) {
  ns <- NS(id)
    withSpinner(
      DTOutput(
        ns("genesMetaDataDf")
      ),
      type = getOption("spinner.type", default = 1),
      color = getOption("spinner.color", default = "#0275D8"),
      size = getOption("spinner.size", default = 1),
      color.background = getOption("spinner.color.background"),
      custom.css = FALSE,
      proxy.height = NULL,
      id = NULL,
      image = NULL,
      image.width = NULL,
      image.height = NULL,
      hide.ui = TRUE
    )
}

#' @export
server <- function(id, table_data, selected_columns) {
  moduleServer(id, function(input, output, session) {
    
    output$genesMetaDataDf <- renderDataTable({
      headers <- create_table$table_headers()
      selected_columns <- table_manage$getHiddenColumns(selected_columns())
      
      create_table$data_table(table_data, headers, selected_columns)
    })

    
  })
}

