box::use(
  echarts4r,
  shiny[h3, moduleServer, NS, tagList],
  rhino[rhinos],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Chart"),
    echarts4r$echarts4rOutput(ns("chart"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$chart <- echarts4r$renderEcharts4r(
      rhinos |>
        echarts4r$group_by(Species) |>
        echarts4r$e_chart(x = Year) |>
        echarts4r$e_line(Population) |>
        echarts4r$e_x_axis(Year) |>
        echarts4r$e_tooltip()
    )
    
  "hello"  
  })
}

