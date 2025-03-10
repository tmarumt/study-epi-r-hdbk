library(shiny)

# ヒストグラムを描画するためのサーバーロジック関数を定義
server <- function(input, output, session) {
  malaria_plot <- reactive({
    plot_epicurve(malaria_data, district = input$select_district, agegroup = input$select_agegroup)
  })

  output$malaria_epicurve <- renderPlot(
    malaria_plot()
  )

  output$download_epicurve <- downloadHandler(
    filename = function() {
      stringr::str_glue("malaria_epicurve_{input$select_district}.png")
    },
    content = function(file) {
      ggsave(file,
        malaria_plot(),
        width = 8, height = 5, dpi = 300
      )
    }
  )
}
