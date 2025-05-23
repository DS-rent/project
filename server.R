### here is shinyApp server, please put your render fuction here ###

library(shiny)
library(ggplot2)
library(dplyr)

server <- function(input, output) {
  data <- reactive({
    df <- read.csv("data/MOI_rent.csv", fileEncoding = "UTF-8-BOM")
    preprocess(df)
  })

  output$histPlot <- renderPlot({
    df <- data()
    ggplot(df %>% filter(!is.na(price_per_m2)), aes(x = price_per_m2)) +
      geom_histogram(binwidth = 50, fill = "skyblue", color = "white") +
      labs(title = "租金單價分佈", x = "元 / 平方公尺", y = "數量")
  })

  output$avgPlot <- renderPlot({
    df <- data()
    df %>%
      filter(!is.na(price_per_m2)) %>%
      group_by(district) %>%
      summarise(avg_price = mean(price_per_m2)) %>%
      ggplot(aes(x = reorder(district, avg_price), y = avg_price)) +
      geom_col(fill = "lightgreen") +
      coord_flip() +
      labs(title = "各行政區平均租金單價", x = "行政區", y = "元 / 平方公尺")
  })

  output$modelOutput <- renderPrint({
    req(input$show_model)
    df <- data()
    model <- lm(price_per_m2 ~ land_area_m2 + district + building_type, data = df)
    summary(model)
  })
}
