library(shiny)
library(ggplot2)
library(dplyr)

# 資料清理函數
preprocess <- function(df) {
  df <- df %>%
    rename(
      district = 鄉鎮市區,
      land_area_m2 = 土地面積平方公尺,
      rent_date = 租賃年月日,
      floor = 租賃層次,
      building_type = 建物型態,
      price_per_m2 = 單價元平方公尺
    )

  df$rent_date <- as.Date(as.character(df$rent_date), format = "%Y%m%d")
  df$land_area_m2 <- as.numeric(gsub("[^0-9.]", "", df$land_area_m2))
  df$price_per_m2 <- as.numeric(gsub("[^0-9.]", "", df$price_per_m2))

  return(df)
}

# UI 介面
ui <- fluidPage(
  titlePanel("租金分析"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "上傳租金 CSV 檔案", accept = ".csv"),
      hr(),
      checkboxInput("show_model", "顯示線性回歸模型摘要", value = FALSE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("租金分布", plotOutput("histPlot")),
        tabPanel("平均租金（依行政區）", plotOutput("avgPlot")),
        tabPanel("模型摘要", verbatimTextOutput("modelOutput"))
      )
    )
  )
)

# Server 程式
server <- function(input, output) {
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath, fileEncoding = "UTF-8-BOM")
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

# 啟動 App
shinyApp(ui = ui, server = server)
