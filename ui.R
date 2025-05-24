library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("租金分析"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sel_district", "行政區", choices = NULL),
      selectInput("sel_type", "建物型態", choices = NULL),
      numericInput("input_area", "土地面積（平方公尺）", value = 50)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("租金分布", plotOutput("histPlot")),
        tabPanel("平均租金（依行政區）", plotOutput("avgPlot")),
        tabPanel("模型預測", textOutput("modelPredictText")),
        tabPanel("模型預測熱力圖", plotOutput("modelPredictPlot")),
        tabPanel("資料集", textOutput("dataSummary"))
      )
    )
  )
)

