library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("租金分析"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("show_model", "顯示線性回歸模型摘要", value = TRUE)
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
