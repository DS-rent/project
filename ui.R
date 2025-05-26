source("utils.R")

# Check if leaflet is available
leaflet_available <- "leaflet" %in% rownames(installed.packages())

header <- dashboardHeader(
  title = "台灣租屋市場分析平台",
  titleWidth = 300
)

sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(
    menuItem("市場總覽", tabName = "overview", icon = icon("chart-line")),
    menuItem("詳細分析", tabName = "analysis", icon = icon("chart-bar")),
    menuItem("價格預測", tabName = "prediction", icon = icon("calculator")),
    menuItem("地區推薦", tabName = "recommendation", icon = icon("map-marked-alt")),
    if(leaflet_available) menuItem("互動地圖", tabName = "map", icon = icon("map")),
    menuItem("原始資料", tabName = "data", icon = icon("table"))
  ),
  
  hr(),
  
  h4("篩選條件", style = "color: white; margin-left: 15px;"),
  p("注意：篩選條件會影響所有功能的可用選項", 
    style = "color: #cccccc; margin-left: 15px; font-size: 12px; margin-bottom: 10px;"),
  
  selectInput(
    "district_filter",
    "行政區:",
    choices = NULL,
    multiple = TRUE,
    width = "90%"
  ),
  
  selectInput(
    "building_type_filter", 
    "建物型態:",
    choices = NULL,
    multiple = TRUE,
    width = "90%"
  ),
  
  sliderInput(
    "price_range",
    "租金單價範圍 (元/坪):",
    min = 0,
    max = 16500,
    value = c(0, 6600),
    step = 165,
    width = "90%"
  ),
  
  sliderInput(
    "area_range", 
    "面積範圍 (坪):",
    min = 0,
    max = 151,
    value = c(0, 60),
    step = 3,
    width = "90%"
  ),
  
  br(),
  actionButton(
    "reset_filters",
    "重置篩選",
    icon = icon("refresh"),
    class = "btn-warning",
    style = "margin-left: 15px;"
  )
)

interactive_map_tab <- if(leaflet_available) {
  list(tabItem(
    tabName = "map",
    fluidRow(
      box(
        title = "台北市租金熱點地圖", 
        status = "primary", 
        solidHeader = TRUE,
        width = 12,
        
        leafletOutput("rental_map", height = "600px")
      )
    )
  ))
} else {
  list()
}

body <- dashboardBody(
  tags$head(
    tags$style(HTML("
      .content-wrapper, .right-side {
        background-color: #f4f4f4;
      }
      .box {
        box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
      }
      .small-box {
        border-radius: 5px;
      }
      .nav-tabs-custom > .nav-tabs > li.active {
        border-top-color: #3c8dbc;
      }
      .data-status {
        background-color: #f0f0f0;
        padding: 8px;
        margin: 5px 0;
        border-radius: 4px;
        font-size: 11px;
        color: #666;
      }
      .status-sufficient {
        background-color: #d4edda;
        color: #155724;
        border: 1px solid #c3e6cb;
      }
      .status-warning {
        background-color: #fff3cd;
        color: #856404;
        border: 1px solid #ffeaa7;
      }
      .status-error {
        background-color: #f8d7da;
        color: #721c24;
        border: 1px solid #f5c6cb;
      }
    "))
  ),
  
  do.call(tabItems, c(
    list(
      # 市場總覽頁面
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("total_listings", width = 3),
          valueBoxOutput("avg_price", width = 3),
          valueBoxOutput("median_price", width = 3),
          valueBoxOutput("price_trend", width = 3)
        ),
        
        fluidRow(
          box(
            title = "租金單價分布", 
            status = "primary", 
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("price_dist_plot")
          ),
          box(
            title = "各行政區平均租金", 
            status = "success", 
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("district_avg_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "建物型態分析", 
            status = "info", 
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("building_type_plot")
          ),
          box(
            title = "面積 vs 租金關係", 
            status = "warning", 
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("area_price_scatter")
          )
        )
      ),
      
      # 詳細分析頁面  
      tabItem(
        tabName = "analysis",
        fluidRow(
          box(
            title = "租金箱型圖 (依行政區)", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("price_boxplot", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "樓層分析", 
            status = "success", 
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("floor_analysis")
          ),
          box(
            title = "時間趨勢分析", 
            status = "info", 
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("time_trend")
          )
        ),
        
        fluidRow(
          box(
            title = "迴歸模型摘要", 
            status = "warning", 
            solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("model_summary")
          )
        )
      ),
      
      # 價格預測頁面
      tabItem(
        tabName = "prediction",
        fluidRow(
          div(class = "col-md-12",
            div(class = "alert alert-info",
              icon("info-circle"), 
              strong(" 使用說明："), 
              "預測功能會根據目前的篩選條件動態調整可用選項。如果選項較少，請調整左側篩選條件以獲得更多資料。"
            )
          )
        ),
        
        fluidRow(
          box(
            title = "租金預測器", 
            status = "primary", 
            solidHeader = TRUE,
            width = 6,
            
            selectInput("pred_district", "選擇行政區:", choices = NULL),
            div(id = "district_status", class = "data-status"),
            
            selectInput("pred_building_type", "選擇建物型態:", choices = NULL),
            div(id = "building_type_status", class = "data-status"),
            
            numericInput("pred_area", "土地面積 (坪):", value = 15, min = 0.3, max = 303),
            selectInput("pred_floor", "選擇樓層:", 
                       choices = c("一層", "二層", "三層", "四層", "五層", "全", "地下層")),
            
            br(),
            actionButton("predict_btn", "預測租金", 
                        icon = icon("calculator"), 
                        class = "btn-primary btn-lg"),
            
            br(), br(),
            
            div(id = "prediction_result",
                h4("預測結果:", style = "color: #3c8dbc;"),
                h2(textOutput("predicted_price"), style = "color: #00a65a;"),
                p("元/坪", style = "color: #666;")
            )
          ),
          
          box(
            title = "預測準確度", 
            status = "success", 
            solidHeader = TRUE,
            width = 6,
            
            h4("模型效能指標:"),
            verbatimTextOutput("model_performance"),
            
            br(),
            
            h4("預測 vs 實際值:"),
            plotlyOutput("prediction_accuracy_plot")
          )
        )
      ),
      
      # 地區推薦頁面
      tabItem(
        tabName = "recommendation",
        fluidRow(
          div(class = "col-md-12",
            div(class = "alert alert-info",
              icon("info-circle"), 
              strong(" 使用說明："), 
              "推薦功能會在目前篩選條件範圍內尋找符合預算的物件。建議先調整篩選條件以縮小搜尋範圍。"
            )
          )
        ),
        
        fluidRow(
          box(
            title = "預算推薦器", 
            status = "primary", 
            solidHeader = TRUE,
            width = 4,
            
            numericInput("budget", "月租預算 (元):", value = 20000, min = 5000, max = 100000, step = 1000),
            numericInput("desired_area", "期望面積 (坪):", value = 9, min = 3, max = 61),
            selectInput("desired_building_type", "偏好建物型態:", choices = NULL),
            
            br(),
            actionButton("recommend_btn", "尋找推薦", 
                        icon = icon("search"), 
                        class = "btn-success btn-lg"),
            
            br(), br(),
            div(id = "recommendation_status", class = "data-status",
                "點擊「尋找推薦」開始搜尋...")
          ),
          
          box(
            title = "推薦結果", 
            status = "success", 
            solidHeader = TRUE,
            width = 8,
            
            DT::dataTableOutput("recommendation_table")
          )
        )
      ),
      
      # 原始資料頁面
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title = "租賃資料瀏覽", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            
            p(class = "text-muted", 
              "顯示根據左側篩選條件過濾後的資料。資料會即時更新。"),
            
            DT::dataTableOutput("raw_data_table")
          )
        )
      )
    ),
    
    interactive_map_tab
  ))
)

ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body,
  skin = "blue"
)
