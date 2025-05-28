source("utils.R")

# Check package availability
leaflet_available <- "leaflet" %in% rownames(installed.packages()) && 
                     require("leaflet", character.only = TRUE, quietly = TRUE)
lubridate_available <- "lubridate" %in% rownames(installed.packages()) && 
                       require("lubridate", character.only = TRUE, quietly = TRUE)

server <- function(input, output, session) {
  
  # 載入並處理資料
  data <- reactive({
    df <- read.csv("data/MOI_rent.csv", fileEncoding = "UTF-8-BOM")
    preprocess(df)
  })
  
  # 篩選後的資料
  filtered_data <- reactive({
    df <- data()
    
    # 價格範圍篩選 (使用坪單價)
    df <- df %>% 
      filter(price_per_ping >= input$price_range[1] & price_per_ping <= input$price_range[2])
    
    # 面積範圍篩選 (使用坪面積)
    df <- df %>%
      filter(land_area_ping >= input$area_range[1] & land_area_ping <= input$area_range[2])
    
    # 行政區篩選
    if (!is.null(input$district_filter) && length(input$district_filter) > 0) {
      df <- df %>% filter(district %in% input$district_filter)
    }
    
    # 建物型態篩選
    if (!is.null(input$building_type_filter) && length(input$building_type_filter) > 0) {
      df <- df %>% filter(building_type %in% input$building_type_filter)
    }
    
    return(df)
  })
  
  # 更新篩選選項 - 使用完整資料
  observe({
    df <- data()
    
    updateSelectInput(session, "district_filter",
                     choices = sort(unique(df$district)),
                     selected = NULL)
    
    updateSelectInput(session, "building_type_filter", 
                     choices = sort(unique(df$building_type)),
                     selected = NULL)
  })
  
  # 更新功能特定選項 - 使用篩選後資料
  observe({
    df <- filtered_data()
    
    # 檢查資料是否足夠
    has_sufficient_data <- nrow(df) >= 5
    
    if (has_sufficient_data) {
      available_districts <- sort(unique(df$district))
      available_building_types <- sort(unique(df$building_type))
      
      updateSelectInput(session, "pred_district",
                       choices = available_districts,
                       selected = if(length(available_districts) > 0) available_districts[1] else NULL)
      
      updateSelectInput(session, "pred_building_type",
                       choices = available_building_types,
                       selected = if(length(available_building_types) > 0) available_building_types[1] else NULL)
      
      updateSelectInput(session, "desired_building_type",
                       choices = c("不限" = "", available_building_types))
    } else {
      # 資料不足時的處理
      updateSelectInput(session, "pred_district", choices = character(0))
      updateSelectInput(session, "pred_building_type", choices = character(0))
      updateSelectInput(session, "desired_building_type", choices = c("不限" = ""))
    }
  })
  
  # 資料狀態反應式值
  data_status <- reactive({
    df <- filtered_data()
    n_records <- nrow(df)
    n_districts <- length(unique(df$district))
    n_building_types <- length(unique(df$building_type))
    
    list(
      total_records = n_records,
      districts_count = n_districts,
      building_types_count = n_building_types,
      sufficient_for_prediction = n_records >= 10 && n_districts >= 2 && n_building_types >= 2,
      sufficient_for_recommendation = n_records >= 3,
      sufficient_for_analysis = n_records >= 1
    )
  })
  
  # 重置篩選
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "district_filter", selected = character(0))
    updateSelectInput(session, "building_type_filter", selected = character(0))
    updateSliderInput(session, "price_range", value = c(0, 6600))
    updateSliderInput(session, "area_range", value = c(0, 60))
  })
  
  # === 市場總覽頁面 ===
  
  # 統計指標卡片
  output$total_listings <- renderValueBox({
    valueBox(
      value = nrow(filtered_data()),
      subtitle = "總物件數",
      icon = icon("home"),
      color = "blue"
    )
  })
  
  output$avg_price <- renderValueBox({
    avg_price <- round(mean(filtered_data()$price_per_ping, na.rm = TRUE), 0)
    valueBox(
      value = paste(avg_price, "元/坪"),
      subtitle = "平均租金",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$median_price <- renderValueBox({
    median_price <- round(median(filtered_data()$price_per_ping, na.rm = TRUE), 0)
    valueBox(
      value = paste(median_price, "元/坪"),
      subtitle = "租金中位數",
      icon = icon("chart-line"),
      color = "yellow"
    )
  })
  
  output$price_trend <- renderValueBox({
    df <- filtered_data()
    if(nrow(df) > 1 && !all(is.na(df$rent_date))) {
      trend <- ifelse(cor(as.numeric(df$rent_date), df$price_per_m2, use = "complete.obs") > 0, "上升", "下降")
      color <- ifelse(trend == "上升", "red", "green")
    } else {
      trend <- "無趨勢"
      color <- "light-blue"
    }
    
    valueBox(
      value = trend,
      subtitle = "價格趨勢",
      icon = icon("chart-line"),
      color = color
    )
  })
  
  # 租金分布圖
  output$price_dist_plot <- renderPlotly({
    df <- filtered_data()
    
    p <- ggplot(df, aes(x = price_per_ping)) +
      geom_histogram(binwidth = 165, fill = "#3498db", alpha = 0.7, color = "white") +
      labs(title = "租金單價分布", x = "租金 (元/坪)", y = "物件數量") +
      theme_minimal()
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # 各行政區平均租金
  output$district_avg_plot <- renderPlotly({
    df <- filtered_data() %>%
      group_by(district) %>%
      summarise(avg_price = mean(price_per_ping, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(avg_price))
    
    p <- ggplot(df, aes(x = reorder(district, avg_price), y = avg_price)) +
      geom_col(fill = "#2ecc71", alpha = 0.8) +
      coord_flip() +
      labs(title = "各行政區平均租金", x = "行政區", y = "平均租金 (元/坪)") +
      theme_minimal()
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # 建物型態分析
  output$building_type_plot <- renderPlotly({
    df <- filtered_data() %>%
      group_by(building_type) %>%
      summarise(
        count = n(),
        avg_price = mean(price_per_m2, na.rm = TRUE),
        .groups = 'drop'
      )
    
    p <- ggplot(df, aes(x = reorder(building_type, count), y = count)) +
      geom_col(fill = "#f39c12", alpha = 0.8) +
      coord_flip() +
      labs(title = "建物型態統計", x = "建物型態", y = "物件數量") +
      theme_minimal()
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # 面積vs租金散點圖
  output$area_price_scatter <- renderPlotly({
    df <- filtered_data()
    
    p <- suppressMessages({
      ggplot(df, aes(x = land_area_ping, y = price_per_ping, color = district)) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = FALSE, color = "red") +
        labs(title = "面積 vs 租金關係", x = "土地面積 (坪)", y = "租金 (元/坪)") +
        theme_minimal() +
        theme(legend.position = "none")
    })
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # === 詳細分析頁面 ===
  
  # 租金箱型圖
  output$price_boxplot <- renderPlotly({
    df <- filtered_data()
    
    p <- ggplot(df, aes(x = reorder(district, price_per_ping, median), y = price_per_ping)) +
      geom_boxplot(fill = "#3498db", alpha = 0.7) +
      coord_flip() +
      labs(title = "各行政區租金分布", x = "行政區", y = "租金 (元/坪)") +
      theme_minimal()
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # 樓層分析
  output$floor_analysis <- renderPlotly({
    df <- filtered_data() %>%
      group_by(floor) %>%
      summarise(avg_price = mean(price_per_ping, na.rm = TRUE), count = n(), .groups = 'drop') %>%
      filter(count >= 3)
    
    p <- ggplot(df, aes(x = reorder(floor, avg_price), y = avg_price)) +
      geom_col(fill = "#9b59b6", alpha = 0.8) +
      coord_flip() +
      labs(title = "樓層 vs 平均租金", x = "樓層", y = "平均租金 (元/坪)") +
      theme_minimal()
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # 時間趨勢分析
  output$time_trend <- renderPlotly({
    df <- filtered_data()
    
    if(lubridate_available && !all(is.na(df$rent_date))) {
      df <- df %>%
        mutate(year_month = format(rent_date, "%Y-%m")) %>%
        group_by(year_month) %>%
        summarise(avg_price = mean(price_per_ping, na.rm = TRUE), .groups = 'drop')
      
      p <- ggplot(df, aes(x = year_month, y = avg_price)) +
        geom_line(group = 1, color = "#e74c3c", linewidth = 1) +
        geom_point(color = "#e74c3c", size = 2) +
        labs(title = "租金時間趨勢", x = "年月", y = "平均租金 (元/坪)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      # Fallback: simple district comparison
      df <- df %>%
        group_by(district) %>%
        summarise(avg_price = mean(price_per_ping, na.rm = TRUE), .groups = 'drop') %>%
        slice_head(n = 10)
      
      p <- ggplot(df, aes(x = reorder(district, avg_price), y = avg_price)) +
        geom_col(fill = "#e74c3c", alpha = 0.8) +
        coord_flip() +
        labs(title = "前10區域平均租金", x = "行政區", y = "平均租金 (元/坪)") +
        theme_minimal()
    }
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # 迴歸模型
  rental_model <- reactive({
    df <- filtered_data()
    
    # Check if we have enough data
    if(nrow(df) < 10) {
      return(NULL)
    }
    
    # Ensure factors have at least 2 levels to avoid contrasts error
    if(length(unique(df$district)) < 2 || 
       length(unique(df$building_type)) < 2 || 
       length(unique(df$floor)) < 2) {
      # Use simplified model with only numeric predictors
      tryCatch({
        lm(price_per_m2 ~ land_area_m2, data = df)
      }, error = function(e) {
        NULL
      })
    } else {
      # Full model if sufficient factor levels
      tryCatch({
        lm(price_per_m2 ~ land_area_m2 + district + building_type + floor, data = df)
      }, error = function(e) {
        # Fallback to simple model
        tryCatch({
          lm(price_per_m2 ~ land_area_m2, data = df)
        }, error = function(e2) {
          NULL
        })
      })
    }
  })
  
  output$model_summary <- renderPrint({
    model <- rental_model()
    status <- data_status()
    if(is.null(model)) {
      if(!status$sufficient_for_prediction) {
        cat("模型無法建立：資料不足（需要至少10筆記錄、2個行政區、2種建物型態）")
        cat("\n目前資料：", status$total_records, "筆記錄，", 
            status$districts_count, "個行政區，", 
            status$building_types_count, "種建物型態")
      } else {
        cat("模型無法建立：其他錯誤")
      }
    } else {
      summary(model)
    }
  })
  
  # === 價格預測頁面 ===
  
  predicted_price <- eventReactive(input$predict_btn, {
    model <- rental_model()
    status <- data_status()
    
    if(is.null(model)) {
      if(!status$sufficient_for_prediction) {
        return("資料不足：請調整篩選條件以獲得更多資料")
      }
      return("無法預測：模型建立失敗")
    }
    
    # 檢查輸入是否在可用選項中
    df <- filtered_data()
    if(!(input$pred_district %in% df$district)) {
      return("所選行政區在目前篩選條件下無資料")
    }
    
    if(!(input$pred_building_type %in% df$building_type)) {
      return("所選建物型態在目前篩選條件下無資料")
    }
    
    tryCatch({
      new_data <- data.frame(
        land_area_m2 = input$pred_area * PING_TO_M2,
        district = input$pred_district,
        building_type = input$pred_building_type,
        floor = input$pred_floor
      )
      
      prediction <- predict(model, new_data)
      prediction_ping <- prediction * PING_TO_M2
      round(prediction_ping, 0)
    }, error = function(e) {
      paste("預測失敗：", e$message)
    })
  })
  
  output$predicted_price <- renderText({
    result <- predicted_price()
    if(is.character(result)) {
      result
    } else {
      paste(result, "元/坪")
    }
  })
  
  output$model_performance <- renderPrint({
    model <- rental_model()
    status <- data_status()
    
    if(is.null(model)) {
      if(!status$sufficient_for_prediction) {
        cat("模型無法建立：資料不足\n")
        cat("需要：至少10筆記錄、2個行政區、2種建物型態\n")
        cat("目前：", status$total_records, "筆記錄，", 
            status$districts_count, "個行政區，", 
            status$building_types_count, "種建物型態")
      } else {
        cat("模型無法建立：其他錯誤")
      }
    } else {
      cat("R-squared:", round(summary(model)$r.squared, 3), "\n")
      cat("Adjusted R-squared:", round(summary(model)$adj.r.squared, 3), "\n")
      cat("RMSE:", round(sqrt(mean(model$residuals^2)), 2), "\n")
      cat("樣本數:", nrow(model$model), "\n")
      cat("模型變數:", paste(names(model$coefficients), collapse = ", "))
    }
  })
  
  output$prediction_accuracy_plot <- renderPlotly({
    model <- rental_model()
    
    if(is.null(model)) {
      status <- data_status()
      message <- if(!status$sufficient_for_prediction) {
        paste("資料不足\n需要更多資料建立模型\n目前：", status$total_records, "筆記錄")
      } else {
        "模型無法建立"
      }
      
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = message, size = 4) +
        theme_void()
      return(ggplotly(p) %>% config(displayModeBar = FALSE))
    }
    
    df <- data.frame(
      actual = model$model$price_per_m2,
      predicted = fitted(model)
    )
    
    p <- ggplot(df, aes(x = actual, y = predicted)) +
      geom_point(alpha = 0.6, color = "#3498db") +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      labs(title = "預測 vs 實際值", x = "實際租金", y = "預測租金") +
      theme_minimal()
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # === 地區推薦頁面 ===
  
  recommendation_result <- eventReactive(input$recommend_btn, {
    df <- filtered_data()
    status <- data_status()
    
    if(!status$sufficient_for_recommendation) {
      return(data.frame(
        訊息 = "資料不足：請調整篩選條件以獲得更多資料（至少需要3筆記錄）"
      ))
    }
    
    # 計算預估月租 = 單價 * 面積 (轉換坪到m²)
    df$estimated_monthly_rent <- df$price_per_ping * input$desired_area
    
    # 篩選符合預算的物件
    suitable <- df %>%
      filter(estimated_monthly_rent <= input$budget)
    
    if(nrow(suitable) == 0) {
      return(data.frame(
        訊息 = paste("沒有符合預算", input$budget, "元的物件，請提高預算或調整篩選條件")
      ))
    }
    
    # 如果有指定建物型態偏好
    if (!is.null(input$desired_building_type) && input$desired_building_type != "") {
      suitable <- suitable %>% filter(building_type == input$desired_building_type)
      
      if(nrow(suitable) == 0) {
        return(data.frame(
          訊息 = paste("沒有符合預算和建物型態偏好的物件")
        ))
      }
    }
    
    # 按行政區彙總推薦
    recommendation <- suitable %>%
      group_by(district, building_type) %>%
      summarise(
        物件數量 = n(),
        `平均單價 (元/坪)` = round(mean(price_per_ping), 0),
        預估月租 = round(mean(estimated_monthly_rent), 0),
        `平均面積 (坪)` = round(mean(land_area_ping), 1),
        .groups = 'drop'
      ) %>%
      arrange(預估月租) %>%
      rename(
        行政區 = district,
        建物型態 = building_type
      )
    
    return(recommendation)
  })
  
  output$recommendation_table <- renderDT({
    result <- recommendation_result()
    
    if("訊息" %in% names(result)) {
      # 顯示錯誤或提示訊息
      datatable(
        result,
        options = list(
          pageLength = 5,
          searching = FALSE,
          paging = FALSE,
          info = FALSE
        ),
        class = 'cell-border stripe'
      )
    } else {
      # 正常的推薦結果
      datatable(
        result,
        options = list(
          pageLength = 10,
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Chinese-traditional.json')
        ),
        class = 'cell-border stripe'
      ) %>%
        formatCurrency(c("平均單價 (元/坪)", "預估月租"), currency = "", interval = 3, mark = ",", digits = 0)
    }
  })
  
  # === 互動地圖頁面 ===
  
  if(leaflet_available) {
    output$rental_map <- renderLeaflet({
      df <- filtered_data() %>%
        group_by(district) %>%
        summarise(
          avg_price = mean(price_per_ping, na.rm = TRUE),
          count = n(),
          .groups = 'drop'
        )
      
      # 台北市各區大概位置（簡化版）
      district_coords <- data.frame(
        district = c("中正區", "大同區", "中山區", "萬華區", "信義區", 
                    "松山區", "大安區", "南港區", "北投區", "內湖區", 
                    "士林區", "文山區"),
        lat = c(25.032, 25.063, 25.063, 25.037, 25.033,
                25.050, 25.026, 25.055, 25.131, 25.082,
                25.088, 24.989),
        lng = c(121.518, 121.513, 121.533, 121.500, 121.565,
                121.578, 121.536, 121.607, 121.501, 121.589,
                121.526, 121.569)
      )
      
      map_data <- merge(df, district_coords, by = "district", all.x = TRUE)
      
      leaflet(map_data) %>%
        addTiles() %>%
        setView(lng = 121.54, lat = 25.05, zoom = 11) %>%
        addCircleMarkers(
          ~lng, ~lat,
          radius = ~sqrt(count) * 2,
          color = ~colorNumeric("YlOrRd", avg_price)(avg_price),
          fillOpacity = 0.7,
          popup = ~paste0(
            "<b>", district, "</b><br/>",
            "平均租金: ", round(avg_price, 0), " 元/坪<br/>",
            "物件數量: ", count
          )
        ) %>%
        addLegend(
          "bottomright",
          pal = colorNumeric("YlOrRd", map_data$avg_price),
          values = ~avg_price,
          title = "平均租金<br/>(元/坪)",
          opacity = 1
        )
    })
  }
  
  # === 原始資料頁面 ===
  
  output$raw_data_table <- renderDT({
    df <- filtered_data() %>%
      select(district, land_area_ping, rent_date, floor, building_type, price_per_ping) %>%
      rename(
        行政區 = district,
        `土地面積 (坪)` = land_area_ping,
        租賃日期 = rent_date,
        樓層 = floor,
        建物型態 = building_type,
        `單價 (元/坪)` = price_per_ping
      )
    
    datatable(
      df,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Chinese-traditional.json')
      ),
      class = 'cell-border stripe'
    ) %>%
      formatCurrency("單價 (元/坪)", currency = "", interval = 3, mark = ",", digits = 0) %>%
      formatRound("土地面積 (坪)", digits = 2)
  })
}
