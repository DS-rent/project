library(shiny)
library(dplyr)
library(caret)
library(caretEnsemble)
library(ggplot2)
library(shinyjs)
library(DT)
library(plotly)
library(leaflet)
library(lubridate)

source("utils.R")

server <- function(input, output, session) {
  shinyjs::useShinyjs()

  # --- Centralized Reactive Values ---
  data <- reactiveVal()
  trained_model <- reactiveVal(NULL)
  prediction_result <- reactiveVal(NULL)
  predict_after_train <- reactiveVal(FALSE)

  # --- Initial Data Load ---
  observe({
    df <- read.csv("data/all_cleaned.csv", fileEncoding = "UTF-8-BOM")
    processed_df <- preprocess(df)
    data(processed_df)
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

    # 年份時間篩選 (使用西元紀年)
    df <- df %>%
      filter(lubridate::year(converted_date) >= input$year_range[1] & lubridate::year(converted_date) <= input$year_range[2])

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
      selected = NULL
    )

    updateSelectInput(session, "building_type_filter",
      choices = sort(unique(df$building_type)),
      selected = NULL
    )
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
        selected = if (length(available_districts) > 0) available_districts[1] else NULL
      )

      updateSelectInput(session, "pred_building_type",
        choices = available_building_types,
        selected = if (length(available_building_types) > 0) available_building_types[1] else NULL
      )

      updateSelectInput(session, "desired_building_type",
        choices = c("不限" = "", available_building_types)
      )
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
    if (nrow(df) > 1 && !all(is.na(df$converted_date))) {
      trend <- ifelse(cor(as.numeric(df$converted_date), df$price_per_ping, use = "complete.obs") > 0, "上升", "下降")
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
      summarise(avg_price = mean(price_per_ping, na.rm = TRUE), .groups = "drop") %>%
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
        avg_price = mean(price_per_ping, na.rm = TRUE),
        .groups = "drop"
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

    # Base plot without color aesthetic
    p <- ggplot(df, aes(x = land_area_ping, y = price_per_ping)) +
      # Apply color aesthetic only to the points
      geom_point(alpha = 0.6, aes(color = district)) +
      labs(title = "面積 vs 租金關係", x = "土地面積 (坪)", y = "租金 (元/坪)") +
      theme_minimal() +
      theme(legend.position = "none")

    # Conditionally add a single smoothing line for the entire dataset
    if (nrow(df) > 1) {
      p <- p + geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "red")
    }

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
      summarise(avg_price = mean(price_per_ping, na.rm = TRUE), count = n(), .groups = "drop") %>%
      filter(count >= 3) %>%
      filter(!(floor %in% c("全", "見其他登記事項")))

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

    if (!all(is.na(df$converted_date))) {
      df <- df %>%
        mutate(
          year_month = format(converted_date, "%Y-%m"),
          year_month_date = as.Date(paste0(year_month, "-01"))
        ) %>%
        group_by(year_month) %>%
        summarise(avg_price = mean(price_per_ping, na.rm = TRUE), .groups = "drop") %>%
        arrange(year_month)

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
        summarise(avg_price = mean(price_per_ping, na.rm = TRUE), .groups = "drop") %>%
        slice_head(n = 10)

      p <- ggplot(df, aes(x = reorder(district, avg_price), y = avg_price)) +
        geom_col(fill = "#e74c3c", alpha = 0.8) +
        coord_flip() +
        labs(title = "前10區域平均租金", x = "行政區", y = "平均租金 (元/坪)") +
        theme_minimal()
    }

    ggplotly(p) %>% config(displayModeBar = FALSE)
  })

  # --- Model Training ---
  observeEvent(input$retrain_model_analysis, {
    df <- filtered_data()
    
    # Guard against insufficient data
    status <- data_status()
    if (!status$sufficient_for_prediction) {
      trained_model(list(model = NULL, error = "資料不足，無法訓練模型。請調整篩選條件。"))
      return()
    }

    id <- showNotification("模型訓練中，請稍候...", duration = NULL, type = "message")
    on.exit(removeNotification(id))

    # --- Dynamic Formula Generation ---
    removed_vars <- c()
    base_formula <- "price_per_ping ~ land_area_ping"
    
    if (length(unique(df$district)) > 1) {
      base_formula <- paste(base_formula, "+ district")
    } else {
      removed_vars <- c(removed_vars, "district")
    }
    
    if (length(unique(df$building_type)) > 1) {
      base_formula <- paste(base_formula, "+ building_type")
    } else {
      removed_vars <- c(removed_vars, "building_type")
    }
    
    if (length(unique(df$floor)) > 1) {
      base_formula <- paste(base_formula, "+ floor")
    } else {
      removed_vars <- c(removed_vars, "floor")
    }
    
    if (length(unique(df$converted_date)) > 1) {
      base_formula <- paste(base_formula, "+ converted_date")
    } else {
      removed_vars <- c(removed_vars, "converted_date")
    }

    final_formula <- as.formula(base_formula)
    
    df_model <- df %>%
      select(any_of(c("price_per_ping", "land_area_ping", "district", "building_type", "floor", "converted_date"))) %>%
      na.omit()

    ctrl <- trainControl(method = "cv", number = 5, savePredictions = "final", classProbs = FALSE, verboseIter = FALSE, allowParallel = TRUE)

    tryCatch({
      model <- caretStack(
        caretList(final_formula, data = df_model, trControl = ctrl, methodList = c("lm", "rf", "knn")),
        method = "glmnet",
        trControl = trainControl(method = "cv", number = 5, savePredictions = "final")
      )
      trained_model(list(model = model, formula = final_formula, removed_vars = removed_vars, error = NULL))
    }, error = function(e) {
      trained_model(list(model = NULL, formula = final_formula, removed_vars = removed_vars, error = e$message))
    })
  })

  output$model_status_text <- renderUI({
    model_obj <- trained_model()
    req(model_obj) # Wait until the model object exists

    if (!is.null(model_obj$error)) {
      tags$div(class = "alert alert-danger", icon("times-circle"), strong("模型狀態:"), model_obj$error)
    } else if (!is.null(model_obj$model)) {
      if (length(model_obj$removed_vars) > 0) {
        tags$div(class = "alert alert-warning", icon("exclamation-triangle"), strong("模型已訓練 (已適應):"), 
                 paste("因資料變異性不足，已自動排除變數:", paste(model_obj$removed_vars, collapse = ", ")))
      } else {
        tags$div(class = "alert alert-success", icon("check-circle"), strong("模型已訓練"), "所有變數皆已納入模型。")
      }
    } else {
      tags$div(class = "alert alert-info", icon("info-circle"), "模型尚未訓練。請點擊「更新模型」按鈕。")
    }
  })

  output$model_summary <- renderPrint({
    model_obj <- trained_model()
    req(model_obj, model_obj$model) # Guard
    
    cat("--- 模型配方 ---\n")
    print(model_obj$formula)
    cat("\n--- 模型摘要 ---\n")
    print(model_obj$model)
  })

  # === 價格預測頁面 ===

  # Helper function for prediction logic
  run_prediction <- function() {
    model_obj <- trained_model()
    req(model_obj, model_obj$model)

    model <- model_obj$model
    
    # Input validation
    df <- filtered_data()
    if (!(input$pred_district %in% df$district)) {
      prediction_result("預測失敗: 所選行政區在目前篩選條件下無資料。")
      return()
    }
    if (!(input$pred_building_type %in% df$building_type)) {
      prediction_result("預測失敗: 所選建物型態在目前篩選條件下無資料。")
      return()
    }

    tryCatch({
      new_data <- data.frame(
        land_area_ping = input$pred_area,
        district = input$pred_district,
        building_type = input$pred_building_type,
        floor = input$pred_floor
      )
      
      for (col in c("district", "building_type", "floor")) {
        if (col %in% names(model$model$xlevels)) {
          new_data[[col]] <- factor(new_data[[col]], levels = model$model$xlevels[[col]])
        }
      }

      prediction <- predict(model, new_data)
      prediction_result(paste(round(prediction, 0), "元/坪"))
    }, error = function(e) {
      prediction_result(paste("預測失敗:", e$message))
    })
  }

  # Observer for the predict button
  observeEvent(input$predict_btn, {
    model_obj <- trained_model()
    if (is.null(model_obj) || is.null(model_obj$model)) {
      predict_after_train(TRUE)
      prediction_result("模型尚未訓練，系統將為您自動訓練後預測...")
      shinyjs::click("retrain_model_analysis")
    } else {
      run_prediction()
    }
  })

  # Observer to run prediction after auto-training
  observeEvent(trained_model(), {
    if (predict_after_train()) {
      run_prediction()
      predict_after_train(FALSE) # Reset the trigger
    }
  })

  output$predicted_price <- renderText({
    prediction_result()
  })

  output$model_performance <- renderPrint({
    model_obj <- trained_model()
    req(model_obj, model_obj$model)
    
    model <- model_obj$model
    
    tryCatch({
      # caretStack models have a different structure
      if (!is.null(model$ens_model) && !is.null(model$ens_model$finalModel)) {
        # Get predictions from the ensemble model
        ensemble_pred <- model$ens_model$pred
        
        if (!is.null(ensemble_pred) && "pred" %in% names(ensemble_pred)) {
          actuals <- ensemble_pred$obs
          predictions <- ensemble_pred$pred
          
          rmse <- sqrt(mean((actuals - predictions)^2))
          rsquared <- cor(actuals, predictions)^2
          mae <- mean(abs(actuals - predictions))
          
          cat("=== 整合模型效能 (Ensemble Model) ===\n")
          cat("RMSE (均方根誤差):", round(rmse, 2), "元/坪\n")
          cat("MAE (平均絕對誤差):", round(mae, 2), "元/坪\n")
          cat("R-squared (決定係數):", round(rsquared, 3), "\n")
          cat("\n")
        }
      }
      
      # Show individual model performances
      if (!is.null(model$models)) {
        cat("=== 基礎模型效能 ===\n")
        for (i in seq_along(model$models)) {
          model_name <- names(model$models)[i]
          base_model <- model$models[[i]]
          
          cat("\n", toupper(model_name), "模型:\n")
          
          # Get the best result
          if (!is.null(base_model$results)) {
            best_result <- base_model$results[which.min(base_model$results$RMSE), ]
            if (nrow(best_result) > 0) {
              cat("  最佳 RMSE:", round(best_result$RMSE[1], 2), "元/坪\n")
              if ("Rsquared" %in% names(best_result)) {
                cat("  最佳 R-squared:", round(best_result$Rsquared[1], 3), "\n")
              }
            }
          }
        }
      }
      
    }, error = function(e) {
      cat("模型效能資料尚未完全產生。\n")
      cat("請確保模型已完成訓練。\n")
    })
  })

  output$prediction_accuracy_plot <- renderPlotly({
    model_obj <- trained_model()
    req(model_obj, model_obj$model)
    
    model <- model_obj$model
    
    tryCatch({
      # Extract predictions from the ensemble model
      predictions_df <- NULL
      
      # First try to get ensemble predictions
      if (!is.null(model$ens_model) && !is.null(model$ens_model$pred)) {
        predictions_df <- model$ens_model$pred
      }
      
      # If no ensemble predictions, try to get from individual models
      if (is.null(predictions_df) && !is.null(model$models)) {
        # Collect predictions from all base models
        all_preds <- list()
        
        for (i in seq_along(model$models)) {
          base_model <- model$models[[i]]
          if (!is.null(base_model$pred)) {
            model_name <- names(model$models)[i]
            pred_df <- base_model$pred
            pred_df$model <- model_name
            all_preds[[i]] <- pred_df
          }
        }
        
        if (length(all_preds) > 0) {
          predictions_df <- do.call(rbind, all_preds)
        }
      }
      
      if (!is.null(predictions_df) && nrow(predictions_df) > 0) {
        # Create the plot
        if ("model" %in% names(predictions_df)) {
          # Multiple models - show all
          p <- ggplot(predictions_df, aes(x = obs, y = pred, color = model)) +
            geom_point(alpha = 0.6) +
            geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", linewidth = 1) +
            facet_wrap(~model) +
            labs(
              title = "預測 vs 實際值 (各模型比較)", 
              x = "實際租金 (元/坪)", 
              y = "預測租金 (元/坪)",
              color = "模型"
            ) +
            theme_minimal() +
            theme(legend.position = "bottom")
        } else {
          # Single model or ensemble
          # Calculate R-squared first
          rsq <- cor(predictions_df$obs, predictions_df$pred)^2
          
          # Determine annotation position based on data distribution
          x_range <- range(predictions_df$obs)
          y_range <- range(predictions_df$pred)
          
          # Position R² in upper-left corner with proper padding
          annotation_x <- x_range[1] + (x_range[2] - x_range[1]) * 0.05
          annotation_y <- y_range[2] - (y_range[2] - y_range[1]) * 0.05
          
          p <- ggplot(predictions_df, aes(x = obs, y = pred)) +
            geom_point(alpha = 0.6, color = "#3498db", size = 2) +
            geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
            geom_smooth(method = "lm", se = TRUE, color = "blue", alpha = 0.2, linewidth = 1) +
            labs(
              x = "實際租金 (元/坪)", 
              y = "預測租金 (元/坪)"
            ) +
            theme_minimal() +
            theme(
              plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
              axis.title = element_text(size = 12)
            ) +
            # Add R-squared annotation with background box for better readability
            annotate("rect", 
                    xmin = annotation_x - (x_range[2] - x_range[1]) * 0.01,
                    xmax = annotation_x + (x_range[2] - x_range[1]) * 0.12,
                    ymin = annotation_y - (y_range[2] - y_range[1]) * 0.08,
                    ymax = annotation_y + (y_range[2] - y_range[1]) * 0.02,
                    fill = "white", alpha = 0.8) +
            annotate("text", 
                    x = annotation_x, 
                    y = annotation_y,
                    label = paste("R² =", round(rsq, 3)),
                    hjust = 0,
                    vjust = 1,
                    size = 5,
                    color = "darkblue",
                    fontface = "bold")
        }
        
        # Convert to plotly with custom title to avoid duplication
        ggplotly(p, tooltip = c("x", "y")) %>% 
          config(displayModeBar = FALSE) %>%
          layout(
            title = list(
              text = "預測 vs 實際值",
              font = list(size = 16)
            ),
            height = 400,
            margin = list(t = 50)
          )
        
      } else {
        # No prediction data available
        showNotification("請先點擊「更新模型」訓練模型", type = "warning", duration = 3)
        
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                  label = "請先訓練模型以查看預測準確度\n\n點擊「更新模型」按鈕開始訓練", 
                  size = 6, hjust = 0.5, color = "#666666") +
          theme_void()
        
        ggplotly(p) %>% 
          config(displayModeBar = FALSE) %>%
          layout(height = 400)
      }
      
    }, error = function(e) {
      showNotification(paste("繪圖錯誤:", e$message), type = "error", duration = 5)
      
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                label = "無法顯示預測圖表\n請檢查模型訓練狀態", 
                size = 5, hjust = 0.5, color = "#cc0000") +
        theme_void()
      
      ggplotly(p) %>% 
        config(displayModeBar = FALSE) %>%
        layout(height = 400)
    })
  })

  # === 地區推薦頁面 ===

  recommendation_result <- eventReactive(input$recommend_btn, {
    df <- filtered_data()
    status <- data_status()

    if (!status$sufficient_for_recommendation) {
      return(data.frame(
        訊息 = "資料不足：請調整篩選條件以獲得更多資料（至少需要3筆記錄）"
      ))
    }

    # 計算預估月租 = 單價 * 面積 (轉換坪到m²)
    df$estimated_monthly_rent <- df$price_per_ping * input$desired_area

    # 篩選符合預算的物件
    suitable <- df %>%
      filter(estimated_monthly_rent <= input$budget) %>%
      filter(lubridate::year(converted_date) == 2024) %>%
      filter(land_area_ping <= input$desired_area)

    if (nrow(suitable) == 0) {
      return(data.frame(
        訊息 = paste("沒有符合預算", input$budget, "元的物件，請提高預算或調整篩選條件")
      ))
    }

    # 如果有指定建物型態偏好
    if (!is.null(input$desired_building_type) && input$desired_building_type != "") {
      suitable <- suitable %>% filter(building_type == input$desired_building_type)

      if (nrow(suitable) == 0) {
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
        .groups = "drop"
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

    if ("訊息" %in% names(result)) {
      # 顯示錯誤或提示訊息
      datatable(
        result,
        options = list(
          pageLength = 5,
          searching = FALSE,
          paging = FALSE,
          info = FALSE
        ),
        class = "cell-border stripe"
      )
    } else {
      # 正常的推薦結果
      datatable(
        result,
        options = list(
          pageLength = 10,
          language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Chinese-traditional.json")
        ),
        class = "cell-border stripe"
      ) %>%
        formatCurrency(c("平均單價 (元/坪)", "預估月租"), currency = "", interval = 3, mark = ",", digits = 0)
    }
  })

  # === 互動地圖頁面 ===

  if (leaflet_available) {
    output$rental_map <- renderLeaflet({
      df <- filtered_data() %>%
        group_by(district) %>%
        summarise(
          avg_price = mean(price_per_ping, na.rm = TRUE),
          count = n(),
          .groups = "drop"
        )

      # 台北市各區大概位置（簡化版）
      district_coords <- data.frame(
        district = c(
          "中正區", "大同區", "中山區", "萬華區", "信義區",
          "松山區", "大安區", "南港區", "北投區", "內湖區",
          "士林區", "文山區"
        ),
        lat = c(
          25.032, 25.063, 25.063, 25.037, 25.033,
          25.050, 25.026, 25.055, 25.131, 25.082,
          25.088, 24.989
        ),
        lng = c(
          121.518, 121.513, 121.533, 121.500, 121.565,
          121.578, 121.536, 121.607, 121.501, 121.589,
          121.526, 121.569
        )
      )

      map_data <- merge(df, district_coords, by = "district", all.x = TRUE)

      leaflet(map_data) %>%
        addTiles() %>%
        setView(lng = 121.54, lat = 25.05, zoom = 11) %>%
        addCircleMarkers(
          ~lng, ~lat,
          radius = ~ sqrt(count) * 2,
          color = ~ colorNumeric("YlOrRd", avg_price)(avg_price),
          fillOpacity = 0.7,
          popup = ~ paste0(
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
      select(district, land_area_ping, converted_date, floor, building_type, price_per_ping) %>%
      rename(
        行政區 = district,
        `土地面積 (坪)` = land_area_ping,
        租賃日期 = converted_date,
        樓層 = floor,
        建物型態 = building_type,
        `單價 (元/坪)` = price_per_ping
      )

    datatable(
      df,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Chinese-traditional.json")
      ),
      class = "cell-border stripe"
    ) %>%
      formatCurrency("單價 (元/坪)", currency = "", interval = 3, mark = ",", digits = 0) %>%
      formatRound("土地面積 (坪)", digits = 2)
  })
}
