source("ui.R", chdir = TRUE)
source("server.R", chdir = TRUE)

# 啟動 App
cat("App is running at http://localhost:3838\n")
shinyApp(ui = ui, server = server, options = list(host = "0.0.0.0", port = 3838))
