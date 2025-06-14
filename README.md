# [Group 8] 租屋市場分析工具

基於內政部實價登錄資料的互動式租屋市場分析應用程式，提供租金分析、價格預測、地區推薦等功能。

### 工作分配

| 姓名   | 系級     | 學號      | 工作分配     | 備註            |
| :----- | :------- | :-------- | :----------- | :-------------- |
| 湯晴羽 | 資管四   | 110306012 |
| 冼冠宇 | 資訊碩一 | 113753126 |
| 呂杰勳 | 資訊碩一 | 113753132 |
| 陳怡然 | 資訊四   | 110703040 | 程式、視覺化 | Insights 看佔比 |
| 劉白   | 資訊三   | 111703029 |
| 涂世豪 | 資訊四   | 110703014 | 後端 | Insights 看佔比 |

## 快速開始

### 1. 啟動應用程式

```bash
# 在專案目錄下執行（套件會自動安裝）
Rscript code/main.R
```

### 2. 開啟瀏覽器

訪問 http://localhost:3838

> **注意**：首次執行時會自動安裝必要套件，請耐心等待安裝完成。

## 主要功能

### 📊 市場總覽

-   即時統計租金分布與趨勢
-   各行政區租金比較分析
-   建物型態統計與平均價格
-   面積與租金關係視覺化

### 📈 詳細分析

-   各行政區租金箱型圖分布
-   樓層與租金關係分析
-   時間序列趨勢追蹤
-   整合式機器學習模型（線性迴歸、隨機森林、KNN）

### 🔮 價格預測

-   **改進的預測功能**：
    -   使用 caretStack 整合多種模型提升預測準確度
    -   自動模型訓練與更新機制
    -   動態變數選擇，自動適應資料特性
-   **視覺化預測準確度**：
    -   預測 vs 實際值散點圖
    -   R² 值顯示（優化位置避免遮擋）
    -   模型效能指標（RMSE、MAE、R-squared）

### 🗺️ 地區推薦

-   根據預算智能推薦合適區域
-   考慮面積需求與建物型態偏好
-   顯示推薦區域的平均租金與物件數量

### 🌍 互動地圖

-   台北市各區租金熱點視覺化
-   圓圈大小代表物件數量
-   顏色深淺表示平均租金

### 📋 原始資料

-   完整資料瀏覽與篩選功能
-   中文介面支援
-   資料匯出功能

## 技術特色

### 資料處理

-   自動資料清理與前處理
-   智能單位轉換（坪/平方公尺）
-   動態資料篩選與即時更新

### 模型優化

-   **Ensemble Learning**：結合多種演算法提升預測準確度
-   **交叉驗證**：5-fold CV 確保模型穩定性
-   **動態特徵選擇**：根據資料自動調整模型變數

### 使用者體驗

-   響應式設計，適配各種螢幕尺寸
-   即時資料狀態提示
-   錯誤處理與友善提示訊息

## 專案結構

```
ds-rent-project/
├── code/
│   ├── main.R        # 應用程式啟動檔
│   ├── ui.R          # 使用者介面定義
│   ├── server.R      # 伺服器邏輯處理（含改進的預測功能）
│   └── utils.R       # 套件管理與資料前處理
├── data/
│   ├── MOI_rent.csv  # 原始租屋資料
│   └── all_cleaned.csv # 清理後的資料
├── result/
│   └── demo-1.png    # 應用程式截圖
└── README.md         # 專案說明文件
```

## 最新更新

### 2025-06-09

-   🐛 修復預測功能錯誤
-   📊 改進預測準確度圖表的可讀性
-   🎨 優化 R² 值顯示位置，避免與資料點重疊
-   🔧 修正 caretStack 模型結構存取問題

## 產出結果

<img src="result/demo-1.png" alt="Demo Screenshot 1" style="width: 46em;">

## 資料來源

[內政部不動產租賃實價登錄](https://lvr.land.moi.gov.tw/jsp/index.jsp)

## 系統需求

-   R 4.0 或以上版本
-   建議記憶體 4GB 以上
-   穩定的網路連線（用於套件安裝）

## 疑難排解

### 常見問題

1. **套件安裝失敗**

    - 確保有穩定的網路連線
    - 嘗試手動安裝：`install.packages(c("shiny", "shinydashboard", "dplyr", "caret", "caretEnsemble"))`

2. **預測功能錯誤**

    - 確保已選擇足夠的資料（至少 10 筆）
    - 點擊「更新模型」重新訓練

3. **記憶體不足**
    - 調整篩選條件減少資料量
    - 關閉其他應用程式釋放記憶體

## 授權

本專案採用 MIT 授權條款
