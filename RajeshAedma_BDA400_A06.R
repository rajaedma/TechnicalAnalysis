# ==============================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 6: Technical Analysis using R, Visualization Phase (15%)
# Interactive Portfolio Visualization Dashboard using R Shiny
# ==============================================================================

# ------------------------------------------------------------------------------
# Step 1: Data Collection and Setup
# Install required packages once if not already installed:
# install.packages(c("shiny", "ggplot2", "quantmod", "TTR", "gridExtra"))
# ------------------------------------------------------------------------------
library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)
library(gridExtra)

# Helper function to safely fetch and clean historical stock data with error handling
fetch_stock_safe <- function(symbol, start_date, end_date, time_frame = "Daily") {
  tryCatch({
    raw_xts <- getSymbols(
      Symbols = toupper(trimws(symbol)),
      src = "yahoo",
      from = start_date,
      to = end_date,
      auto.assign = FALSE
    )
    
    # Remove any rows with missing values
    raw_xts <- na.omit(raw_xts)
    if (nrow(raw_xts) == 0) return(NULL)
    
    # Aggregate data according to selected time frame
    if (time_frame == "Weekly") {
      raw_xts <- to.weekly(raw_xts, indexAt = "lastof", OHLC = TRUE)
    } else if (time_frame == "Monthly") {
      raw_xts <- to.monthly(raw_xts, indexAt = "lastof", OHLC = TRUE)
    }
    
    # Convert xts object into a clean data frame for ggplot2
    df <- data.frame(
      Date   = as.Date(index(raw_xts)),
      Open   = as.numeric(Op(raw_xts)),
      High   = as.numeric(Hi(raw_xts)),
      Low    = as.numeric(Lo(raw_xts)),
      Close  = as.numeric(Cl(raw_xts)),
      Volume = as.numeric(Vo(raw_xts))
    )
    return(df)
  }, error = function(e) {
    return(NULL)
  })
}

# ------------------------------------------------------------------------------
# Step 2: Visualizing Stock Data - Shiny UI Definition
# ------------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("BDA400 Portfolio Technical Analysis & Trading Signal Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("1. Data Source & Filters"),
      textInput("stock_symbol", "Stock Ticker Symbol (Yahoo Finance):", value = "AAPL"),
      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = "2023-01-01",
        end = "2023-12-31"
      ),
      selectInput(
        "time_frame",
        "Select Time Frame:",
        choices = c("Daily", "Weekly", "Monthly"),
        selected = "Daily"
      ),
      selectInput(
        "chart_type",
        "Select Price Chart Type:",
        choices = c("Line Graph", "Area Chart", "Candlestick"),
        selected = "Candlestick"
      ),
      
      hr(),
      h4("2. Technical Indicators"),
      checkboxGroupInput(
        "technical_indicators",
        "Toggle Indicator Overlays:",
        choices = c("Moving Averages", "RSI", "MACD"),
        selected = c("Moving Averages", "RSI", "MACD")
      ),
      
      hr(),
      h4("3. Trading Strategy Parameters"),
      numericInput("short_ma_len", "Short-Term SMA Period:", value = 10, min = 2, max = 100),
      numericInput("long_ma_len", "Long-Term SMA Period:", value = 25, min = 5, max = 250),
      checkboxInput("show_signals", "Annotate Buy / Sell Signals on Chart", value = TRUE),
      actionButton("update_btn", "Update Dashboard", class = "btn-primary", width = "100%")
    ),
    
    mainPanel(
      width = 9,
      uiOutput("status_banner"),
      plotOutput("stock_chart", height = "720px"),
      hr(),
      h4("Recent Trading Signals & Price Summary"),
      tableOutput("signal_table")
    )
  )
)

# ------------------------------------------------------------------------------
# Step 2, 3 & 4: Shiny Server Logic (Indicators, Trading Rules & Annotations)
# ------------------------------------------------------------------------------
server <- function(input, output) {
  
  # Reactive data retrieval triggered on load or button click
  stock_df <- eventReactive(input$update_btn, {
    validate(
      need(trimws(input$stock_symbol) != "", "Please enter a valid stock ticker symbol."),
      need(input$date_range[1] < input$date_range[2], "Start date must be earlier than end date."),
      need(input$short_ma_len < input$long_ma_len, "Short-term MA period must be less than Long-term MA period.")
    )
    
    # Fetch extra prior days so Moving Averages, RSI, and MACD warm up properly
    buffer_start <- as.Date(input$date_range[1]) - 120
    df <- fetch_stock_safe(
      symbol = input$stock_symbol,
      start_date = buffer_start,
      end_date = input$date_range[2],
      time_frame = input$time_frame
    )
    
    validate(
      need(!is.null(df) && nrow(df) > 0, paste("Could not retrieve data for symbol:", input$stock_symbol))
    )
    df
  }, ignoreNULL = FALSE)
  
  # Compute indicators and trading rules reactively
  analyzed_data <- reactive({
    df <- stock_df()
    n_rows <- nrow(df)
    
    # --- Step 3: Compute Technical Indicators ---
    # 1. Moving Averages (Short & Long SMA)
    if (n_rows >= input$short_ma_len) {
      df$Short_MA <- SMA(df$Close, n = input$short_ma_len)
    } else {
      df$Short_MA <- NA
    }
    
    if (n_rows >= input$long_ma_len) {
      df$Long_MA <- SMA(df$Close, n = input$long_ma_len)
    } else {
      df$Long_MA <- NA
    }
    
    # 2. Relative Strength Index (RSI - 14 period)
    if (n_rows > 14) {
      df$RSI <- RSI(df$Close, n = 14)
    } else {
      df$RSI <- NA
    }
    
    # 3. MACD (12, 26, 9 standard parameters)
    if (n_rows > 35) {
      macd_mat <- MACD(df$Close, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA")
      df$MACD_Line   <- macd_mat[, "macd"]
      df$Signal_Line <- macd_mat[, "signal"]
      df$MACD_Hist   <- df$MACD_Line - df$Signal_Line
    } else {
      df$MACD_Line   <- NA
      df$Signal_Line <- NA
      df$MACD_Hist   <- NA
    }
    
    # --- Step 4: Implement Trading Rules ---
    # Moving Average Crossover Rule:
    # Buy when Short MA crosses above Long MA; Sell when Short MA crosses below Long MA; Hold otherwise
    df$Position <- ifelse(
      is.na(df$Short_MA) | is.na(df$Long_MA), "Hold",
      ifelse(df$Short_MA > df$Long_MA, "Bullish", "Bearish")
    )
    
    df$Signal <- "Hold"
    if (n_rows > 1) {
      for (i in 2:n_rows) {
        if (!is.na(df$Short_MA[i]) && !is.na(df$Long_MA[i]) &&
            !is.na(df$Short_MA[i - 1]) && !is.na(df$Long_MA[i - 1])) {
          if (df$Short_MA[i] > df$Long_MA[i] && df$Short_MA[i - 1] <= df$Long_MA[i - 1]) {
            df$Signal[i] <- "Buy"
          } else if (df$Short_MA[i] < df$Long_MA[i] && df$Short_MA[i - 1] >= df$Long_MA[i - 1]) {
            df$Signal[i] <- "Sell"
          }
        }
      }
    }
    
    # Filter to user-selected date range after indicator warm-up
    filtered_df <- subset(df, Date >= as.Date(input$date_range[1]) & Date <= as.Date(input$date_range[2]))
    filtered_df
  })
  
  # Status summary banner
  output$status_banner <- renderUI({
    df <- analyzed_data()
    latest <- tail(df, 1)
    buy_count <- sum(df$Signal == "Buy", na.rm = TRUE)
    sell_count <- sum(df$Signal == "Sell", na.rm = TRUE)
    
    wellPanel(
      style = "padding: 10px; background-color: #f4f7fa;",
      strong(paste0("Ticker: ", toupper(input$stock_symbol), " (", input$time_frame, ") | ")),
      span(paste0("Latest Close: $", round(latest$Close, 2), " | ")),
      span(style = "color: #1b7837; font-weight: bold;", paste0("Buy Signals: ", buy_count, " | ")),
      span(style = "color: #c51b7d; font-weight: bold;", paste0("Sell Signals: ", sell_count))
    )
  })
  
  # Main reactive chart rendering
  output$stock_chart <- renderPlot({
    df <- analyzed_data()
    validate(need(nrow(df) > 1, "Not enough data points in the selected date range to plot."))
    
    # Base Price Plot (Step 2: Line, Area, or Candlestick)
    p_price <- ggplot(df, aes(x = Date))
    
    if (input$chart_type == "Line Graph") {
      p_price <- p_price +
        geom_line(aes(y = Close), color = "#1f4e78", linewidth = 0.9)
    } else if (input$chart_type == "Area Chart") {
      p_price <- p_price +
        geom_area(aes(y = Close), fill = "#4a90e2", alpha = 0.25) +
        geom_line(aes(y = Close), color = "#1f4e78", linewidth = 0.9)
    } else if (input$chart_type == "Candlestick") {
      df$Candle_Color <- ifelse(df$Close >= df$Open, "Up", "Down")
      candle_width <- ifelse(input$time_frame == "Daily", 0.6,
                             ifelse(input$time_frame == "Weekly", 3.5, 14))
      p_price <- ggplot(df, aes(x = Date)) +
        geom_linerange(aes(ymin = Low, ymax = High, color = Candle_Color), linewidth = 0.5) +
        geom_rect(aes(
          xmin = Date - candle_width / 2,
          xmax = Date + candle_width / 2,
          ymin = pmin(Open, Close),
          ymax = pmax(Open, Close),
          fill = Candle_Color,
          color = Candle_Color
        )) +
        scale_fill_manual(values = c("Up" = "#2ca02c", "Down" = "#d62728"), guide = "none") +
        scale_color_manual(values = c("Up" = "#2ca02c", "Down" = "#d62728"), guide = "none")
    }
    
    # Step 3: Overlay Moving Averages on Price Chart
    if ("Moving Averages" %in% input$technical_indicators) {
      p_price <- p_price +
        geom_line(aes(y = Short_MA), color = "#ff7f0e", linewidth = 0.85, na.rm = TRUE) +
        geom_line(aes(y = Long_MA), color = "#9467bd", linewidth = 0.85, na.rm = TRUE)
    }
    
    # Step 4: Annotate Buy/Sell Signals on Price Chart
    if (isTRUE(input$show_signals)) {
      action_df <- subset(df, Signal %in% c("Buy", "Sell"))
      if (nrow(action_df) > 0) {
        p_price <- p_price +
          geom_point(
            data = action_df,
            aes(x = Date, y = ifelse(Signal == "Buy", Low * 0.985, High * 1.015), shape = Signal),
            size = 3.5,
            color = ifelse(action_df$Signal == "Buy", "#006d2c", "#a50f15")
          ) +
          geom_text(
            data = action_df,
            aes(
              x = Date,
              y = ifelse(Signal == "Buy", Low * 0.985, High * 1.015),
              label = Signal
            ),
            vjust = ifelse(action_df$Signal == "Buy", 1.5, -0.7),
            fontface = "bold",
            size = 3.5,
            color = ifelse(action_df$Signal == "Buy", "#006d2c", "#a50f15")
          ) +
          scale_shape_manual(values = c("Buy" = 24, "Sell" = 25), guide = "none")
      }
    }
    
    p_price <- p_price +
      labs(
        title = paste0(toupper(input$stock_symbol), " - ", input$chart_type, " (", input$time_frame, ")"),
        subtitle = ifelse(
          "Moving Averages" %in% input$technical_indicators,
          paste0("Orange: ", input$short_ma_len, "-Period SMA | Purple: ", input$long_ma_len, "-Period SMA"),
          "Price Action"
        ),
        x = NULL,
        y = "Price (USD)"
      ) +
      theme_minimal(base_size = 12)
    
    plot_list <- list(p_price)
    heights_vec <- c(3)
    
    # Step 3: Optional RSI Panel
    if ("RSI" %in% input$technical_indicators && !all(is.na(df$RSI))) {
      p_rsi <- ggplot(df, aes(x = Date, y = RSI)) +
        geom_line(color = "#1f78b4", linewidth = 0.8, na.rm = TRUE) +
        geom_hline(yintercept = 70, linetype = "dashed", color = "#d62728") +
        geom_hline(yintercept = 30, linetype = "dashed", color = "#2ca02c") +
        ylim(0, 100) +
        labs(title = "Relative Strength Index (RSI - 14)", x = NULL, y = "RSI") +
        theme_minimal(base_size = 11)
      plot_list <- append(plot_list, list(p_rsi))
      heights_vec <- c(heights_vec, 1.3)
    }
    
    # Step 3: Optional MACD Panel
    if ("MACD" %in% input$technical_indicators && !all(is.na(df$MACD_Line))) {
      p_macd <- ggplot(df, aes(x = Date)) +
        geom_col(aes(y = MACD_Hist), fill = "#bdbdbd", alpha = 0.7, na.rm = TRUE) +
        geom_line(aes(y = MACD_Line), color = "#1f78b4", linewidth = 0.8, na.rm = TRUE) +
        geom_line(aes(y = Signal_Line), color = "#e31a1c", linewidth = 0.8, na.rm = TRUE) +
        labs(title = "MACD (12, 26, 9) - Blue: MACD | Red: Signal", x = "Date", y = "MACD") +
        theme_minimal(base_size = 11)
      plot_list <- append(plot_list, list(p_macd))
      heights_vec <- c(heights_vec, 1.4)
    }
    
    grid.arrange(grobs = plot_list, ncol = 1, heights = heights_vec)
  })
  
  # Summary table of generated Buy/Sell signals and recent rows
  output$signal_table <- renderTable({
    df <- analyzed_data()
    signals_only <- subset(df, Signal %in% c("Buy", "Sell"))
    if (nrow(signals_only) == 0) {
      display_df <- tail(df, 8)
    } else {
      display_df <- tail(signals_only, 10)
    }
    display_df$Date <- as.character(display_df$Date)
    display_df[, c("Date", "Open", "High", "Low", "Close", "Short_MA", "Long_MA", "RSI", "Signal")]
  }, digits = 2)
}

# Launch the Shiny Application
shinyApp(ui = ui, server = server)
