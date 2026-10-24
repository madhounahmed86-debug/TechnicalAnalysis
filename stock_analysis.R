
# BDA400 Assignment 2 - Stock Analysis

library(quantmod)
library(TTR)
library(ggplot2)

# Load stock symbols from portfolio.txt
load_stock_data <- function(portfolio_file = "portfolio.txt") {
  symbols <- trimws(readLines(portfolio_file))
  symbols <- symbols[nzchar(symbols)]

  stocks <- list()

  for (symbol in symbols) {
    message("Downloading ", symbol)

    data <- getSymbols(
      symbol,
      src = "yahoo",
      from = Sys.Date() - 365,
      auto.assign = FALSE
    )

    stocks[[symbol]] <- data
  }

  return(stocks)
}

# Calculate statistics using adjusted closing prices
calculate_statistics <- function(stock_data) {
  prices <- as.numeric(Ad(stock_data))
  prices <- prices[is.finite(prices)]

  moving_avg <- TTR::SMA(prices, n = 20)

  # Calculate the most frequently occurring value
  frequencies <- table(prices)
  modes <- as.numeric(names(frequencies)[
    frequencies == max(frequencies)
  ])

  return(data.frame(
    Latest_Adjusted_Close = tail(prices, 1),
    Moving_Average_20_Days = tail(moving_avg, 1),
    Mean = mean(prices),
    Mode = paste(modes, collapse = ", "),
    Median = median(prices),
    Standard_Deviation = sd(prices)
  ))
}

# Run the analysis
stocks <- load_stock_data()

for (symbol in names(stocks)) {
  cat("\nStock:", symbol, "\n")
  print(tail(stocks[[symbol]], 6))

  cat("\nStatistics:\n")
  print(calculate_statistics(stocks[[symbol]]))
}

# Create output folder and save charts
dir.create("output", showWarnings = FALSE)

for (symbol in names(stocks)) {
  png(file.path("output", paste0(symbol, "_chart.png")),
      width = 1000, height = 600)

  chartSeries(
    stocks[[symbol]],
    name = paste(symbol, "Stock Price"),
    theme = chartTheme("white")
  )

  dev.off()
}
