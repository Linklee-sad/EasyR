source("setup.R")
library(shiny)
source("R/directory.R")
source("R/plot_editor.R")
source("R/timeseries.R")

mock_market <- function(Symbols, src, from, to, auto.assign, warnings) {
  columns <- if (identical(src, "yahoo")) {
    matrix(c(10, 11, 9, 10.5, 1000, 10.4, 11, 12, 10, 11.5, 1200, 11.4,
      12, 13, 11, 12.5, 1300, 12.4, 13, 14, 12, 13.5, 1400, 13.4), nrow = 4, byrow = TRUE,
      dimnames = list(NULL, paste0(Symbols, c(".Open", ".High", ".Low", ".Close", ".Volume", ".Adjusted"))))
  } else matrix(c(2.1, 2.2, 2.3, 2.4), ncol = 1, dimnames = list(NULL, Symbols))
  xts::xts(columns, order.by = as.Date(c("2024-01-02", "2024-01-03", "2024-01-04", "2024-01-05")))
}
yahoo_data <- download_yahoo_data("aapl", "2024-01-01", "2024-01-10", mock_market)
stopifnot(identical(online_dataset_name("yahoo", "aapl", "2024-01-01", "2024-01-10"), "Yahoo · AAPL · 2024-01-01_2024-01-10"))
fred_payload <- list(observations = data.frame(date = c("2024-01-01", "2024-01-02"), value = c("3.5", ".")))
fred_parsed <- parse_fred_observations(fred_payload, "DGS10")
stopifnot(identical(names(yahoo_data), c("日期", "开盘价", "最高价", "最低价", "收盘价", "成交量", "复权收盘价")),
  nrow(yahoo_data) == 4L, identical(names(fred_parsed), c("日期", "DGS10")),
  identical(fred_parsed$DGS10[1], 3.5), is.na(fred_parsed$DGS10[2]))
registered <- NULL
registered_name <- register_online_dataset(function(values) {
  registered <<- values
  names(values)
}, yahoo_data, "Yahoo · AAPL")
stopifnot(identical(registered_name, "Yahoo · AAPL"), identical(registered[[1]], yahoo_data))
fails_key <- tryCatch({ download_fred_data("GDP", "2024-01-01", "2024-01-10", "short"); "unexpected success" }, error = conditionMessage)
stopifnot(grepl("32 位", fails_key))

dates <- as.Date(c("2024-01-03", "2024-01-01", "2024-01-02", "2024-01-02", "2024-01-04", NA))
dirty <- data.frame(date = dates, price = c(3, 1, 2, 4, 6, 9))
level <- prepare_time_series(dirty, 1, 2, "level")
stopifnot(level$excluded == 1, level$duplicate_rows == 1, nrow(level$data) == 4,
  identical(as.numeric(level$data$.value), c(1, 3, 3, 6)), grepl("同一时间", level$report))
percent <- prepare_time_series(dirty, 1, 2, "percent")
stopifnot(isTRUE(all.equal(percent$data$.value, c(200, 0, 100))))

character_dates <- data.frame(date = c("2024/01/01", "2024/01/02", "2024/01/03"), value = 1:3)
stopifnot(inherits(prepare_time_series(character_dates, 1, 2)$data$.time, "Date"))

seasonal_data <- data.frame(date = as.Date("2024-01-01") + 0:39,
  value = 100 + 0:39 * 0.2 + sin(2 * pi * (0:39) / 5))
series <- prepare_time_series(seasonal_data, 1, 2)
series_plot <- build_time_series_plot(series, TRUE, 5, "linear")
acf_plot <- build_time_series_acf_plot(series, 10)
decomposition_plot <- build_time_series_decomposition_plot(series, 5)
edited_plot <- apply_plot_editor(series_plot, list(title = "Edited series", palette = "warm", line_width = 2,
  point_size = 4, font_size = 14))
stopifnot(inherits(series_plot, "ggplot"), length(ggplot2::ggplot_build(series_plot)$data) == 3,
  inherits(acf_plot, "ggplot"), inherits(decomposition_plot, "ggplot"),
  nrow(ggplot2::ggplot_build(decomposition_plot)$layout$layout) == 4,
  edited_plot$labels$title == "Edited series", edited_plot$layers[[1]]$aes_params$linewidth == 2)

fails <- function(expr, pattern) {
  message <- tryCatch({ force(expr); "unexpected success" }, error = conditionMessage)
  stopifnot(grepl(pattern, message))
}
fails(prepare_time_series(data.frame(time = 1:5, value = rep(0, 5)), 1, 2, "log_return"), "大于 0")
fails(build_time_series_plot(series, TRUE, 100), "不能大于")
fails(build_time_series_decomposition_plot(series, 30), "两个完整周期")

set.seed(42)
garch_n <- 800L
garch_innovation <- stats::rnorm(garch_n)
garch_variance <- garch_series <- numeric(garch_n)
garch_variance[1] <- 1
garch_series[1] <- garch_innovation[1]
for (i in 2:garch_n) {
  garch_variance[i] <- 0.05 + 0.12 * garch_series[i - 1L]^2 + 0.82 * garch_variance[i - 1L]
  garch_series[i] <- garch_innovation[i] * sqrt(garch_variance[i])
}
garch_input <- data.frame(date = as.Date("2020-01-01") + seq_len(garch_n) - 1L, return = garch_series)
garch_series_result <- prepare_time_series(garch_input, 1, 2, "level")
garch_series_result$garch <- fit_garch_analysis(garch_series_result, 1, 1, TRUE, 10)
stopifnot(nrow(garch_series_result$garch$coefficients) == 3L,
  nrow(garch_series_result$garch$diagnostics) == 4L,
  length(garch_series_result$garch$conditional_sigma) == garch_n,
  is.finite(garch_series_result$garch$persistence),
  is.finite(garch_series_result$garch$next_sigma),
  inherits(build_garch_volatility_plot(garch_series_result), "ggplot"),
  inherits(build_garch_diagnostic_plot(garch_series_result, 20), "ggplot"),
  !is.null(ai_model_context("时间序列分析", garch_series_result)$garch))
fails(fit_garch_analysis(series, 5, 5), "至少需要")

set.seed(7)
forecast_n <- 120L
forecast_input <- data.frame(
  date = seq.Date(as.Date("2015-01-01"), by = "month", length.out = forecast_n),
  value = 100 + .35 * seq_len(forecast_n) + 8 * sin(2 * pi * seq_len(forecast_n) / 12) + stats::rnorm(forecast_n, 0, 1.5)
)
forecast_series <- prepare_time_series(forecast_input, 1, 2, "level")
forecast_series$forecast <- fit_time_series_forecast(forecast_series, method = "auto", horizon = 12,
  validation_n = 20, seasonal = TRUE, frequency = 12, max_arima_order = 2, time_step = "auto")
forecast_result <- forecast_series$forecast
stopifnot(nrow(forecast_result$future) == 12L, nrow(forecast_result$metrics) == 5L,
  nrow(forecast_result$validation) == 20L, nrow(forecast_result$parameters) >= 2L,
  inherits(forecast_result$future$时间, "Date"),
  forecast_result$future$时间[1] == as.Date("2025-01-01"),
  all(forecast_result$future$下限95 <= forecast_result$future$预测值),
  all(forecast_result$future$上限95 >= forecast_result$future$预测值),
  inherits(build_forecast_plot(forecast_series), "ggplot"),
  !is.null(ai_model_context("时间序列分析", forecast_series)$forecast))
manual_forecast <- fit_time_series_forecast(forecast_series, method = "manual_arima", horizon = 6,
  validation_n = 20, seasonal = TRUE, frequency = 12,
  manual_order = c(1, 1, 1), manual_seasonal_order = c(0, 1, 1), time_step = "month")
stopifnot(nrow(manual_forecast$future) == 6L, grepl("ARIMA", manual_forecast$selected_label))
business_dates <- forecast_future_time(as.Date(c("2025-01-02", "2025-01-03")), 3, "business_day")
stopifnot(identical(business_dates, as.Date(c("2025-01-06", "2025-01-07", "2025-01-08"))))
fails(fit_time_series_forecast(series, validation_n = 30), "有效时间点过少")

test_directory <- tempfile("rmod-timeseries-")
dir.create(test_directory)
changing <- reactiveVal(seasonal_data)
testServer(timeseries_server, args = list(data = reactive(changing()), directory = reactive(test_directory)), {
  session$setInputs(time = "1", value = "2", transform = "level", moving_average = TRUE,
    window = 5, trend = "linear", max_lag = 10, decompose = TRUE, frequency = 5)
  session$setInputs(run = 1)
  stopifnot(!is.null(result()), nrow(result()$data) == 40, nzchar(output$report),
    inherits(series_plot(), "ggplot"), inherits(acf_plot(), "ggplot"),
    inherits(decomposition_plot(), "ggplot"), nzchar(output$table))
  session$setInputs(transform = "difference")
  stopifnot(is.null(result()))
  session$setInputs(run = 2)
  stopifnot(!is.null(result()), nrow(result()$data) == 39)
  changing(seasonal_data[-1, ])
  session$flushReact()
  stopifnot(is.null(result()))
  downloaded_data(yahoo_data); downloaded_label("Yahoo Finance · AAPL"); session$flushReact()
  session$setInputs(time = "1", value = "7", transform = "level", moving_average = FALSE, trend = "none", max_lag = 1)
  session$setInputs(run = 3)
  stopifnot(identical(analysis_data(), yahoo_data), !is.null(result()), grepl("Yahoo Finance", output$active_source))
  downloaded_data(NULL)
  stopifnot(is.null(downloaded_data()), identical(analysis_data(), changing()))
})
testServer(timeseries_server, args = list(data = reactive(forecast_input), directory = reactive(test_directory)), {
  session$setInputs(time = "1", value = "2", transform = "level", moving_average = FALSE,
    trend = "none", max_lag = 20, decompose = FALSE, garch_enabled = FALSE,
    forecast_enabled = TRUE, forecast_method = "auto", forecast_horizon = 12,
    forecast_validation = 20, forecast_seasonal = TRUE, forecast_frequency = 12,
    forecast_max_order = 2, forecast_time_step = "auto",
    forecast_p = 1, forecast_d = 1, forecast_q = 1, forecast_P = 0, forecast_D = 1, forecast_Q = 1)
  session$setInputs(run = 1)
  stopifnot(!is.null(result()$forecast), nrow(result()$forecast$future) == 12L,
    nzchar(output$forecast_report), nzchar(output$forecast_metrics), nzchar(output$forecast_comparison),
    nzchar(output$forecast_parameters), nzchar(output$forecast_table), inherits(forecast_plot(), "ggplot"))
})
unlink(test_directory, recursive = TRUE)
cat("Yahoo / FRED 数据转换、数据源切换、时间解析、序列变换、趋势、ACF、STL、GARCH、ARIMA/ETS 预测验证与 PNG 保存检查通过。\n")
