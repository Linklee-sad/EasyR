source("setup.R")
library(shiny)
source("R/directory.R")
source("R/plot_editor.R")
source("R/timeseries.R")

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
})
unlink(test_directory, recursive = TRUE)
cat("时间解析、重复时间合并、序列变换、移动平均、趋势、ACF、STL 分解与 PNG 保存检查通过。\n")
