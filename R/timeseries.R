ts_number <- function(x) {
  if (!is.finite(x)) return("无法计算")
  format(signif(x, 4), trim = TRUE, scientific = abs(x) > 0 && abs(x) < 0.001)
}

ts_pvalue <- function(p) {
  if (!is.finite(p)) "无法计算" else if (p < 0.001) "< 0.001" else paste0("= ", formatC(p, digits = 3, format = "f"))
}

parse_time_axis <- function(x) {
  if (inherits(x, "Date")) return(list(value = x, type = "date"))
  if (inherits(x, "POSIXt")) return(list(value = as.POSIXct(x), type = "datetime"))
  if (is.numeric(x)) return(list(value = as.numeric(x), type = "numeric"))
  raw <- trimws(as.character(x))
  raw[raw == ""] <- NA_character_
  numeric_value <- suppressWarnings(as.numeric(raw))
  if (all(is.na(raw) | !is.na(numeric_value))) return(list(value = numeric_value, type = "numeric"))
  parsed <- rep(as.Date(NA), length(raw))
  formats <- c("%Y-%m-%d", "%Y/%m/%d", "%Y%m%d", "%d/%m/%Y", "%m/%d/%Y", "%Y-%m", "%Y/%m")
  for (format in formats) {
    missing <- is.na(parsed) & !is.na(raw)
    parsed[missing] <- suppressWarnings(as.Date(raw[missing], format = format))
  }
  if (!any(!is.na(parsed))) stop("时间字段无法识别。请使用日期、日期时间或数值序号。", call. = FALSE)
  list(value = parsed, type = "date")
}

prepare_time_series <- function(data, time, value, transform = "level") {
  time <- as.integer(time); value <- as.integer(value)
  valid_column <- function(i) length(i) == 1 && !is.na(i) && i %in% seq_along(data)
  if (!valid_column(time)) stop("请选择有效时间字段。", call. = FALSE)
  if (!valid_column(value) || !is.numeric(data[[value]])) stop("请选择有效数值字段。", call. = FALSE)
  if (time == value) stop("时间字段和数值字段不能相同。", call. = FALSE)
  parsed <- parse_time_axis(data[[time]])
  time_value <- parsed$value
  numeric_value <- data[[value]]
  valid <- !is.na(time_value) & !is.na(numeric_value) & is.finite(numeric_value)
  if (is.numeric(time_value)) valid <- valid & is.finite(time_value)
  excluded <- sum(!valid)
  time_value <- time_value[valid]
  numeric_value <- numeric_value[valid]
  if (length(numeric_value) < 3) stop("有效时间序列数据不足 3 行。", call. = FALSE)
  order_index <- order(time_value)
  time_value <- time_value[order_index]
  numeric_value <- numeric_value[order_index]
  duplicate_rows <- length(time_value) - length(unique(time_value))
  if (duplicate_rows) {
    groups <- match(time_value, unique(time_value))
    numeric_value <- as.numeric(tapply(numeric_value, groups, mean))
    time_value <- time_value[!duplicated(time_value)]
  }
  if (length(numeric_value) < 3) stop("合并重复时间后不足 3 个时间点。", call. = FALSE)
  transform_labels <- c(level = "原始值", difference = "一阶差分", percent = "百分比变化（%）", log_return = "对数收益率（%）")
  if (!transform %in% names(transform_labels)) stop("请选择有效的序列变换。", call. = FALSE)
  transformed_time <- time_value
  transformed_value <- numeric_value
  if (transform == "difference") {
    transformed_time <- time_value[-1]
    transformed_value <- diff(numeric_value)
  } else if (transform == "percent") {
    transformed_time <- time_value[-1]
    transformed_value <- 100 * (numeric_value[-1] / numeric_value[-length(numeric_value)] - 1)
  } else if (transform == "log_return") {
    if (any(numeric_value <= 0)) stop("对数收益率要求所有有效原始值均大于 0。", call. = FALSE)
    transformed_time <- time_value[-1]
    transformed_value <- 100 * diff(log(numeric_value))
  }
  finite <- is.finite(transformed_value)
  transform_excluded <- sum(!finite)
  d <- data.frame(.time = transformed_time[finite], .value = transformed_value[finite])
  if (nrow(d) < 3) stop("变换后有效时间点不足 3 个。", call. = FALSE)
  gaps <- diff(as.numeric(d$.time))
  interval <- if (length(gaps)) stats::median(gaps) else NA_real_
  regular <- length(gaps) < 2 || all(abs(gaps - interval) <= sqrt(.Machine$double.eps) * max(1, abs(interval)))
  index <- seq_len(nrow(d))
  trend_model <- stats::lm(d$.value ~ index)
  trend_table <- suppressWarnings(summary(trend_model))$coefficients
  acf1 <- if (nrow(d) > 1 && stats::sd(d$.value) > 0) as.numeric(stats::acf(d$.value, plot = FALSE, lag.max = 1)$acf[2]) else NA_real_
  report <- c(
    "时间序列分析摘要",
    paste0("时间字段：", names(data)[time], "；数值字段：", names(data)[value], "；分析序列：", transform_labels[[transform]], "。"),
    sprintf("原始数据 %d 行；因时间无效、数值缺失或非有限而排除 %d 行；合并 %d 行重复时间；变换后另排除 %d 个非有限结果；最终分析 %d 个时间点。",
      nrow(data), excluded, duplicate_rows, transform_excluded, nrow(d)),
    paste0("时间范围：", format(min(d$.time)), " 至 ", format(max(d$.time)), "。记录已按时间升序排列；同一时间的多个数值按均值合并。"),
    paste0("描述统计：均值 = ", ts_number(mean(d$.value)), "；标准差 = ", ts_number(stats::sd(d$.value)),
      "；最小值 = ", ts_number(min(d$.value)), "；最大值 = ", ts_number(max(d$.value)), "。"),
    paste0("线性时间趋势（以观测顺序为单位）：斜率 = ", ts_number(coef(trend_model)[2]), "；p ", ts_pvalue(trend_table[2, 4]),
      "。该结果是描述性趋势；序列相关会使常规 lm 标准误和 p 值失真。"),
    paste0("一阶样本自相关 ACF(1) = ", ts_number(acf1), "。ACF 的滞后单位是相邻观测，而不是固定日历天数。"),
    if (regular) paste0("时间间隔规则，典型间隔为 ", ts_number(interval), "。") else
      paste0("时间间隔不完全规则，中位间隔为 ", ts_number(interval), "。移动平均、ACF 和季节分解均按观测顺序计算；金融交易日数据中的周末缺口不会自动补齐。"),
    "趋势、季节性和自相关描述统计结构，不自动建立因果关系，也不等同于样本外预测。季节分解要求用户给出的周期与数据频率具有实际含义。"
  )
  list(data = d, report = paste(report, collapse = "\n\n"), time_name = names(data)[time],
    value_name = names(data)[value], transform = transform, transform_label = transform_labels[[transform]],
    excluded = excluded, duplicate_rows = duplicate_rows, transform_excluded = transform_excluded,
    regular = regular, interval = interval, trend_model = trend_model, acf1 = acf1)
}

ts_plot_theme <- function() {
  family <- switch(Sys.info()[["sysname"]], Darwin = "Arial Unicode MS", Windows = "Microsoft YaHei", "sans")
  ggplot2::theme_minimal(base_size = 13, base_family = family) +
    ggplot2::theme(plot.title.position = "plot", legend.position = "bottom")
}

build_time_series_plot <- function(result, moving_average = TRUE, window = 5, trend = "none") {
  d <- result$data
  window <- as.integer(window)
  p <- ggplot2::ggplot(d, ggplot2::aes(.time, .value)) +
    ggplot2::geom_line(ggplot2::aes(colour = "序列"), linewidth = 0.65, alpha = 0.85) +
    ts_plot_theme()
  colours <- c("序列" = "#2563eb", "移动平均" = "#d97706", "线性趋势" = "#dc2626", "LOESS 趋势" = "#059669")
  if (isTRUE(moving_average)) {
    if (is.na(window) || window < 2 || window > 200) stop("移动平均窗口应在 2～200 之间。", call. = FALSE)
    if (window > nrow(d)) stop("移动平均窗口不能大于有效时间点数量。", call. = FALSE)
    d$.moving <- as.numeric(stats::filter(d$.value, rep(1 / window, window), sides = 1))
    p <- p + ggplot2::geom_line(data = d, ggplot2::aes(.time, .moving, colour = "移动平均"), linewidth = 1, na.rm = TRUE)
  }
  if (identical(trend, "linear")) {
    d$.trend <- stats::fitted(stats::lm(d$.value ~ seq_len(nrow(d))))
    p <- p + ggplot2::geom_line(data = d, ggplot2::aes(.time, .trend, colour = "线性趋势"), linewidth = 1)
  } else if (identical(trend, "loess")) {
    if (nrow(d) < 10) stop("LOESS 趋势至少需要 10 个有效时间点。", call. = FALSE)
    fit <- stats::loess(d$.value ~ seq_len(nrow(d)), span = 0.3)
    d$.trend <- stats::predict(fit)
    p <- p + ggplot2::geom_line(data = d, ggplot2::aes(.time, .trend, colour = "LOESS 趋势"), linewidth = 1)
  } else if (!identical(trend, "none")) stop("请选择有效趋势线。", call. = FALSE)
  p + ggplot2::scale_colour_manual(values = colours, name = NULL) +
    ggplot2::labs(title = paste0(result$value_name, " · ", result$transform_label),
      subtitle = "按时间升序；移动平均为向后滚动窗口", x = result$time_name, y = result$transform_label)
}

build_time_series_acf_plot <- function(result, max_lag = 30) {
  n <- nrow(result$data)
  max_lag <- as.integer(max_lag)
  if (is.na(max_lag) || max_lag < 1) stop("最大滞后阶数至少为 1。", call. = FALSE)
  max_lag <- min(max_lag, n - 1)
  if (stats::sd(result$data$.value) == 0) stop("序列没有变化，无法计算自相关。", call. = FALSE)
  values <- stats::acf(result$data$.value, plot = FALSE, lag.max = max_lag)$acf
  d <- data.frame(.lag = seq.int(0, length(values) - 1), .acf = as.numeric(values))
  bound <- stats::qnorm(0.975) / sqrt(n)
  ggplot2::ggplot(d, ggplot2::aes(.lag, .acf)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#64748b") +
    ggplot2::geom_hline(yintercept = c(-bound, bound), linetype = "dashed", colour = "#dc2626") +
    ggplot2::geom_segment(ggplot2::aes(xend = .lag, y = 0, yend = .acf), colour = "#2563eb", linewidth = 0.65) +
    ggplot2::geom_point(colour = "#2563eb", size = 2) + ts_plot_theme() +
    ggplot2::labs(title = "自相关函数（ACF）", subtitle = "红色虚线为白噪声近似 95% 界限；未调整多重比较",
      x = "滞后阶数（相邻观测）", y = "自相关系数")
}

build_time_series_decomposition_plot <- function(result, frequency) {
  frequency <- as.integer(frequency)
  if (is.na(frequency) || frequency < 2 || frequency > 10000) stop("季节周期应为 2～10000 的整数。", call. = FALSE)
  if (nrow(result$data) < 2 * frequency + 1) stop("STL 分解至少需要两个完整周期以上的数据，请降低周期或增加数据。", call. = FALSE)
  fit <- stats::stl(stats::ts(result$data$.value, frequency = frequency), s.window = "periodic", robust = TRUE)
  components <- fit$time.series
  labels <- c("原始序列", "趋势", "季节项", "余项")
  d <- data.frame(.time = rep(result$data$.time, 4),
    .component = factor(rep(labels, each = nrow(result$data)), levels = labels),
    .value = c(result$data$.value, components[, "trend"], components[, "seasonal"], components[, "remainder"]))
  ggplot2::ggplot(d, ggplot2::aes(.time, .value)) + ggplot2::geom_line(colour = "#2563eb", linewidth = 0.55) +
    ggplot2::facet_grid(.component ~ ., scales = "free_y") + ts_plot_theme() +
    ggplot2::theme(legend.position = "none", strip.text.y = ggplot2::element_text(angle = 0)) +
    ggplot2::labs(title = paste0("STL 季节分解（周期 = ", frequency, "）"),
      subtitle = "周期表示每个完整季节循环包含的观测数", x = result$time_name, y = NULL)
}

timeseries_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("时间序列分析"),
    p("选择时间字段和数值字段。程序会按时间排序，并将重复时间的数值按均值合并。"),
    fluidRow(column(5, selectInput(ns("time"), "时间字段", NULL)),
      column(5, selectInput(ns("value"), "数值字段", NULL)),
      column(2, selectInput(ns("transform"), "序列变换", c("原始值" = "level", "一阶差分" = "difference", "百分比变化" = "percent", "对数收益率" = "log_return")))),
    fluidRow(column(3, checkboxInput(ns("moving_average"), "显示移动平均", TRUE)),
      column(3, numericInput(ns("window"), "移动平均窗口", 5, min = 2, max = 200)),
      column(3, selectInput(ns("trend"), "趋势线", c("不显示" = "none", "线性趋势" = "linear", "LOESS 趋势" = "loess"))),
      column(3, numericInput(ns("max_lag"), "ACF 最大滞后", 30, min = 1, max = 500))),
    fluidRow(column(3, checkboxInput(ns("decompose"), "生成 STL 季节分解", FALSE)),
      column(3, conditionalPanel(paste0("input['", ns("decompose"), "']"),
        numericInput(ns("frequency"), "季节周期（观测数）", 12, min = 2, max = 10000)))),
    helpText("示例：月度数据周期常为 12，季度数据为 4；交易日周周期可尝试 5。周期必须结合数据含义确定。"),
    actionButton(ns("run"), "运行时间序列分析", class = "btn-primary"),
    tags$div(style = "margin:12px 0", textOutput(ns("status"))),
    tabsetPanel(
      tabPanel("分析摘要", tags$div(style = "white-space:pre-wrap;line-height:1.9", textOutput(ns("report")))),
      tabPanel("序列图（ggplot2）", ggplot_editor_ui(ns("series_editor"), height = "500px")),
      tabPanel("自相关图（ggplot2）", ggplot_editor_ui(ns("acf_editor"), height = "500px")),
      tabPanel("季节分解（ggplot2）", ggplot_editor_ui(ns("decomposition_editor"), height = "760px")),
      tabPanel("分析数据", DT::DTOutput(ns("table")))
    )
  )
}

timeseries_server <- function(id, data, directory = reactive(getwd())) {
  moduleServer(id, function(input, output, session) {
    result <- reactiveVal(NULL)
    status <- reactiveVal("选择时间和数值字段后，点击“运行时间序列分析”。")
    observeEvent(data(), {
      d <- data()
      choices <- setNames(as.character(seq_along(d)), paste0(seq_along(d), ". ", names(d)))
      preferred <- which(vapply(d, function(x) inherits(x, c("Date", "POSIXt")), logical(1)))
      fallback <- if (length(preferred)) as.character(preferred[1]) else unname(head(choices, 1))
      selected <- if (length(input$time) == 1 && input$time %in% unname(choices)) input$time else fallback
      updateSelectInput(session, "time", choices = choices, selected = selected)
    })
    observeEvent(list(data(), input$time), {
      d <- data()
      numeric <- setdiff(which(vapply(d, is.numeric, logical(1))), as.integer(input$time))
      choices <- setNames(as.character(numeric), paste0(numeric, ". ", names(d)[numeric]))
      selected <- if (length(input$value) == 1 && input$value %in% unname(choices)) input$value else unname(head(choices, 1))
      updateSelectInput(session, "value", choices = choices, selected = selected)
    })
    observe({
      result(NULL); status("数据或字段选择已更新，请点击“运行时间序列分析”。")
      data(); input$time; input$value; input$transform
    }, priority = 100)
    observeEvent(input$run, {
      result(NULL)
      tryCatch({
        prepared <- prepare_time_series(data(), input$time, input$value, input$transform)
        result(prepared)
        status(sprintf("分析完成：使用 %d 个时间点。", nrow(prepared$data)))
      }, error = function(e) {
        message <- if (inherits(e, "shiny.silent.error")) "请先导入数据并选择字段。" else conditionMessage(e)
        status(paste("未能分析：", message)); showNotification(message, type = "error", duration = 10)
      })
    })
    series_plot <- reactive({
      req(result())
      tryCatch(build_time_series_plot(result(), isTRUE(input$moving_average), input$window, input$trend),
        error = function(e) validate(need(FALSE, conditionMessage(e))))
    })
    output$status <- renderText(status())
    output$report <- renderText({ req(result()); result()$report })
    acf_plot <- reactive({
      req(result())
      tryCatch(build_time_series_acf_plot(result(), input$max_lag),
        error = function(e) validate(need(FALSE, conditionMessage(e))))
    })
    decomposition_plot <- reactive({
      req(result()); validate(need(isTRUE(input$decompose), "勾选“生成 STL 季节分解”后查看。"))
      tryCatch(build_time_series_decomposition_plot(result(), input$frequency),
        error = function(e) validate(need(FALSE, conditionMessage(e))))
    })
    ggplot_editor_server("series_editor", series_plot, directory, "easyr-time-series")
    ggplot_editor_server("acf_editor", acf_plot, directory, "easyr-time-series-acf")
    ggplot_editor_server("decomposition_editor", decomposition_plot, directory, "easyr-time-series-stl")
    output$table <- DT::renderDT({
      req(result())
      display <- result()$data; names(display) <- c(result()$time_name, result()$transform_label)
      DT::datatable(display, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
    })
    reactive(result())
  })
}
