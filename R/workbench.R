clean_table <- function(data, columns, missing = "keep", deduplicate = FALSE) {
  if (!length(columns)) stop("请至少选择一个字段。")
  result <- data[, as.integer(columns), drop = FALSE]
  if (missing == "drop") result <- result[complete.cases(result), , drop = FALSE]
  if (missing == "median") {
    result[] <- lapply(result, function(x) {
      if (is.numeric(x) && any(is.finite(x))) x[is.na(x)] <- median(x[is.finite(x)])
      x
    })
  }
  if (deduplicate) result <- unique(result)
  result
}

workbench_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("02 整理数据"),
    selectizeInput(ns("columns"), "保留字段", choices = NULL, multiple = TRUE),
    selectInput(ns("missing"), "缺失值", c("保留原样" = "keep", "删除含缺失值的行" = "drop", "用中位数填充数值字段" = "median")),
    checkboxInput(ns("deduplicate"), "删除重复行", FALSE),
    actionButton(ns("reset"), "恢复原始数据"),
    helpText("选项修改后立即生效。重复行按所选字段判断；中位数填充保留非数值字段和全空字段。"),
    downloadButton(ns("csv"), "下载处理后的 CSV")
  )
}

workbench_server <- function(id, original) {
  moduleServer(id, function(input, output, session) {
    reset <- function() {
      req(original())
      choices <- setNames(as.character(seq_along(original())), paste0(seq_along(original()), ". ", names(original())))
      updateSelectizeInput(session, "columns", choices = choices, selected = unname(choices))
      updateSelectInput(session, "missing", selected = "keep")
      updateCheckboxInput(session, "deduplicate", value = FALSE)
    }
    observeEvent(original(), reset())
    observeEvent(input$reset, reset())
    data <- reactive({
      req(original())
      validate(need(length(input$columns) > 0, "请在左侧至少选择一个字段。"))
      indices <- as.integer(input$columns)
      req(all(indices %in% seq_along(original())))
      clean_table(original(), indices, input$missing, isTRUE(input$deduplicate))
    })
    output$csv <- downloadHandler(
      filename = function() paste0("r-mod-", Sys.Date(), ".csv"),
      content = function(file) {
        value <- data()
        con <- file(file, open = "wb")
        on.exit(close(con))
        writeBin(charToRaw("\ufeff"), con)
        lines <- capture.output(write.csv(value, row.names = FALSE, na = ""))
        writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = "\r\n"), "\r\n"))), con)
      }
    )
    data
  })
}

analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("描述统计与绘图"),
    p("对当前处理后的数据计算统计量。"),
    tableOutput(ns("summary")),
    fluidRow(
      column(4, selectInput(ns("kind"), "图形", c("直方图" = "hist", "散点图" = "scatter", "类别频数图" = "bar"))),
      column(4, selectInput(ns("x"), "横轴 / 字段", NULL)),
      column(4, conditionalPanel(sprintf("input['%s'] == 'scatter'", ns("kind")), selectInput(ns("y"), "纵轴", NULL)))
    ),
    plotOutput(ns("plot"), height = "420px"),
    helpText("数值图忽略缺失值和无穷值；类别图显示频数最高的 20 类（不计缺失值）。"),
    downloadButton(ns("png"), "下载 PNG 图片")
  )
}

analysis_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observeEvent(list(data(), input$kind), {
      d <- data()
      eligible <- if (identical(input$kind, "bar")) seq_along(d) else which(vapply(d, is.numeric, logical(1)))
      choices <- setNames(as.character(eligible), paste0(eligible, ". ", names(d)[eligible]))
      selected <- function(old, fallback) if (length(old) == 1 && old %in% unname(choices)) old else fallback
      first <- if (length(choices)) unname(choices[1]) else character()
      second <- if (length(choices) > 1) unname(choices[2]) else first
      updateSelectInput(session, "x", choices = choices, selected = selected(input$x, first))
      updateSelectInput(session, "y", choices = choices, selected = selected(input$y, second))
    })
    output$summary <- renderTable({
      d <- data()
      indices <- which(vapply(d, is.numeric, logical(1)))
      validate(need(length(indices) > 0, "当前没有数值字段，可选择类别频数图。"))
      do.call(rbind, lapply(indices, function(i) {
        x <- d[[i]]; x <- x[is.finite(x)]
        data.frame(字段 = names(d)[i], 有效数 = length(x),
          均值 = if (length(x)) mean(x) else NA_real_,
          中位数 = if (length(x)) median(x) else NA_real_,
          标准差 = if (length(x) > 1) sd(x) else NA_real_, check.names = FALSE)
      }))
    }, digits = 3, striped = TRUE)
    draw <- function() {
      d <- data()
      validate(need(nrow(d) > 0, "没有可绘制的数据，请调整清洗选项。"))
      req(input$x)
      i <- as.integer(input$x)
      req(i %in% seq_along(d))
      x <- d[[i]]
      par(mar = c(6, 4, 3, 1))
      if (input$kind == "bar") {
        counts <- head(sort(table(as.character(x), useNA = "no"), decreasing = TRUE), 20)
        validate(need(length(counts) > 0, "此字段没有非缺失数据。"))
        barplot(counts, las = 2, col = "#3b82f6", border = NA, ylab = "Count", main = names(d)[i])
      } else {
        validate(need(is.numeric(x), "请选择数值字段。"))
        if (input$kind == "scatter") {
          req(input$y)
          j <- as.integer(input$y); req(j %in% seq_along(d))
          y <- d[[j]]
          validate(need(is.numeric(y), "请选择数值纵轴。"))
          ok <- is.finite(x) & is.finite(y)
          validate(need(any(ok), "这两个字段没有可配对的有效数值。"))
          plot(x[ok], y[ok], pch = 19, col = "#2563eb88", xlab = names(d)[i], ylab = names(d)[j])
        } else {
          x <- x[is.finite(x)]
          validate(need(length(x) > 0, "此字段没有有效数值。"))
          hist(x, col = "#3b82f6", border = "white", main = names(d)[i], xlab = names(d)[i])
        }
      }
    }
    output$plot <- renderPlot(draw())
    output$png <- downloadHandler(
      filename = function() paste0("r-mod-chart-", Sys.Date(), ".png"),
      content = function(file) {
        png(file, width = 1400, height = 900, res = 150)
        on.exit(dev.off())
        draw()
      }
    )
  })
}
