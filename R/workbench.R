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
    downloadButton(ns("csv"), "下载处理后的 CSV"),
    actionButton(ns("save_csv"), "保存 CSV 到工作目录"),
    tags$div(style = "overflow-wrap:anywhere", textOutput(ns("saved_csv")))
  )
}

workbench_server <- function(id, original, directory = reactive(getwd()), dataset_id = reactive("default")) {
  moduleServer(id, function(input, output, session) {
    configurations <- reactiveVal(list())
    last_id <- reactiveVal(NULL)
    capture_config <- function(id) {
      if (!length(id) || is.null(input$columns)) return()
      values <- configurations()
      values[[id]] <- list(columns = input$columns, missing = if (is.null(input$missing)) "keep" else input$missing,
        deduplicate = isTRUE(input$deduplicate))
      configurations(values)
    }
    restore_config <- function(id, force_default = FALSE) {
      d <- original(); req(d)
      choices <- setNames(as.character(seq_along(d)), paste0(seq_along(d), ". ", names(d)))
      saved <- if (!force_default) configurations()[[id]] else NULL
      selected <- if (is.null(saved)) unname(choices) else intersect(saved$columns, unname(choices))
      if (!length(selected)) selected <- unname(choices)
      freezeReactiveValue(input, "columns"); freezeReactiveValue(input, "missing"); freezeReactiveValue(input, "deduplicate")
      updateSelectizeInput(session, "columns", choices = choices, selected = selected)
      updateSelectInput(session, "missing", selected = if (is.null(saved)) "keep" else saved$missing)
      updateCheckboxInput(session, "deduplicate", value = if (is.null(saved)) FALSE else saved$deduplicate)
    }
    observeEvent(list(dataset_id(), original()), {
      new_id <- dataset_id()
      old_id <- last_id()
      if (length(old_id) && !identical(old_id, new_id)) capture_config(old_id)
      if (!length(new_id) || is.null(original())) { last_id(NULL); return() }
      restore_config(new_id)
      last_id(new_id)
    }, ignoreNULL = FALSE)
    observeEvent(list(input$columns, input$missing, input$deduplicate), {
      if (length(last_id())) capture_config(last_id())
    }, ignoreInit = TRUE)
    observeEvent(input$reset, {
      req(dataset_id(), original())
      values <- configurations(); values[[dataset_id()]] <- NULL; configurations(values)
      restore_config(dataset_id(), force_default = TRUE)
    })
    data <- reactive({
      req(original())
      validate(need(length(input$columns) > 0, "请在左侧至少选择一个字段。"))
      indices <- as.integer(input$columns)
      req(all(indices %in% seq_along(original())))
      clean_table(original(), indices, input$missing, isTRUE(input$deduplicate))
    })
    write_csv <- function(file) {
        value <- data()
        con <- file(file, open = "wb")
        on.exit(close(con))
        writeBin(charToRaw("\ufeff"), con)
        lines <- capture.output(write.csv(value, row.names = FALSE, na = ""))
        writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = "\r\n"), "\r\n"))), con)
    }
    output$csv <- downloadHandler(
      filename = function() paste0("easyr-", Sys.Date(), ".csv"),
      content = write_csv
    )
    saved_csv <- reactiveVal("")
    output$saved_csv <- renderText(saved_csv())
    observeEvent(input$save_csv, {
      req(data())
      tryCatch({
        path <- save_to_workdir(directory(), "easyr-data", ".csv", write_csv)
        saved_csv(paste("上次保存：", path))
        showNotification("CSV 已保存到工作目录。", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })
    data
  })
}
