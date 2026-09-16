read_table_file <- function(path, filename, encoding = "UTF-8", sep = ",", sheet = NULL) {
  ext <- tolower(tools::file_ext(filename))
  data <- switch(ext,
    csv = read.csv(path, fileEncoding = encoding, sep = sep,
                   check.names = FALSE, stringsAsFactors = FALSE),
    xlsx = as.data.frame(readxl::read_excel(path, sheet = sheet)),
    xls = as.data.frame(readxl::read_excel(path, sheet = sheet)),
    stop("请选择 CSV、XLS 或 XLSX 文件。")
  )
  if (!ncol(data)) stop("文件没有可读取的列。")
  data
}

import_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("01 导入数据"),
    fileInput(ns("file"), "选择表格（最大 30 MB）", accept = c(".csv", ".xlsx", ".xls"),
              buttonLabel = "选择文件", placeholder = "尚未选择文件"),
    selectInput(ns("encoding"), "CSV 编码", c("UTF-8", "GB18030")),
    selectInput(ns("sep"), "CSV 分隔符", c("逗号" = ",", "分号" = ";", "制表符" = "\t")),
    uiOutput(ns("sheet_ui")),
    actionButton(ns("import"), "导入数据", class = "btn-primary"),
    actionButton(ns("demo"), "试用示例"),
    hr(), textOutput(ns("status")),
    helpText("CSV 默认第一行为列名。更改选项后，请再次点击导入。")
  )
}

import_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactiveVal(NULL)
    status <- reactiveVal("选择文件，或点击“试用示例”开始。")
    sheets <- reactive({
      req(input$file)
      if (!tolower(tools::file_ext(input$file$name)) %in% c("xls", "xlsx")) return(NULL)
      tryCatch(readxl::excel_sheets(input$file$datapath), error = function(e) {
        showNotification(paste("无法读取工作表：", conditionMessage(e)), type = "error")
        NULL
      })
    })
    output$sheet_ui <- renderUI({
      if (length(sheets())) selectInput(session$ns("sheet"), "Excel 工作表", sheets())
    })
    observeEvent(input$import, {
      req(input$file)
      tryCatch({
        result <- read_table_file(input$file$datapath, input$file$name,
                                  input$encoding, input$sep, input$sheet)
        data(result)
        status(paste("当前数据：", input$file$name))
      }, error = function(e) {
        showNotification(paste("导入失败：", conditionMessage(e)), type = "error", duration = 10)
        status("导入失败。若已有数据，右侧仍显示上次成功导入的内容。")
      })
    })
    observeEvent(input$demo, {
      data(iris)
      status("当前数据：内置鸢尾花示例 iris")
    })
    output$status <- renderText(status())
    reactive(data())
  })
}
