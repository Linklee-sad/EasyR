preview_ui <- function(id) {
  ns <- NS(id)
  tagList(h3("查看处理后的数据"), textOutput(ns("overview")),
          tabsetPanel(
            tabPanel("数据预览", DT::DTOutput(ns("table"))),
            tabPanel("字段概况", DT::DTOutput(ns("columns"))),
            tabPanel("AI 数据顾问", ai_data_advisor_ui(ns("ai_advisor")))
          ))
}

preview_server <- function(id, data, ai_config = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    ai_data_advisor_server("ai_advisor", data, ai_config)
    output$overview <- renderText({
      if (is.null(data())) return("尚未导入数据。请在左侧选择文件或试用示例。")
      sprintf("%s 行 · %s 列 · %s 个缺失值", nrow(data()), ncol(data()), sum(is.na(data())))
    })
    output$table <- DT::renderDT({
      req(data())
      DT::datatable(data(), rownames = FALSE, escape = TRUE,
                    options = list(pageLength = 10, scrollX = TRUE))
    }, server = TRUE)
    output$columns <- DT::renderDT({
      req(data())
      fields <- data.frame(
        字段 = names(data()),
        类型 = vapply(data(), function(x) paste(class(x), collapse = "/"), character(1)),
        缺失数 = vapply(data(), function(x) sum(is.na(x)), integer(1)),
        check.names = FALSE
      )
      DT::datatable(fields, rownames = FALSE, escape = TRUE, options = list(scrollX = TRUE))
    })
  })
}
