startup_ui <- function(id) {
  ns <- NS(id)
  tags$div(class = "startup-shell",
    tags$div(class = "startup-card",
      tags$div(class = "startup-brand",
        tags$div(class = "startup-brand-mark", icon("chart-simple")),
        tags$div(tags$h1("EasyR"), tags$p("数据分析工作台"))
      ),
      tags$div(class = "startup-actions",
        actionButton(ns("new_project"), "创建新项目", icon = icon("plus"),
          class = "btn-primary startup-primary"),
        tags$div(class = "startup-divider", tags$span("或者")),
        fileInput(ns("project_file"), "打开已有 EasyR 项目", accept = c(".easyr", ".rds"),
          buttonLabel = "选择项目文件", placeholder = "尚未选择项目文件"),
        tags$p(class = "startup-file-hint", "选择后自动打开项目"),
        actionButton(ns("sample"), "试用示例数据", icon = icon("flask"),
          class = "btn-link startup-sample"),
        tags$div(class = "startup-status", textOutput(ns("status")))
      )
    )
  )
}

startup_server <- function(id, imported, project_channel, app_input, app_session, enter_workspace) {
  moduleServer(id, function(input, output, session) {
    status <- reactiveVal("创建一个新项目，或打开之前保存的 .easyr 项目。")

    observeEvent(input$new_project, enter_workspace())

    observeEvent(input$sample, {
      imported$add(list(`内置示例 iris` = iris))
      enter_workspace()
    })

    observeEvent(input$project_file, {
      req(input$project_file)
      tryCatch({
        project <- restore_easyr_project_upload(
          input$project_file, imported, project_channel, app_input, app_session
        )
        status(sprintf("恢复完成：%d 个数据集。", length(project$import$datasets)))
        enter_workspace()
        showNotification("EasyR 项目已恢复。", type = "message")
      }, error = function(e) {
        status(paste0("打开失败：", conditionMessage(e)))
        showNotification(conditionMessage(e), type = "error", duration = 10)
      })
    })

    output$status <- renderText(status())
  })
}
