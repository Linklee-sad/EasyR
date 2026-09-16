validate_workdir <- function(path) {
  if (length(path) != 1 || is.na(path) || !nzchar(trimws(path))) stop("请选择或输入一个文件夹。", call. = FALSE)
  path <- path.expand(trimws(path))
  if (!dir.exists(path)) stop("文件夹不存在，请重新选择。", call. = FALSE)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (file.access(path, 4) != 0 || file.access(path, 2) != 0) stop("此文件夹没有读取或写入权限，请选择其他文件夹。", call. = FALSE)
  path
}

save_to_workdir <- function(directory, prefix, extension, writer) {
  directory <- validate_workdir(directory)
  destination <- tempfile(paste0(prefix, "-", format(Sys.time(), "%Y%m%d-%H%M%S"), "-"), tmpdir = directory, fileext = extension)
  complete <- FALSE
  on.exit(if (!complete && file.exists(destination)) unlink(destination))
  writer(destination)
  complete <- TRUE
  destination
}

directory_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("工作目录"),
    textInput(ns("path"), "文件夹路径", placeholder = "选择文件夹，或粘贴完整路径"),
    actionButton(ns("browse"), "浏览文件夹"),
    actionButton(ns("confirm"), "确定工作目录", class = "btn-primary"),
    tags$div(style = "margin-top:10px;overflow-wrap:anywhere", textOutput(ns("current"))),
    helpText("点击“保存到工作目录”会把结果写入此文件夹。普通下载按钮仍由浏览器决定保存位置。")
  )
}

directory_server <- function(id, start = getwd()) {
  moduleServer(id, function(input, output, session) {
    start_path <- normalizePath(start, winslash = "/", mustWork = TRUE)
    current <- reactiveVal(start_path)
    browsing <- reactiveVal(start_path)
    updateTextInput(session, "path", value = start_path)
    output$current <- renderText(paste("当前工作目录：", current()))
    output$browsing <- renderText(browsing())
    output$folders <- renderUI({
      folders <- sort(list.dirs(browsing(), full.names = TRUE, recursive = FALSE))
      selectInput(session$ns("child"), "子文件夹", choices = setNames(folders, basename(folders)))
    })
    observeEvent(input$browse, {
      browsing(current())
      showModal(modalDialog(
        title = "选择工作目录", size = "m",
        tags$div(style = "overflow-wrap:anywhere", textOutput(session$ns("browsing"))),
        uiOutput(session$ns("folders")),
        actionButton(session$ns("up"), "上一级"),
        actionButton(session$ns("home"), "个人文件夹"),
        actionButton(session$ns("enter"), "打开子文件夹"),
        footer = tagList(modalButton("取消"), actionButton(session$ns("choose"), "选择此文件夹", class = "btn-primary"))
      ))
    })
    observeEvent(input$up, browsing(dirname(browsing())))
    observeEvent(input$home, browsing(path.expand("~")))
    observeEvent(input$enter, {
      req(input$child)
      if (dir.exists(input$child)) browsing(normalizePath(input$child, winslash = "/"))
    })
    observeEvent(input$choose, {
      updateTextInput(session, "path", value = browsing())
      removeModal()
      showNotification("已选择文件夹，请点击“确定工作目录”使其生效。", type = "message")
    })
    observeEvent(input$confirm, {
      tryCatch({
        selected <- validate_workdir(input$path)
        current(selected)
        updateTextInput(session, "path", value = selected)
        showNotification("工作目录已更新。", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })
    reactive(current())
  })
}
