source("setup.R")
library(shiny)
source("R/import.R")
source("R/project.R")
source("R/home.R")

rendered <- htmltools::renderTags(startup_ui("startup"))$html
stopifnot(
  grepl("startup-card", rendered),
  grepl("创建新项目", rendered),
  grepl("打开已有 EasyR 项目", rendered),
  grepl("选择后自动打开项目", rendered),
  !grepl("startup-open_project", rendered, fixed = TRUE),
  grepl("试用示例数据", rendered)
)

values <- reactiveVal(list())
active <- reactiveVal(NULL)
revision <- reactiveVal(0L)
imported <- list(
  datasets = reactive(values()),
  name = reactive(active()),
  add = function(items) {
    values(items)
    active(names(items)[1L])
    revision(revision() + 1L)
  }
)
entered <- reactiveVal(0L)
app_session <- new.env(parent = emptyenv())
app_session$onFlushed <- function(callback, once = FALSE) callback()
app_session$sendInputMessage <- function(inputId, message) invisible(NULL)
channel <- new.env(parent = emptyenv())

testServer(startup_server, args = list(
  imported = imported, project_channel = channel, app_input = reactiveValues(),
  app_session = app_session, enter_workspace = function() entered(entered() + 1L)
), {
  session$setInputs(new_project = 1)
  session$flushReact()
  stopifnot(entered() == 1L)
  session$setInputs(sample = 1)
  session$flushReact()
  stopifnot(entered() == 2L, identical(names(values()), "内置示例 iris"), identical(values()[[1L]], iris))
})

app_env <- new.env(parent = globalenv())
sys.source("app.R", envir = app_env)
app_html <- htmltools::renderTags(app_env$ui)$html
stopifnot(
  grepl("startup-new_project", app_html, fixed = TRUE),
  grepl("data-value=\"data_workspace\"", app_html, fixed = TRUE),
  !grepl("data-value=\"home\"", app_html, fixed = TRUE)
)

startup_project <- build_easyr_project(
  list(datasets = list("startup.csv" = data.frame(x = 1:3, y = 4:6)), active = "startup.csv")
)
startup_project_file <- tempfile(fileext = ".easyr")
saveRDS(startup_project, startup_project_file)
startup_upload <- data.frame(
  name = "startup.easyr", size = file.info(startup_project_file)$size,
  type = "application/octet-stream", datapath = startup_project_file,
  stringsAsFactors = FALSE
)

testServer(app_env$server, {
  session$flushReact()
  stopifnot(identical(workspace_started(), FALSE))
  session$setInputs(`startup-new_project` = 1)
  session$flushReact()
  stopifnot(identical(workspace_started(), TRUE))
})

testServer(app_env$server, {
  session$setInputs(`startup-project_file` = startup_upload)
  session$flushReact()
  stopifnot(identical(workspace_started(), TRUE), grepl("startup.csv", output[["import-dataset_status"]]))
})
unlink(startup_project_file)

cat("开始界面、新建项目、示例数据和无首页工作台检查通过。\n")
