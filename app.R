source("setup.R", local = TRUE)
library(shiny)
# Transport ceiling for a batch request. The import panel still validates each
# individual file against the selected limit of up to 1000 MB.
options(shiny.maxRequestSize = 4 * 1024^3)
source("R/import.R")
source("R/i18n.R")
source("R/preview.R")
source("R/workbench.R")
source("R/plotting.R")
source("R/directory.R")
source("R/plot_editor.R")
source("R/regression.R")
source("R/timeseries.R")

ui <- fluidPage(
  tags$head(tags$style(HTML("body{background:#f5f7fb;color:#172b4d} .container-fluid{max-width:1400px;margin:auto} .well{background:white;border:1px solid #e3e8ef;border-radius:12px} h2{font-weight:700} .btn-primary{background:#2563eb;border-color:#2563eb} .dataset-status-bar{background:#eaf2ff;border:1px solid #bdd3f8;border-radius:12px;padding:12px 16px 2px;margin:10px 0 16px} .dataset-status-text{padding-top:30px;font-weight:600;color:#174a8b} .dataset-status-bar .btn{margin-top:25px} .language-switch{position:fixed;right:22px;top:14px;z-index:2000} .language-switch .btn{background:white;border:1px solid #9db7df;box-shadow:0 2px 8px rgba(23,74,139,.16);font-weight:600}"))),
  language_switch_ui(),
  titlePanel("EasyR · 数据工作台"),
  p("导入表格、整理数据、绘图并导出结果，全程通过按钮操作。"),
  dataset_bar_ui("import"),
  sidebarLayout(sidebarPanel(directory_ui("directory"), hr(), import_ui("import"), hr(), workbench_ui("clean")),
    mainPanel(tabsetPanel(
      tabPanel("数据预览", preview_ui("preview")),
      tabPanel("统计与绘图", analysis_ui("analysis")),
      tabPanel("线性回归", regression_ui("regression")),
      tabPanel("时间序列", timeseries_ui("timeseries"))
    )))
)
server <- function(input, output, session) {
  language_switch_server(input, output, session)
  directory <- directory_server("directory")
  imported <- import_server("import")
  data <- workbench_server("clean", imported$data, directory, imported$name)
  preview_server("preview", data)
  analysis_server("analysis", data, directory)
  regression_server("regression", data, directory)
  timeseries_server("timeseries", data, directory)
}
shinyApp(ui, server)
