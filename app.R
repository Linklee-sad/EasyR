required <- c("shiny", "readxl", "DT")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("请先运行 Rscript setup.R，缺少：", paste(missing, collapse = ", "))
library(shiny)
options(shiny.maxRequestSize = 30 * 1024^2)
source("R/import.R")
source("R/preview.R")

ui <- fluidPage(
  tags$head(tags$style(HTML("body{background:#f5f7fb;color:#172b4d} .container-fluid{max-width:1400px;margin:auto} .well{background:white;border:1px solid #e3e8ef;border-radius:12px} h2{font-weight:700} .btn-primary{background:#2563eb;border-color:#2563eb}"))),
  titlePanel("R Mod · 数据工作台"),
  p("点击导入，预览数据。让 R 的每一项能力都成为一个可操作的模块。"),
  sidebarLayout(sidebarPanel(import_ui("import")), mainPanel(preview_ui("preview")))
)
server <- function(input, output, session) {
  data <- import_server("import")
  preview_server("preview", data)
}
shinyApp(ui, server)
