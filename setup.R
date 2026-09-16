packages <- c("shiny", "readxl", "DT")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
message("安装完成。运行 shiny::runApp() 启动。")
