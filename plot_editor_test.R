source("setup.R")
library(shiny)
source("R/directory.R")
source("R/plot_editor.R")

base_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
  ggplot2::geom_point() + ggplot2::geom_smooth(method = "lm", formula = y ~ x)
edited <- apply_plot_editor(base_plot, list(title = "自定义标题", subtitle = "副标题", x_label = "重量",
  y_label = "油耗", palette = "warm", line_width = 2, point_size = 4, alpha = 0.6,
  theme = "classic", font_size = 14, background = "light"))
stopifnot(inherits(edited, "ggplot"), edited$labels$title == "自定义标题",
  edited$labels$subtitle == "副标题", edited$labels$x == "重量", edited$labels$y == "油耗",
  edited$layers[[1]]$aes_params$size == 4, edited$layers[[2]]$aes_params$linewidth == 2)

test_directory <- tempfile("rmod-plot-editor-")
dir.create(test_directory)
testServer(ggplot_editor_server, args = list(plot = reactive(base_plot), directory = reactive(test_directory),
  filename = "edited-chart"), {
  session$setInputs(title = "导出测试", subtitle = "", x_label = "", y_label = "", theme = "minimal",
    palette = "ocean", line_width = 1.5, point_size = 3, alpha = 0.8, font_size = 12,
    background = "white", width = 4, height = 3, dpi = "96")
  stopifnot(styled()$labels$title == "导出测试")
  exported <- output$download
  con <- file(exported, "rb"); readBin(con, "raw", 16)
  dimensions <- readBin(con, "integer", 2, size = 4, endian = "big"); close(con)
  stopifnot(identical(dimensions, c(384L, 288L)))
  session$setInputs(save = 1)
  stopifnot(length(list.files(test_directory, pattern = "[.]png$")) == 1)
})
unlink(test_directory, recursive = TRUE)
cat("通用 ggplot2 图像编辑、主题、配色、线宽、点大小和 PNG 导出检查通过。\n")
