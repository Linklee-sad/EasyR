source("setup.R")
library(shiny)
source("R/directory.R")
source("R/plot_editor.R")
source("R/regression.R")
source("R/pca.R")

result <- fit_pca_analysis(iris, 1:4, TRUE, 5)
reference <- stats::prcomp(iris[, 1:4], center = TRUE, scale. = TRUE)
stopifnot(result$used == nrow(iris), result$excluded == 0,
  identical(result$variables, names(iris)[1:4]),
  isTRUE(all.equal(result$model$sdev, reference$sdev)),
  isTRUE(all.equal(sum(result$variance$方差解释率), 1)),
  isTRUE(all.equal(tail(result$variance$累计解释率, 1), 1)),
  nrow(result$loadings) == 4, nrow(result$scores) == nrow(iris),
  identical(result$scores$分组, as.character(iris$Species)),
  grepl("80%", result$report), grepl("载荷", result$report))

unscaled <- fit_pca_analysis(mtcars, 1:5, FALSE)
unscaled_reference <- stats::prcomp(mtcars[, 1:5], center = TRUE, scale. = FALSE)
stopifnot(!unscaled$standardize, isTRUE(all.equal(unscaled$model$sdev, unscaled_reference$sdev)))

with_missing <- data.frame(a = c(1:9, NA), b = 11:20, c = rep(1, 10), group = rep(c("A", "B"), 5))
cleaned <- fit_pca_analysis(with_missing, 1:3, TRUE, 4)
stopifnot(cleaned$used == 9, cleaned$excluded == 1, identical(cleaned$removed, "c"),
  identical(cleaned$variables, c("a", "b")))
invalid <- tryCatch({ fit_pca_analysis(iris, c(1, 5)); "unexpected" }, error = conditionMessage)
stopifnot(grepl("数值字段", invalid))

scree <- build_pca_scree_plot(result)
scores <- build_pca_scores_plot(result)
loadings <- build_pca_loadings_plot(result)
stopifnot(inherits(scree, "ggplot"), inherits(scores, "ggplot"), inherits(loadings, "ggplot"))
test_directory <- tempfile("easyr-pca-")
dir.create(test_directory)
plots <- list(scree = scree, scores = scores, loadings = loadings)
for (name in names(plots)) {
  file <- file.path(test_directory, paste0(name, ".png"))
  ggplot2::ggsave(file, plots[[name]], width = 8, height = 5, dpi = 96)
  stopifnot(file.info(file)$size > 1000)
}

testServer(pca_server, args = list(data = reactive(iris), directory = reactive(test_directory)), {
  session$setInputs(variables = c("1", "2", "3", "4"), group = "5", standardize = TRUE)
  session$setInputs(run = 1)
  stopifnot(!is.null(result()), result()$used == nrow(iris), grepl("分析完成", status()))
  report_file <- output$download_report
  scores_file <- output$download_scores
  loadings_file <- output$download_loadings
  stopifnot(file.exists(report_file), file.exists(scores_file), file.exists(loadings_file),
    grepl("主成分分析", paste(readLines(report_file, warn = FALSE), collapse = "")),
    nrow(read.csv(scores_file, fileEncoding = "UTF-8-BOM")) == nrow(iris),
    nrow(read.csv(loadings_file, fileEncoding = "UTF-8-BOM")) == 4)
  session$setInputs(save_report = 1)
  stopifnot(length(list.files(test_directory, pattern = "easyr-pca.*[.]txt$")) == 1)
  session$setInputs(standardize = FALSE)
  stopifnot(is.null(result()), grepl("已更新", status()))
})

# Regression test: datasets may have numeric columns but no field with 2–20
# distinct values. The grouping selector must then contain only “不分组”.
no_group_data <- data.frame(a = seq_len(40), b = seq_len(40) * 2, c = seq_len(40) * 3)
testServer(pca_server, args = list(data = reactive(no_group_data), directory = reactive(test_directory)), {
  session$flushReact()
  stopifnot(is.null(result()), grepl("已更新", status()))
})

unlink(test_directory, recursive = TRUE)
cat("PCA 标准化、方差解释率、载荷、得分、空分组选项、分组图形和结果导出检查通过。\n")
