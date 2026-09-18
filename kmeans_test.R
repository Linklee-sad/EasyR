source("setup.R")
library(shiny)
source("R/directory.R")
source("R/plot_editor.R")
source("R/regression.R")
source("R/kmeans.R")

result <- fit_kmeans_analysis(iris, 1:4, centers = 3, standardize = TRUE,
  nstart = 20, iter_max = 100, seed = 2026)
repeat_result <- fit_kmeans_analysis(iris, 1:4, centers = 3, standardize = TRUE,
  nstart = 20, iter_max = 100, seed = 2026)
stopifnot(result$used == nrow(iris), result$excluded == 0,
  nrow(result$centers) == 3, sum(result$summary$样本数) == nrow(iris),
  nrow(result$assignments) == nrow(iris),
  is.finite(result$average_silhouette), result$average_silhouette > 0,
  result$quality$数值[result$quality$指标 == "组间变异占比"] > 0,
  identical(result$model$cluster, repeat_result$model$cluster),
  grepl("轮廓系数", result$report), grepl("欧氏距离", result$report))

unscaled <- fit_kmeans_analysis(mtcars, 1:4, centers = 3, standardize = FALSE,
  nstart = 10, iter_max = 100, seed = 9)
stopifnot(!unscaled$standardize, nrow(unscaled$centers) == 3,
  all(names(mtcars)[1:4] %in% names(unscaled$centers)))

with_missing <- data.frame(a = c(1:11, NA), b = c(12:23), constant = 1)
cleaned <- fit_kmeans_analysis(with_missing, 1:3, centers = 2, standardize = TRUE,
  nstart = 5, iter_max = 100, seed = 1)
stopifnot(cleaned$used == 11, cleaned$excluded == 1, identical(cleaned$removed, "constant"),
  identical(cleaned$variables, c("a", "b")))
invalid <- tryCatch({ fit_kmeans_analysis(iris, 1:4, centers = 21); "unexpected" }, error = conditionMessage)
stopifnot(grepl("聚类数量", invalid))

cluster_plot <- build_kmeans_cluster_plot(result)
elbow_plot <- build_kmeans_elbow_plot(result)
silhouette_plot <- build_kmeans_silhouette_plot(result)
stopifnot(inherits(cluster_plot, "ggplot"), inherits(elbow_plot, "ggplot"), inherits(silhouette_plot, "ggplot"))
test_directory <- tempfile("easyr-kmeans-")
dir.create(test_directory)
plots <- list(clusters = cluster_plot, elbow = elbow_plot, silhouette = silhouette_plot)
for (name in names(plots)) {
  file <- file.path(test_directory, paste0(name, ".png"))
  ggplot2::ggsave(file, plots[[name]], width = 8, height = 5, dpi = 96)
  stopifnot(file.info(file)$size > 1000)
}

testServer(kmeans_server, args = list(data = reactive(iris), directory = reactive(test_directory)), {
  session$setInputs(variables = c("1", "2", "3", "4"), centers = 3, standardize = TRUE,
    seed = 2026, nstart = 10, iter_max = 100)
  session$setInputs(run = 1)
  stopifnot(!is.null(result()), result()$used == nrow(iris), grepl("聚类完成", status()))
  report_file <- output$download_report
  assignments_file <- output$download_assignments
  centers_file <- output$download_centers
  stopifnot(file.exists(report_file), file.exists(assignments_file), file.exists(centers_file),
    grepl("K-means", paste(readLines(report_file, warn = FALSE), collapse = "")),
    nrow(read.csv(assignments_file, fileEncoding = "UTF-8-BOM")) == nrow(iris),
    nrow(read.csv(centers_file, fileEncoding = "UTF-8-BOM")) == 3)
  session$setInputs(save_report = 1)
  stopifnot(length(list.files(test_directory, pattern = "easyr-kmeans.*[.]txt$")) == 1)
  session$setInputs(seed = 7)
  stopifnot(is.null(result()), grepl("已更新", status()))
})

unlink(test_directory, recursive = TRUE)
cat("K-means 标准化、聚类中心、质量指标、肘部图、轮廓系数和结果导出检查通过。\n")
