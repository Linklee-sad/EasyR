source("setup.R")
library(shiny)
source("R/directory.R")
source("R/plot_editor.R")
source("R/regression.R")

fit <- fit_readable_lm(mtcars, 1, c(6, 4))
expected <- lm(mpg ~ wt + hp, data = mtcars)
stopifnot(isTRUE(all.equal(unname(coef(fit$model)), unname(coef(expected)))))
stopifnot(isTRUE(all.equal(fit$coefficients$下限95, unname(confint(expected)[, 1]))))
expected_standardized <- unname(coef(expected)[c("wt", "hp")]) * c(sd(mtcars$wt), sd(mtcars$hp)) / sd(mtcars$mpg)
expected_vif <- 1 / (1 - cor(mtcars$wt, mtcars$hp)^2)
stopifnot(isTRUE(all.equal(fit$coefficients$标准化系数[-1], expected_standardized)),
  isTRUE(all.equal(fit$coefficients$VIF[-1], rep(expected_vif, 2))))
stopifnot(fit$used == 32, fit$excluded == 0, grepl("专业统计解读", fit$report),
  grepl("F\\(", fit$report), grepl("AIC", fit$report), grepl("最大 VIF", fit$report),
  grepl("标准化 β", fit$report), grepl("推断前提与解释边界", fit$report),
  grepl("不能仅凭本回归结果作因果解释", fit$report))
stopifnot(inherits(build_lm_fit_plot(fit), "ggplot"), inherits(build_lm_residual_plot(fit), "ggplot"),
  inherits(build_lm_qq_plot(fit), "ggplot"))
stopifnot(length(ggplot2::ggplot_build(build_lm_fit_plot(fit))$data) == 2)

simple <- fit_readable_lm(mtcars, 1, 6)
simple_plot <- build_lm_fit_plot(simple)
stopifnot(inherits(simple_plot, "ggplot"), length(ggplot2::ggplot_build(simple_plot)$data) == 2,
  grepl("95%", simple_plot$labels$subtitle))

categorical <- fit_readable_lm(iris, 1, c(2, 5))
expected <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
stopifnot(isTRUE(all.equal(unname(coef(categorical$model)), unname(coef(expected)))))
stopifnot(any(grepl("versicolor vs setosa", categorical$coefficients$字段)))
stopifnot(grepl("以“setosa”为参照类别", categorical$report), grepl("参照水平“setosa”", categorical$report))

dirty <- mtcars
dirty$mpg[1] <- NA
dirty$wt[2] <- Inf
dirty$hp[3] <- NA
stopifnot(fit_readable_lm(dirty, 1, c(6, 4))$excluded == 3)
odd <- mtcars
names(odd)[c(1, 6)] <- c("收入（元）", "a` + system('never')")
stopifnot(fit_readable_lm(odd, 1, 6)$used == 32)

fails <- function(expr, pattern) {
  message <- tryCatch({ force(expr); "unexpected success" }, error = conditionMessage)
  stopifnot(grepl(pattern, message))
}
fails(fit_readable_lm(mtcars, 1, integer()), "至少选择")
fails(fit_readable_lm(mtcars, 1, 1), "不能同时")
fails(fit_readable_lm(data.frame(y = rep(1, 10), x = 1:10), 1, 2), "没有变化")
fails(fit_readable_lm(data.frame(y = 1:10, x = 1:10, z = 2 * (1:10)), 1, 2:3), "共线")
fails(fit_readable_lm(data.frame(y = 1:3, x = 1:3, z = c(1, 3, 2)), 1, 2:3), "样本数不足")
fails(fit_readable_lm(data.frame(y = 1:10, g = rep("a", 10)), 1, 2), "只有一个类别")
perfect <- fit_readable_lm(data.frame(y = 1:10, x = 1:10), 1, 2)
stopifnot(perfect$near_perfect, grepl("完美拟合", perfect$report))

test_directory <- tempfile("rmod-regression-")
dir.create(test_directory)
changing <- reactiveVal(mtcars)
testServer(regression_server, args = list(data = reactive(changing()), directory = reactive(test_directory)), {
  session$setInputs(outcome = "1", predictors = c("6", "4"))
  session$setInputs(run = 1)
  stopifnot(!is.null(result()), result()$used == 32, nzchar(output$report))
  stopifnot(nzchar(output$coefficients))
  stopifnot(inherits(fit_plot(), "ggplot"), inherits(residual_plot(), "ggplot"), inherits(qq_plot(), "ggplot"))
  report_file <- output$download
  stopifnot(any(grepl("专业统计解读", readLines(report_file, encoding = "UTF-8"))))
  session$setInputs(save = 1)
  stopifnot(length(list.files(test_directory)) == 1)
  session$setInputs(predictors = "6")
  stopifnot(is.null(result()))
  session$setInputs(run = 2)
  stopifnot(!is.null(result()))
  changing(mtcars[-1, ])
  session$flushReact()
  stopifnot(is.null(result()))
})
unlink(test_directory, recursive = TRUE)
cat("lm 数值对照、专业统计解读、异常输入、结果失效与报告保存检查通过。\n")
