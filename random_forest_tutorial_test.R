source("setup.R")
library(shiny)
source("R/ai.R")
source("R/random_forest_tutorial.R")

simulation <- rf_tutorial_simulate(trees = 10, mtry = 1, seed = 2026)
stopifnot(nrow(simulation$data) == 140L, simulation$trees == 10L,
  ncol(simulation$individual) == 10L, nrow(simulation$model$inbag) == 140L,
  rf_tutorial_total_steps(simulation) == 17L,
  all(c("x1", "x2") %in% simulation$importance$字段), nzchar(rf_tutorial_font_family()))

expected_phases <- c("data", "bootstrap", "features", "split", "tree", "forest", "oob", "importance")
steps <- c(1L, 2L, 3L, 4L, 5L, 6L, simulation$trees + 6L, simulation$trees + 7L)
phases <- vapply(steps, function(step) rf_tutorial_step_info(step, simulation)$phase, character(1))
stopifnot(identical(phases, expected_phases))
plots <- lapply(steps, function(step) rf_tutorial_plot(simulation, step))
stopifnot(all(vapply(plots, inherits, logical(1), "ggplot")),
  nrow(rf_tutorial_vote_grid(simulation, 5)) == nrow(simulation$grid),
  nrow(rf_tutorial_tree_table(simulation)) > 1L)

testServer(rf_tutorial_server, {
  session$setInputs(trees = 8, mtry = 1, seed = 2026)
  session$flushReact()
  stopifnot(step() == 1L, simulation()$trees == 8L, inherits(tutorial_plot(), "ggplot"),
    nzchar(output$progress), nzchar(output$lesson), nzchar(output$stats), nzchar(output$tree_table))
  session$setInputs(`next` = 1); session$flushReact()
  stopifnot(step() == 2L)
  session$setInputs(previous = 1); session$flushReact()
  stopifnot(step() == 1L)
  old_seed <- simulation()$seed
  session$setInputs(regenerate = 1); session$flushReact()
  stopifnot(simulation()$seed == old_seed + 1L, step() == 1L)
})

cat("随机森林 Bootstrap、随机特征、分裂、投票、OOB、重要性分步教程与中文字体检查通过。\n")
