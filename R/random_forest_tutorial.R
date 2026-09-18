rf_tutorial_data <- function(n = 140L, seed = 2026L) {
  n <- max(80L, min(300L, as.integer(n))); set.seed(as.integer(seed))
  x1 <- stats::runif(n, -2, 2); x2 <- stats::runif(n, -2, 2)
  signal <- x1^2 + x2^2 + 0.45 * sin(2.4 * x1) - 0.25 * x2
  boundary <- stats::median(signal)
  class <- factor(ifelse(signal + stats::rnorm(n, 0, .28) > boundary, "蓝色类", "橙色类"),
    levels = c("橙色类", "蓝色类"))
  data.frame(x1 = x1, x2 = x2, class = class, row_id = seq_len(n))
}

rf_tutorial_simulate <- function(trees = 12L, mtry = 1L, seed = 2026L, n = 140L) {
  if (!requireNamespace("randomForest", quietly = TRUE)) stop("缺少 randomForest 包。", call. = FALSE)
  trees <- max(5L, min(30L, as.integer(trees))); mtry <- max(1L, min(2L, as.integer(mtry)))
  data <- rf_tutorial_data(n, seed)
  set.seed(as.integer(seed) + 1000L)
  model <- randomForest::randomForest(data[, c("x1", "x2")], data$class, ntree = trees,
    mtry = mtry, keep.inbag = TRUE, importance = TRUE, nodesize = 3)
  grid <- expand.grid(x1 = seq(-2.12, 2.12, length.out = 72L), x2 = seq(-2.12, 2.12, length.out = 72L))
  individual <- predict(model, grid, predict.all = TRUE)$individual
  if (is.null(dim(individual))) individual <- matrix(individual, ncol = trees)
  root <- randomForest::getTree(model, 1L, labelVar = TRUE)[1L, , drop = FALSE]
  chosen <- as.character(root[["split var"]])
  set.seed(as.integer(seed) + 2000L)
  candidates <- unique(c(chosen, sample(c("x1", "x2"), mtry, replace = FALSE)))
  candidates <- head(candidates, mtry)
  importance_matrix <- randomForest::importance(model)
  importance_column <- if ("MeanDecreaseAccuracy" %in% colnames(importance_matrix)) "MeanDecreaseAccuracy" else colnames(importance_matrix)[1L]
  importance <- data.frame(字段 = c("x1", "x2"), 置换重要性 = as.numeric(importance_matrix[, importance_column]))
  list(data = data, model = model, grid = grid, individual = individual, root = root,
    candidates = candidates, importance = importance, trees = trees, mtry = mtry, seed = as.integer(seed))
}

rf_tutorial_total_steps <- function(simulation) simulation$trees + 7L

rf_tutorial_font_family <- function() {
  switch(Sys.info()[["sysname"]], Darwin = "PingFang SC", Windows = "Microsoft YaHei", "sans")
}

rf_tutorial_step_info <- function(step, simulation) {
  total <- rf_tutorial_total_steps(simulation); step <- max(1L, min(total, as.integer(step)))
  if (step == 1L) return(list(title = "第 1 步：认识训练数据", phase = "data",
    text = "每个点是一条训练样本，位置由两个特征决定，颜色是已知类别。随机森林要学习二维空间中类别变化的规律。",
    formula = "训练资料：D = {(xᵢ, yᵢ)}。这里 xᵢ = (x1, x2)，yᵢ 是类别。"))
  if (step == 2L) return(list(title = "第 2 步：Bootstrap 重复抽样", phase = "bootstrap",
    text = "第一棵树从原样本中有放回抽取同样数量的数据。有些样本会重复出现，有些没有被抽中；未抽中的样本就是这棵树的 OOB 样本。",
    formula = "D₁* ~ Bootstrap(D)。抽样次数等于 n，但不同样本数通常约为 63.2% × n。"))
  if (step == 3L) return(list(title = "第 3 步：节点随机选择候选特征", phase = "features",
    text = paste0("每次节点分裂只从 mtry = ", simulation$mtry, " 个随机候选特征中寻找切分。本次根节点的教学候选集合为：", paste(simulation$candidates, collapse = "、"), "。"),
    formula = "在全部 p 个特征中随机抽取 mtry 个候选，使不同树之间降低相关性。"))
  if (step == 4L) return(list(title = "第 4 步：选择降低不纯度最多的分裂", phase = "split",
    text = paste0("第一棵树的根节点最终选择 ", simulation$root[["split var"]], " < ",
      format(signif(as.numeric(simulation$root[["split point"]]), 4), trim = TRUE), "。分裂后两个子节点中的类别更集中。"),
    formula = "Gini(t) = 1 − Σₖ pₖ²；选择使加权子节点 Gini 降幅最大的切分。"))
  if (step == 5L) return(list(title = "第 5 步：长成一棵决策树", phase = "tree",
    text = "树会递归重复随机候选与最佳分裂，直到达到停止条件。背景颜色是第一棵树形成的分类区域，边界通常呈阶梯状。",
    formula = "单棵树方差较大：训练数据稍有变化，切分结构可能明显改变。"))
  if (step >= 6L && step <= simulation$trees + 5L) {
    count <- step - 5L
    return(list(title = paste0("第 6 步：森林投票，已加入 ", count, " / ", simulation$trees, " 棵树"), phase = "forest", tree_count = count,
      text = "每棵树独立给出类别，森林采用多数票。随着树增加，局部边界和预测通常逐渐稳定；树之间的随机性使错误不易完全重合。",
      formula = "ŷ(x) = mode{T₁(x), T₂(x), …, Tᴮ(x)}。背景越深表示投票一致度越高。"))
  }
  if (step == simulation$trees + 6L) return(list(title = "第 7 步：OOB 误差", phase = "oob",
    text = "每棵树可用自己没有抽到的样本进行预测。汇总每个样本的 OOB 投票，就能在不额外划分验证集的情况下观察误差是否随树数量稳定。",
    formula = "OOB error = OOB 预测错误的样本数 ÷ 可获得 OOB 预测的样本数。"))
  list(title = "第 8 步：变量重要性与解释边界", phase = "importance",
    text = "置换重要性观察打乱一个特征后 OOB 准确率下降多少。重要性表示预测贡献，不提供影响方向，也不能解释为因果效应。",
    formula = "Permutation importance(j) = 打乱 xⱼ 后的误差 − 原始 OOB 误差。")
}

rf_tutorial_vote_grid <- function(simulation, tree_count) {
  tree_count <- max(1L, min(simulation$trees, as.integer(tree_count)))
  votes <- simulation$individual[, seq_len(tree_count), drop = FALSE]
  levels <- levels(simulation$data$class)
  count_first <- rowSums(votes == levels[1L])
  count_second <- tree_count - count_first
  prediction <- ifelse(count_second > count_first, levels[2L], levels[1L])
  confidence <- pmax(count_first, count_second) / tree_count
  transform(simulation$grid, prediction = factor(prediction, levels = levels), confidence = confidence)
}

rf_tutorial_plot <- function(simulation, step) {
  info <- rf_tutorial_step_info(step, simulation); d <- simulation$data
  colours <- c("橙色类" = "#d97706", "蓝色类" = "#2563eb")
  theme <- ggplot2::theme_minimal(base_size = 13, base_family = rf_tutorial_font_family()) + ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", colour = "#172b4d"),
    plot.subtitle = ggplot2::element_text(colour = "#60738f"), legend.position = "bottom",
    panel.grid.minor = ggplot2::element_blank())
  if (info$phase == "oob") {
    errors <- data.frame(树 = seq_len(simulation$trees), OOB错误率 = simulation$model$err.rate[, "OOB"])
    return(ggplot2::ggplot(errors, ggplot2::aes(树, OOB错误率)) +
      ggplot2::geom_line(colour = "#2563eb", linewidth = 1) + ggplot2::geom_point(colour = "#2563eb", size = 2) +
      ggplot2::scale_y_continuous(labels = function(x) paste0(round(100 * x, 1), "%")) + theme +
      ggplot2::labs(title = info$title, subtitle = "观察误差是否随着树数量增加而趋于稳定", x = "森林中的树数量", y = "OOB 错误率"))
  }
  if (info$phase == "importance") {
    importance <- simulation$importance
    return(ggplot2::ggplot(importance, ggplot2::aes(stats::reorder(字段, 置换重要性), 置换重要性)) +
      ggplot2::geom_col(fill = "#2563eb", width = .62) + ggplot2::coord_flip() + theme +
      ggplot2::labs(title = info$title, subtitle = "数值越大，打乱该特征后模型性能下降越明显", x = NULL, y = "置换重要性"))
  }
  base <- ggplot2::ggplot() + theme + ggplot2::coord_equal(xlim = c(-2.15, 2.15), ylim = c(-2.15, 2.15)) +
    ggplot2::scale_colour_manual(values = colours, name = "真实类别") +
    ggplot2::labs(title = info$title, x = "特征 x1", y = "特征 x2")
  if (info$phase == "bootstrap") {
    counts <- simulation$model$inbag[, 1L]
    sampled <- transform(d, count = counts, sample_status = ifelse(counts == 0L, "OOB：未抽中", "Bootstrap：已抽中"))
    return(base + ggplot2::geom_point(data = sampled, ggplot2::aes(x1, x2, colour = class,
      size = pmax(1, count), shape = sample_status), alpha = .82) +
      ggplot2::scale_size_continuous(range = c(2, 7), name = "重复次数") +
      ggplot2::scale_shape_manual(values = c("Bootstrap：已抽中" = 16, "OOB：未抽中" = 4), name = NULL) +
      ggplot2::labs(subtitle = paste0("第一棵树：", sum(counts > 0), " 个不同样本被抽中；", sum(counts == 0), " 个 OOB 样本")))
  }
  if (info$phase %in% c("tree", "forest")) {
    count <- if (info$phase == "tree") 1L else info$tree_count
    grid <- rf_tutorial_vote_grid(simulation, count)
    return(base + ggplot2::geom_raster(data = grid, ggplot2::aes(x1, x2, fill = prediction, alpha = confidence)) +
      ggplot2::scale_fill_manual(values = colours, name = "预测类别") +
      ggplot2::scale_alpha_continuous(range = c(.12, .48), limits = c(.5, 1), name = "投票一致度") +
      ggplot2::geom_point(data = d, ggplot2::aes(x1, x2, colour = class), size = 2, alpha = .8) +
      ggplot2::labs(subtitle = if (count == 1L) "第一棵树的分类区域" else paste0(count, " 棵树多数投票形成的分类区域")))
  }
  p <- base + ggplot2::geom_point(data = d, ggplot2::aes(x1, x2, colour = class), size = 2.5, alpha = .78)
  if (info$phase == "features") {
    p <- p + ggplot2::labs(subtitle = paste0("根节点教学候选集合：", paste(simulation$candidates, collapse = "、"),
      "；实际随机森林会在每个节点重新抽取候选集合"))
  } else if (info$phase == "split") {
    variable <- as.character(simulation$root[["split var"]]); point <- as.numeric(simulation$root[["split point"]])
    p <- if (identical(variable, "x1")) p + ggplot2::geom_vline(xintercept = point, colour = "#dc2626", linewidth = 1.2, linetype = "dashed") else
      p + ggplot2::geom_hline(yintercept = point, colour = "#dc2626", linewidth = 1.2, linetype = "dashed")
    p <- p + ggplot2::labs(subtitle = paste0("根节点最佳分裂：", variable, " < ", format(signif(point, 4), trim = TRUE)))
  } else p <- p + ggplot2::labs(subtitle = "非线性类别边界的二维教学数据")
  p
}

rf_tutorial_tree_table <- function(simulation, tree = 1L) {
  tree <- max(1L, min(simulation$trees, as.integer(tree)))
  value <- randomForest::getTree(simulation$model, tree, labelVar = TRUE)
  value$节点 <- rownames(value)
  value$节点类型 <- ifelse(value$status == -1, "叶节点", "分裂节点")
  value$分裂规则 <- ifelse(value$status == -1, paste0("预测：", value$prediction),
    paste0(value$`split var`, " < ", format(signif(value$`split point`, 4), trim = TRUE)))
  value[, c("节点", "节点类型", "分裂规则", "left daughter", "right daughter"), drop = FALSE]
}

rf_tutorial_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(".rf-tutorial{background:linear-gradient(145deg,#f8fbff,#fff);border:1px solid #cfe0f5;border-radius:14px;padding:16px}.rf-tutorial-head{display:flex;align-items:center;gap:10px;margin-bottom:8px}.rf-tutorial-head h3{margin:0;color:#173d70;font-weight:850}.rf-tutorial-controls{background:#eef5ff;border-radius:12px;padding:12px;margin:12px 0}.rf-player{display:flex;gap:8px;flex-wrap:wrap;margin:10px 0}.rf-step-card{background:#fff;border-left:5px solid #2563eb;border-radius:10px;padding:14px 16px;box-shadow:0 2px 10px rgba(31,78,139,.08);min-height:145px}.rf-step-title{font-size:20px;font-weight:850;color:#173d70;margin-bottom:8px}.rf-step-text{font-size:15px;line-height:1.7;color:#405875}.rf-formula{background:#f1f5f9;border-radius:8px;padding:10px 12px;margin-top:10px;color:#24476f;font-family:ui-monospace,SFMono-Regular,Menlo,monospace}.rf-progress{height:10px;background:#dce7f5;border-radius:999px;overflow:hidden;margin:10px 0}.rf-progress>span{display:block;height:100%;background:linear-gradient(90deg,#2563eb,#06b6d4);transition:width .25s}.rf-stat{display:inline-block;background:#eaf3ff;color:#174a8b;border-radius:999px;padding:5px 10px;margin:2px;font-weight:700;font-size:12px}")),
    tags$div(class = "rf-tutorial",
      tags$div(class = "rf-tutorial-head", icon("graduation-cap"), h3("随机森林原理演示")),
      p("使用二维模拟数据逐步观察抽样、随机特征、节点分裂、单棵树、森林投票、OOB 误差与变量重要性。"),
      tags$div(class = "rf-tutorial-controls",
        fluidRow(
          column(4, sliderInput(ns("trees"), "演示树数量", min = 5, max = 30, value = 8, step = 1)),
          column(4, sliderInput(ns("mtry"), "每次分裂候选特征数 mtry", min = 1, max = 2, value = 1, step = 1)),
          column(4, numericInput(ns("seed"), "随机种子", 2026, min = 1, step = 1))
        ),
        tags$div(class = "rf-player",
          actionButton(ns("previous"), "上一步", icon = icon("backward-step")),
          actionButton(ns("next"), "下一步", icon = icon("forward-step"), class = "btn-primary"),
          actionButton(ns("reset"), "回到开头", icon = icon("rotate-left")),
          actionButton(ns("regenerate"), "换一组数据", icon = icon("shuffle"))
        ),
        uiOutput(ns("progress"))
      ),
      fluidRow(
        column(8, plotOutput(ns("plot"), height = "570px")),
        column(4, uiOutput(ns("lesson")), uiOutput(ns("stats")))
      ),
      tags$details(
        tags$summary("查看第一棵树的节点结构"),
        DT::DTOutput(ns("tree_table"))
      )
    )
  )
}

rf_tutorial_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    step <- reactiveVal(1L); generation <- reactiveVal(0L)
    simulation <- reactive({
      generation()
      rf_tutorial_simulate(input$trees %||% 12L, input$mtry %||% 1L,
        as.integer(input$seed %||% 2026L) + generation())
    })
    observeEvent(list(input$trees, input$mtry, input$seed), step(1L), ignoreInit = TRUE)
    observeEvent(input$regenerate, { generation(generation() + 1L); step(1L) })
    observeEvent(input$previous, step(max(1L, step() - 1L)))
    observeEvent(input[["next"]], step(min(rf_tutorial_total_steps(simulation()), step() + 1L)))
    observeEvent(input$reset, step(1L))
    tutorial_plot <- reactive(rf_tutorial_plot(simulation(), step()))
    output$plot <- renderPlot({ tutorial_plot() }, res = 105)
    output$progress <- renderUI({
      total <- rf_tutorial_total_steps(simulation()); current <- min(step(), total)
      tags$div(tags$strong(paste0("步骤 ", current, " / ", total)),
        tags$div(class = "rf-progress", tags$span(style = paste0("width:", round(100 * current / total), "%"))))
    })
    output$lesson <- renderUI({
      info <- rf_tutorial_step_info(step(), simulation())
      tags$div(class = "rf-step-card", tags$div(class = "rf-step-title", info$title),
        tags$div(class = "rf-step-text", info$text), tags$div(class = "rf-formula", info$formula))
    })
    output$stats <- renderUI({
      sim <- simulation(); info <- rf_tutorial_step_info(step(), sim)
      inbag <- sim$model$inbag[, 1L]
      current_trees <- if (identical(info$phase, "forest")) info$tree_count else if (info$phase %in% c("oob", "importance")) sim$trees else 1L
      oob <- sim$model$err.rate[min(current_trees, sim$trees), "OOB"]
      tags$div(style = "margin-top:12px",
        tags$span(class = "rf-stat", paste0("样本：", nrow(sim$data))),
        tags$span(class = "rf-stat", paste0("mtry：", sim$mtry)),
        tags$span(class = "rf-stat", paste0("当前树数：", current_trees)),
        tags$span(class = "rf-stat", paste0("首棵树 OOB：", sum(inbag == 0L))),
        tags$span(class = "rf-stat", paste0("当前 OOB 错误率：", if (is.finite(oob)) paste0(round(100 * oob, 1), "%") else "计算中")))
    })
    output$tree_table <- DT::renderDT({
      DT::datatable(rf_tutorial_tree_table(simulation(), 1L), rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE))
    })
    list(step = reactive(step()), simulation = simulation, tutorial_plot = tutorial_plot)
  })
}
