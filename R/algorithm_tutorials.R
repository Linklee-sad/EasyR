algorithm_tutorial_font <- function() {
  switch(Sys.info()[["sysname"]], Darwin = "PingFang SC", Windows = "Microsoft YaHei", "sans")
}

algorithm_tutorial_lessons <- function(key) {
  lessons <- list(
    regression = list(
      list("第 1 步：明确回归问题", "用一个或多个预测变量解释连续型结果。先确认结果变量是数值，并区分预测、解释与因果。", "`Y = β₀ + β₁X₁ + … + βₚXₚ + ε`", "系数描述在其他变量不变时的条件关系。"),
      list("第 2 步：最小二乘拟合", "回归线选择使残差平方和最小的一组系数。图中的竖线表示观测值与拟合值之间的残差。", "`β̂ = arg min Σ(yᵢ − ŷᵢ)²`", "大残差会被平方，因此异常值可能明显影响模型。"),
      list("第 3 步：解释回归系数", "`β̂ⱼ` 表示其他预测变量保持不变时，`Xⱼ` 每增加 1 单位，结果的平均变化量。类别变量系数相对参考组解释。", "`ŷ = β̂₀ + β̂₁x₁ + … + β̂ₚxₚ`", "先核对单位、参考组和变量编码，再解释正负方向。"),
      list("第 4 步：量化不确定性", "标准误、置信区间和 p 值反映抽样不确定性。统计显著不等于影响很大，区间宽度也受样本量和共线性影响。", "`95% CI = β̂ⱼ ± t* × SE(β̂ⱼ)`", "同时报告估计值、置信区间和实际业务意义。"),
      list("第 5 步：检查模型假设", "重点检查线性关系、残差独立性、方差稳定性和强影响点。残差图出现曲线或漏斗形状时，应重新考虑变量形式。", "`eᵢ = yᵢ − ŷᵢ`", "正态性主要影响小样本推断，并不要求原始变量正态。"),
      list("第 6 步：评价与验证", "R²描述样本内解释比例，RMSE和MAE描述预测误差。预测用途应使用独立测试集、交叉验证或按时间划分。", "`RMSE = √[Σ(yᵢ − ŷᵢ)² / n]`", "不能只凭高 R² 判断模型可靠，更不能直接推出因果关系。")
    ),
    svm = list(
      list("第 1 步：寻找分隔边界", "支持向量机寻找能区分类别的超平面。二维中它是一条线，更高维中称为超平面。", "`f(x) = wᵀx + b`", "预测类别由 `f(x)` 的符号决定。"),
      list("第 2 步：最大化间隔", "模型不仅要分开类别，还希望边界到两侧最近样本的距离尽可能大，以提高泛化能力。", "`margin = 2 / ‖w‖`", "真正决定边界的是最靠近间隔的支持向量。"),
      list("第 3 步：软间隔与 C", "现实数据通常无法完全分开。松弛变量允许少数样本进入间隔或被错分，`C` 控制错误惩罚。", "`min ½‖w‖² + C Σξᵢ`", "较大 C 更重视训练误差；较小 C 更重视宽间隔。"),
      list("第 4 步：核函数", "核技巧用相似度隐式映射到高维空间，从而形成非线性边界。线性核更易解释，径向基核更灵活。", "`K_RBF(x,z) = exp(−γ‖x−z‖²)`", "核函数决定边界形状，必须结合验证数据选择。"),
      list("第 5 步：γ 与复杂度", "RBF 核的 `γ` 控制单个样本影响范围。较大 γ 形成更局部、更弯曲的边界；较小 γ 形成更平滑的边界。", "`γ ↑  ⇒  influence radius ↓`", "`C` 与 `γ` 会共同影响欠拟合和过拟合。"),
      list("第 6 步：缩放与验证", "SVM依赖距离，量纲差异会改变边界，因此通常需要标准化。用测试集或交叉验证选择参数并报告分类或回归指标。", "`zⱼ = (xⱼ − x̄ⱼ) / sⱼ`", "类别不平衡时还应查看召回率、F1和平衡准确率。")
    ),
    pca = list(
      list("第 1 步：识别相关结构", "PCA把多个相关数值变量压缩成少量互不相关的主成分。每个点代表一条观测。", "`X: n × p data matrix`", "PCA是无监督方法，不使用结果变量。"),
      list("第 2 步：中心化与标准化", "中心化让每个变量均值为零；当变量量纲不同或方差差异很大时，通常还要除以标准差。", "`zᵢⱼ = (xᵢⱼ − x̄ⱼ) / sⱼ`", "是否标准化会直接改变主成分方向。"),
      list("第 3 步：寻找最大方差方向", "第一主成分是在单位长度约束下，使投影方差最大的方向；后续主成分与之前方向正交。", "`v₁ = arg max Var(Xv),  subject to ‖v‖=1`", "主成分轴由数据的协方差或相关矩阵决定。"),
      list("第 4 步：得分与载荷", "得分是每条观测在主成分轴上的坐标；载荷表示原变量如何组合成主成分。", "`scores = XV`", "载荷绝对值越大，变量对该主成分贡献通常越明显。"),
      list("第 5 步：选择主成分数量", "碎石图和累计解释率帮助决定保留多少成分。拐点、解释率阈值与下游任务表现应结合判断。", "`explained ratioₖ = λₖ / Σλⱼ`", "没有适用于所有数据的固定解释率阈值。"),
      list("第 6 步：正确解释", "PCA适合降维、可视化和缓解共线性，但主成分的符号可整体翻转，且线性组合未必具有直接业务含义。", "`sign(vₖ)` can be reversed", "离群点会影响方向；使用前应检查数据质量。")
    ),
    kmeans = list(
      list("第 1 步：定义聚类任务", "K-means在没有标签的情况下，把相似观测分为 K 组，并用每组均值作为中心。", "`K = number of clusters`", "聚类编号没有大小顺序，也不是真实类别。"),
      list("第 2 步：初始化中心", "算法先选择 K 个初始中心。不同起点可能到达不同局部最优，因此通常运行多次并保留较好结果。", "`μ₁,…,μ_K ← initial centers`", "增加随机起点次数通常比只运行一次更稳健。"),
      list("第 3 步：分配到最近中心", "每条观测被分配给欧氏距离最近的中心，形成当前聚类。", "`cᵢ = arg minₖ ‖xᵢ − μₖ‖²`", "距离对量纲敏感，通常应先标准化变量。"),
      list("第 4 步：更新聚类中心", "对每一组重新计算均值，再重复分配与更新，直到分组不再变化或目标函数稳定。", "`μₖ = mean{xᵢ : cᵢ = k}`", "K-means优化组内平方和，不保证得到全局最优。"),
      list("第 5 步：选择 K", "肘部图观察组内平方和下降何时明显放缓；轮廓系数同时比较组内紧密度和组间分离度。", "`silhouette ∈ [−1, 1]`", "K的选择还应考虑稳定性、业务意义和可操作性。"),
      list("第 6 步：理解适用边界", "K-means偏好大小接近、近似球形的簇，对离群点和非线性形状敏感。", "`WCSS = ΣₖΣᵢ∈Cₖ ‖xᵢ−μₖ‖²`", "聚类结果是探索性结构，需要结合领域知识验证。")
    ),
    timeseries = list(
      list("第 1 步：尊重时间顺序", "时间序列观测有先后依赖，不能像普通横截面数据那样随机打乱。先确认频率、时间间隔、缺口和重复时间。", "`y₁, y₂, …, y_T`", "训练和验证必须按时间先后划分。"),
      list("第 2 步：分解结构", "序列常由趋势、季节性和随机波动组成。移动平均和 STL 可帮助观察这些结构。", "`yₜ = trendₜ + seasonalₜ + remainderₜ`", "分解用于描述结构，不自动证明产生趋势的原因。"),
      list("第 3 步：平稳化与变换", "许多模型要求均值和相关结构相对稳定。差分可去除趋势，对数收益率常用于金融价格。", "`Δyₜ = yₜ − yₜ₋₁`", "过度差分也会放大噪声，应结合图形和诊断判断。"),
      list("第 4 步：理解 ACF 与 PACF", "ACF衡量不同滞后下的相关性；PACF衡量控制较短滞后后的直接相关。它们帮助识别 ARIMA 结构。", "`ρ(k) = Corr(yₜ, yₜ₋ₖ)`", "显著相关不等于因果关系。"),
      list("第 5 步：拟合与诊断", "ARIMA刻画均值动态，ETS刻画水平、趋势与季节性。残差应接近无自相关噪声，可用 Ljung–Box 检验辅助判断。", "`ARIMA(p,d,q)`", "模型复杂度应与数据长度和验证表现相匹配。"),
      list("第 6 步：预测与区间", "点预测给出中心估计，预测区间表达未来不确定性，并通常随预测步数增加而变宽。", "`forecast = point estimate ± uncertainty`", "评价预测要使用时间顺序留出集，并同时查看 MAE、RMSE 等指标。"),
      list("第 7 步：条件波动率", "金融收益率常出现波动聚集。GARCH用过去冲击和过去条件方差描述随时间变化的波动。", "`σₜ² = ω + αεₜ₋₁² + βσₜ₋₁²`", "GARCH预测波动强度，不直接预测收益方向。")
    )
  )
  lessons[[key]]
}

algorithm_tutorial_defaults <- function(key) {
  switch(key,
    regression = list(sample_n = 40L, slope = 1.5, noise = 2, outlier = FALSE),
    svm = list(shape = "curved", kernel = "radial", cost = 1, gamma = 1),
    pca = list(correlation = .8, scale_ratio = 1, standardize = TRUE),
    kmeans = list(true_groups = 3L, centers = 3L, overlap = .55, standardize = TRUE),
    timeseries = list(trend_strength = .16, seasonal_strength = 5, noise = 2, horizon = 12L))
}

algorithm_tutorial_value <- function(parameters, name, default) {
  value <- parameters[[name]]
  if (is.null(value) || !length(value) || anyNA(value)) default else value
}

algorithm_tutorial_parameters_ui <- function(ns, key) {
  controls <- switch(key,
    regression = fluidRow(
      column(3, sliderInput(ns("sample_n"), "样本量", 20, 150, 40, step = 10)),
      column(3, sliderInput(ns("slope"), "真实斜率", -3, 3, 1.5, step = .25)),
      column(3, sliderInput(ns("noise"), "随机噪声", .2, 6, 2, step = .2)),
      column(3, checkboxInput(ns("outlier"), "加入一个异常点", FALSE))),
    svm = fluidRow(
      column(3, selectInput(ns("shape"), "数据形状", c("弯曲边界" = "curved", "近似线性" = "linear"))),
      column(3, selectInput(ns("kernel"), "核函数", c("RBF 径向基" = "radial", "线性核" = "linear"))),
      column(3, sliderInput(ns("cost"), "惩罚 C", .1, 20, 1, step = .1)),
      column(3, sliderInput(ns("gamma"), "RBF γ", .1, 5, 1, step = .1))),
    pca = fluidRow(
      column(4, sliderInput(ns("correlation"), "变量相关程度", -.95, .95, .8, step = .05)),
      column(4, sliderInput(ns("scale_ratio"), "X2 量纲倍数", .25, 5, 1, step = .25)),
      column(4, checkboxInput(ns("standardize"), "计算前标准化", TRUE))),
    kmeans = fluidRow(
      column(3, sliderInput(ns("true_groups"), "模拟数据真实组数", 2, 5, 3, step = 1)),
      column(3, sliderInput(ns("centers"), "算法指定 K", 2, 6, 3, step = 1)),
      column(3, sliderInput(ns("overlap"), "组间重叠程度", .2, 1.4, .55, step = .05)),
      column(3, checkboxInput(ns("standardize"), "聚类前标准化", TRUE))),
    timeseries = fluidRow(
      column(3, sliderInput(ns("trend_strength"), "趋势强度", -.3, .5, .16, step = .02)),
      column(3, sliderInput(ns("seasonal_strength"), "季节波动", 0, 12, 5, step = .5)),
      column(3, sliderInput(ns("noise"), "随机噪声", .2, 6, 2, step = .2)),
      column(3, sliderInput(ns("horizon"), "预测期数", 3, 36, 12, step = 1)))
  )
  tags$div(class = "algorithm-tutorial-parameters", controls,
    tags$div(class = "algorithm-tutorial-regenerate",
      actionButton(ns("regenerate"), "换一组模拟数据", icon = icon("shuffle"))))
}

algorithm_tutorial_plot <- function(key, step, parameters = algorithm_tutorial_defaults(key), generation = 0L) {
  set.seed(2026L + as.integer(generation))
  font <- algorithm_tutorial_font()
  theme <- ggplot2::theme_minimal(base_size = 13, base_family = font) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", colour = "#173d70"),
      plot.subtitle = ggplot2::element_text(colour = "#60738f"), legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank())
  step <- as.integer(step)
  if (key == "regression") {
    n <- as.integer(algorithm_tutorial_value(parameters, "sample_n", 40L))
    slope <- as.numeric(algorithm_tutorial_value(parameters, "slope", 1.5))
    noise <- as.numeric(algorithm_tutorial_value(parameters, "noise", 2))
    x <- seq(0, 10, length.out = n); y <- 2.2 + slope * x + stats::rnorm(n, 0, noise)
    if (isTRUE(algorithm_tutorial_value(parameters, "outlier", FALSE))) y[n] <- y[n] + 6 * noise
    d <- data.frame(x, y); fit <- stats::lm(y ~ x, d); d$fit <- stats::fitted(fit); d$residual <- stats::residuals(fit)
    if (step == 4L) {
      ci <- data.frame(term = c("截距", "X"), estimate = stats::coef(fit), low = stats::confint(fit)[, 1], high = stats::confint(fit)[, 2])
      return(ggplot2::ggplot(ci, ggplot2::aes(estimate, term)) + ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "#94a3b8") + ggplot2::geom_errorbar(ggplot2::aes(xmin = low, xmax = high), orientation = "y", width = .15, colour = "#2563eb", linewidth = 1) + ggplot2::geom_point(colour = "#dc2626", size = 3) + theme + ggplot2::labs(title = "系数估计与 95% 置信区间", x = "估计值", y = NULL))
    }
    if (step == 5L) return(ggplot2::ggplot(d, ggplot2::aes(fit, residual)) + ggplot2::geom_hline(yintercept = 0, linetype = "dashed") + ggplot2::geom_point(colour = "#2563eb", size = 2.6) + theme + ggplot2::labs(title = "残差诊断", x = "拟合值", y = "残差"))
    if (step == 6L) return(ggplot2::ggplot(d, ggplot2::aes(y, fit)) + ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "#d97706") + ggplot2::geom_point(colour = "#2563eb", size = 2.6) + ggplot2::coord_equal() + theme + ggplot2::labs(title = "实际值与预测值", x = "实际值", y = "预测值"))
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point(colour = "#2563eb", size = 2.6) + theme + ggplot2::labs(title = "线性回归示意", x = "预测变量 X", y = "结果 Y")
    if (step >= 2L) p <- p + ggplot2::geom_line(ggplot2::aes(y = fit), colour = "#dc2626", linewidth = 1.1)
    if (step == 2L) p <- p + ggplot2::geom_segment(ggplot2::aes(xend = x, yend = fit), colour = "#d97706", alpha = .7)
    return(p)
  }
  if (key == "svm") {
    shape <- algorithm_tutorial_value(parameters, "shape", "curved")
    kernel <- algorithm_tutorial_value(parameters, "kernel", "radial")
    cost <- as.numeric(algorithm_tutorial_value(parameters, "cost", 1))
    gamma <- as.numeric(algorithm_tutorial_value(parameters, "gamma", 1))
    if (shape == "linear") {
      d <- rbind(data.frame(x = stats::rnorm(45, -1.1, .7), y = stats::rnorm(45, -.7, .75), class = "A"), data.frame(x = stats::rnorm(45, 1.1, .7), y = stats::rnorm(45, .7, .75), class = "B"))
    } else {
      angle <- stats::runif(100, 0, 2 * pi); radius <- c(stats::rnorm(50, .75, .16), stats::rnorm(50, 1.65, .2))
      d <- data.frame(x = radius * cos(angle), y = radius * sin(angle), class = rep(c("A", "B"), each = 50))
    }
    d$class <- factor(d$class)
    model <- e1071::svm(class ~ x + y, d, kernel = kernel, cost = cost, gamma = gamma, scale = TRUE)
    sequence_x <- seq(min(d$x) - .35, max(d$x) + .35, length.out = 90); sequence_y <- seq(min(d$y) - .35, max(d$y) + .35, length.out = 90)
    grid <- expand.grid(x = sequence_x, y = sequence_y); grid$class <- stats::predict(model, grid)
    p <- ggplot2::ggplot() + theme + ggplot2::coord_equal() + ggplot2::scale_colour_manual(values = c(A = "#d97706", B = "#2563eb")) + ggplot2::labs(title = "支持向量机边界示意", subtitle = paste0(if (kernel == "radial") "RBF 径向基核" else "线性核", "；C = ", cost, if (kernel == "radial") paste0("；gamma = ", gamma) else ""), colour = "真实类别")
    if (step >= 4L) p <- p + ggplot2::geom_raster(data = grid, ggplot2::aes(x, y, fill = class), alpha = .42) + ggplot2::scale_fill_manual(values = c(A = "#fed7aa", B = "#bfdbfe"), name = "预测区域")
    p <- p + ggplot2::geom_point(data = d, ggplot2::aes(x, y, colour = class), size = 2.6)
    if (step %in% c(2L, 3L)) p <- p + ggplot2::geom_point(data = d[model$index, , drop = FALSE], ggplot2::aes(x, y), shape = 1, colour = "#172b4d", size = 4, stroke = 1.1)
    return(p)
  }
  if (key == "pca") {
    correlation <- as.numeric(algorithm_tutorial_value(parameters, "correlation", .8))
    scale_ratio <- as.numeric(algorithm_tutorial_value(parameters, "scale_ratio", 1))
    standardize <- isTRUE(algorithm_tutorial_value(parameters, "standardize", TRUE))
    x <- stats::rnorm(100); y <- scale_ratio * (correlation * x + sqrt(max(.001, 1 - correlation^2)) * stats::rnorm(100)); d <- data.frame(x, y)
    pc <- stats::prcomp(d, center = TRUE, scale. = standardize); arrows <- data.frame(x = 0, y = 0, xend = 2.1 * pc$rotation[1, ], yend = 2.1 * pc$rotation[2, ], pc = c("PC1", "PC2"))
    if (step == 5L) {
      variance <- pc$sdev^2 / sum(pc$sdev^2); v <- data.frame(pc = c("PC1", "PC2"), variance)
      return(ggplot2::ggplot(v, ggplot2::aes(pc, variance)) + ggplot2::geom_col(fill = "#2563eb", width = .62) + ggplot2::geom_text(ggplot2::aes(label = paste0(round(100 * variance, 1), "%")), vjust = -0.4) + ggplot2::scale_y_continuous(labels = function(z) paste0(round(100 * z), "%"), limits = c(0, 1)) + theme + ggplot2::labs(title = "碎石图：各主成分解释率", x = NULL, y = "解释方差比例"))
    }
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point(colour = "#2563eb", alpha = .7, size = 2.4) + theme + ggplot2::coord_equal() + ggplot2::labs(title = "PCA：寻找数据变化最大的方向", x = "变量 X1", y = "变量 X2")
    if (step >= 3L) p <- p + ggplot2::geom_segment(data = arrows, ggplot2::aes(x, y, xend = xend, yend = yend, colour = pc), inherit.aes = FALSE, arrow = grid::arrow(length = grid::unit(0.18, "cm")), linewidth = 1.2) + ggplot2::scale_colour_manual(values = c(PC1 = "#dc2626", PC2 = "#d97706"))
    return(p)
  }
  if (key == "kmeans") {
    true_groups <- as.integer(algorithm_tutorial_value(parameters, "true_groups", 3L))
    centers_k <- as.integer(algorithm_tutorial_value(parameters, "centers", 3L))
    overlap <- as.numeric(algorithm_tutorial_value(parameters, "overlap", .55))
    standardize <- isTRUE(algorithm_tutorial_value(parameters, "standardize", TRUE))
    angle <- seq(0, 2 * pi, length.out = true_groups + 1L)[-1L]
    d <- do.call(rbind, lapply(seq_len(true_groups), function(i) data.frame(x = stats::rnorm(36, 1.8 * cos(angle[i]), overlap), y = stats::rnorm(36, 1.8 * sin(angle[i]), overlap), truth = factor(i))))
    model_data <- if (standardize) scale(d[c("x", "y")]) else as.matrix(d[c("x", "y")])
    if (step == 5L) {
      wcss <- vapply(1:8, function(k) stats::kmeans(model_data, centers = k, nstart = 20)$tot.withinss, numeric(1))
      elbow <- data.frame(K = 1:8, WCSS = wcss)
      return(ggplot2::ggplot(elbow, ggplot2::aes(K, WCSS)) + ggplot2::geom_line(colour = "#2563eb", linewidth = 1) + ggplot2::geom_point(colour = "#dc2626", size = 2.7) + ggplot2::scale_x_continuous(breaks = 1:8) + theme + ggplot2::labs(title = "肘部图：选择聚类数量 K", x = "K", y = "组内平方和 WCSS"))
    }
    km <- stats::kmeans(model_data, centers = centers_k, nstart = 20); d$cluster <- factor(km$cluster); centers <- aggregate(d[c("x", "y")], list(cluster = d$cluster), mean)
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + theme + ggplot2::coord_equal() + ggplot2::labs(title = "K-means 分配与中心更新", x = "标准化特征 X1", y = "标准化特征 X2")
    if (step < 3L) return(p + ggplot2::geom_point(colour = "#2563eb", alpha = .75, size = 2.5))
    p + ggplot2::geom_point(ggplot2::aes(colour = cluster), alpha = .8, size = 2.5) + ggplot2::geom_point(data = centers, ggplot2::aes(x, y), inherit.aes = FALSE, shape = 4, stroke = 2.2, size = 6, colour = "#172b4d") + ggplot2::scale_colour_brewer(palette = "Set1", name = "聚类")
  } else {
    trend_strength <- as.numeric(algorithm_tutorial_value(parameters, "trend_strength", .16))
    seasonal_strength <- as.numeric(algorithm_tutorial_value(parameters, "seasonal_strength", 5))
    noise <- as.numeric(algorithm_tutorial_value(parameters, "noise", 2))
    horizon <- as.integer(algorithm_tutorial_value(parameters, "horizon", 12L))
    t <- 1:120; trend <- 40 + trend_strength * t; seasonal <- seasonal_strength * sin(2 * pi * t / 12); y <- trend + seasonal + stats::rnorm(120, 0, noise); d <- data.frame(t, y, trend, seasonal)
    if (step == 3L) d <- data.frame(t = t[-1], y = diff(y))
    if (step == 4L) {
      ac <- stats::acf(y, plot = FALSE, lag.max = 24); acd <- data.frame(lag = as.numeric(ac$lag)[-1], acf = as.numeric(ac$acf)[-1])
      return(ggplot2::ggplot(acd, ggplot2::aes(lag, acf)) + ggplot2::geom_hline(yintercept = 0, colour = "#64748b") + ggplot2::geom_segment(ggplot2::aes(xend = lag, yend = 0), colour = "#2563eb", linewidth = .9) + theme + ggplot2::labs(title = "自相关函数 ACF", x = "滞后阶数", y = "自相关"))
    }
    if (step == 5L) {
      fit <- stats::arima(y, order = c(1, 1, 1)); residual <- stats::na.omit(stats::residuals(fit)); ac <- stats::acf(residual, plot = FALSE, lag.max = 24); acd <- data.frame(lag = as.numeric(ac$lag)[-1], acf = as.numeric(ac$acf)[-1])
      return(ggplot2::ggplot(acd, ggplot2::aes(lag, acf)) + ggplot2::geom_hline(yintercept = 0, colour = "#64748b") + ggplot2::geom_segment(ggplot2::aes(xend = lag, yend = 0), colour = "#d97706", linewidth = .9) + theme + ggplot2::labs(title = "模型残差 ACF", subtitle = "理想残差不应保留明显的时间相关结构", x = "滞后阶数", y = "残差自相关"))
    }
    if (step == 6L) {
      future <- 120 + seq_len(horizon); center <- 40 + trend_strength * future + seasonal_strength * sin(2 * pi * future / 12); width <- noise * sqrt(1 + seq_len(horizon) / 4); forecast <- data.frame(t = future, center, low = center - 1.96 * width, high = center + 1.96 * width)
      return(ggplot2::ggplot() + ggplot2::geom_line(data = d, ggplot2::aes(t, y), colour = "#2563eb", linewidth = .75) + ggplot2::geom_ribbon(data = forecast, ggplot2::aes(t, ymin = low, ymax = high), fill = "#93c5fd", alpha = .45) + ggplot2::geom_line(data = forecast, ggplot2::aes(t, center), colour = "#dc2626", linewidth = 1) + theme + ggplot2::labs(title = "未来点预测与 95% 预测区间", x = "时间", y = "观测值 / 预测值"))
    }
    if (step == 7L) {
      sigma <- numeric(180); e <- numeric(180); sigma[1] <- 1
      for (i in 2:180) { sigma[i] <- sqrt(.12 + .18 * e[i - 1]^2 + .76 * sigma[i - 1]^2); e[i] <- stats::rnorm(1, 0, sigma[i]) }
      vol <- data.frame(t = 1:180, value = e)
      return(ggplot2::ggplot(vol, ggplot2::aes(t, value)) + ggplot2::geom_line(colour = "#2563eb", linewidth = .65) + theme + ggplot2::labs(title = "波动聚集示意", subtitle = "大波动和小波动常分别成段出现", x = "时间", y = "收益或变化"))
    }
    p <- ggplot2::ggplot(d, ggplot2::aes(t, y)) + ggplot2::geom_line(colour = "#2563eb", linewidth = .8) + theme + ggplot2::labs(title = if (step == 3L) "一阶差分后的序列" else "带趋势和季节性的时间序列", x = "时间", y = if (step == 3L) "差分" else "观测值")
    if (step == 2L) p <- p + ggplot2::geom_line(ggplot2::aes(y = trend), colour = "#dc2626", linewidth = 1.1)
    p
  }
}

algorithm_title_ui <- function(ns, title) {
  tagList(
    tags$style(HTML(".algorithm-title-row{display:flex;align-items:center;width:100%;gap:16px;margin-top:20px;margin-bottom:10px}.algorithm-title-row h3{margin:0;flex:0 0 auto;white-space:nowrap}.algorithm-title-row .algorithm-tutorial-toggle{margin-left:auto;flex:0 0 auto;border:1px solid #8db7ef;background:#eef6ff;color:#174a8b;border-radius:999px;font-weight:750;padding:4px 11px}.algorithm-title-row .algorithm-tutorial-toggle:hover,.algorithm-title-row .algorithm-tutorial-toggle:focus{background:#dcecff;color:#123f78;border-color:#6ea3e6}@media(max-width:600px){.algorithm-title-row{gap:8px;margin-top:16px}.algorithm-title-row .algorithm-tutorial-toggle{padding:3px 9px}}")),
    tags$div(class = "algorithm-title-row", h3(title),
      actionButton(ns("tutorial_toggle"), "原理教程", icon = icon("graduation-cap"), class = "btn-sm algorithm-tutorial-toggle"))
  )
}

algorithm_tutorial_ui <- function(id, key) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(".algorithm-tutorial{background:linear-gradient(145deg,#f8fbff,#fff);border:1px solid #cfe0f5;border-radius:14px;padding:16px;margin-bottom:16px}.algorithm-tutorial-header{font-size:20px;font-weight:850;color:#173d70}.algorithm-tutorial-parameters{background:#fff;border:1px solid #d9e6f6;border-radius:12px;padding:12px 12px 2px;margin:12px 0}.algorithm-tutorial-parameters .form-group{margin-bottom:8px}.algorithm-tutorial-regenerate{display:flex;justify-content:flex-end;margin:0 0 10px}.algorithm-tutorial-controls{display:flex;gap:8px;flex-wrap:wrap;background:#eef5ff;border-radius:12px;padding:12px;margin:12px 0}.algorithm-tutorial-card{background:#fff;border-left:5px solid #2563eb;border-radius:10px;padding:14px 16px;box-shadow:0 2px 10px rgba(31,78,139,.08);min-height:265px}.algorithm-tutorial-card h4{margin-top:0;color:#173d70;font-weight:850}.algorithm-tutorial-markdown{font-size:15px;line-height:1.75;color:#405875}.algorithm-tutorial-formula{background:#f1f5f9;border-radius:8px;padding:10px 12px;margin:12px 0;color:#24476f;font-family:ui-monospace,SFMono-Regular,Menlo,monospace;overflow-wrap:anywhere}.algorithm-tutorial-tip{background:#eaf3ff;border-radius:8px;padding:10px 12px;color:#174a8b}.algorithm-tutorial-progress{height:9px;background:#dce7f5;border-radius:999px;overflow:hidden;margin-top:8px}.algorithm-tutorial-progress span{display:block;height:100%;background:linear-gradient(90deg,#2563eb,#06b6d4)}.algorithm-tutorial-stats{display:flex;gap:6px;flex-wrap:wrap;margin:10px 0}.algorithm-tutorial-stat{background:#eaf3ff;color:#174a8b;border-radius:999px;padding:5px 10px;font-size:12px;font-weight:750}")),
    tags$div(class = "algorithm-tutorial",
      tags$div(class = "algorithm-tutorial-header", icon("graduation-cap"), " 算法原理分步教程"),
      p("调整参数观察图形和计算结果如何变化，再使用下一步理解算法过程。教程数据与当前导入的数据相互独立。"),
      algorithm_tutorial_parameters_ui(ns, key),
      tags$div(class = "algorithm-tutorial-controls",
        actionButton(ns("previous"), "上一步", icon = icon("backward-step")),
        actionButton(ns("next"), "下一步", icon = icon("forward-step"), class = "btn-primary"),
        actionButton(ns("reset"), "回到开头", icon = icon("rotate-left"))),
      uiOutput(ns("progress")), uiOutput(ns("stats")),
      fluidRow(column(7, plotOutput(ns("plot"), height = "430px")), column(5, uiOutput(ns("lesson"))))
    )
  )
}

algorithm_tutorial_server <- function(id, key) {
  moduleServer(id, function(input, output, session) {
    lessons <- algorithm_tutorial_lessons(key); step <- reactiveVal(1L); total <- length(lessons); generation <- reactiveVal(0L)
    parameters <- reactive(switch(key,
      regression = list(sample_n = input$sample_n, slope = input$slope, noise = input$noise, outlier = input$outlier),
      svm = list(shape = input$shape, kernel = input$kernel, cost = input$cost, gamma = input$gamma),
      pca = list(correlation = input$correlation, scale_ratio = input$scale_ratio, standardize = input$standardize),
      kmeans = list(true_groups = input$true_groups, centers = input$centers, overlap = input$overlap, standardize = input$standardize),
      timeseries = list(trend_strength = input$trend_strength, seasonal_strength = input$seasonal_strength, noise = input$noise, horizon = input$horizon)))
    observeEvent(input$previous, step(max(1L, step() - 1L)))
    observeEvent(input[["next"]], step(min(total, step() + 1L)))
    observeEvent(input$reset, step(1L))
    observeEvent(input$regenerate, generation(generation() + 1L))
    output$progress <- renderUI(tags$div(tags$strong(paste0("步骤 ", step(), " / ", total)),
      tags$div(class = "algorithm-tutorial-progress", tags$span(style = paste0("width:", round(100 * step() / total), "%")))))
    tutorial_plot <- reactive(algorithm_tutorial_plot(key, step(), parameters(), generation()))
    output$plot <- renderPlot(tutorial_plot(), res = 105)
    output$stats <- renderUI({
      values <- parameters()
      labels <- switch(key,
        regression = c(sample_n = "样本量", slope = "真实斜率", noise = "噪声", outlier = "异常点"),
        svm = c(shape = "数据", kernel = "核", cost = "C", gamma = "γ"),
        pca = c(correlation = "相关程度", scale_ratio = "量纲倍数", standardize = "标准化"),
        kmeans = c(true_groups = "真实组数", centers = "指定 K", overlap = "重叠", standardize = "标准化"),
        timeseries = c(trend_strength = "趋势", seasonal_strength = "季节波动", noise = "噪声", horizon = "预测期数"))
      tags$div(class = "algorithm-tutorial-stats", lapply(names(labels), function(name) {
        value <- values[[name]]
        if (is.logical(value)) value <- if (isTRUE(value)) "是" else "否"
        if (identical(name, "shape")) value <- if (identical(value, "curved")) "弯曲边界" else "近似线性"
        if (identical(name, "kernel")) value <- if (identical(value, "radial")) "RBF" else "线性"
        tags$span(class = "algorithm-tutorial-stat", paste0(labels[[name]], "：", value))
      }))
    })
    output$lesson <- renderUI({
      item <- lessons[[step()]]
      body <- commonmark::markdown_html(item[[2]])
      tags$div(class = "algorithm-tutorial-card", h4(item[[1]]),
        tags$div(class = "algorithm-tutorial-markdown", HTML(body)),
        tags$div(class = "algorithm-tutorial-formula", HTML(commonmark::markdown_html(item[[3]]))),
        tags$div(class = "algorithm-tutorial-tip", tags$strong("使用提示："), item[[4]]))
    })
    list(step = reactive(step()), parameters = parameters, tutorial_plot = tutorial_plot)
  })
}

algorithm_tutorial_toggle_server <- function(input, session) {
  observeEvent(input$tutorial_toggle, {
    opened <- input$tutorial_toggle %% 2L == 1L
    updateActionButton(session, "tutorial_toggle", label = if (opened) "收起教程" else "原理教程",
      icon = icon(if (opened) "chevron-up" else "graduation-cap"))
  })
}
