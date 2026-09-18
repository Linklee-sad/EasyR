distribution_catalog <- function() c(
  "正态分布" = "normal", "均匀分布" = "uniform", "指数分布" = "exponential",
  "Gamma 分布" = "gamma", "Beta 分布" = "beta", "卡方分布" = "chisq",
  "t 分布" = "t", "F 分布" = "f", "对数正态分布" = "lognormal",
  "Weibull 分布" = "weibull", "Logistic 分布" = "logistic", "Cauchy 分布" = "cauchy",
  "Bernoulli 分布" = "bernoulli", "二项分布" = "binomial", "Poisson 分布" = "poisson",
  "几何分布" = "geometric", "负二项分布" = "negative_binomial", "超几何分布" = "hypergeometric"
)

distribution_parameter_specs <- function(distribution) {
  item <- function(id, label, value, min = NULL, max = NULL, step = NULL) {
    list(id = id, label = label, value = value, min = min, max = max, step = step)
  }
  switch(distribution,
    normal = list(item("mean", "均值 μ", 0), item("sd", "标准差 σ", 1, 0.0001, NULL, 0.1)),
    uniform = list(item("min", "下限 a", 0), item("max", "上限 b", 1)),
    exponential = list(item("rate", "率参数 λ", 1, 0.0001, NULL, 0.1)),
    gamma = list(item("shape", "形状参数 shape", 2, 0.0001, NULL, 0.1), item("rate", "率参数 rate", 1, 0.0001, NULL, 0.1)),
    beta = list(item("shape1", "形状参数 α", 2, 0.0001, NULL, 0.1), item("shape2", "形状参数 β", 5, 0.0001, NULL, 0.1)),
    chisq = list(item("df", "自由度 df", 5, 0.0001, NULL, 1)),
    t = list(item("df", "自由度 df", 5, 0.0001, NULL, 1)),
    f = list(item("df1", "分子自由度 df1", 5, 0.0001, NULL, 1), item("df2", "分母自由度 df2", 10, 0.0001, NULL, 1)),
    lognormal = list(item("meanlog", "对数均值 meanlog", 0), item("sdlog", "对数标准差 sdlog", 1, 0.0001, NULL, 0.1)),
    weibull = list(item("shape", "形状参数 shape", 2, 0.0001, NULL, 0.1), item("scale", "尺度参数 scale", 1, 0.0001, NULL, 0.1)),
    logistic = list(item("location", "位置参数 location", 0), item("scale", "尺度参数 scale", 1, 0.0001, NULL, 0.1)),
    cauchy = list(item("location", "位置参数 location", 0), item("scale", "尺度参数 scale", 1, 0.0001, NULL, 0.1)),
    bernoulli = list(item("prob", "成功概率 p", 0.5, 0, 1, 0.05)),
    binomial = list(item("size", "试验次数 n", 10, 1, NULL, 1), item("prob", "成功概率 p", 0.5, 0, 1, 0.05)),
    poisson = list(item("lambda", "均值参数 λ", 4, 0.0001, NULL, 0.5)),
    geometric = list(item("prob", "成功概率 p", 0.3, 0.0001, 1, 0.05)),
    negative_binomial = list(item("size", "目标成功次数 size", 5, 0.0001, NULL, 1), item("prob", "成功概率 p", 0.5, 0.0001, 1, 0.05)),
    hypergeometric = list(item("m", "总体成功元素数 m", 20, 1, NULL, 1), item("n", "总体失败元素数 n", 30, 1, NULL, 1), item("k", "抽取数量 k", 10, 1, NULL, 1)),
    stop("不支持的分布。", call. = FALSE)
  )
}

distribution_label <- function(distribution) {
  labels <- names(distribution_catalog()); values <- unname(distribution_catalog())
  labels[match(distribution, values)]
}

distribution_is_discrete <- function(distribution) {
  distribution %in% c("bernoulli", "binomial", "poisson", "geometric", "negative_binomial", "hypergeometric")
}

validate_distribution_parameters <- function(distribution, p) {
  positive <- function(names) if (any(!is.finite(unlist(p[names])) | unlist(p[names]) <= 0)) stop("尺度、率、形状和自由度参数必须大于 0。", call. = FALSE)
  if (distribution == "normal") positive("sd")
  if (distribution == "uniform" && (!is.finite(p$min) || !is.finite(p$max) || p$max <= p$min)) stop("均匀分布的上限必须大于下限。", call. = FALSE)
  if (distribution == "exponential") positive("rate")
  if (distribution == "gamma") positive(c("shape", "rate"))
  if (distribution == "beta") positive(c("shape1", "shape2"))
  if (distribution %in% c("chisq", "t")) positive("df")
  if (distribution == "f") positive(c("df1", "df2"))
  if (distribution == "lognormal") positive("sdlog")
  if (distribution == "weibull") positive(c("shape", "scale"))
  if (distribution %in% c("logistic", "cauchy")) positive("scale")
  if (distribution %in% c("bernoulli", "binomial") && (!is.finite(p$prob) || p$prob < 0 || p$prob > 1)) stop("概率 p 必须在 0 到 1 之间。", call. = FALSE)
  if (distribution %in% c("geometric", "negative_binomial") && (!is.finite(p$prob) || p$prob <= 0 || p$prob > 1)) stop("该分布的概率 p 必须大于 0 且不超过 1。", call. = FALSE)
  if (distribution == "binomial" && (!is.finite(p$size) || p$size < 1 || p$size != floor(p$size))) stop("二项分布的试验次数必须是正整数。", call. = FALSE)
  if (distribution == "poisson") positive("lambda")
  if (distribution == "negative_binomial") positive("size")
  if (distribution == "hypergeometric") {
    if (any(!is.finite(c(p$m, p$n, p$k))) || any(c(p$m, p$n, p$k) < 1) || any(c(p$m, p$n, p$k) != floor(c(p$m, p$n, p$k)))) stop("超几何分布参数必须是正整数。", call. = FALSE)
    if (p$k > p$m + p$n) stop("抽取数量 k 不能超过总体元素数 m + n。", call. = FALSE)
  }
  invisible(TRUE)
}

draw_distribution <- function(distribution, n, p) {
  switch(distribution,
    normal = stats::rnorm(n, p$mean, p$sd), uniform = stats::runif(n, p$min, p$max),
    exponential = stats::rexp(n, p$rate), gamma = stats::rgamma(n, p$shape, rate = p$rate),
    beta = stats::rbeta(n, p$shape1, p$shape2), chisq = stats::rchisq(n, p$df),
    t = stats::rt(n, p$df), f = stats::rf(n, p$df1, p$df2),
    lognormal = stats::rlnorm(n, p$meanlog, p$sdlog), weibull = stats::rweibull(n, p$shape, p$scale),
    logistic = stats::rlogis(n, p$location, p$scale), cauchy = stats::rcauchy(n, p$location, p$scale),
    bernoulli = stats::rbinom(n, 1, p$prob), binomial = stats::rbinom(n, p$size, p$prob),
    poisson = stats::rpois(n, p$lambda), geometric = stats::rgeom(n, p$prob),
    negative_binomial = stats::rnbinom(n, p$size, p$prob),
    hypergeometric = stats::rhyper(n, p$m, p$n, p$k))
}

distribution_theory <- function(distribution, p) {
  values <- switch(distribution,
    normal = c(p$mean, p$sd^2), uniform = c((p$min + p$max) / 2, (p$max - p$min)^2 / 12),
    exponential = c(1 / p$rate, 1 / p$rate^2), gamma = c(p$shape / p$rate, p$shape / p$rate^2),
    beta = c(p$shape1 / (p$shape1 + p$shape2), p$shape1 * p$shape2 / ((p$shape1 + p$shape2)^2 * (p$shape1 + p$shape2 + 1))),
    chisq = c(p$df, 2 * p$df),
    t = c(if (p$df > 1) 0 else NA_real_, if (p$df > 2) p$df / (p$df - 2) else if (p$df > 1) Inf else NA_real_),
    f = c(if (p$df2 > 2) p$df2 / (p$df2 - 2) else NA_real_,
      if (p$df2 > 4) 2 * p$df2^2 * (p$df1 + p$df2 - 2) / (p$df1 * (p$df2 - 2)^2 * (p$df2 - 4)) else if (p$df2 > 2) Inf else NA_real_),
    lognormal = c(exp(p$meanlog + p$sdlog^2 / 2), (exp(p$sdlog^2) - 1) * exp(2 * p$meanlog + p$sdlog^2)),
    weibull = c(p$scale * gamma(1 + 1 / p$shape), p$scale^2 * (gamma(1 + 2 / p$shape) - gamma(1 + 1 / p$shape)^2)),
    logistic = c(p$location, pi^2 * p$scale^2 / 3), cauchy = c(NA_real_, NA_real_),
    bernoulli = c(p$prob, p$prob * (1 - p$prob)), binomial = c(p$size * p$prob, p$size * p$prob * (1 - p$prob)),
    poisson = c(p$lambda, p$lambda), geometric = c((1 - p$prob) / p$prob, (1 - p$prob) / p$prob^2),
    negative_binomial = c(p$size * (1 - p$prob) / p$prob, p$size * (1 - p$prob) / p$prob^2),
    hypergeometric = { total <- p$m + p$n; c(p$k * p$m / total, p$k * p$m / total * p$n / total * (total - p$k) / (total - 1)) })
  names(values) <- c("mean", "variance"); values
}

distribution_density <- function(distribution, x, p) {
  switch(distribution,
    normal = stats::dnorm(x, p$mean, p$sd), uniform = stats::dunif(x, p$min, p$max),
    exponential = stats::dexp(x, p$rate), gamma = stats::dgamma(x, p$shape, rate = p$rate),
    beta = stats::dbeta(x, p$shape1, p$shape2), chisq = stats::dchisq(x, p$df),
    t = stats::dt(x, p$df), f = stats::df(x, p$df1, p$df2),
    lognormal = stats::dlnorm(x, p$meanlog, p$sdlog), weibull = stats::dweibull(x, p$shape, p$scale),
    logistic = stats::dlogis(x, p$location, p$scale), cauchy = stats::dcauchy(x, p$location, p$scale),
    bernoulli = stats::dbinom(x, 1, p$prob), binomial = stats::dbinom(x, p$size, p$prob),
    poisson = stats::dpois(x, p$lambda), geometric = stats::dgeom(x, p$prob),
    negative_binomial = stats::dnbinom(x, p$size, p$prob),
    hypergeometric = stats::dhyper(x, p$m, p$n, p$k))
}

distribution_cdf <- function(distribution, x, p) {
  switch(distribution,
    normal = stats::pnorm(x, p$mean, p$sd), uniform = stats::punif(x, p$min, p$max),
    exponential = stats::pexp(x, p$rate), gamma = stats::pgamma(x, p$shape, rate = p$rate),
    beta = stats::pbeta(x, p$shape1, p$shape2), chisq = stats::pchisq(x, p$df),
    t = stats::pt(x, p$df), f = stats::pf(x, p$df1, p$df2),
    lognormal = stats::plnorm(x, p$meanlog, p$sdlog), weibull = stats::pweibull(x, p$shape, p$scale),
    logistic = stats::plogis(x, p$location, p$scale), cauchy = stats::pcauchy(x, p$location, p$scale),
    bernoulli = stats::pbinom(x, 1, p$prob), binomial = stats::pbinom(x, p$size, p$prob),
    poisson = stats::ppois(x, p$lambda), geometric = stats::pgeom(x, p$prob),
    negative_binomial = stats::pnbinom(x, p$size, p$prob),
    hypergeometric = stats::phyper(x, p$m, p$n, p$k))
}

distribution_quantile <- function(distribution, probability, p) {
  switch(distribution,
    normal = stats::qnorm(probability, p$mean, p$sd), uniform = stats::qunif(probability, p$min, p$max),
    exponential = stats::qexp(probability, p$rate), gamma = stats::qgamma(probability, p$shape, rate = p$rate),
    beta = stats::qbeta(probability, p$shape1, p$shape2), chisq = stats::qchisq(probability, p$df),
    t = stats::qt(probability, p$df), f = stats::qf(probability, p$df1, p$df2),
    lognormal = stats::qlnorm(probability, p$meanlog, p$sdlog), weibull = stats::qweibull(probability, p$shape, p$scale),
    logistic = stats::qlogis(probability, p$location, p$scale), cauchy = stats::qcauchy(probability, p$location, p$scale),
    bernoulli = stats::qbinom(probability, 1, p$prob), binomial = stats::qbinom(probability, p$size, p$prob),
    poisson = stats::qpois(probability, p$lambda), geometric = stats::qgeom(probability, p$prob),
    negative_binomial = stats::qnbinom(probability, p$size, p$prob),
    hypergeometric = stats::qhyper(probability, p$m, p$n, p$k))
}

distribution_description <- function(distribution) switch(distribution,
  normal = "正态分布是对称钟形分布，由均值控制位置、标准差控制离散程度。",
  uniform = "均匀分布在给定区间内各位置具有相同密度。",
  exponential = "指数分布常用于描述 Poisson 过程中的等待时间，并具有无记忆性。",
  gamma = "Gamma 分布是正值右偏分布，可用于等待时间、寿命和金额数据。",
  beta = "Beta 分布定义在 0 到 1 之间，常用于概率或比例。",
  chisq = "卡方分布是若干独立标准正态变量平方和的分布，常用于方差推断。",
  t = "t 分布对称但尾部比正态分布更厚，自由度增大时逐渐接近正态分布。",
  f = "F 分布是两个独立卡方变量按自由度标准化后的比值，常用于方差分析。",
  lognormal = "对数正态分布表示取对数后服从正态分布的正值变量。",
  weibull = "Weibull 分布常用于寿命与可靠性分析，形状参数控制风险随时间的变化。",
  logistic = "Logistic 分布与正态分布类似但尾部更厚，并用于 Logistic 模型的误差结构。",
  cauchy = "Cauchy 分布具有极厚尾部，理论均值和方差都不存在，是大数定律失效的经典演示。",
  bernoulli = "Bernoulli 分布描述一次只有成功或失败两种结果的试验。",
  binomial = "二项分布描述固定次数独立 Bernoulli 试验中的成功次数。",
  poisson = "Poisson 分布描述固定时间或空间区间内独立事件的发生次数。",
  geometric = "几何分布描述首次成功之前的失败次数，并具有离散无记忆性。",
  negative_binomial = "负二项分布描述达到指定成功次数之前的失败次数，也常用于过度离散计数数据。",
  hypergeometric = "超几何分布描述有限总体中不放回抽样得到的成功元素数量。")

distribution_value_text <- function(x) {
  if (is.na(x)) "不存在" else if (is.infinite(x)) "无穷大" else format(signif(x, 5), trim = TRUE)
}

fit_distribution_demo <- function(distribution, parameters, sample_size = 1000,
                                  repetitions = 500, seed = 2026) {
  if (!distribution %in% unname(distribution_catalog())) stop("请选择有效的概率分布。", call. = FALSE)
  validate_distribution_parameters(distribution, parameters)
  sample_size <- as.integer(sample_size); repetitions <- as.integer(repetitions); seed <- as.integer(seed)
  if (!is.finite(sample_size) || sample_size < 10L || sample_size > 100000L) stop("单次样本量应在 10 到 100,000 之间。", call. = FALSE)
  if (!is.finite(repetitions) || repetitions < 10L || repetitions > 5000L) stop("重复抽样次数应在 10 到 5,000 之间。", call. = FALSE)
  if (sample_size * repetitions > 5000000) stop("样本量 × 重复次数不能超过 5,000,000，请降低其中一个设置。", call. = FALSE)
  if (!is.finite(seed)) stop("随机种子必须是整数。", call. = FALSE)
  set.seed(seed)
  sample <- draw_distribution(distribution, sample_size, parameters)
  repeated <- matrix(draw_distribution(distribution, sample_size * repetitions, parameters), nrow = sample_size)
  sample_means <- colMeans(repeated)
  theory <- distribution_theory(distribution, parameters)
  sample_table <- data.frame(序号 = seq_along(sample), 数值 = sample, check.names = FALSE)
  means_table <- data.frame(重复编号 = seq_along(sample_means), 样本均值 = sample_means, check.names = FALSE)
  statistics <- data.frame(
    指标 = c("均值", "方差", "标准差", "中位数", "最小值", "最大值", "样本均值的均值", "样本均值的方差"),
    理论值 = c(theory["mean"], theory["variance"], sqrt(theory["variance"]), NA, NA, NA,
      theory["mean"], theory["variance"] / sample_size),
    模拟值 = c(mean(sample), stats::var(sample), stats::sd(sample), stats::median(sample), min(sample), max(sample),
      mean(sample_means), stats::var(sample_means)), check.names = FALSE
  )
  label <- distribution_label(distribution)
  mean_note <- if (is.finite(theory["mean"])) {
    paste0("理论均值为 ", distribution_value_text(theory["mean"]), "，本次样本均值为 ", distribution_value_text(mean(sample)), "。")
  } else paste0(label, "的理论均值不存在，因此累计样本均值不一定稳定收敛。")
  variance_note <- if (is.finite(theory["variance"])) {
    paste0("理论方差为 ", distribution_value_text(theory["variance"]), "，本次样本方差为 ", distribution_value_text(stats::var(sample)), "。")
  } else paste0("理论方差", if (is.infinite(theory["variance"])) "为无穷大" else "不存在", "，常规标准误与正态中心极限定理不能直接套用。")
  clt_note <- if (is.finite(theory["mean"]) && is.finite(theory["variance"])) {
    paste0("在独立同分布且方差有限的条件下，样本均值近似服从均值 ", distribution_value_text(theory["mean"]),
      "、方差 ", distribution_value_text(theory["variance"] / sample_size), " 的正态分布。重复抽样结果可与该近似比较。")
  } else "由于理论均值或方差不满足常规条件，本例不叠加样本均值的正态近似曲线；这可用于演示中心极限定理的适用边界。"
  report <- paste(c(
    paste0(label, "：模拟与教学解读"),
    "一、分布特点", distribution_description(distribution),
    "二、本次设置", paste0("单次样本量：", sample_size, "；重复抽样次数：", repetitions, "；随机种子：", seed, "。"),
    "三、理论与模拟", mean_note, variance_note,
    "四、大数定律", "当理论均值存在且观测独立同分布时，累计样本均值通常会随样本量增加而接近理论均值。短期波动、厚尾和极端值会影响收敛速度。",
    "五、样本均值与中心极限定理", clt_note,
    "六、教学提示", "单次模拟具有随机性。改变随机种子、样本量和分布参数并重复比较，可以观察小样本波动、偏态、厚尾、离散性以及样本均值分布如何变化。模拟接近理论值不等于每次都完全一致。"
  ), collapse = "\n\n")
  list(distribution = distribution, label = label, parameters = parameters, sample = sample,
    sample_means = sample_means, sample_table = sample_table, means_table = means_table,
    statistics = statistics, theory = theory, sample_size = sample_size,
    repetitions = repetitions, report = report, discrete = distribution_is_discrete(distribution))
}

build_distribution_shape_plot <- function(result) {
  x <- result$sample; p <- result$parameters
  if (result$discrete) {
    observed <- as.data.frame(prop.table(table(x)), stringsAsFactors = FALSE)
    names(observed) <- c("数值", "样本比例"); observed$数值 <- as.numeric(as.character(observed$数值))
    lower <- floor(distribution_quantile(result$distribution, 0.0005, p)); upper <- ceiling(distribution_quantile(result$distribution, 0.9995, p))
    lower <- max(0, min(lower, min(x))); upper <- max(upper, max(x))
    support <- if (upper - lower <= 300) lower:upper else unique(round(seq(lower, upper, length.out = 300)))
    theoretical <- data.frame(数值 = support, 理论概率 = distribution_density(result$distribution, support, p))
    return(ggplot2::ggplot(observed, ggplot2::aes(数值, 样本比例)) +
      ggplot2::geom_col(fill = "#93c5fd", colour = "#2563eb", alpha = 0.8) +
      ggplot2::geom_line(data = theoretical, ggplot2::aes(数值, 理论概率), colour = "#d97706", linewidth = 1) +
      ggplot2::geom_point(data = theoretical, ggplot2::aes(数值, 理论概率), colour = "#d97706", size = 2) +
      lm_plot_theme() + ggplot2::labs(title = paste0(result$label, "：样本与理论概率"),
        subtitle = "蓝柱为样本比例；橙线与点为理论概率质量函数", x = "数值", y = "概率"))
  }
  limits <- stats::quantile(x, c(0.002, 0.998), names = FALSE, type = 8)
  if (!all(is.finite(limits)) || limits[1] == limits[2]) limits <- range(x)
  grid <- seq(limits[1], limits[2], length.out = 600)
  theoretical <- data.frame(数值 = grid, 理论密度 = distribution_density(result$distribution, grid, p))
  plot_sample <- x[x >= limits[1] & x <= limits[2]]
  ggplot2::ggplot(data.frame(数值 = plot_sample), ggplot2::aes(数值)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), bins = 35,
      fill = "#93c5fd", colour = "white", alpha = 0.85) +
    ggplot2::geom_line(data = theoretical, ggplot2::aes(数值, 理论密度), colour = "#d97706", linewidth = 1.1) +
    lm_plot_theme() + ggplot2::labs(title = paste0(result$label, "：样本与理论密度"),
      subtitle = "蓝色直方图为模拟样本；橙线为理论概率密度", x = "数值", y = "密度")
}

build_distribution_cdf_plot <- function(result) {
  values <- sort(result$sample); empirical <- seq_along(values) / length(values)
  d <- data.frame(数值 = values, 经验分布 = empirical,
    理论分布 = distribution_cdf(result$distribution, values, result$parameters))
  ggplot2::ggplot(d, ggplot2::aes(数值)) +
    ggplot2::geom_step(ggplot2::aes(y = 经验分布), colour = "#2563eb", linewidth = 0.9) +
    ggplot2::geom_line(ggplot2::aes(y = 理论分布), colour = "#d97706", linewidth = 1) +
    lm_plot_theme() + ggplot2::labs(title = paste0(result$label, "：经验与理论分布函数"),
      subtitle = "蓝线为经验 CDF；橙线为理论 CDF", x = "数值", y = "累计概率")
}

build_distribution_qq_plot <- function(result) {
  probability <- stats::ppoints(length(result$sample))
  theoretical <- distribution_quantile(result$distribution, probability, result$parameters)
  d <- data.frame(理论分位数 = theoretical, 样本分位数 = sort(result$sample))
  finite <- is.finite(d$理论分位数) & is.finite(d$样本分位数); d <- d[finite, , drop = FALSE]
  limits <- range(c(d$理论分位数, d$样本分位数))
  if (limits[1] == limits[2]) limits <- limits + c(-0.5, 0.5)
  ggplot2::ggplot(d, ggplot2::aes(理论分位数, 样本分位数)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "#d97706", linewidth = 0.9) +
    ggplot2::geom_point(colour = "#2563eb", alpha = 0.6, size = 2) +
    ggplot2::coord_equal(xlim = limits, ylim = limits) + lm_plot_theme() +
    ggplot2::labs(title = paste0(result$label, "：理论 Q-Q 图"),
      subtitle = "点接近橙色 45° 线表示样本分位数接近理论分布", x = "理论分位数", y = "样本分位数")
}

build_distribution_lln_plot <- function(result) {
  d <- data.frame(样本量 = seq_along(result$sample), 累计均值 = cumsum(result$sample) / seq_along(result$sample))
  plot <- ggplot2::ggplot(d, ggplot2::aes(样本量, 累计均值)) +
    ggplot2::geom_line(colour = "#2563eb", linewidth = 0.8) + lm_plot_theme() +
    ggplot2::labs(title = paste0(result$label, "：累计均值与大数定律"),
      subtitle = if (is.finite(result$theory["mean"])) "橙色虚线为理论均值" else "该分布的理论均值不存在",
      x = "累计样本量", y = "累计样本均值")
  if (is.finite(result$theory["mean"])) plot <- plot + ggplot2::geom_hline(yintercept = result$theory["mean"], linetype = "dashed", colour = "#d97706", linewidth = 1)
  plot
}

build_sampling_means_plot <- function(result) {
  d <- data.frame(样本均值 = result$sample_means)
  plot <- ggplot2::ggplot(d, ggplot2::aes(样本均值)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), bins = 35,
      fill = "#86efac", colour = "white", alpha = 0.85) + lm_plot_theme() +
    ggplot2::labs(title = paste0(result$label, "：重复抽样的样本均值分布"),
      subtitle = paste0(result$repetitions, " 次重复，每次样本量 ", result$sample_size), x = "样本均值", y = "密度")
  if (is.finite(result$theory["mean"]) && is.finite(result$theory["variance"]) && result$theory["variance"] > 0) {
    limits <- range(result$sample_means); grid <- seq(limits[1], limits[2], length.out = 500)
    normal <- data.frame(样本均值 = grid,
      密度 = stats::dnorm(grid, result$theory["mean"], sqrt(result$theory["variance"] / result$sample_size)))
    plot <- plot + ggplot2::geom_line(data = normal, ggplot2::aes(样本均值, 密度), colour = "#d97706", linewidth = 1.1) +
      ggplot2::labs(subtitle = paste0(result$repetitions, " 次重复，每次样本量 ", result$sample_size, "；橙线为中心极限定理正态近似"))
  } else if (is.finite(result$theory["mean"]) && identical(as.numeric(result$theory["variance"]), 0)) {
    plot <- plot + ggplot2::geom_vline(xintercept = result$theory["mean"], colour = "#d97706", linewidth = 1.1) +
      ggplot2::labs(subtitle = paste0(result$repetitions, " 次重复，每次样本量 ", result$sample_size, "；该参数设置下样本均值固定"))
  }
  plot
}

distribution_demo_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("概率分布与抽样演示"),
    p("设置分布参数、样本量和重复次数，比较理论结果与随机模拟，并演示大数定律和中心极限定理。"),
    fluidRow(column(5, selectInput(ns("distribution"), "概率分布", distribution_catalog())),
      column(3, numericInput(ns("sample_size"), "每次样本量", 1000, min = 10, max = 100000, step = 10)),
      column(2, numericInput(ns("repetitions"), "重复抽样次数", 500, min = 10, max = 5000, step = 10)),
      column(2, numericInput(ns("seed"), "随机种子", 2026, min = 1, step = 1))),
    uiOutput(ns("parameters")),
    helpText("样本量 × 重复次数最多为 5,000,000。改变参数与随机种子后重新生成，可以比较分布形状、抽样波动和收敛速度。"),
    actionButton(ns("run"), "生成分布样本", class = "btn-primary"),
    tags$div(style = "margin:12px 0", textOutput(ns("status"))),
    tabsetPanel(
      tabPanel("教学解读", tags$div(style = "white-space:pre-wrap;line-height:1.9", textOutput(ns("report")))),
      tabPanel("理论与样本统计", DT::DTOutput(ns("statistics"))),
      tabPanel("分布形状", ggplot_editor_ui(ns("shape_editor"), height = "500px")),
      tabPanel("分布函数 CDF", ggplot_editor_ui(ns("cdf_editor"), height = "500px")),
      tabPanel("Q-Q 图", ggplot_editor_ui(ns("qq_editor"), height = "500px")),
      tabPanel("大数定律", ggplot_editor_ui(ns("lln_editor"), height = "500px")),
      tabPanel("样本均值与中心极限定理", ggplot_editor_ui(ns("means_editor"), height = "500px")),
      tabPanel("模拟数据", DT::DTOutput(ns("sample_table")), DT::DTOutput(ns("means_table")))
    ), hr(),
    downloadButton(ns("download_report"), "下载教学报告 TXT"),
    downloadButton(ns("download_sample"), "下载随机样本 CSV"),
    downloadButton(ns("download_means"), "下载重复抽样均值 CSV")
  )
}

distribution_demo_server <- function(id, directory = reactive(getwd())) {
  moduleServer(id, function(input, output, session) {
    result <- reactiveVal(NULL)
    status <- reactiveVal("设置分布和参数后，点击“生成分布样本”。")
    output$parameters <- renderUI({
      req(input$distribution); specs <- distribution_parameter_specs(input$distribution)
      fluidRow(lapply(specs, function(spec) {
        arguments <- list(inputId = session$ns(paste0("param_", spec$id)), label = spec$label, value = spec$value)
        if (!is.null(spec$min)) arguments$min <- spec$min
        if (!is.null(spec$max)) arguments$max <- spec$max
        if (!is.null(spec$step)) arguments$step <- spec$step
        column(max(3, floor(12 / length(specs))), do.call(numericInput, arguments))
      }))
    })
    parameter_values <- reactive({
      req(input$distribution)
      specs <- distribution_parameter_specs(input$distribution)
      values <- lapply(specs, function(spec) input[[paste0("param_", spec$id)]])
      req(all(vapply(values, function(x) length(x) == 1L, logical(1))))
      names(values) <- vapply(specs, `[[`, character(1), "id"); values
    })
    observe({
      result(NULL); status("分布或模拟设置已更新，请点击“生成分布样本”。")
      input$distribution; input$sample_size; input$repetitions; input$seed; parameter_values()
    }, priority = 100)
    observeEvent(input$run, {
      result(NULL)
      tryCatch({
        fitted <- fit_distribution_demo(input$distribution, parameter_values(), input$sample_size, input$repetitions, input$seed)
        result(fitted)
        status(sprintf("模拟完成：生成 %d 个样本，并完成 %d 次重复抽样。", fitted$sample_size, fitted$repetitions))
      }, error = function(e) {
        message <- conditionMessage(e); status(paste("未能生成：", message)); showNotification(message, type = "error", duration = 10)
      })
    })
    output$status <- renderText(status())
    output$report <- renderText({ req(result()); result()$report })
    output$statistics <- DT::renderDT({ req(result()); DT::datatable(result()$statistics, rownames = FALSE, options = list(dom = "t", scrollX = TRUE)) })
    output$sample_table <- DT::renderDT({ req(result()); DT::datatable(result()$sample_table, rownames = FALSE, options = list(pageLength = 10)) })
    output$means_table <- DT::renderDT({ req(result()); DT::datatable(result()$means_table, rownames = FALSE, options = list(pageLength = 10)) })
    shape_plot <- reactive({ req(result()); build_distribution_shape_plot(result()) })
    cdf_plot <- reactive({ req(result()); build_distribution_cdf_plot(result()) })
    qq_plot <- reactive({ req(result()); build_distribution_qq_plot(result()) })
    lln_plot <- reactive({ req(result()); build_distribution_lln_plot(result()) })
    means_plot <- reactive({ req(result()); build_sampling_means_plot(result()) })
    ggplot_editor_server("shape_editor", shape_plot, directory, "easyr-distribution-shape")
    ggplot_editor_server("cdf_editor", cdf_plot, directory, "easyr-distribution-cdf")
    ggplot_editor_server("qq_editor", qq_plot, directory, "easyr-distribution-qq")
    ggplot_editor_server("lln_editor", lln_plot, directory, "easyr-distribution-lln")
    ggplot_editor_server("means_editor", means_plot, directory, "easyr-distribution-sample-means")
    write_csv_bom <- function(value, file) {
      con <- file(file, open = "wb"); on.exit(close(con)); writeBin(charToRaw("\ufeff"), con)
      lines <- capture.output(write.csv(value, row.names = FALSE, na = ""))
      writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = "\r\n"), "\r\n"))), con)
    }
    output$download_report <- downloadHandler(filename = function() paste0("easyr-distribution-", Sys.Date(), ".txt"),
      content = function(file) { req(result()); writeLines(enc2utf8(result()$report), file, useBytes = TRUE) })
    output$download_sample <- downloadHandler(filename = function() paste0("easyr-distribution-sample-", Sys.Date(), ".csv"),
      content = function(file) { req(result()); write_csv_bom(result()$sample_table, file) })
    output$download_means <- downloadHandler(filename = function() paste0("easyr-distribution-means-", Sys.Date(), ".csv"),
      content = function(file) { req(result()); write_csv_bom(result()$means_table, file) })
    result
  })
}
