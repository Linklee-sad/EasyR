source("setup.R")
library(shiny)
source("R/ai.R")

stopifnot(ai_config_ready(list(api_key = "secret", endpoint = "https://example.com/v1", model = "model")))
stopifnot(!ai_config_ready(list(api_key = "", endpoint = "https://example.com/v1", model = "model")))
stopifnot(ai_config_ready(list(provider = "custom", api_key = "", endpoint = "http://127.0.0.1:11434/v1", model = "local-model")))
stopifnot(identical(ai_clean_endpoint("https://example.com/v1", "chat"), "https://example.com/v1/chat/completions"))
stopifnot(identical(ai_clean_endpoint("https://example.com/v1/responses", "responses"), "https://example.com/v1/responses"))

local_config_path <- tempfile("easyr-ai-config-", fileext = ".json")
local_value <- list(provider = "custom", model = "local-model", endpoint = "http://127.0.0.1:11434/v1",
  protocol = "chat", api_key = "fake-key", private_note = "must-not-be-saved")
ai_write_local_config(local_value, local_config_path)
local_loaded <- ai_read_local_config(local_config_path)
stopifnot(file.exists(local_config_path), identical(local_loaded$provider, "custom"),
  identical(local_loaded$api_key, "fake-key"), is.null(local_loaded$private_note),
  !grepl("must-not-be-saved", paste(readLines(local_config_path), collapse = ""), fixed = TRUE))
ai_delete_local_config(local_config_path)
stopifnot(!file.exists(local_config_path), is.null(ai_read_local_config(local_config_path)))

d <- data.frame(name = c("甲", "乙"), email = c("a@example.com", "b@example.com"), value = c(1, NA))
summary_only <- ai_dataset_profile(d, FALSE)
with_sample <- ai_dataset_profile(d, TRUE)
stopifnot(!grepl("a@example.com", summary_only, fixed = TRUE))
stopifnot(!grepl("a@example.com", with_sample, fixed = TRUE))
stopifnot(grepl("<已隐藏敏感值>", with_sample, fixed = TRUE))
stopifnot(all(c("name", "email") %in% ai_sensitive_columns(d)))

detailed <- ai_analysis_summary(iris)
stopifnot(identical(detailed$rows, 150L), identical(detailed$columns, 5L),
  length(detailed$fields) == 5L,
  all(c("p01", "p05", "q1", "median", "q3", "p95", "p99", "skewness", "excess_kurtosis") %in%
    names(detailed$fields[[1]]$summary)),
  nrow(detailed$correlations$pearson_strongest_pairs) == 6L,
  nrow(detailed$fields[[5]]$top_categories) == 3L)
large_context <- ai_context_json(list(results = data.frame(index = 1:150, value = c(1:149, Inf))))
stopifnot(grepl('"rows_total": 150', large_context, fixed = TRUE),
  grepl('"rows_included": 100', large_context, fixed = TRUE),
  !grepl("Infinity|NaN", large_context))

original_ai_call <- ai_call
captured_report <- new.env(parent = emptyenv())
ai_call <- function(config, system_prompt, user_prompt, max_tokens = 1600L, transport = NULL) {
  captured_report$prompt <- user_prompt
  "# 模拟报告"
}
testServer(ai_report_server, args = list(
  config = reactive(list(provider = "custom", api_key = "", endpoint = "http://localhost/v1", model = "demo", protocol = "chat", language = "zh", max_tokens = 1000L)),
  algorithm = "测试算法", local_report = reactive("本地报告"),
  computed_context = reactive(list(sample = list(n = 123L), metrics = data.frame(name = "RMSE", value = 1.25)))
), {
  session$setInputs(confirm = TRUE, generate = 1)
  session$flushReact()
  stopifnot(grepl("computed_results_json", captured_report$prompt, fixed = TRUE),
    grepl('"n": 123', captured_report$prompt, fixed = TRUE),
    grepl("只使用所提供的数据", captured_report$prompt, fixed = TRUE))
})
ai_call <- original_ai_call

captured <- new.env(parent = emptyenv())
fake_transport <- function(endpoint, key, payload, protocol) {
  captured$endpoint <- endpoint; captured$key <- key; captured$payload <- payload; captured$protocol <- protocol
  "模拟成功"
}
config <- list(api_key = "top-secret", endpoint = "https://example.com/v1", model = "demo", protocol = "chat")
answer <- ai_call(config, "system", "user", 321, fake_transport)
stopifnot(identical(answer, "模拟成功"), identical(captured$protocol, "chat"))
stopifnot(identical(captured$endpoint, "https://example.com/v1/chat/completions"))
stopifnot(identical(captured$payload$messages[[2]]$content, "user"))
stopifnot(identical(captured$payload$max_tokens, 321L))

settings_path <- tempfile("easyr-ai-settings-", fileext = ".json")
testServer(ai_settings_server, args = list(config_path = settings_path), {
  session$setInputs(provider = "custom", model = "third-party-model", api_key = "", endpoint = "http://127.0.0.1:11434/v1", protocol = "chat")
  session$flushReact()
  current <- config()
  stopifnot(identical(current$provider, "custom"), identical(current$model, "third-party-model"),
    identical(current$endpoint, "http://127.0.0.1:11434/v1"), identical(current$protocol, "chat"), identical(current$api_key, ""))
  session$setInputs(api_key = "saved-key", allow_local_save = TRUE, save_local = 1)
  session$flushReact()
  stopifnot(file.exists(settings_path), identical(ai_read_local_config(settings_path)$api_key, "saved-key"))
  session$setInputs(delete_local = 1)
  session$flushReact()
  stopifnot(!file.exists(settings_path))
})

stopifnot(identical(ai_extract_text(list(output_text = "ok"), "responses"), "ok"))
stopifnot(identical(ai_extract_text(list(choices = list(list(message = list(content = "ok")))), "chat"), "ok"))
parsed <- ai_extract_json("```json\n{\"parameters\":{\"k\":3},\"explanation\":\"测试\"}\n```")
stopifnot(identical(parsed$parameters$k, 3L), identical(parsed$explanation, "测试"))

rendered <- as.character(ai_markdown_html("# 标题\n\n**重点**\n\n| 参数 | 数值 |\n|---|---|\n| K | 3 |\n\n<script>alert(1)</script>\n\n[危险链接](javascript:alert(1))"))
stopifnot(grepl("<h1>标题</h1>", rendered, fixed = TRUE))
stopifnot(grepl("<table>", rendered, fixed = TRUE))
stopifnot(!grepl("<script>", rendered, fixed = TRUE))
stopifnot(!grepl('href="javascript:', rendered, fixed = TRUE))

long_markdown <- paste(vapply(1:6, function(i) paste0("## 第", i, "节\n\n", paste(rep(paste0("第", i, "节内容。"), 180), collapse = "")), character(1)), collapse = "\n\n")
pages <- ai_paginate_markdown(long_markdown, target_chars = 1200)
stopifnot(length(pages) >= 3L, all(vapply(pages, function(x) nzchar(trimws(x)), logical(1))))
stopifnot(sum(grepl("^## 第", unlist(strsplit(pages, "\n")))) == 6L)

pager_test_server <- function(id) moduleServer(id, function(input, output, session) {
  answer <- reactiveVal(long_markdown)
  pager <- ai_pager_server(input, output, answer)
})
testServer(pager_test_server, {
  session$flushReact()
  stopifnot(pager$count() >= 2L, pager$page() == 1L)
  session$setInputs(next_page = 1); session$flushReact()
  stopifnot(pager$page() == 2L)
  session$setInputs(previous_page = 1); session$flushReact()
  stopifnot(pager$page() == 1L)
})

source("R/regression.R")
lm_result <- fit_readable_lm(iris, 1, c(2, 5))
lm_context <- ai_model_context("线性回归", lm_result)
lm_context_json <- ai_context_json(lm_context)
stopifnot(!is.null(lm_context$analysis_sample_summary),
  !is.null(lm_context$fit_statistics$r_squared),
  !is.null(lm_context$coefficients_with_uncertainty),
  !is.null(lm_context$diagnostics$cooks_distance),
  grepl("pearson_strongest_pairs", lm_context_json, fixed = TRUE))

cat("AI 配置、请求协议、数据摘要脱敏、完整计算上下文、Markdown 安全渲染与分页、模拟调用和参数 JSON 解析检查通过。\n")
