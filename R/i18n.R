rmod_translations <- function() c(
  "EasyR · 数据工作台" = "EasyR · Data Workbench",
  "导入表格、整理数据、绘图并导出结果，全程通过按钮操作。" = "Import tables, clean data, create charts, and export results without writing code.",
  "当前数据集" = "Current dataset", "移除当前数据集" = "Remove current dataset",
  "尚未导入数据集" = "No dataset loaded",
  "01 导入数据" = "01 Import data", "单个文件上传上限" = "Per-file upload limit",
  "选择一个或多个表格" = "Select one or more tables", "选择文件" = "Choose files",
  "尚未选择文件" = "No files selected", "CSV 编码" = "CSV encoding", "CSV 分隔符" = "CSV delimiter",
  "100 MB" = "100 MB", "500 MB（推荐）" = "500 MB (recommended)", "1000 MB" = "1000 MB",
  "逗号" = "Comma", "分号" = "Semicolon", "制表符" = "Tab",
  "第一行是字段名" = "First row contains column names", "Excel 工作表" = "Excel sheet",
  "导入并加入数据集" = "Import and add datasets", "试用示例" = "Try sample data",
  "选择一个或多个 CSV，或点击“试用示例”开始。" = "Select one or more CSV files, or click Try sample data.",
  "可以一次选择多个 CSV。批量文件共用当前编码、分隔符和首行设置；Excel 请每次选择一个。取消“第一行是字段名”后，程序会自动生成字段名。表头中的空白字段也会自动命名；第一列为空时命名为“序号”。" = "You can select multiple CSV files at once. Batch files share encoding, delimiter, and header settings. Import Excel files one at a time. If the first row is not a header, column names are generated automatically. Blank headers are also named automatically; a blank first column is named Index.",
  "02 整理数据" = "02 Clean data", "按“清洗 → 筛选排序 → 编辑 → 导出”的顺序处理当前数据集。" = "Process the current dataset in order: clean, filter and sort, edit, then export.",
  "当前处理结果" = "Current result", "① 清洗" = "1 Clean", "② 筛选排序" = "2 Filter & sort", "③ 编辑" = "3 Edit", "④ 查找导出" = "4 Find & export",
  "选择字段与缺失值" = "Columns and missing values", "这些规则会立即应用；下方预览和所有分析模块同步更新。" = "These rules apply immediately; the preview and every analysis module update at the same time.",
  "保留字段" = "Keep columns", "默认缺失值处理" = "Default missing-value handling",
  "保留原样" = "Keep as is", "删除含缺失值的行" = "Drop rows with missing values",
  "用均值填充数值字段" = "Fill numeric columns with means",
  "用中位数填充数值字段" = "Fill numeric columns with medians",
  "用众数填充各字段" = "Fill columns with modes", "用 0 填充数值字段" = "Fill numeric columns with zero",
  "向前填充（使用上一行）" = "Forward fill (use previous row)",
  "向后填充（使用下一行）" = "Backward fill (use next row)",
  "线性插值（数值字段）" = "Linear interpolation (numeric columns)", "删除完全重复的行" = "Remove exact duplicate rows",
  "按字段单独设置" = "Set rules by column", "单列规则会覆盖上面的默认处理方式。例如，收入用中位数，行业用众数，备注保留原样。" = "A column rule overrides the default above. For example, use the median for income, mode for industry, and keep notes unchanged.",
  "字段" = "Column", "这个字段的处理方式" = "Handling for this column", "删除该字段为空的行" = "Drop rows where this column is missing",
  "用均值填充" = "Fill with mean", "用中位数填充" = "Fill with median", "用众数填充" = "Fill with mode", "用 0 填充" = "Fill with zero",
  "向前填充" = "Forward fill", "向后填充" = "Backward fill", "线性插值" = "Linear interpolation",
  "保存此字段规则" = "Save column rule", "清除全部单列规则" = "Clear all column rules",
  "尚未设置单列规则，全部字段使用默认处理方式。" = "No column-specific rules. All columns use the default handling.",
  "清理文字内容" = "Clean text", "适合处理 CSV 中多余空格和看似空白的单元格。" = "Useful for extra spaces and apparently blank cells in CSV files.",
  "文字字段（可多选）" = "Text columns (multiple allowed)", "去除文字首尾空格" = "Trim leading and trailing spaces",
  "把空字符串转换为缺失值 NA" = "Convert empty strings to NA", "应用文字清理" = "Clean text",
  "按条件筛选行" = "Filter rows by condition", "先选择字段和条件，再决定删除匹配行或只保留匹配行。" = "Choose a column and condition, then delete or keep matching rows.",
  "条件字段" = "Condition column", "条件" = "Condition",
  "大于" = "Greater than", "大于等于" = "Greater than or equal to", "小于" = "Less than", "小于等于" = "Less than or equal to",
  "等于" = "Equals", "不等于" = "Does not equal", "包含文字" = "Contains text", "不包含文字" = "Does not contain text",
  "为空" = "Is missing", "不为空" = "Is not missing", "介于两个值之间" = "Between two values",
  "比较值 / 起始值" = "Comparison value / start", "结束值（仅区间条件）" = "End value (between only)", "处理方式" = "Action",
  "删除符合条件的行" = "Delete matching rows", "只保留符合条件的行" = "Keep matching rows", "执行筛选" = "Apply filter",
  "数据排序" = "Sort data", "排序字段" = "Sort column", "排序方向" = "Sort direction", "升序" = "Ascending", "降序" = "Descending",
  "缺失值排在最后" = "Put missing values last", "应用排序" = "Apply sort",
  "精确修改行和单元格" = "Edit rows and cells", "适合少量修正；大批量删除建议使用条件筛选。" = "Use this for small corrections; use conditional filtering for bulk row deletion.",
  "增加一个空白行" = "Add a blank row",
  "要修改的行号" = "Row number to edit", "要修改的字段" = "Column to edit", "新值" = "New value",
  "设为缺失值 NA" = "Set to missing (NA)", "修改单元格" = "Update cell", "删除指定行" = "Rows to delete", "删除这些行" = "Delete rows",
  "字段操作" = "Column operations", "当前字段" = "Current column", "新的字段名" = "New column name", "重命名" = "Rename", "删除字段" = "Delete column", "新增字段名" = "New column name",
  "转换字段类型" = "Convert column type", "转换当前字段" = "Convert current column", "类别" = "Category",
  "新增字段类型" = "New column type", "文字" = "Text", "数值" = "Numeric", "整数" = "Integer", "逻辑值" = "Logical", "日期" = "Date",
  "默认值" = "Default value", "默认填为缺失值 NA" = "Fill with missing values (NA)", "增加字段" = "Add column",
  "查找数据" = "Find data", "在指定字段或全部数据中查找，并返回当前处理结果中的行号。" = "Search one column or the full dataset and return row numbers from the current result.",
  "查找字段" = "Search column", "全部字段" = "All columns", "查找内容" = "Search text",
  "完全匹配" = "Exact match", "区分大小写" = "Case sensitive", "查找" = "Find",
  "撤销上一步" = "Undo last step", "恢复导入版本" = "Restore imported version", "下载处理后的 CSV" = "Download cleaned CSV",
  "筛选、排序和编辑操作会保存为当前数据集的工作版本。可逐步撤销，也可恢复最初导入的数据。" = "Filtering, sorting, and editing are saved as the current dataset's working version. Undo one step at a time or restore the imported data.",
  "基础清洗选项会立即生效。行、单元格和列操作需要点击对应按钮，并以当前处理后数据为基础；执行后会成为当前数据集的工作版本。点击“恢复原始数据”可撤销全部整理操作。" = "Basic cleaning options apply immediately. Row, cell, and column changes use the currently processed data and are saved as this dataset's working version. Reset current dataset restores the imported data.",
  "导出处理结果" = "Export result", "导出的内容与数据预览和分析模块当前使用的数据完全一致。" = "The exported data is exactly what the preview and analysis modules currently use.",
  "保存 CSV 到项目文件夹" = "Save CSV to project folder",
  "选项修改后立即生效。均值、中位数、0 和线性插值只处理数值字段；众数和前后填充也可处理文字字段。线性插值只填补两个有效值之间的空缺。全空字段保持不变。" = "Changes apply immediately. Mean, median, zero, and linear interpolation affect numeric columns only. Mode and forward/backward fill also support text columns. Linear interpolation fills gaps only between two valid values. Entirely empty columns stay unchanged.",
  "数据预览" = "Data Preview", "字段概况" = "Column Summary", "查看处理后的数据" = "Processed Data",
  "AI 数据顾问" = "AI Data Advisor", "AI 设置" = "AI Settings", "AI 增强解读" = "AI Enhanced Report",
  "选择供应商、模型并填写 API 密钥。第三方兼容服务还可以自行填写 API URL。默认只在当前会话中使用。" = "Choose a provider and model, then enter an API key. Compatible third-party services can use a custom API URL. By default, the settings are used only for the current session.",
  "供应商" = "Provider", "模型名称" = "Model name", "API 密钥" = "API key", "API 地址" = "API endpoint",
  "模型名称（可直接输入）" = "Model name (type or select)", "第三方供应商" = "Third-party provider", "第三方 API URL" = "Third-party API URL",
  "接口协议" = "API protocol", "默认发送范围" = "Default data scope", "AI 报告语言" = "AI report language",
  "最大输出长度（tokens）" = "Maximum output length (tokens)", "仅字段结构与统计摘要（推荐）" = "Schema and statistical summary only (recommended)",
  "摘要 + 最多 5 行样本" = "Summary plus up to 5 sample rows", "自定义兼容接口" = "Custom compatible endpoint",
  "测试连接" = "Test connection", "隐私说明" = "Privacy", "尚未测试连接。" = "Connection has not been tested.",
  "本地保存" = "Local storage", "我选择将当前 AI 连接配置保存在这台电脑上" = "Save the current AI connection settings on this computer",
  "配置包含供应商、模型、API 地址、协议和 API Key，不包含数据集或个人信息。API Key 会以可读取文本保存在当前用户的配置目录，请勿在公共电脑上启用。" = "The configuration contains the provider, model, API endpoint, protocol, and API key. It contains no dataset or personal information. The API key is stored as readable text in the current user's configuration directory; do not enable this on a shared computer.",
  "保存到本机" = "Save locally", "删除本地配置" = "Delete local configuration",
  "EasyR 项目保存与恢复" = "Save and Restore EasyR Project",
  "保存全部数据集、当前数据集、整理状态以及分析和绘图参数。项目文件不包含 API Key。" = "Save all datasets, the active dataset, data preparation state, and analysis and chart settings. The project file does not contain API keys.",
  "保存 EasyR 项目" = "Save EasyR project", "打开已有项目" = "Open an existing project",
  "选择项目" = "Choose project", "尚未选择项目文件" = "No project file selected",
  "恢复这个项目" = "Restore this project", "尚未保存或恢复项目。" = "No project has been saved or restored yet.",
  "EasyR 数据工作台" = "EasyR Data Workbench", "导入 · 整理 · 探索 · 建模 · 预测 · 报告" = "Import · Prepare · Explore · Model · Forecast · Report",
  "数据分析工作台" = "Data Analysis Workbench", "创建新项目" = "Create New Project",
  "或者" = "or", "打开已有 EasyR 项目" = "Open an Existing EasyR Project",
  "选择项目文件" = "Choose Project File", "尚未选择项目文件" = "No project file selected",
  "打开项目" = "Open Project", "选择后自动打开项目" = "The project opens automatically after selection",
  "试用示例数据" = "Try Sample Data",
  "创建一个新项目，或打开之前保存的 .easyr 项目。" = "Create a new project or open a saved .easyr project.",
  "首页" = "Home", "数据" = "Data", "探索" = "Explore", "建模" = "Modeling",
  "数据工作区" = "Data Workspace", "数据浏览" = "Browse Data",
  "左侧导入数据，在这里浏览内容并完成整理。" = "Import data on the left, then browse and prepare it here.",
  "导入与项目" = "Import & Projects", "整理数据" = "Prepare Data", "项目文件" = "Project File",
  "保存当前工作，或从之前的 EasyR 项目继续。" = "Save your current work or continue from an earlier EasyR project.",
  "从数据到结论，按步骤完成" = "Go from data to conclusions, step by step",
  "导入和整理数据，探索规律，建立模型，再生成可以复核和导出的分析结果。所有功能都可以通过按钮操作。" = "Import and prepare data, explore patterns, build models, and create reviewable, exportable results. Every feature is available through buttons.",
  "推荐工作流程" = "Recommended workflow", "1 导入或恢复" = "1 Import or restore", "2 整理数据" = "2 Prepare data",
  "3 探索规律" = "3 Explore patterns", "4 建模验证" = "4 Model and validate", "5 导出报告" = "5 Export report",
  "导入 CSV／Excel，或者恢复之前保存的 .easyr 项目。" = "Import CSV/Excel files or restore a saved .easyr project.",
  "开始导入" = "Start importing", "处理缺失值、筛选排序，并完成行列增删改查。" = "Handle missing values, filter and sort, and edit rows and columns.",
  "整理当前数据" = "Prepare current data", "探索与绘图" = "Explore & Plot",
  "使用描述统计、ggplot2、3D 图形和分布演示理解数据。" = "Understand data with descriptive statistics, ggplot2, 3D charts, and distribution demos.",
  "开始探索" = "Start exploring", "统计建模" = "Statistical Modeling",
  "使用回归、随机森林、SVM、PCA 和 K-means，并查看原理教程。" = "Use regression, random forest, SVM, PCA, and K-means with interactive tutorials.",
  "选择模型" = "Choose a model", "分析趋势、季节性、相关结构、GARCH 波动和未来预测。" = "Analyze trends, seasonality, dependence, GARCH volatility, and forecasts.",
  "分析序列" = "Analyze a series", "AI 数据顾问" = "AI Data Advisor",
  "连接自己的模型供应商，获得数据处理、参数和报告建议。" = "Connect your model provider for data preparation, parameter, and report suggestions.",
  "设置 AI" = "Configure AI", "当前会话中的数据集" = "Datasets in this session",
  "尚未导入数据。可以从左侧选择文件、试用内置示例，或恢复 EasyR 项目。" = "No data has been imported. Choose files on the left, try the sample data, or restore an EasyR project.",
  "默认不会保存 API Key；只有勾选并点击“保存到本机”才会创建本地配置文件。" = "The API key is not saved by default. A local file is created only after selecting the option and clicking Save locally.",
  "本地配置位于当前用户的系统配置目录，不在 EasyR 项目和 GitHub 仓库中。" = "The local configuration is stored in the current user's system configuration directory, outside the EasyR project and GitHub repository.",
  "EasyR 自动使用供应商的默认文本模型。默认只发送字段结构和统计摘要，不发送原始数据行。" = "EasyR uses the provider's default text model. It sends only the schema and statistical summary, never raw data rows.",
  "描述你想用这份数据完成什么，AI 会提出清洗、变量处理、算法选择和参数设置建议。" = "Describe what you want to achieve. AI will suggest cleaning, variable handling, algorithms, and parameter settings.",
  "分析目标" = "Analysis goal", "变量含义（可选）" = "Variable meanings (optional)", "希望 AI 重点回答" = "AI focus",
  "完整分析路线" = "Complete analysis workflow", "数据清洗" = "Data cleaning", "算法选择" = "Algorithm selection", "算法参数推荐" = "Algorithm parameter recommendations",
  "查看发送内容" = "Preview data to send", "让 AI 分析" = "Ask AI to analyze", "AI 推荐参数" = "AI Parameter Recommendations",
  "AI 会参考当前数据规模、字段和参数范围提出建议。应用前会先显示建议，不会自动运行模型。" = "AI uses the current data size, columns, and allowed ranges. It shows recommendations before applying them and never runs the model automatically.",
  "建模目标（可选）" = "Modeling goal (optional)", "生成参数建议" = "Generate parameter recommendations", "应用这些参数" = "Apply these parameters",
  "本次分析目的" = "Purpose of this analysis", "变量含义与业务背景（可选）" = "Variable meanings and context (optional)",
  "希望 AI 重点解释的问题（可选）" = "Questions for AI to focus on (optional)", "生成 AI 增强报告" = "Generate AI enhanced report",
  "下载 AI 建议 Markdown" = "Download AI advice (Markdown)", "下载 AI 报告 Markdown" = "Download AI report (Markdown)",
  "上一页" = "Previous", "下一页" = "Next",
  "统计与绘图" = "Statistics & Charts", "分布演示" = "Distribution Demos", "线性回归" = "Linear Regression", "随机森林" = "Random Forest", "支持向量机" = "Support Vector Machine", "主成分分析" = "Principal Component Analysis", "K-means 聚类" = "K-means Clustering", "时间序列" = "Time Series",
  "原理演示" = "Interactive Demo", "收起演示" = "Hide Demo",
  "随机森林原理演示" = "Random Forest Interactive Demo",
  "ggplot2 绘图工作台" = "ggplot2 Chart Workbench", "图形" = "Chart type",
  "直方图" = "Histogram", "密度图" = "Density plot", "散点图" = "Scatter plot",
  "折线趋势图" = "Line Trend Chart", "Q-Q 正态检验图" = "Normal Q-Q Plot",
  "经验累积分布图（ECDF）" = "Empirical CDF (ECDF)", "相关性热图" = "Correlation Heatmap",
  "3D 散点图（可旋转）" = "3D scatter plot (interactive)", "箱线图" = "Box plot",
  "小提琴图" = "Violin plot", "类别频数图" = "Category counts",
  "横轴 / 数值字段" = "X axis / numeric column", "纵轴" = "Y axis", "Z 轴（数值字段）" = "Z axis (numeric column)",
  "相关性字段（数值，可多选）" = "Correlation columns (numeric; multiple allowed)",
  "相关系数" = "Correlation", "相关系数类型" = "Correlation method", "Pearson 线性相关" = "Pearson linear correlation",
  "Spearman 秩相关" = "Spearman rank correlation", "显示相关系数数值" = "Show correlation values",
  "分组字段（可选，仅显示 2～20 个类别的字段）" = "Group column (optional; 2–20 categories)",
  "不分组" = "No grouping", "显示均值竖线" = "Show mean line", "显示中位数竖线" = "Show median line",
  "显示数据点" = "Show data points", "添加 LOESS 趋势线" = "Add LOESS trend",
  "显示趋势置信带" = "Show trend confidence band",
  "分箱数" = "Number of bins", "添加线性回归线" = "Add linear fit", "显示 95% 均值置信带" = "Show 95% mean confidence band",
  "按组分面显示（选择分组后生效）" = "Facet by group", "叠加原始数据点" = "Overlay raw points",
  "横向显示" = "Horizontal orientation", "编辑图像" = "Edit chart", "收起图像编辑" = "Hide chart editor",
  "图表标题（留空使用字段名）" = "Chart title (blank uses column name)", "横轴标题（留空自动）" = "X-axis title (automatic if blank)",
  "纵轴标题（留空自动）" = "Y-axis title (automatic if blank)", "Z 轴标题（留空自动）" = "Z-axis title (automatic if blank)",
  "图像标题（留空保留原标题）" = "Chart title (blank keeps original)", "副标题（留空保留原副标题）" = "Subtitle (blank keeps original)",
  "横轴标题（留空保留）" = "X-axis title (blank keeps original)", "纵轴标题（留空保留）" = "Y-axis title (blank keeps original)",
  "主题" = "Theme", "简洁" = "Minimal", "经典" = "Classic", "黑白网格" = "Black & white grid",
  "单组颜色" = "Single-series color", "分组配色" = "Group palette", "配色" = "Palette",
  "保留原图" = "Keep original", "蓝绿" = "Blue–green", "暖色" = "Warm", "绿色" = "Green", "黑白灰" = "Grayscale",
  "蓝色" = "Blue", "紫色" = "Purple", "橙色" = "Orange", "蓝绿黄" = "Blue–green–yellow",
  "紫红黄" = "Purple–red–yellow", "暗紫橙" = "Dark purple–orange",
  "背景" = "Background", "白色" = "White", "浅灰蓝" = "Light blue-gray", "透明" = "Transparent",
  "透明度" = "Opacity", "点大小" = "Point size", "线宽" = "Line width", "字号" = "Font size",
  "图片宽度（英寸）" = "Image width (inches)", "图片高度（英寸）" = "Image height (inches)",
  "导出宽度（英寸）" = "Export width (inches)", "导出高度（英寸）" = "Export height (inches)", "导出 DPI" = "Export DPI",
  "恢复默认绘图设置" = "Reset chart settings", "恢复图像设置" = "Reset image settings",
  "下载 PNG 图片" = "Download PNG", "下载 PNG" = "Download PNG", "保存 PNG 到项目文件夹" = "Save PNG to project folder",
  "查看数值字段描述统计" = "View numeric summary statistics",
  "概率分布与抽样演示" = "Probability Distributions & Sampling Demos",
  "设置分布参数、样本量和重复次数，比较理论结果与随机模拟，并演示大数定律和中心极限定理。" = "Set distribution parameters, sample size, and repetitions to compare theory with simulation and demonstrate the law of large numbers and central limit theorem.",
  "概率分布" = "Probability distribution", "每次样本量" = "Sample size", "重复抽样次数" = "Sampling repetitions",
  "正态分布" = "Normal", "均匀分布" = "Uniform", "指数分布" = "Exponential", "Gamma 分布" = "Gamma",
  "Beta 分布" = "Beta", "卡方分布" = "Chi-squared", "t 分布" = "Student's t", "F 分布" = "F",
  "对数正态分布" = "Log-normal", "Weibull 分布" = "Weibull", "Logistic 分布" = "Logistic", "Cauchy 分布" = "Cauchy",
  "Bernoulli 分布" = "Bernoulli", "二项分布" = "Binomial", "Poisson 分布" = "Poisson", "几何分布" = "Geometric",
  "负二项分布" = "Negative binomial", "超几何分布" = "Hypergeometric",
  "均值 μ" = "Mean μ", "标准差 σ" = "Standard deviation σ", "下限 a" = "Lower bound a", "上限 b" = "Upper bound b",
  "率参数 λ" = "Rate λ", "形状参数 shape" = "Shape", "率参数 rate" = "Rate",
  "形状参数 α" = "Shape α", "形状参数 β" = "Shape β", "自由度 df" = "Degrees of freedom",
  "分子自由度 df1" = "Numerator degrees of freedom", "分母自由度 df2" = "Denominator degrees of freedom",
  "对数均值 meanlog" = "Log mean", "对数标准差 sdlog" = "Log standard deviation", "尺度参数 scale" = "Scale",
  "位置参数 location" = "Location", "成功概率 p" = "Success probability p", "试验次数 n" = "Number of trials n",
  "均值参数 λ" = "Mean parameter λ", "目标成功次数 size" = "Target successes", "总体成功元素数 m" = "Population successes m",
  "总体失败元素数 n" = "Population failures n", "抽取数量 k" = "Draw size k",
  "样本量 × 重复次数最多为 5,000,000。改变参数与随机种子后重新生成，可以比较分布形状、抽样波动和收敛速度。" = "Sample size × repetitions may not exceed 5,000,000. Change parameters and the random seed to compare distribution shapes, sampling variation, and convergence speed.",
  "生成分布样本" = "Generate samples", "教学解读" = "Teaching Notes", "理论与样本统计" = "Theory vs Sample Statistics",
  "分布形状" = "Distribution Shape", "分布函数 CDF" = "CDF", "Q-Q 图" = "Q-Q Plot", "大数定律" = "Law of Large Numbers",
  "样本均值与中心极限定理" = "Sample Means & CLT", "模拟数据" = "Simulated Data",
  "下载教学报告 TXT" = "Download teaching report (TXT)", "下载随机样本 CSV" = "Download random sample (CSV)",
  "下载重复抽样均值 CSV" = "Download repeated sample means (CSV)",
  "选择要解释的数值字段作为因变量，再选择一个或多个自变量。使用左侧清洗后的数据。" = "Choose a numeric outcome and one or more predictors. The model uses the cleaned dataset.",
  "因变量 Y（数值）" = "Outcome Y (numeric)", "自变量 X（可多选）" = "Predictors X (multiple allowed)",
  "运行线性回归" = "Run linear regression", "专业解读" = "Professional Report", "系数表" = "Coefficient Table",
  "拟合图（ggplot2）" = "Fit Plot (ggplot2)", "诊断图（ggplot2）" = "Diagnostic Plots (ggplot2)",
  "残差与拟合值" = "Residuals vs Fitted", "正态 Q-Q" = "Normal Q-Q",
  "下载专业解读报告 TXT" = "Download professional report (TXT)", "保存专业报告到项目文件夹" = "Save report to project folder",
  "使用左侧清洗后的数据建立随机森林回归或分类模型，并用独立测试集评估预测表现。" = "Build a random forest regression or classification model from the cleaned data and evaluate predictions on an independent test set.",
  "目标字段" = "Target column", "预测字段（可多选）" = "Predictor columns (multiple allowed)",
  "任务类型" = "Task type", "自动判断" = "Automatic", "回归" = "Regression", "分类" = "Classification",
  "训练集比例" = "Training proportion", "随机种子" = "Random seed", "树的数量" = "Number of trees",
  "每次分裂候选字段数（0 = 自动）" = "Candidate columns per split (0 = automatic)",
  "自动判断时，文字／类别目标以及不超过 10 个不同值的数值目标按分类处理，其余数值目标按回归处理。分类采用分层随机划分。" = "In automatic mode, text or categorical targets and numeric targets with no more than 10 unique values use classification; other numeric targets use regression. Classification uses a stratified random split.",
  "运行随机森林" = "Run random forest", "模型评估" = "Model Evaluation",
  "评估图（ggplot2）" = "Evaluation Plot (ggplot2)", "变量重要性" = "Variable Importance",
  "测试集预测" = "Test-set Predictions", "下载随机森林报告 TXT" = "Download random forest report (TXT)",
  "下载测试集预测 CSV" = "Download test predictions (CSV)",
  "保存随机森林报告到项目文件夹" = "Save random forest report to project folder",
  "支持向量机（SVM）" = "Support Vector Machine (SVM)",
  "建立支持向量机分类或回归模型。非线性核可以处理弯曲的分类边界和非线性关系。" = "Build a support vector classification or regression model. Nonlinear kernels can represent curved decision boundaries and nonlinear relationships.",
  "核函数" = "Kernel", "径向基 RBF（推荐起点）" = "Radial basis RBF (recommended starting point)",
  "线性核" = "Linear kernel", "多项式核" = "Polynomial kernel", "Sigmoid 核" = "Sigmoid kernel",
  "成本参数 C" = "Cost C", "γ（0 = 自动）" = "Gamma (0 = automatic)", "多项式次数" = "Polynomial degree",
  "核函数常数项" = "Kernel constant", "回归 ε" = "Regression epsilon", "自动标准化数值特征" = "Scale numeric features automatically",
  "运行支持向量机" = "Run support vector machine", "支持向量" = "Support Vectors",
  "下载 SVM 报告 TXT" = "Download SVM report (TXT)", "保存 SVM 报告到项目文件夹" = "Save SVM report to project folder",
  "主成分分析（PCA）" = "Principal Component Analysis (PCA)",
  "选择两个或更多数值字段，通过主成分提取数据中的主要变化结构。使用左侧清洗后的数据。" = "Select two or more numeric columns and use principal components to extract the main variation patterns from the cleaned data.",
  "分析字段（数值，可多选）" = "Analysis columns (numeric, multiple allowed)",
  "得分图分组字段（可选）" = "Score-plot grouping column (optional)",
  "分析前标准化字段（推荐）" = "Standardize columns before analysis (recommended)",
  "字段量纲或波动范围不同时建议保持标准化。分组字段只影响得分图颜色，不参与主成分计算。" = "Keep standardization enabled when columns use different units or ranges. The grouping column only controls score-plot colors and is not included in PCA.",
  "运行主成分分析" = "Run PCA", "方差解释率" = "Explained Variance",
  "主成分得分图" = "Component Scores Plot", "载荷" = "Loadings", "主成分得分数据" = "Component Scores",
  "下载 PCA 报告 TXT" = "Download PCA report (TXT)", "下载主成分得分 CSV" = "Download component scores (CSV)",
  "下载载荷 CSV" = "Download loadings (CSV)", "保存 PCA 报告到项目文件夹" = "Save PCA report to project folder",
  "选择两个或更多数值字段，根据样本之间的距离自动发现数据分组。使用左侧清洗后的数据。" = "Select two or more numeric columns and discover groups from distances between observations in the cleaned data.",
  "聚类字段（数值，可多选）" = "Clustering columns (numeric, multiple allowed)", "聚类数量 K" = "Number of clusters K",
  "聚类前标准化字段（推荐）" = "Standardize columns before clustering (recommended)",
  "随机初始值次数" = "Random initializations", "最大迭代次数" = "Maximum iterations",
  "字段单位或波动范围不同时建议保持标准化。增加随机初始值次数通常能降低落入较差局部解的风险，但会增加计算时间。" = "Keep standardization enabled when columns use different units or ranges. More random initializations usually reduce the risk of a poor local solution but increase computation time.",
  "运行 K-means 聚类" = "Run K-means", "聚类质量" = "Cluster Quality", "二维聚类图" = "2D Cluster Plot",
  "肘部图" = "Elbow Plot", "轮廓系数" = "Silhouette Scores", "聚类中心" = "Cluster Centers",
  "聚类结果数据" = "Clustered Data", "下载 K-means 报告 TXT" = "Download K-means report (TXT)",
  "下载聚类结果 CSV" = "Download clustered data (CSV)", "下载聚类中心 CSV" = "Download cluster centers (CSV)",
  "保存 K-means 报告到项目文件夹" = "Save K-means report to project folder",
  "时间序列分析" = "Time Series Analysis",
  "在线下载金融与 FRED 数据" = "Download Financial & FRED Data",
  "Yahoo Finance 适合股票、ETF 和指数；FRED 适合利率、GDP、CPI 等经济序列。下载后会加入顶部数据集列表。" = "Yahoo Finance provides stocks, ETFs, and indices; FRED provides interest rates, GDP, CPI, and other economic series. Downloads are added to the dataset list at the top.",
  "数据源" = "Data source", "Yahoo Finance（quantmod）" = "Yahoo Finance (quantmod)", "FRED 经济数据" = "FRED economic data",
  "Yahoo 代码" = "Yahoo symbol", "例如 AAPL、0700.HK、^GSPC" = "For example: AAPL, 0700.HK, ^GSPC",
  "FRED 序列 ID" = "FRED series ID", "例如 GDP、CPIAUCSL、DGS10" = "For example: GDP, CPIAUCSL, DGS10",
  "FRED API Key" = "FRED API key", "32 位小写字母或数字" = "32 lowercase letters or digits",
  "FRED 要求每位用户使用自己的 API Key。Key 只保存在当前会话内，不会写入文件。" = "FRED requires each user to use their own API key. It stays only in the current session and is never written to a file.",
  "使用 FRED 下载即表示同意" = "By downloading from FRED, you agree to the", "FRED API 使用条款" = "FRED API Terms of Use",
  "日期范围" = "Date range", "下载并使用" = "Download and use",
  "下载原始 CSV" = "Download raw CSV", "时间字段" = "Time column", "数值字段" = "Value column",
  "序列变换" = "Transformation", "原始值" = "Level", "一阶差分" = "First difference",
  "百分比变化" = "Percent change", "对数收益率" = "Log return",
  "显示移动平均" = "Show moving average", "移动平均窗口" = "Moving-average window", "趋势线" = "Trend line",
  "不显示" = "None", "线性趋势" = "Linear trend", "LOESS 趋势" = "LOESS trend",
  "ACF 最大滞后" = "Maximum ACF lag", "生成 STL 季节分解" = "Create STL decomposition",
  "季节周期（观测数）" = "Seasonal period (observations)", "运行时间序列分析" = "Run time-series analysis",
  "分析摘要" = "Analysis Summary", "序列图（ggplot2）" = "Series Plot (ggplot2)",
  "自相关图（ggplot2）" = "Autocorrelation Plot (ggplot2)", "季节分解（ggplot2）" = "Seasonal Decomposition (ggplot2)",
  "分析数据" = "Analysis Data"
)

language_switch_ui <- function() {
  dictionary <- jsonlite::toJSON(as.list(rmod_translations()), auto_unbox = TRUE, ensure_ascii = FALSE)
  script <- sprintf("(function(){
    const dict=%s; let language='zh'; let translating=false;
    function translated(key){
      if(dict[key]) return dict[key]; let match;
      if((match=key.match(/^已载入 (\\d+) 个数据集 · 当前：(.*) · (\\d+) 行 × (\\d+) 列$/))) return 'Loaded '+match[1]+' dataset(s) · Current: '+match[2]+' · '+match[3]+' rows × '+match[4]+' columns';
      if((match=key.match(/^(\\d+) 行 · (\\d+) 列 · (\\d+) 个缺失值 · (\\d+) 个重复行$/))) return match[1]+' rows · '+match[2]+' columns · '+match[3]+' missing values · '+match[4]+' duplicate rows';
      if((match=key.match(/^(\\d+) 行 · (\\d+) 列 · (\\d+) 个缺失值$/))) return match[1]+' rows · '+match[2]+' columns · '+match[3]+' missing values';
      if((match=key.match(/^变量 (\\d+) 名称$/))) return 'Variable '+match[1]+' name';
      if((match=key.match(/^约束 (\\d+) 名称$/))) return 'Constraint '+match[1]+' name';
      if((match=key.match(/^(x\\d+) 系数$/))) return match[1]+' coefficient';
      if(key==='已加入内置鸢尾花示例 iris。') return 'Added the built-in iris sample dataset.';
      if(key==='内置示例 iris') return 'Built-in sample: iris';
      return null;
    }
    function replaceText(node){
      if(node.nodeType!==Node.TEXT_NODE) return;
      const raw=node.nodeValue; const trimmed=raw.trim(); if(!trimmed) return;
      if(node.__rmodOriginal===undefined) node.__rmodOriginal=raw;
      if(language==='zh'){ if(node.nodeValue!==node.__rmodOriginal) node.nodeValue=node.__rmodOriginal; return; }
      const original=node.__rmodOriginal; const key=original.trim(); const value=translated(key); if(!value) return;
      const lead=original.match(/^\\s*/)[0], trail=original.match(/\\s*$/)[0]; const desired=lead+value+trail;
      if(node.nodeValue!==desired) node.nodeValue=desired;
    }
    function translate(root){ translating=true; const walker=document.createTreeWalker(root,NodeFilter.SHOW_TEXT); let node;
      while((node=walker.nextNode())) replaceText(node);
      root.querySelectorAll&&root.querySelectorAll('[placeholder]').forEach(function(el){
        if(el.__rmodPlaceholder===undefined) el.__rmodPlaceholder=el.getAttribute('placeholder')||'';
        el.setAttribute('placeholder',language==='en'?(translated(el.__rmodPlaceholder)||el.__rmodPlaceholder):el.__rmodPlaceholder);
      }); translating=false;
    }
    let installed=false;
    function install(){ if(installed||!window.Shiny) return; installed=true;
      Shiny.addCustomMessageHandler('rmod-language',function(message){ language=message.language; translate(document.body); document.documentElement.lang=language==='en'?'en':'zh-CN'; });
      new MutationObserver(function(mutations){ if(translating||language!=='en') return; mutations.forEach(function(m){
        m.addedNodes.forEach(function(n){ if(n.nodeType===Node.TEXT_NODE) replaceText(n); else if(n.nodeType===Node.ELEMENT_NODE) translate(n); });
        if(m.type==='characterData') { const n=m.target; if(n.__rmodOriginal!==undefined && n.nodeValue.trim()!==(translated(n.__rmodOriginal.trim())||'')) n.__rmodOriginal=n.nodeValue; replaceText(n); }
      }); }).observe(document.body,{childList:true,subtree:true,characterData:true});
    }
    install(); document.addEventListener('shiny:connected',install);
  })();", dictionary)
  tagList(
    tags$div(class = "language-switch", actionButton("toggle_language", "English", icon = icon("globe"))),
    tags$script(HTML(script))
  )
}

language_switch_server <- function(input, output, session) {
  language <- reactiveVal("zh")
  observeEvent(input$toggle_language, {
    next_language <- if (identical(language(), "zh")) "en" else "zh"
    language(next_language)
    updateActionButton(session, "toggle_language", label = if (next_language == "en") "中文" else "English", icon = icon("globe"))
    session$sendCustomMessage("rmod-language", list(language = next_language))
  })
  reactive(language())
}
