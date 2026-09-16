rmod_translations <- function() c(
  "EasyR · 数据工作台" = "EasyR · Data Workbench",
  "导入表格、整理数据、绘图并导出结果，全程通过按钮操作。" = "Import tables, clean data, create charts, and export results without writing code.",
  "当前数据集" = "Current dataset", "移除当前数据集" = "Remove current dataset",
  "尚未导入数据集" = "No dataset loaded", "工作目录" = "Working directory",
  "当前工作目录" = "Current working directory", "文件夹路径" = "Folder path", "浏览文件夹" = "Browse folders",
  "确定工作目录" = "Set working directory", "上一级" = "Parent folder", "打开子文件夹" = "Open folder",
  "点击“保存到工作目录”会把结果写入此文件夹。普通下载按钮仍由浏览器决定保存位置。" = "Save-to-working-directory buttons write results here. Regular downloads use the browser's download location.",
  "01 导入数据" = "01 Import data", "单个文件上传上限" = "Per-file upload limit",
  "选择一个或多个表格" = "Select one or more tables", "选择文件" = "Choose files",
  "尚未选择文件" = "No files selected", "CSV 编码" = "CSV encoding", "CSV 分隔符" = "CSV delimiter",
  "100 MB" = "100 MB", "500 MB（推荐）" = "500 MB (recommended)", "1000 MB" = "1000 MB",
  "逗号" = "Comma", "分号" = "Semicolon", "制表符" = "Tab",
  "第一行是字段名" = "First row contains column names", "Excel 工作表" = "Excel sheet",
  "导入并加入数据集" = "Import and add datasets", "试用示例" = "Try sample data",
  "选择一个或多个 CSV，或点击“试用示例”开始。" = "Select one or more CSV files, or click Try sample data.",
  "可以一次选择多个 CSV。批量文件共用当前编码、分隔符和首行设置；Excel 请每次选择一个。取消“第一行是字段名”后，程序会自动生成字段名。" = "You can select multiple CSV files at once. Batch files share encoding, delimiter, and header settings. Import Excel files one at a time. If the first row is not a header, column names are generated automatically.",
  "02 整理数据" = "02 Clean data", "保留字段" = "Keep columns", "缺失值" = "Missing values",
  "保留原样" = "Keep as is", "删除含缺失值的行" = "Drop rows with missing values",
  "用中位数填充数值字段" = "Fill numeric columns with medians", "删除重复行" = "Remove duplicate rows",
  "恢复原始数据" = "Reset current dataset", "下载处理后的 CSV" = "Download cleaned CSV",
  "保存 CSV 到工作目录" = "Save CSV to working directory",
  "选项修改后立即生效。重复行按所选字段判断；中位数填充保留非数值字段和全空字段。" = "Changes apply immediately. Duplicate rows are evaluated using the selected columns; median filling leaves nonnumeric and entirely empty columns unchanged.",
  "数据预览" = "Data Preview", "字段概况" = "Column Summary", "查看处理后的数据" = "Processed Data",
  "统计与绘图" = "Statistics & Charts", "线性回归" = "Linear Regression", "时间序列" = "Time Series",
  "ggplot2 绘图工作台" = "ggplot2 Chart Workbench", "图形" = "Chart type",
  "直方图" = "Histogram", "密度图" = "Density plot", "散点图" = "Scatter plot",
  "3D 散点图（可旋转）" = "3D scatter plot (interactive)", "箱线图" = "Box plot",
  "小提琴图" = "Violin plot", "类别频数图" = "Category counts",
  "横轴 / 数值字段" = "X axis / numeric column", "纵轴" = "Y axis", "Z 轴（数值字段）" = "Z axis (numeric column)",
  "分组字段（可选，仅显示 2～20 个类别的字段）" = "Group column (optional; 2–20 categories)",
  "不分组" = "No grouping", "显示均值竖线" = "Show mean line", "显示中位数竖线" = "Show median line",
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
  "下载 PNG 图片" = "Download PNG", "下载 PNG" = "Download PNG", "保存 PNG 到工作目录" = "Save PNG to working directory",
  "查看数值字段描述统计" = "View numeric summary statistics",
  "选择要解释的数值字段作为因变量，再选择一个或多个自变量。使用左侧清洗后的数据。" = "Choose a numeric outcome and one or more predictors. The model uses the cleaned dataset.",
  "因变量 Y（数值）" = "Outcome Y (numeric)", "自变量 X（可多选）" = "Predictors X (multiple allowed)",
  "运行线性回归" = "Run linear regression", "专业解读" = "Professional Report", "系数表" = "Coefficient Table",
  "拟合图（ggplot2）" = "Fit Plot (ggplot2)", "诊断图（ggplot2）" = "Diagnostic Plots (ggplot2)",
  "残差与拟合值" = "Residuals vs Fitted", "正态 Q-Q" = "Normal Q-Q",
  "下载专业解读报告 TXT" = "Download professional report (TXT)", "保存专业报告到工作目录" = "Save report to working directory",
  "时间序列分析" = "Time Series Analysis", "时间字段" = "Time column", "数值字段" = "Value column",
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
      if((match=key.match(/^当前工作目录：\\s*(.*)$/))) return 'Current working directory: '+match[1];
      if((match=key.match(/^已载入 (\\d+) 个数据集 · 当前：(.*) · (\\d+) 行 × (\\d+) 列$/))) return 'Loaded '+match[1]+' dataset(s) · Current: '+match[2]+' · '+match[3]+' rows × '+match[4]+' columns';
      if((match=key.match(/^(\\d+) 行 · (\\d+) 列 · (\\d+) 个缺失值$/))) return match[1]+' rows · '+match[2]+' columns · '+match[3]+' missing values';
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
