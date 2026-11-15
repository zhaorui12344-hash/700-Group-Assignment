#!/usr/bin/env Rscript
# Assignment 3 — R 学习版（中文注释）
# 每次阅读一个编号板块。这是一份可完整运行的脚本，不是伪代码。
# 它执行相同分析、生成原中文版 HTML 报告，并完成同一套独立检查。
# 另一个学习文件仅注释语言不同，不代表另一套统计方法，不必为了分析重复运行两份。
# 原始变量名、CSV 标签及报告文字保持不变，方便逐项比对结果。
# 
# 运行：Rscript --vanilla
# Assignment3_Learning_Chinese.R
# RStudio：打开文件，把工作目录设为本文件所在目录，再点击 Source。
# 请保持所提供的目录结构。独立版使用旁边 data/ 与
# reference_original/；分类仓库直接复用
# ../02_Assignment/group/，避免重复文件。输出位于 runs/Chinese/。
# 需要 R 及推荐包 cluster；只有缺少时才用
# install.packages("cluster") 安装。
# 本脚本不会安装软件或访问网络。完整距离计算需要一定内存。
# 
# 先认识 R 语法
# # 开头是注释，R 不执行；<- 把右边结果存入左边对象。
# c() 组合数值，data.frame() 是矩形数据表，list() 可以容纳不同类型对象。
# d$Age 取名为 Age 的列；d[行,列] 选区域，留空表示全部。
# d[[v]] 取名称保存在 v 中的列；R 的位置从 1 开始计数。
# == 是比较，= 常用于指定参数；! 表示取反，& 表示同时满足。
# %in% 判断是否属于某个集合，~ 表示模型公式，^ 表示乘方，%*% 表示矩阵相乘。
# NA 是不可用，不是零；is.na() 检查它。NULL 表示对象或部分不存在。
# function() 定义可复用步骤，for() 重复操作，lapply() 对每项执行同一函数。
# 花括号把步骤包成整体；不要只运行半个循环或半个函数。
# 随机种子固定随机选择以便复现，不能保证结果就是真理。
# 
# 阅读顺序
# 0 路径与日志；1 工具；2 数据与探索（W3/W4/W6）；3 分群（W7）；
# 4 回归与验证（W8 加有用途的补充）；5 证据报告；6 独立核验。
# 如果还不认识标准分数、置信区间或 R²，可先阅读配套 Word 学习指南。
# 每段的来源行号可以对照未改动的原始代码；每行都归属一个教学板块。
# 一个板块可包含多个紧密相关的操作，请连同输入与输出一起理解。
# 
# 0. 找到脚本位置、保护源文件，并把新输出放进独立目录。路径相对脚本定位，不绑定某台电脑。sink
# 记录控制台输出，split=TRUE 同时显示。不要在半途打断后凭记忆乱序运行零散代码。
args <- commandArgs(trailingOnly = FALSE)
entry <- sub("^--file=", "", args[grepl("^--file=", args)])
LEARN_DIR <- if (length(entry)) dirname(normalizePath(entry[1])) else normalizePath(getwd())
source_candidates <- c(file.path(LEARN_DIR, "data", "Group_presentation.csv"),
  file.path(LEARN_DIR, "..", "02_Assignment", "group", "Group_presentation.csv"))
if (!any(file.exists(source_candidates))) stop("Cannot find the source CSV; keep the supplied folder structure.")
SOURCE <- normalizePath(source_candidates[file.exists(source_candidates)][1], mustWork = TRUE)
reference_candidates <- c(file.path(LEARN_DIR, "reference_original"),
  file.path(LEARN_DIR, "..", "02_Assignment", "group", "analysis"))
REFERENCE_DIR <- reference_candidates[dir.exists(reference_candidates)][1]
if (is.na(REFERENCE_DIR)) stop("Cannot find the original reference scripts.")
ROOT <- file.path(LEARN_DIR, "runs", "Chinese")
options(stringsAsFactors = FALSE, scipen = 5, warn = 1)
for (p in c("tables", "plots", "models", "logs", "data", "R"))
  dir.create(file.path(ROOT, p), recursive = TRUE, showWarnings = FALSE)
if (!requireNamespace("cluster", quietly = TRUE))
  stop("The recommended R package 'cluster' is required. Install it before running.")
log_con <- file(file.path(ROOT, "logs", "analysis_log.txt"), "wt")
sink(log_con, split = TRUE)
cat("Assignment 3 learning edition\nStarted:", format(Sys.time()), "\n")

# BEGIN ORIGINAL MODULE: 00_helpers.R

# ====================================================================
# 1.01 可复用工具
# 来源：00_helpers.R:1
# 函数就是有名字的操作步骤；定义函数不等于已经分析数据。括号里的参数是输入，返回值可以交给下一步使用。本节把重复操作写成工具，后面遇到调用时可以回来对照。

# ====================================================================
# 1.02 保存结果表
# 来源：00_helpers.R:2
# tab() 把数据表保存为 ROOT/tables/name.csv。paste0
# 无空格拼接文字，file.path 组合路径；row.names=FALSE
# 避免额外行号列。na="" 把无法计算的值存为空白，空白绝不能读成 0。invisible(x)
# 返回对象但不自动打印。
tab <- function(x, name, row.names = FALSE) {
  write.csv(x, file.path(ROOT, "tables", paste0(name, ".csv")), row.names = row.names,
            na = "", fileEncoding = "UTF-8")
  invisible(x)
}

# ====================================================================
# 1.03 绘图与关闭设备
# 来源：00_helpers.R:7
# png() 打开图片输出设备，之后的绘图命令会画进文件。par()
# 只设置外观，不改变数据；force(expr)
# 执行调用时传入的绘图代码。on.exit(dev.off())
# 保证函数退出时关闭设备，防止图片没有写完整。
plot_file <- function(name, expr, width = 1500, height = 1000) {
  png(file.path(ROOT, "plots", paste0(name, ".png")), width = width, height = height,
      res = 150, type = if (Sys.info()[["sysname"]] == "Darwin") "quartz" else "cairo")
  on.exit(dev.off())
  par(mar = c(4.2, 4.5, 3.2, 1.5), las = 1, col.axis = "#334155",
      col.lab = "#172B4D", col.main = "#172B4D", family = "sans")
  force(expr)
}

# ====================================================================
# 1.04 格式与分位数
# 来源：00_helpers.R:15
# fmt() 只改变展示小数位，不修改真实估计。ptext() 把极小 p 值写为
# <0.001，而不是误导性的 0。q(x,.99) 是第 99 百分位：大约 99%
# 的观察值不高于它，需考虑并列值和插值；它不是 99% 置信区间。
fmt <- function(x, digits = 2) formatC(x, digits = digits, format = "f", big.mark = ",")
ptext <- function(p) ifelse(is.na(p), "NA", ifelse(p < .001, "<0.001", sprintf("%.4f", p)))
q <- function(x, p) as.numeric(quantile(x, p, na.rm = TRUE, names = FALSE))

# ====================================================================
# 1.05 偏度与单变量概括
# 来源：00_helpers.R:18
# skew() 用标准化三阶中心矩衡量偏斜，长右尾通常让它为正。summary_numeric()
# 同时返回数量、缺失、位置、离散程度和极端值指标。标准差有原变量单位，偏度描述形状。对应
# W3：不能只报一个均值。
skew <- function(x) { x <- x[is.finite(x)]; mean((x - mean(x))^3) / mean((x - mean(x))^2)^1.5 }
summary_numeric <- function(x) c(n = length(x), missing = sum(is.na(x)), mean = mean(x),
  sd = sd(x), min = min(x), p01 = q(x, .01), q25 = q(x, .25), median = median(x),
  q75 = q(x, .75), p99 = q(x, .99), max = max(x), skewness = skew(x),
  zeros = sum(x == 0), negative = sum(x < 0))

# ====================================================================
# 1.06 为每组建立画像
# 来源：00_helpers.R:23
# lapply 对每个群执行同样步骤，do.call(rbind,...)
# 把结果上下合并。dat[行,列] 选子表，[[v]]
# 按变量名取列。均值和中位数使用原始数据。mean(Income==0) 能算比例，因为 TRUE
# 相当于 1、FALSE 相当于 0。
profile <- function(dat, grp, vars) do.call(rbind, lapply(sort(unique(grp)), function(g) {
  part <- dat[grp == g, , drop = FALSE]
  out <- data.frame(group = as.character(g), n = nrow(part), share = nrow(part) / nrow(dat))
  for (v in vars) {
    out[[paste0(v, "_mean")]] <- mean(part[[v]])
    out[[paste0(v, "_median")]] <- median(part[[v]])
    out[[paste0(v, "_sd")]] <- sd(part[[v]])
  }
  out$zero_income_rate <- mean(part$Income == 0)
  out$negative_yield_rate <- mean(part$Yield < 0)
  out
}))

# ====================================================================
# 1.07 比较两次分群
# 来源：00_helpers.R:35
# ARI 比较两次分组对记录配对的判断，并扣除随机一致部分。choose2(x)=x(x-1)/2 是
# x 条记录能组成的配对数。交叉表不要求两次群号相同；这次的 1 群可能对应另一次的 4
# 群。ARI=1 表示完全一致，但一致不代表业务上必然正确。
adjusted_rand <- function(a, b) {
  ct <- table(a, b); choose2 <- function(x) x * (x - 1) / 2
  n2 <- choose2(sum(ct)); obs <- sum(choose2(ct))
  ea <- sum(choose2(rowSums(ct))); eb <- sum(choose2(colSums(ct)))
  expected <- ea * eb / n2; den <- (ea + eb) / 2 - expected
  if (den == 0) return(1)
  (obs - expected) / den
}

# ====================================================================
# 1.08 检查算法是否停稳
# 来源：00_helpers.R:43
# Lloyd 实现可能没有 ifault
# 字段，因此不存在该字段不算出错。同时要求迭代次数有限且小于上限。检查针对计算是否正常结束，不能证明找到了唯一解或全局最优解。
kmeans_converged <- function(fit, iter_max = 200L) {
  (is.null(fit$ifault) || identical(as.integer(fit$ifault), 0L)) &&
    length(fit$iter) == 1L && is.finite(fit$iter) && fit$iter < iter_max
}

# ====================================================================
# 1.09 计算预测误差
# 来源：00_helpers.R:48
# RMSE 是平方误差平均后开方，特别重视大错误；MAE
# 是绝对误差的平均值，单位仍为金额。R²=1−SSE/SST 以当前评价数据的结果均值作参照，测试
# R² 可能为负。mean_error 使用预测减实际，正值表示整体高估；它与后面残差的符号相反。
metrics <- function(y, pred, name) data.frame(model = name, n = length(y),
  RMSE = sqrt(mean((y - pred)^2)), MAE = mean(abs(y - pred)),
  R2 = 1 - sum((y - pred)^2) / sum((y - mean(y))^2), mean_error = mean(pred - y))

# ====================================================================
# 1.10 HC3 稳健协方差
# 来源：00_helpers.R:51
# X 是含截距的模型矩阵，crossprod(X) 表示 X 的转置乘
# X。三明治形式是逆矩阵、残差信息矩阵、逆矩阵相乘。残差除以
# 1−杠杆值，提高特殊设计位置的影响。HC3
# 调整异方差下的系数不确定性，不改变预测值，也不修复因果问题。
hc3 <- function(model) {
  X <- model.matrix(model); bread <- solve(crossprod(X))
  meat <- crossprod(X, X * (resid(model) / (1 - hatvalues(model)))^2)
  bread %*% meat %*% bread
}

# ====================================================================
# 1.11 系数与区间
# 来源：00_helpers.R:56
# coef() 取全部系数（含截距），diag() 取协方差矩阵对角线，开方得到标准误。95%
# 区间等于估计值加减 t 临界值乘 HC3 标准误。保留常规与稳健两套结果便于比较。小 p
# 值描述模型前提下的条件关联，不能解释为业务行动成功概率。
coef_table <- function(model) {
  b <- coef(model); se <- sqrt(diag(hc3(model))); crit <- qt(.975, df.residual(model))
  ss <- coef(summary(model))
  data.frame(term = names(b), estimate = b, classical_se = ss[, 2],
    classical_p = ss[, 4], HC3_se = se, HC3_t = b / se,
    HC3_p = 2 * pt(-abs(b / se), df.residual(model)),
    HC3_low95 = b - crit * se, HC3_high95 = b + crit * se, row.names = NULL)
}

# ====================================================================
# 1.12 方差膨胀因子
# 来源：00_helpers.R:64
# 把每个预测变量对其他预测变量回归，得到辅助
# R²；VIF=1/(1−R²)。信息重复越多，系数不确定性越大。drop=FALSE
# 保持矩阵结构，即使只剩一列。对应 W8 的共线性检查；VIF 低不能说明其他假设都成立。
vif_table <- function(model) {
  X <- model.matrix(model)[, -1, drop = FALSE]
  data.frame(term = colnames(X), VIF = vapply(seq_len(ncol(X)), function(j) {
    if (ncol(X) == 1) return(1)
    1 / (1 - summary(lm(X[, j] ~ X[, -j, drop = FALSE]))$r.squared)
  }, numeric(1)))
}

# ====================================================================
# 1.13 安全显示报告文字
# 来源：00_helpers.R:71
# HTML 把 <、>、& 当特殊语法，替换成实体后才能按普通文字展示。html_table()
# 将数据框转换为表格，并特别处理 p 值格式。本节只组织既有结果，不另做统计建模。
html_escape <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
html_table <- function(x, digits = 3) {
  if (nrow(x) == 0) return("<p>No rows</p>")
  text_cols <- !vapply(x, is.numeric, logical(1))
  x[] <- lapply(names(x), function(nm) {
    z <- x[[nm]]
    if (is.numeric(z) && grepl("(^p$|_p$|^p_holm$)", nm)) return(html_escape(ptext(z)))
    if (is.numeric(z)) format(round(z, digits), trim = TRUE, scientific = FALSE, big.mark = ",") else html_escape(z)
  })
  paste0('<div class="table-wrap"><table><thead><tr>',
    paste0(ifelse(text_cols, '<th class="text-column">', '<th>'), html_escape(names(x)), "</th>", collapse = ""),
    "</tr></thead><tbody>", paste(apply(x, 1, function(z)
      paste0("<tr>", paste0("<td>", z, "</td>", collapse = ""), "</tr>")), collapse = ""),
    "</tbody></table></div>")
}
# END ORIGINAL MODULE: 00_helpers.R

# BEGIN ORIGINAL MODULE: 01_data_eda.R

# ====================================================================
# 2.01 读取并检查数据
# 来源：01_data_eda.R:1
# SOURCE 指向 CSV，md5sum
# 生成指纹便于检查文件是否改变。check.names=FALSE 保留原列名。Cat_Age
# 虽然用数字编码，却是类别，因此不放进连续变量概括。stopifnot
# 在数据不符合预期时停止，不会静默修改原始数据。
SOURCE <- normalizePath(SOURCE)
source_md5 <- unname(tools::md5sum(SOURCE))
d <- read.csv(SOURCE, check.names = FALSE)
original_vars <- c("Tot_Prod", "Income", "No_Trans", "Cat_Age", "Age", "Yield", "CLV")
numeric_vars <- setdiff(original_vars, "Cat_Age")
stopifnot(identical(names(d), original_vars), nrow(d) == 10000,
          all(vapply(d, is.numeric, logical(1))), !anyNA(d), all(is.finite(as.matrix(d))),
          all(d$Age >= 18 & d$Age <= 99), all(d$Tot_Prod %in% 0:5),
          all(d$Cat_Age %in% 1:6), all(d$Income >= 0), all(d$No_Trans >= 0))

# ====================================================================
# 2.02 新建分析标记
# 来源：01_data_eda.R:11
# row_id 只是追踪用行号，不是真实客户 ID。Under30 和 IncomeZero
# 把逻辑判断变成 0/1 标记。cut() 明确划出年龄段，第一段为 18–29
# 岁。新增这些列不改变原始七列。
d$row_id <- seq_len(nrow(d)) # 只是行号；源数据没有客户 ID。
d$Under30 <- as.integer(d$Age < 30)
d$IncomeZero <- as.integer(d$Income == 0)
d$AgeBand <- cut(d$Age, c(17, 29, 35, 50, 64, 79, 100),
                 labels = c("18-29", "30-35", "36-50", "51-64", "65-79", "80-99"))

# ====================================================================
# 2.03 核对年龄类别含义
# 来源：01_data_eda.R:16
# aggregate() 汇总每个类别对应的数值年龄，嵌套 ifelse
# 重建编码规则，再逐行验证。Cat_Age=2 实际覆盖 18–35 岁，因此不能代替 30
# 岁以下。这里应用 W3 的测量水平：编码与数量不是一回事。
age_mapping <- aggregate(Age ~ Cat_Age, d, function(x) c(n = length(x), min = min(x), max = max(x)))
age_mapping <- data.frame(Cat_Age = age_mapping$Cat_Age, age_mapping$Age)
tab(age_mapping, "01_age_category_mapping")
expected_cat <- ifelse(d$Age <= 35, 2L, ifelse(d$Age <= 50, 3L,
                  ifelse(d$Age <= 64, 4L, ifelse(d$Age <= 79, 5L, 6L))))
stopifnot(all(d$Cat_Age == expected_cat))

# ====================================================================
# 2.04 审计记录与变量字典
# 来源：01_data_eda.R:22
# 审计表记录缺失、重复值组合、零收入和负利润。没有客户 ID
# 时，相同数值不能证明是同一个人。字典同时记录已知含义与未知信息，例如收入和交易次数的统计周期。不能自行把
# Income 称为年薪。
audit <- data.frame(check = c("source_rows", "source_columns", "explicit_missing_cells",
  "duplicate_value_rows_beyond_first", "zero_income_n", "zero_income_with_transactions_n",
  "negative_yield_n", "age_mapping_mismatches", "original_md5"),
  value = c(nrow(d), length(original_vars), sum(is.na(d)), sum(duplicated(d[original_vars])),
    sum(d$IncomeZero), sum(d$IncomeZero == 1 & d$No_Trans > 0), sum(d$Yield < 0),
    sum(d$Cat_Age != expected_cat), source_md5))
tab(audit, "01_data_audit"); print(audit)
dictionary <- data.frame(variable = original_vars,
  type = c("Discrete count", "Numeric; ambiguous zero", "Discrete count", "Ordinal category",
           "Numeric years", "Numeric profit; negatives valid", "Derived numeric estimate"),
  meaning = c("Total products owned, permitted 0-5", "Recorded income; period/unit not specified",
    "Transaction count; period not specified", "Observed categories 2-6; category 1 absent",
    "Age in years, 18-99", "Current profit in dollars, may be negative",
    "Expected lifetime value in dollars; unknown existing formula"))
tab(dictionary, "01_data_dictionary")

# ====================================================================
# 2.05 单变量统计与频数
# 来源：01_data_eda.R:37
# 先概括每个数值变量，再为产品数和年龄类别生成频数与比例。频数回答每个值有多少人，均值回答算术中心在哪里。遇到极端值拉高均值，要一起看中位数、标准差和范围。对应
# W3。
univariate <- data.frame(variable = numeric_vars,
  do.call(rbind, lapply(d[numeric_vars], summary_numeric)), row.names = NULL)
tab(univariate, "01_univariate_summary")
for (v in c("Tot_Prod", "Cat_Age")) {
  f <- as.data.frame(table(d[[v]])); names(f) <- c("value", "n")
  f$proportion <- f$n / nrow(d); tab(f, paste0("01_frequency_", v))
}

# ====================================================================
# 2.06 筛查异常观察值
# 来源：01_data_eda.R:44
# IQR=Q3−Q1 描述中间一半数据的跨度。超出 Q1−1.5×IQR 或 Q3+1.5×IQR
# 只是被标记，不自动等于错误。另存最高十条记录便于按行号核查。本段不删记录，也不覆盖金额。
outliers <- do.call(rbind, lapply(numeric_vars, function(v) {
  x <- d[[v]]; iqr <- IQR(x); lo <- q(x, .25) - 1.5 * iqr; hi <- q(x, .75) + 1.5 * iqr
  data.frame(variable = v, lower = lo, upper = hi, below = sum(x < lo), above = sum(x > hi),
             cap99 = q(x, .99), over_cap99 = sum(x > q(x, .99)))
}))
tab(outliers, "01_outlier_screening")
for (v in c("Income", "Yield", "CLV", "No_Trans"))
  tab(d[order(-d[[v]])[1:10], c("row_id", original_vars)], paste0("01_top10_", v))

# ====================================================================
# 2.07 比较描述画像
# 来源：01_data_eda.R:52
# 分别查看零收入与正收入、年龄段、30
# 岁以下及逐岁画像。画像先说明样本长什么样，再决定是否检验。零收入但有交易提示需要核实记录过程，不能直接证明该收入是缺失值。
zero_profile <- profile(d, d$IncomeZero, numeric_vars)
age_profile <- profile(d, d$AgeBand, numeric_vars)
young_profile <- profile(d, d$Under30, numeric_vars)
tab(zero_profile, "01_zero_income_profile"); tab(age_profile, "01_age_band_profile")
tab(young_profile, "01_under30_profile")
tab(profile(d, d$Age, numeric_vars), "01_exact_age_profile")


# ====================================================================
# 2.08 相关与多重检验
# 来源：01_data_eda.R:60
# combn 列出数值变量的两两组合。Pearson 用原始数值看线性关联，Spearman
# 用排序降低极端金额的影响。一次做许多检验会增加偶然显著机会，所以用 Holm 调整 p 值。去掉最大
# Yield 后重算只是敏感性检查，不替代主分析。对应 W6。
cor_tests <- do.call(rbind, lapply(combn(numeric_vars, 2, simplify = FALSE), function(v) {
  do.call(rbind, lapply(c("pearson", "spearman"), function(method) {
    test <- cor.test(d[[v[1]]], d[[v[2]]], method = method, exact = FALSE)
    data.frame(x = v[1], y = v[2], method = method, r = unname(test$estimate), p = test$p.value)
  }))
}))
cor_tests$p_holm <- p.adjust(cor_tests$p, "holm")
tab(cor_tests, "01_correlations_tests")
tab(cor(d[numeric_vars]), "01_pearson_matrix", TRUE)
tab(cor(d[numeric_vars], method = "spearman"), "01_spearman_matrix", TRUE)
tab(cor(d[-which.max(d$Yield), numeric_vars]), "01_without_max_yield_correlation", TRUE)


# ====================================================================
# 2.09 两组均值比较
# 来源：01_data_eda.R:73
# Welch t.test 比较 1 组减 0 组，并允许方差不同。对
# Under30，负差值表示年轻组记录结果较低。置信区间描述前提下总体均值差的不确定性，不是 95%
# 客户所在范围。再除以合并标准差得到无单位差异。对应 W4 的两组比较。
group_tests <- do.call(rbind, lapply(c("Under30", "IncomeZero"), function(g) {
  do.call(rbind, lapply(c("CLV", "Yield", "Income", "No_Trans", "Tot_Prod"), function(v) {
    a <- d[d[[g]] == 1, v]; b <- d[d[[g]] == 0, v]; tt <- t.test(a, b)
    pooled_sd <- sqrt(((length(a) - 1) * var(a) + (length(b) - 1) * var(b)) /
                       (length(a) + length(b) - 2))
    data.frame(grouping = g, outcome = v, n1 = length(a), n0 = length(b),
      mean1 = mean(a), mean0 = mean(b), difference_1_minus_0 = mean(a) - mean(b),
      low95 = tt$conf.int[1], high95 = tt$conf.int[2], t = unname(tt$statistic),
      df = unname(tt$parameter), p = tt$p.value, standardized_difference = (mean(a) - mean(b)) / pooled_sd)
  }))
}))
group_tests$p_holm <- p.adjust(group_tests$p, "holm")
tab(group_tests, "01_welch_two_group_tests")

# ====================================================================
# 2.10 卡方检验与效应量
# 来源：01_data_eda.R:86
# table()
# 建立年龄段与零收入状态的列联表。卡方比较观察人数与独立假设下的期望人数，需检查最小期望频数。Cramér
# V 描述关联大小，p 值则不是效应大小。prop.table(...,1)
# 按每行总数相除，得到年龄段内部比例。
chi <- chisq.test(table(d$AgeBand, d$IncomeZero))
chi_result <- data.frame(comparison = "AgeBand x IncomeZero", statistic = unname(chi$statistic),
  df = unname(chi$parameter), p = chi$p.value, minimum_expected = min(chi$expected),
  CramersV = sqrt(unname(chi$statistic) / nrow(d)))
tab(chi_result, "01_zero_income_chisquare")
tab(prop.table(table(d$AgeBand, d$IncomeZero), 1), "01_zero_income_by_age", TRUE)


# ====================================================================
# 2.11 多组均值比较
# 来源：01_data_eda.R:94
# 保留经典 ANOVA 对照 W6，同时以允许方差不同的 Welch ANOVA
# 为主。Brown–Forsythe
# 比较各记录到组内中位数的绝对距离，检查方差差异。eta_squared 是经典组间平方和占比，不是把
# Welch F 转换所得。后续逐对 Welch 与 Holm 用于比较年龄段，仍然只是观察性差异。
anova_results <- do.call(rbind, lapply(c("CLV", "Yield", "Income", "No_Trans"), function(v) {
  y <- d[[v]]; g <- d$AgeBand
  welch <- oneway.test(y ~ g, var.equal = FALSE)
  classic <- summary(aov(y ~ g))[[1]]
  deviations <- abs(y - ave(y, g, FUN = median))
  lev <- summary(aov(deviations ~ g))[[1]]
  ssb <- sum(tapply(y, g, length) * (tapply(y, g, mean) - mean(y))^2)
  data.frame(outcome = v, classic_F = classic[1, "F value"], classic_p = classic[1, "Pr(>F)"],
    Welch_F = unname(welch$statistic), Welch_df1 = welch$parameter[1], Welch_df2 = welch$parameter[2],
    Welch_p = welch$p.value, Brown_Forsythe_p = lev[1, "Pr(>F)"], eta_squared = ssb / sum((y - mean(y))^2))
}))
tab(anova_results, "01_age_band_anova")
posthoc <- pairwise.t.test(d$CLV, d$AgeBand, pool.sd = FALSE, p.adjust.method = "holm")
tab(posthoc$p.value, "01_CLV_age_posthoc_Welch_Holm", TRUE)


# ====================================================================
# 2.12 分布图
# 来源：01_data_eda.R:109
# mfrow=c(2,3) 排列六格，hist()
# 把数值分箱。全范围直方图与箱线图用来揭示长尾，而不是藏起尾部。中心箱体被挤得很小也可能说明极端值很大。图片供人检查，不会自动判定数据合格。
plot_file("01_distributions", {
  par(mfrow = c(2, 3))
  for (v in numeric_vars) hist(d[[v]], breaks = 45, main = v, xlab = v,
    col = "#4E7896", border = "white")
}, 1800, 1100)
plot_file("02_outliers", {
  par(mfrow = c(2, 2))
  for (v in c("Income", "No_Trans", "Yield", "CLV"))
    boxplot(d[[v]], main = paste(v, "- full range"), col = "#AAC7D8", ylab = v)
}, 1500, 1100)

# ====================================================================
# 2.13 年龄与 CLV 图
# 来源：01_data_eda.R:119
# 散点图纵轴显示到 CLV 第 99
# 百分位以便阅读，橙线仍使用每个年龄的全部记录。outline=FALSE
# 只隐藏箱线图离群点标记。30 岁参考线对应作业问题。这些显示设置不删除检验或模型中的记录。
plot_file("03_age_clv", {
  par(mfrow = c(1, 2))
  smoothScatter(d$Age, d$CLV, ylim = c(0, q(d$CLV, .99)), xlab = "Age", ylab = "CLV ($)",
    main = "Age and CLV (vertical view to 99th percentile)")
  abline(v = 30, lty = 2, col = "#A94F21")
  a <- aggregate(CLV ~ Age, d, mean); lines(a$Age, a$CLV, col = "#D85B36", lwd = 2)
  boxplot(CLV ~ AgeBand, d, outline = FALSE, col = "#AAC7D8", xlab = "Age band", ylab = "CLV ($)",
    main = "CLV by age band (outlier points hidden)")
}, 1700, 850)

# ====================================================================
# 2.14 关联与零收入图
# 来源：01_data_eda.R:128
# 散点图比较 CLV 与四个变量，透明点有助于看重叠。零收入柱状图把比例乘 100
# 显示为百分比，交易箱线图检查这些记录是否仍活跃。最后保存新增分析标记的数据，源 CSV 保持不变。
plot_file("04_clv_associations", {
  par(mfrow = c(2, 2))
  for (v in c("Income", "No_Trans", "Tot_Prod", "Yield"))
    plot(d[[v]], d$CLV, pch = 16, cex = .4, col = adjustcolor("#2E6F91", .2),
         xlab = v, ylab = "CLV ($)", main = paste("CLV vs", v))
}, 1600, 1100)
plot_file("05_zero_income", {
  par(mfrow = c(1, 2))
  z <- tapply(d$IncomeZero, d$AgeBand, mean)
  barplot(z * 100, col = "#4E7896", ylab = "Income recorded as zero (%)", xlab = "Age band", ylim = c(0, max(z) * 115))
  boxplot(No_Trans ~ IncomeZero, d, outline = FALSE, names = c("Positive income", "Zero income"),
    col = "#AAC7D8", ylab = "Transactions", main = "Activity despite zero recorded income")
}, 1500, 800)
write.csv(d, file.path(ROOT, "data", "analysis_data.csv"), row.names = FALSE)
cat("EDA complete. No source records deleted. Source MD5:", source_md5, "\n")
# END ORIGINAL MODULE: 01_data_eda.R

# BEGIN ORIGINAL MODULE: 02_clusters.R

# ====================================================================
# 3.01 选择主动变量
# 来源：02_clusters.R:1
# 产品数、收入、交易数和年龄定义相似性。Yield 与 CLV
# 是被动结果，不能先按盈利分组，再声称发现了盈利差异。年龄已用 Age 表达，不再重复加入
# Cat_Age。对应 W7 的变量选择。
cluster_vars <- c("Tot_Prod", "Income", "No_Trans", "Age")
stopifnot(!any(c("Yield", "CLV", "Cat_Age") %in% cluster_vars))

# ====================================================================
# 3.02 限制极端距离
# 来源：02_clusters.R:6
# 只复制聚类输入，为收入和交易算第 99 百分位，再用 pmin
# 把超限输入限制在阈值。它只限制距离计算，不删除客户。画像仍使用保留原值的
# d，后面还会检查这种选择是否影响分群。
cluster_input <- d[cluster_vars]
caps <- sapply(cluster_input[c("Income", "No_Trans")], q, p = .99)
for (v in names(caps)) cluster_input[[v]] <- pmin(cluster_input[[v]], caps[v])
tab(data.frame(variable = names(caps), upper_cap = caps,
               changed_rows = sapply(names(caps), function(v) sum(d[[v]] > caps[v]))), "02_cluster_input_caps")

# ====================================================================
# 3.03 标准化
# 来源：02_clusters.R:11
# scale() 对每列计算 z=(x−均值)/标准差。例如比均值高一个标准差都变为
# z=1，不管原来单位是岁还是金额。断言检查均值约为 0、标准差约为
# 1。标准化防止大单位主导欧氏距离，却不会把偏态数据变成正态。对应 W3、W7。
Z <- scale(cluster_input)
stopifnot(max(abs(colMeans(Z))) < 1e-10, max(abs(apply(Z, 2, sd) - 1)) < 1e-10)
tab(cor(cluster_input), "02_cluster_input_correlations", TRUE)

# ====================================================================
# 3.04 共用评价样本
# 来源：02_clusters.R:14
# set.seed 固定随机流便于复现。所有候选 k 都使用同一批 2,500 条记录算抽样
# silhouette。dist() 计算两两欧氏距离，list()
# 建立存放模型与结果的容器。共用样本避免每次换人干扰群数比较。
set.seed(7003)
sil_sample <- sort(sample(seq_len(nrow(d)), 2500))
sample_dist <- dist(Z[sil_sample, ])
fits <- list(); candidate_rows <- list()

# ====================================================================
# 3.05 拟合不同群数
# 来源：02_clusters.R:18
# 依次拟合 2 到 8 群，kmeans 在分配客户和更新中心之间迭代。nstart=50
# 尝试五十个随机起点并保留组内平方和最低者。silhouette 比较本群与最近其他群距离，接近 0
# 说明重叠。还保存群规模与原始画像，因为一个分数解释不了业务群体。
for (k in 2:8) {
  set.seed(7000 + k)
  fit <- kmeans(Z, centers = k, nstart = 50, iter.max = 200, algorithm = "Lloyd")
  stopifnot(kmeans_converged(fit))
  fits[[as.character(k)]] <- fit
  s <- cluster::silhouette(fit$cluster[sil_sample], sample_dist)
  candidate_rows[[as.character(k)]] <- data.frame(k = k, sampled_silhouette = mean(s[, 3]),
    negative_silhouette_share = mean(s[, 3] < 0), smallest_n = min(fit$size),
    largest_n = max(fit$size), within_ss = fit$tot.withinss, between_fraction = fit$betweenss / fit$totss)
  tab(profile(d, fit$cluster, numeric_vars), paste0("02_candidate_k", k, "_profiles"))
  cat("k=", k, " sampled silhouette=", round(mean(s[, 3]), 4), "\n", sep = "")
}

# ====================================================================
# 3.06 入围与全样本复核
# 来源：02_clusters.R:30
# 合并候选结果，以最小群占 2% 作为分析者设定的可操作规则。抽样得分最高的两个方案入围。10,000
# 条记录约有五千万个距离配对，这一步需要内存与时间。仅对入围方案算完整 silhouette，其他保留
# NA，表示未算，不是 0。
candidates <- do.call(rbind, candidate_rows); rownames(candidates) <- NULL
eligible <- candidates[candidates$smallest_n >= .02 * nrow(d), ]
if (nrow(eligible) == 0) eligible <- candidates
shortlist <- head(eligible$k[order(-eligible$sampled_silhouette)], 2)
full_dist <- dist(Z)
exact_sil <- list()
candidates$full_silhouette <- NA_real_
for (k in shortlist) {
  s <- cluster::silhouette(fits[[as.character(k)]]$cluster, full_dist)
  exact_sil[[as.character(k)]] <- s
  candidates$full_silhouette[candidates$k == k] <- mean(s[, 3])
}

# ====================================================================
# 3.07 说明六群选择的取舍
# 来源：02_clusters.R:43
# 两群约 0.3205、六群约 0.3163，只差 0.0042。PROFILE_REVIEW_K=6
# 明确记录看过画像后的人工判断，不是假装它得分最高。W7
# 提醒分数接近时重视简单方案，所以两群仍合理。六群保留年龄、产品与活跃度差异，代价是执行更复杂。
best <- max(candidates$full_silhouette, na.rm = TRUE)
near_best <- candidates$k[!is.na(candidates$full_silhouette) & candidates$full_silhouette >= best - .01]
PROFILE_REVIEW_K <- 6L
stopifnot(PROFILE_REVIEW_K %in% near_best)
selected_k <- PROFILE_REVIEW_K

# ====================================================================
# 3.08 保存候选画像对比
# 来源：02_clusters.R:52
# 对每个入围方案记录群均龄、产品数、交易数的跨度及盈利中位数和 CLV 均值，说明改变 k
# 后哪些差异被合并或分开。被动盈利结果参与了解释选择，因此后续群间显著性是探索性描述，不是独立确认。
profile_comparison <- do.call(rbind, lapply(shortlist, function(k) {
  p <- profile(d, fits[[as.character(k)]]$cluster, numeric_vars)
  data.frame(k = k, exact_silhouette = candidates$full_silhouette[candidates$k == k],
    smallest_n = min(p$n), largest_n = max(p$n),
    mean_age_range = diff(range(p$Age_mean)), mean_products_range = diff(range(p$Tot_Prod_mean)),
    mean_transactions_range = diff(range(p$No_Trans_mean)),
    median_yield_min = min(p$Yield_median), median_yield_max = max(p$Yield_median),
    mean_CLV_min = min(p$CLV_mean), mean_CLV_max = max(p$CLV_mean),
    review = if (k == 2) "Broad engagement split; merges age and single-product activity differences"
      else "Separates younger low-activity, older profitable, and single-product high-activity profiles")
}))
tab(profile_comparison, "02_shortlist_profile_comparison")

# ====================================================================
# 3.09 固定可复述的群号
# 来源：02_clusters.R:64
# k-means 群号没有天然顺序。按均龄、再按收入排序形成可复述的 C1–C6，match()
# 将旧号映射为新号。中心和成员标签一起重排，防止表与模型对不上。只是改名，没有改变谁与谁同群。
km <- fits[[as.character(selected_k)]]
raw_profile <- profile(d, km$cluster, numeric_vars)
order_old <- as.integer(raw_profile$group[order(raw_profile$Age_mean, raw_profile$Income_mean)])
cluster_id <- match(km$cluster, order_old)
d$Cluster <- factor(cluster_id, levels = seq_len(selected_k))
centers <- km$centers[order_old, , drop = FALSE]
rownames(centers) <- seq_len(selected_k)

# ====================================================================
# 3.10 原量纲业务画像
# 来源：02_clusters.R:72
# 在未截尾原始数据上算画像，再对每群 Yield 和 CLV
# 求总和，并除以全体总和得到贡献份额。平均利润与总利润是不同问题：规模大的中等盈利群，总贡献可能超过规模小的高盈利群。
cluster_profile <- profile(d, cluster_id, numeric_vars)
names(cluster_profile)[1] <- "Cluster"
cluster_profile$Yield_total <- as.numeric(tapply(d$Yield, d$Cluster, sum))
cluster_profile$CLV_total <- as.numeric(tapply(d$CLV, d$Cluster, sum))
cluster_profile$Yield_share <- cluster_profile$Yield_total / sum(d$Yield)
cluster_profile$CLV_share <- cluster_profile$CLV_total / sum(d$CLV)

# ====================================================================
# 3.11 命名与导出群体
# 来源：02_clusters.R:79
# 命名依据观察到的收入、年龄、产品数和活跃度阈值，不臆造职业或具体产品。vapply
# 要求每群返回一个文字名称；如名称重复就添加群号。导出数值画像、类别比例和逐行归属，方便追溯每个汇总值。
name_from_profile <- function(i) {
  p <- cluster_profile[i, ]
  if (p$Income_mean >= 3000) return("高记录收入·高活跃客户")
  if (p$Age_mean >= 60) return("年长·单产品为主客户")
  if (p$Tot_Prod_mean >= 1.8 && p$No_Trans_mean >= 330) return("多产品·高活跃客户")
  if (p$Tot_Prod_mean >= 1.8) return("多产品·中等活跃客户")
  if (p$No_Trans_mean >= 330) return("单产品·高活跃客户")
  "较年轻·单产品低活跃客户"
}
cluster_names <- vapply(seq_len(selected_k), name_from_profile, character(1))
if (anyDuplicated(cluster_names)) cluster_names <- paste0(cluster_names, "（", seq_len(selected_k), "）")
cluster_profile$name <- cluster_names
tab(candidates, "02_k_comparison"); tab(cluster_profile, "02_cluster_profiles")
tab(centers, "02_cluster_standardised_centers", TRUE)
tab(prop.table(table(d$Cluster, factor(d$Cat_Age, levels = 1:6)), 1), "02_cluster_age_category_proportions", TRUE)
tab(prop.table(table(d$Cluster, factor(d$Tot_Prod, levels = 0:5)), 1), "02_cluster_product_count_proportions", TRUE)
tab(data.frame(row_id = d$row_id, Cluster = cluster_id, name = cluster_names[cluster_id]), "02_cluster_membership")

# ====================================================================
# 3.12 记录质量并释放内存
# 来源：02_clusters.R:96
# 记录完整 silhouette 均值和负值比例，说明分群分离度与边界归属情况。负
# silhouette 不能证明数据错误。rm() 在用完后移除大距离对象，gc()
# 回收内存；它们不删除源文件或客户记录。
chosen_sil <- exact_sil[[as.character(selected_k)]]
sil_summary <- data.frame(k = selected_k, exact_mean = mean(chosen_sil[, 3]),
  negative_share = mean(chosen_sil[, 3] < 0), sample_n = length(sil_sample),
  selected_rule = "Profile-reviewed k=6 within 0.01 of best exact shortlist; all groups >=2%; preserve age/activity distinctions")
tab(sil_summary, "02_selected_quality")
rm(full_dist, exact_sil, sample_dist); invisible(gc())


# ====================================================================
# 3.13 随机起点稳定性
# 来源：02_clusters.R:104
# 用五个不同随机种子重拟合同一个六群方案，每次三十个起点。ARI 比较成员归属且不受群号交换影响。接近
# 1 说明固定处理下计算较稳定，还不能说明改变收入处理后也稳定。
seed_stability <- do.call(rbind, lapply(c(11, 23, 47, 101, 2026), function(seed) {
  set.seed(seed); other <- kmeans(Z, selected_k, nstart = 30, iter.max = 200, algorithm = "Lloyd")
  stopifnot(kmeans_converged(other))
  data.frame(seed = seed, adjusted_rand = adjusted_rand(cluster_id, other$cluster), within_ss = other$tot.withinss)
}))
tab(seed_stability, "02_seed_stability")


# ====================================================================
# 3.14 替代预处理方案
# 来源：02_clusters.R:113
# 构造不封顶、收入取对数、零值填补、排除收入四种方案。log1p(x)=log(1+x)
# 压缩大值且能处理零。填补使用同年龄类别正收入中位数，只是假设情景，不代表查到了真实收入。主方案仍保留记录零值。
alternatives <- list(raw_no_caps = d[cluster_vars], capped_log_income = cluster_input,
                     zeros_as_missing_imputed = cluster_input,
                     exclude_income = cluster_input[setdiff(cluster_vars, "Income")])
alternatives$capped_log_income$Income <- log1p(alternatives$capped_log_income$Income)
positive_age_median <- tapply(d$Income[d$Income > 0], d$Cat_Age[d$Income > 0], median)
imputed <- pmin(positive_age_median[as.character(d$Cat_Age[d$Income == 0])], caps["Income"])
alternatives$zeros_as_missing_imputed$Income[d$Income == 0] <- imputed

# ====================================================================
# 3.15 衡量处理敏感性
# 来源：02_clusters.R:120
# 每种方案在自己的变量空间内标准化后重拟合。ARI 量化成员变化，画像说明实际差异。不同变换空间的
# silhouette 不能直接证明哪个分群更真实。这一步是公开结果对选择的依赖。
sensitivity <- do.call(rbind, lapply(names(alternatives), function(nm) {
  z <- scale(alternatives[[nm]])
  set.seed(7000 + selected_k)
  other <- kmeans(z, selected_k, nstart = 50, iter.max = 200, algorithm = "Lloyd")
  stopifnot(kmeans_converged(other))
  s <- cluster::silhouette(other$cluster[sil_sample], dist(z[sil_sample, ]))
  tab(profile(d, other$cluster, numeric_vars), paste0("02_sensitivity_", nm, "_profiles"))
  data.frame(scenario = nm, adjusted_rand_vs_primary = adjusted_rand(cluster_id, other$cluster),
    sampled_silhouette_own_space = mean(s[, 3]), smallest_n = min(other$size))
}))
tab(sensitivity, "02_preprocessing_sensitivity")


# ====================================================================
# 3.16 比较被动盈利结果
# 来源：02_clusters.R:133
# Welch 比较各群 Yield 与 CLV
# 均值，不要求方差相等。大样本会让很小差异显著，因此要同时看效应量和中位数。逐对检验用 Holm
# 降低多重比较误报。这些是探索性画像比较，不是加入某群导致的因果收益。
cluster_tests <- do.call(rbind, lapply(c("Yield", "CLV"), function(v) {
  y <- d[[v]]; g <- d$Cluster; w <- oneway.test(y ~ g, var.equal = FALSE)
  deviations <- abs(y - ave(y, g, FUN = median))
  levene <- summary(aov(deviations ~ g))[[1]]
  ssb <- sum(tapply(y, g, length) * (tapply(y, g, mean) - mean(y))^2)
  data.frame(outcome = v, Welch_F = unname(w$statistic), df1 = w$parameter[1], df2 = w$parameter[2],
    p = w$p.value, eta_squared = ssb / sum((y - mean(y))^2), Brown_Forsythe_p = levene[1, "Pr(>F)"])
}))
tab(cluster_tests, "02_passive_outcome_comparisons")
for (v in c("Yield", "CLV")) tab(pairwise.t.test(d[[v]], d$Cluster,
  pool.sd = FALSE, p.adjust.method = "holm")$p.value, paste0("02_", v, "_posthoc_Welch_Holm"), TRUE)

# ====================================================================
# 3.17 保存完整拟合方案
# 来源：02_clusters.R:144
# saveRDS
# 保存模型、变量名、阈值、均值、标准差及群号映射。后续使用需要同一套尺度，不能对新客户重新
# scale() 后还当作原坐标。保存成员归属也便于独立核对中心和人数。
saveRDS(list(kmeans = km, selected_k = selected_k, centers_relabelled = centers,
  center = attr(Z, "scaled:center"), scale = attr(Z, "scaled:scale"), caps = caps,
  variables = cluster_vars, cluster = cluster_id, names = cluster_names, label_map = order_old),
  file.path(ROOT, "models", "clustering.rds"))

# ====================================================================
# 3.18 绘制选择依据与画像
# 来源：02_clusters.R:148
# 第一图同时展示共用样本分数、入围完整分数和辅助 elbow
# 图。标准化中心展示相对特征，人数柱展示规模。盈利均值与中位数并列，揭示尾部对均值的影响。几种图需合读，不能只靠一张图自动判定通过。
plot_file("06_select_k", {
  par(mfrow = c(1, 2))
  plot(candidates$k, candidates$sampled_silhouette, type = "b", pch = 19, col = "#2E6F91",
    xlab = "Number of clusters k", ylab = "Mean silhouette", main = "Same 2,500 customers for all k")
  points(candidates$k, candidates$full_silhouette, pch = 17, col = "#C45C26", cex = 1.4)
  abline(v = selected_k, lty = 2, col = "#C45C26")
  legend("topright", c("Sampled", "Exact shortlisted"), pch = c(19, 17), col = c("#2E6F91", "#C45C26"), bty = "n")
  plot(candidates$k, candidates$within_ss, type = "b", pch = 19, col = "#2E6F91",
    xlab = "Number of clusters k", ylab = "Within-cluster sum of squares", main = "Elbow as supporting information")
}, 1600, 800)
plot_file("07_cluster_profiles", {
  par(mfrow = c(1, 2))
  barplot(t(centers), beside = TRUE, names.arg = paste("C", seq_len(selected_k)),
    col = c("#2E6F91", "#D7A64A", "#61A590", "#A78CB9"), ylab = "Standardised centroid",
    main = "Active variables define the segments")
  abline(h = 0, col = "gray60")
  legend("topright", cluster_vars, fill = c("#2E6F91", "#D7A64A", "#61A590", "#A78CB9"), cex = .8, bty = "n")
  barplot(cluster_profile$n, names.arg = paste("C", seq_len(selected_k)),
    col = "#4E7896", ylab = "Customers", main = "Cluster sizes")
}, 1700, 850)
plot_file("08_cluster_profitability", {
  par(mfrow = c(1, 2))
  barplot(rbind(cluster_profile$Yield_mean, cluster_profile$Yield_median), beside = TRUE,
    names.arg = paste("C", seq_len(selected_k)), col = c("#2E6F91", "#61A590"),
    ylab = "Yield ($)", main = "Current profit: mean and median")
  abline(h = 0, col = "gray50"); legend("topleft", c("Mean", "Median"), fill = c("#2E6F91", "#61A590"), bty = "n")
  barplot(rbind(cluster_profile$CLV_mean, cluster_profile$CLV_median), beside = TRUE,
    names.arg = paste("C", seq_len(selected_k)), col = c("#2E6F91", "#61A590"),
    ylab = "Recorded CLV ($)", main = "Existing lifetime value estimates")
}, 1700, 850)
cat("Selected k:", selected_k, "exact silhouette:", mean(chosen_sil[, 3]), "\n")
print(cluster_profile[, c("Cluster", "name", "n", "Age_mean", "Tot_Prod_mean", "Income_mean", "No_Trans_mean", "Yield_mean", "CLV_mean")])
# END ORIGINAL MODULE: 02_clusters.R

# BEGIN ORIGINAL MODULE: 03_models.R

# ====================================================================
# 4.01 准备年龄的不同形式
# 来源：03_models.R:1
# Age_c=Age−45 让截距对应 45 岁而不是 0 岁；这不是除以标准差的标准化。Age_c2
# 允许弯曲关系；pmax(Age−30,0) 建立 30
# 岁前为零、之后递增的连续折线项。三种形状用于检查年龄问题，而不是预设关系必为直线。
d$Age_c <- d$Age - 45 # 固定参照值，不从测试集估计。
d$Age_c2 <- d$Age_c^2
d$Age_after30 <- pmax(d$Age - 30, 0)


# ====================================================================
# 4.02 将相同输入组合一起切分
# 来源：03_models.R:11
# paste()
# 将产品数、交易数、年龄拼成键。相同键整体进入训练或测试，降低相同输入组合跨边界的信息泄漏。按是否
# 30 岁以下分层抽键，保证两边包含该群。20% 的键不一定等于 20% 的行；没有 ID
# 仍不能证明客户层面独立。
key <- paste(d$Tot_Prod, d$No_Trans, d$Age, sep = "|")
keys <- unique(key)
key_age <- d$Under30[match(keys, key)]
set.seed(37008)
test_keys <- unlist(lapply(split(keys, key_age), function(x) sample(x, floor(.2 * length(x)))), use.names = FALSE)
is_test <- key %in% test_keys
train <- d[!is_test, ]; test <- d[is_test, ]
stopifnot(!length(intersect(key[!is_test], key[is_test])))

# ====================================================================
# 4.03 五个训练折
# 来源：03_models.R:21
# 把每个训练键分到五折之一，同键的所有行跟随该折。交叉验证轮流留出一折，独立测试集不参与拟合和选择。保存逐行切分便于审计。这是对
# W8 基础拟合的补充，用于检查泛化。
train_keys <- unique(key[!is_test])
set.seed(37009)
fold_map <- sample(rep(1:5, length.out = length(train_keys)))
names(fold_map) <- train_keys
fold <- unname(fold_map[key[!is_test]])
split_table <- data.frame(row_id = d$row_id, partition = ifelse(is_test, "test", "train"),
  cv_fold = ifelse(is_test, NA_integer_, fold_map[key]), predictor_group = key)
tab(split_table, "03_partition_assignment")
tab(profile(d, ifelse(is_test, "test", "train"), numeric_vars), "03_partition_profiles")

# ====================================================================
# 4.04 候选公式与交叉验证
# 来源：03_models.R:30
# R 中 outcome ~ predictors 表示模型，+ 添加项，lm
# 默认包含截距。每个候选用四折拟合、预测第五折，轮流五次。汇总时先按折人数加权平方误差，再开平方；直接平均五个
# RMSE 不完全相同。
formulas <- list(
  linear = CLV ~ Tot_Prod + No_Trans + Age_c,
  quadratic_age = CLV ~ Tot_Prod + No_Trans + Age_c + Age_c2,
  age_hinge30 = CLV ~ Tot_Prod + No_Trans + Age_c + Age_after30)
cv_rows <- list()
for (name in names(formulas)) for (f in 1:5) {
  fit <- lm(formulas[[name]], train[fold != f, ])
  m <- metrics(train$CLV[fold == f], predict(fit, train[fold == f, ]), name)
  m$fold <- f; cv_rows[[length(cv_rows) + 1]] <- m
}
cv <- do.call(rbind, cv_rows)
cv_summary <- do.call(rbind, lapply(names(formulas), function(name) {
  x <- cv[cv$model == name, ]
  data.frame(model = name, pooled_RMSE = sqrt(weighted.mean(x$RMSE^2, x$n)),
    weighted_MAE = weighted.mean(x$MAE, x$n), fold_RMSE_sd = sd(x$RMSE),
    parameters = length(coef(lm(formulas[[name]], train))))
}))

# ====================================================================
# 4.05 不用测试集调参
# 来源：03_models.R:48
# 在最佳交叉验证 RMSE 的 1%
# 以内优先参数最少的模型。这是明确的实用规则，避免为微小改善增加复杂项。随后在全部训练行拟合所选公式，并检查主预测变量未混入
# Income、Yield、Cat_Age 或 Cluster。
best_cv <- min(cv_summary$pooled_RMSE)
acceptable <- cv_summary[cv_summary$pooled_RMSE <= best_cv * 1.01, ]
selected_name <- acceptable$model[order(acceptable$parameters, acceptable$pooled_RMSE)][1]
primary_formula <- formulas[[selected_name]]
primary <- lm(primary_formula, train)
stopifnot(!any(c("Income", "Yield", "Cat_Age", "Cluster") %in% all.vars(primary_formula)[-1]))

# ====================================================================
# 4.06 评价主模型
# 来源：03_models.R:54
# 提取 HC3 系数区间、VIF
# 和训练摘要，再预测留出记录。可部署基准对所有人预测训练均值，不偷看测试结果；但 R²
# 定义中的分母使用当前评价集均值。把基准与模型误差一起保存，优于基准并不代表没有误差。
primary_coef <- coef_table(primary)
primary_vif <- vif_table(primary)
primary_summary <- summary(primary)
test_pred <- predict(primary, test)
baseline_test <- metrics(test$CLV, rep(mean(train$CLV), nrow(test)), "Training_mean_baseline")
test_metrics <- rbind(baseline_test, metrics(test$CLV, test_pred, paste0("Primary_", selected_name)))
tab(cv, "03_CV_fold_metrics"); tab(cv_summary, "03_CV_model_comparison")
tab(test_metrics, "03_primary_test_metrics"); tab(primary_coef, "03_primary_coefficients_HC3")
tab(primary_vif, "03_primary_VIF")

# ====================================================================
# 4.07 训练拟合与测试的区别
# 来源：03_models.R:63
# 训练 R² 描述样本内变异解释程度，调整 R² 对增加参数作惩罚。总体 F
# 检验在前提下比较当前线性模型与只有截距的模型。残差标准误有结果变量单位。训练指标不能替代测试
# MAE、RMSE、R²；对应 W8 的摘要解读。
fit_stats <- data.frame(model = selected_name, train_n = nrow(train), test_n = nrow(test),
  R2_train = primary_summary$r.squared, adjusted_R2 = primary_summary$adj.r.squared,
  residual_SE = primary_summary$sigma, F = unname(primary_summary$fstatistic[1]),
  F_df1 = unname(primary_summary$fstatistic[2]), F_df2 = unname(primary_summary$fstatistic[3]),
  F_p = pf(primary_summary$fstatistic[1], primary_summary$fstatistic[2],
           primary_summary$fstatistic[3], lower.tail = FALSE))
tab(fit_stats, "03_primary_training_fit")
capture.output(summary(primary), file = file.path(ROOT, "models", "primary_summary.txt"))


# ====================================================================
# 4.08 检查衍生目标的来源
# 来源：03_models.R:72
# CLV 是旧估计值，原公式未知。仅把加入 Income 和 Yield
# 的方案作为诊断，观察拟合是否可能在重构公式。这里分数更高不能验证真实未来利润，也不应按测试结果挑最漂亮者当主模型。
formula_reconstruction <- lm(CLV ~ Income + Yield + No_Trans + Tot_Prod + Age_c + Under30, train)
income_added <- lm(update(primary_formula, . ~ . + Income), train)
income_zero_model <- lm(update(primary_formula, . ~ . + Income + IncomeZero), train)

# ====================================================================
# 4.09 仅用训练信息填补
# 来源：03_models.R:77
# 复制训练与测试数据，仅用训练集中各年龄类别的正收入算中位数，再把同一套数值应用到两份副本。get/assign
# 按文字名称读取、写回对象。保留 IncomeZero
# 记录原值为零的事实。这是假设性敏感性方案，不覆盖原始收入。
zero_imputed_train <- train; zero_imputed_test <- test
age_medians <- tapply(train$Income[train$Income > 0], train$Cat_Age[train$Income > 0], median)
for (nm in c("zero_imputed_train", "zero_imputed_test")) {
  part <- get(nm); z <- part$Income == 0
  part$Income[z] <- age_medians[as.character(part$Cat_Age[z])]
  assign(nm, part)
}
income_imputed <- lm(update(primary_formula, . ~ . + Income + IncomeZero), zero_imputed_train)

# ====================================================================
# 4.10 比较目标来源诊断
# 来源：03_models.R:85
# rbind 把主模型与诊断模型在相同留出结果上的误差上下合并。保存表格让比较透明。加入 Income
# 与 Yield 后约 0.96 的 R²
# 与衍生标签相符，但既不能证明找回旧公式，也不能证明预测真实未来现金流。
clv_diagnostics <- rbind(
  metrics(test$CLV, test_pred, "Primary_excludes_Income_Yield"),
  metrics(test$CLV, predict(income_added, test), "Diagnostic_add_Income"),
  metrics(test$CLV, predict(income_zero_model, test), "Diagnostic_Income_plus_zero_flag"),
  metrics(test$CLV, predict(income_imputed, zero_imputed_test), "Diagnostic_train_age_median_imputation"),
  metrics(test$CLV, predict(formula_reconstruction, test), "Diagnostic_include_Income_and_Yield"))
tab(clv_diagnostics, "03_CLV_target_provenance_sensitivity")
tab(coef_table(formula_reconstruction), "03_formula_reconstruction_coefficients")


# ====================================================================
# 4.11 30 岁以下的条件关联
# 来源：03_models.R:94
# 在二次年龄形状和核心预测变量旁加入 Under30 指标，再比较加不加 Income、Yield
# 时的系数。该系数是给定其他项后的额外模型差异，不是原始均值差。缺少真实终身结果，不能把它当低估金额。
young_adjusted <- lm(CLV ~ Tot_Prod + No_Trans + Age_c + Age_c2 + Under30, train)
young_adjusted_income <- lm(CLV ~ Tot_Prod + No_Trans + Age_c + Age_c2 + Under30 + Income + Yield, train)
young_compare <- rbind(
  cbind(specification = "Primary predictors plus quadratic age", coef_table(young_adjusted)[coef_table(young_adjusted)$term == "Under30", ]),
  cbind(specification = "Also adjust Income and Yield (derived-target diagnostic)",
        coef_table(young_adjusted_income)[coef_table(young_adjusted_income)$term == "Under30", ]))
tab(young_compare, "03_under30_adjusted_association")


# ====================================================================
# 4.12 残差与影响诊断
# 来源：03_models.R:105
# 残差=实际−拟合。将残差平方对拟合值回归，得到 BP 风格方差诊断
# n×R²，这里与一自由度卡方分布比较。Cook 距离衡量对拟合的影响，杠杆值描述特殊输入位置。4/n
# 和 2p/n 只是筛查经验线，不是强制删除规则。
res <- resid(primary); fitted_y <- fitted(primary)
bp_aux <- lm(I(res^2) ~ fitted_y)
bp_lm <- length(res) * summary(bp_aux)$r.squared
influence <- data.frame(row_id = train$row_id, observed = train$CLV, fitted = fitted_y,
  residual = res, standardized_residual = rstandard(primary), leverage = hatvalues(primary),
  cooks_distance = cooks.distance(primary))
influence$flag_cook <- influence$cooks_distance > 4 / nrow(train)
influence$flag_leverage <- influence$leverage > 2 * length(coef(primary)) / nrow(train)
diagnostics <- data.frame(diagnostic = c("BP_style_nR2_fitted_variance", "BP_style_p", "Residual_skewness",
  "Cook_above_4_over_n", "Leverage_above_2p_over_n", "Abs_standardized_residual_above3", "Max_VIF"),
  value = c(bp_lm, pchisq(bp_lm, 1, lower.tail = FALSE), skew(res), sum(influence$flag_cook),
    sum(influence$flag_leverage), sum(abs(influence$standardized_residual) > 3), max(primary_vif$VIF)))
tab(diagnostics, "03_primary_diagnostics")
tab(influence[order(-influence$cooks_distance), ], "03_influence_records")


# ====================================================================
# 4.13 极端训练记录的敏感性
# 来源：03_models.R:122
# 阈值只从训练行估计，并对测试输入应用相同交易阈值。替代模型可封顶训练 CLV，但评价始终使用原始测试
# CLV。另一个方案移除训练中的 Cook
# 标记行。公开系数与误差变化，不能看完测试改善就偷偷换主分析。
cap_train <- train
train_limits <- c(No_Trans = q(train$No_Trans, .99), CLV = q(train$CLV, .99))
cap_train$No_Trans <- pmin(cap_train$No_Trans, train_limits["No_Trans"])
cap_train$CLV <- pmin(cap_train$CLV, train_limits["CLV"])
cap_test <- test; cap_test$No_Trans <- pmin(test$No_Trans, train_limits["No_Trans"])
cap_fit <- lm(primary_formula, cap_train)
drop_cook_fit <- lm(primary_formula, train[!influence$flag_cook, ])
sensitivity_metrics <- rbind(metrics(test$CLV, test_pred, "Primary_all_training_rows"),
  metrics(test$CLV, predict(cap_fit, cap_test), "Sensitivity_training_p99_caps"),
  metrics(test$CLV, predict(drop_cook_fit, test), "Sensitivity_omit_training_Cook_flags"))
sensitivity_coefs <- rbind(cbind(scenario = "Primary", coef_table(primary)),
  cbind(scenario = "Training_p99_caps", coef_table(cap_fit)),
  cbind(scenario = "Omit_training_Cook_flags", coef_table(drop_cook_fit)))
tab(sensitivity_metrics, "03_outlier_sensitivity_metrics")
tab(sensitivity_coefs, "03_outlier_sensitivity_coefficients")
tab(data.frame(variable = names(train_limits), train_cap = train_limits), "03_training_only_caps")


# ====================================================================
# 4.14 比较预测变量贡献
# 来源：03_models.R:141
# 每次移除一个变量块，重拟合简化模型并比较残差平方和。partial R²
# 以简化模型误差为分母，incremental R² 以结果总变异为分母。分母不同，各块
# partial R² 通常不能相加得到总 R²。这样避免直接比较单位不同的系数，但仍不是因果重要性。
blocks <- list(products = "Tot_Prod", transactions = "No_Trans", age = intersect(
  c("Age_c", "Age_c2", "Age_after30"), attr(terms(primary), "term.labels")))
importance <- do.call(rbind, lapply(names(blocks), function(nm) {
  remain <- setdiff(attr(terms(primary), "term.labels"), blocks[[nm]])
  reduced <- lm(reformulate(remain, response = "CLV"), train)
  data.frame(block = nm, partial_R2_train = (deviance(reduced) - deviance(primary)) / deviance(reduced),
    incremental_R2_train = (deviance(reduced) - deviance(primary)) / sum((train$CLV - mean(train$CLV))^2))
}))
importance <- importance[order(-importance$partial_R2_train), ]; tab(importance, "03_predictor_contributions")


# ====================================================================
# 4.15 补充当前利润模型
# 来源：03_models.R:153
# Yield
# 是当前利润，与记录终身价值不同。用训练折比较线性与二次年龄形式，再在同一留出样本评价所选形式。排除
# CLV，因为其未知构造可能已包含 Yield。测试 R² 约 0.06
# 表示个体利润预测较弱，即使某些系数显著也不能忽略。
yield_formulas <- list(linear = Yield ~ Tot_Prod + No_Trans + Age_c + Income + IncomeZero,
  quadratic_age = Yield ~ Tot_Prod + No_Trans + Age_c + Age_c2 + Income + IncomeZero)
yield_cv <- do.call(rbind, lapply(names(yield_formulas), function(nm) {
  pp <- rep(NA_real_, nrow(train))
  for (f in 1:5) pp[fold == f] <- predict(lm(yield_formulas[[nm]], train[fold != f, ]), train[fold == f, ])
  metrics(train$Yield, pp, nm)
}))
yield_name <- if (yield_cv$RMSE[yield_cv$model == "linear"] <= min(yield_cv$RMSE) * 1.01)
  "linear" else yield_cv$model[which.min(yield_cv$RMSE)]
yield_fit <- lm(yield_formulas[[yield_name]], train)
yield_pred <- predict(yield_fit, test)
yield_metrics <- rbind(metrics(test$Yield, rep(mean(train$Yield), nrow(test)), "Yield_training_mean_baseline"),
  metrics(test$Yield, yield_pred, paste0("Yield_", yield_name)))
tab(yield_cv, "03_Yield_CV"); tab(yield_metrics, "03_Yield_test_metrics")
tab(coef_table(yield_fit), "03_Yield_coefficients_HC3")
tab(vif_table(yield_fit), "03_Yield_VIF")
capture.output(summary(yield_fit), file = file.path(ROOT, "models", "yield_summary.txt"))


# ====================================================================
# 4.16 检查分组误差
# 来源：03_models.R:173
# 把实际值、预测值与行号、群号、年轻标记一起保存，再分组算误差以查看表现是否均匀。群体由全体数据形成，这里只作描述性误差检查，不作为回归输入或调参目标。小群的误差估计更不稳定。
predictions <- data.frame(row_id = test$row_id, Cluster = test$Cluster, Under30 = test$Under30,
  CLV = test$CLV, CLV_pred = test_pred, Yield = test$Yield, Yield_pred = yield_pred)
tab(predictions, "03_test_predictions")
subgroup_metrics <- do.call(rbind, lapply(split(seq_len(nrow(test)), test$Cluster), function(ii)
  metrics(test$CLV[ii], test_pred[ii], paste0("Cluster_", as.character(test$Cluster[ii[1]])))))
tab(subgroup_metrics, "03_CLV_test_error_by_cluster")
tab(do.call(rbind, lapply(split(seq_len(nrow(test)), test$Under30), function(ii)
  metrics(test$CLV[ii], test_pred[ii], paste0("Under30_", test$Under30[ii[1]])))), "03_CLV_test_error_by_under30")


# ====================================================================
# 4.17 区分训练模型与全数据模型
# 来源：03_models.R:183
# 评价完成后，在全部记录上重拟合所选公式并保存。其系数可与训练拟合不同，所以不能把全数据拟合值与早前测试表现混用。RDS
# 同时保存两种模型、切分与阈值；报告的评价仍来自原训练模型的留出结果。
final_model <- lm(primary_formula, d)
final_coef <- coef_table(final_model)
tab(final_coef, "03_final_all_data_coefficients_HC3")
saveRDS(list(training_model = primary, final_model = final_model, formula = primary_formula,
  selected_name = selected_name, test_row_ids = test$row_id, train_row_ids = train$row_id,
  cv_fold = fold, target_provenance_diagnostic = formula_reconstruction,
  yield_training_model = yield_fit, train_limits = train_limits), file.path(ROOT, "models", "regression.rds"))
write.csv(d, file.path(ROOT, "data", "analysis_with_clusters.csv"), row.names = FALSE)


# ====================================================================
# 4.18 阅读诊断图
# 来源：03_models.R:194
# plot.lm 的 1、2、3、5 图分别检查残差规律、正态
# Q–Q、波动与杠杆。预测对实际图同时保留全范围和放大图，不能藏起极端失败。abline(0,1)
# 是截距 0、斜率 1 的对角线。图形放大不会改变测试指标。
plot_file("09_regression_diagnostics", {
  par(mfrow = c(2, 2)); plot(primary, which = c(1, 2, 3, 5), id.n = 3, ask = FALSE)
}, 1600, 1200)
plot_file("10_test_predictions", {
  par(mfrow = c(1, 2))
  plot(test$CLV, test_pred, pch = 16, col = adjustcolor("#2E6F91", .28), xlab = "Observed CLV ($)",
    ylab = "Predicted CLV ($)", main = "Held-out customers: complete range"); abline(0, 1, lty = 2, col = "#C45C26")
  zoom <- q(test$CLV, .99)
  plot(test$CLV, test_pred, xlim = c(0, zoom), ylim = c(0, zoom), pch = 16,
    col = adjustcolor("#2E6F91", .25), xlab = "Observed CLV ($)", ylab = "Predicted CLV ($)",
    main = "Zoom to 99th percentile (all metrics use all rows)"); abline(0, 1, lty = 2, col = "#C45C26")
}, 1700, 850)

# ====================================================================
# 4.19 固定其他输入看年龄关联
# 来源：03_models.R:206
# 构造一个产品、训练交易中位数、年龄 18–99 的例子。X 乘 beta 得到拟合均值，X V X
# 转置给出不确定性。95% HC3 阴影描述均值，不是单个客户预测区间。最后输出 Yield
# 诊断和控制台摘要，完成模型审查。
plot_file("11_adjusted_age_pattern", {
  par(mar = c(6, 4.1, 4.1, 2.1))
  nd <- data.frame(Tot_Prod = 1, No_Trans = median(train$No_Trans), Age = 18:99)
  nd$Age_c <- nd$Age - 45; nd$Age_c2 <- nd$Age_c^2; nd$Age_after30 <- pmax(nd$Age - 30, 0)
  X <- model.matrix(delete.response(terms(primary)), nd)
  mu <- as.vector(X %*% coef(primary)); se <- sqrt(rowSums((X %*% hc3(primary)) * X))
  plot(nd$Age, mu, type = "n", ylim = range(mu + 1.96 * se, mu - 1.96 * se),
    xlab = "Age", ylab = "Adjusted recorded CLV ($)", main = "One product; transactions held at training median")
  polygon(c(nd$Age, rev(nd$Age)), c(mu - 1.96 * se, rev(mu + 1.96 * se)), border = NA, col = "#DCEAF2")
  lines(nd$Age, mu, col = "#2E6F91", lwd = 2); abline(v = 30, lty = 2, col = "#C45C26")
  mtext("95% HC3 mean-response interval; association, not a causal or future-value curve", side = 1, line = 4.6, cex = .75)
}, 1400, 850)
plot_file("12_yield_diagnostics", {
  par(mfrow = c(2, 2)); plot(yield_fit, which = c(1, 2, 3, 5), id.n = 3, ask = FALSE)
}, 1600, 1200)
cat("Selected CLV model:", selected_name, "\n"); print(test_metrics); print(primary_coef)
cat("Yield check:\n"); print(yield_metrics)
cat("Under30 adjusted comparisons:\n"); print(young_compare)
# END ORIGINAL MODULE: 03_models.R

# BEGIN ORIGINAL MODULE: 04_report.R

# ====================================================================
# 5.01 选取解释所需证据
# 来源：04_report.R:1
# 本模块使用已经计算的对象，不重拟合模型。取测试结果、相关系数项及高低群位置；which.max
# 返回位置，不是最大值本身。确定业务重点时需区分人均利润、总利润与记录 CLV。
clv_test <- test_metrics[2, ]; yield_test <- yield_metrics[2, ]
coef_for <- function(term, model = primary) coef_table(model)[coef_table(model)$term == term, ]
product_b <- coef_for("Tot_Prod"); trans_b <- coef_for("No_Trans"); age_b <- coef_for("Age_c")
yield_trans_b <- coef_for("No_Trans", yield_fit)
high <- which.max(cluster_profile$CLV_mean); low <- which.min(cluster_profile$CLV_mean)
hp <- cluster_profile[high, ]; lp <- cluster_profile[low, ]
cross_index <- which.max(ifelse(cluster_profile$Tot_Prod_mean <= 1.2, cluster_profile$No_Trans_mean, -Inf))
cp <- cluster_profile[cross_index, ]
profit_hi <- which.max(cluster_profile$Yield_mean); profit_lo <- which.min(cluster_profile$Yield_mean)
total_profit_hi <- which.max(cluster_profile$Yield_total)
stage_yield <- cluster_tests[cluster_tests$outcome == "Yield", ]
stage_clv <- cluster_tests[cluster_tests$outcome == "CLV", ]

recommendations <- data.frame(

# ====================================================================
# 5.02 建议连接两类证据
# 来源：04_report.R:17
# 每条建议包括目标群、分群证据、模型证据、行动和验证指标。用 fmt()
# 将计算值插入文字，避免手填无法追溯的数字。建议是待验证试点，不是承诺因果收益；数据并未观察具体产品类型、渠道成本或偏好。
  action = c("维护高活跃客户关系", "对少产品客户做需求导向的交叉销售试点",
             "检查高交易客户的服务成本", "先核实零收入记录，再更新客户价值判断"),
  target = c(paste0("C", hp$Cluster, "：", hp$name), paste0("C", cp$Cluster, "：", cp$name),
             paste0("C", cp$Cluster, "中的高频交易客户"), paste0("C", lp$Cluster, "中的零收入记录客户")),
  cluster_evidence = c(
    paste0("占客户 ", fmt(hp$share * 100, 1), "%，占记录 CLV ", fmt(hp$CLV_share * 100, 1),
           "%；平均 CLV $", fmt(hp$CLV_mean, 0)),
    paste0("平均产品数 ", fmt(cp$Tot_Prod_mean), "，平均 CLV $", fmt(cp$CLV_mean, 0)),
    paste0("平均交易 ", fmt(cp$No_Trans_mean, 0), "，亏损客户比例 ", fmt(cp$negative_yield_rate * 100, 1), "%"),
    paste0("该群零收入比例 ", fmt(lp$zero_income_rate * 100, 1), "%；零收入不能等同没有经济能力")),
  model_evidence = c(
    paste0("主 CLV 模型交易数、产品数系数为正；该群测试 MAE $",
           fmt(subgroup_metrics$MAE[subgroup_metrics$model == paste0("Cluster_", hp$Cluster)], 0)),
    paste0("控制交易和年龄后，每多一产品与记录 CLV 高 $", fmt(product_b$estimate, 0),
           " 相关；95% HC3 CI [", fmt(product_b$HC3_low95, 0), ", ", fmt(product_b$HC3_high95, 0), "]"),
    paste0("补充 Yield 模型中每 100 次交易与 Yield 变化 $", fmt(100 * yield_trans_b$estimate, 0),
           " 相关；但测试 R² 仅 ", fmt(yield_test$R2, 3)),
    paste0("加入收入后测试 CLV R² 从 ", fmt(clv_test$R2, 3), " 升至 ",
           fmt(clv_diagnostics$R2[2], 3), "；收益可能来自旧 CLV 公式，不能视作真实终身价值验证")),
  implementation = c("采用服务回访、问题优先处理和自愿留存方案，先检查客户需求与服务成本。",
    "从适合的客户中随机开展小规模需求评估与产品试点，不能只凭分群推销具体产品。",
    "调查交易渠道与单次服务成本，测试自愿数字渠道支持；暂不直接按模型提高费用。",
    "区分无收入、未申报、未更新等原因，记录来源与时间；不得用推测收入覆盖原始值。"),
  validation_KPI = c("试点与对照组留存率、净 Yield、服务成本及投诉率。",
    "试点与对照组适配产品采纳率、净 Yield、客户投诉；不能把模型系数当增量收益承诺。",
    "单位交易成本、客户净 Yield、渠道使用及服务可达性。",
    "经确认记录比例、收入状态变化、重新分群稳定性和价值估计变化。"))

# ====================================================================
# 5.03 记录分析决策
# 来源：04_report.R:44
# 决策表记录零值、负利润、极端值、年龄重复信息、重复行、模型选择和目标来源的处理。决策与结果放在一起便于审查。下方历史报告保留原分析阶段“暂不做
# PPT”的文字；最终 PPT 已在独立演示交付目录完成。
tab(recommendations, "04_business_recommendations")

decisions <- data.frame(issue = c("Income=0", "Negative Yield", "Extreme observations", "Age and Cat_Age",
  "Duplicates", "Primary clustering", "Primary regression", "Model selection", "CLV interpretation"),
  decision = c("Preserve as ambiguous recorded zeros; profile and compare imputation/omission alternatives.",
    "Retain: negative profit is explicitly permitted.",
    "Retain original rows/outcomes. Cap only clustering Income and No_Trans at p99; model outlier changes are sensitivity checks.",
    "Age is active; Cat_Age is categorical profiling only. Do not include both as model inputs.",
    "Retain two repeated value rows: no customer ID to establish duplicate persons; group identical primary predictors when splitting.",
    paste0("k=", selected_k, "; 4 standardised inputs; seed and preprocessing sensitivity checked."),
    paste0("Recorded CLV on ", paste(attr(terms(primary), "term.labels"), collapse = ", "), "; exclude Income and Yield."),
    "Grouped five-fold CV within training; prefer simpler candidate within 1% of best CV RMSE; test used for evaluation.",

# ====================================================================
# 5.04 对照作业与课堂
# 来源：04_report.R:56
# 要求对照表把每项要求指向具体结果，课程表连接
# W3、W4、W6、W7、W8。补充方法分别服务稳健推断、多重检验、泛化或稳定性，不是为了堆方法。原阶段检查表是历史记录，不代表新
# PPT 尚未完成。
    "Predicting an existing derived CLV does not verify future lifetime value, causal drivers or underestimation."))
tab(decisions, "04_analysis_decisions")
coverage <- data.frame(requirement = c("Part1: records, variable types and summaries", "Part1: investigate Income=0",
  "Part1: outlier decision", "Part1: Age/Cat_Age", "Part1: CLV bivariate and under30",
  "Part2: justify inputs and exclude outcomes", "Part2: standardisation and alternative k",
  "Part2: all variable profiles, sizes and names", "Part3: justified predictive model and outputs",
  "Part3: diagnostics and target provenance", "Part4: most/least profitable and CLV drivers",
  "Part4: at least three evidence-linked actions", "Part5: presentation slides"),
  evidence = c("01_data_dictionary; 01_univariate_summary", "01_zero_income_profile; 02_preprocessing_sensitivity",
    "01_outlier_screening; 03_outlier_sensitivity_metrics", "01_age_category_mapping; selected model inputs",
    "01_correlations_tests; 01_welch_two_group_tests; 03_under30_adjusted_association", "02_cluster_input_correlations; R/02_clusters.R",
    "02_k_comparison; 02_selected_quality", "02_cluster_profiles; 02_cluster_age_category_proportions",
    "03_CV_model_comparison; 03_primary_test_metrics; models/primary_summary.txt", "03_primary_diagnostics; 03_CLV_target_provenance_sensitivity",
    "02_cluster_profiles; 03_predictor_contributions", "04_business_recommendations", "Deferred at user's explicit request"),
  status = c(rep("完成", 12), "本阶段不制作"))
tab(coverage, "04_requirement_checklist")
course_mapping <- data.frame(week = c("W3", "W4", "W6", "W7", "W8", "有明确用途的补充方法"),
  application = c("变量测量水平、频数、均值/中位数/标准差、偏态、z-score。",
    "Welch 两样本 t 检验；年龄组与零收入的卡方检验；关联与因果的区分。",
    "Pearson 相关、ANOVA、方差检查与组间比较；用分组避免遗漏非线性。",
    "k-means、标准化、silhouette、比较不同 k、主动与被动变量画像、命名。",
    "lm、回归系数、R²/调整 R²、F 检验、VIF、残差及影响点检查。",
    "Spearman 检查极端值影响；Welch/Holm 处理方差不齐和多重比较；分组交叉验证与测试集评估泛化；HC3 检查异方差下推断；ARI 检查分群稳定性。"))
tab(course_mapping, "04_course_mapping")

# ====================================================================
# 5.05 组装 HTML 文档
# 来源：04_report.R:80
# parts 存放文字片段，add() 用 <<- 更新外层对象；heading、para、fig
# 给内容加 HTML 标签。CSS
# 只控制外观与小屏排版，不改统计值。转义保证符号按文字展示，相对链接让报告与 tables、plots
# 文件夹一同移动后仍可用。

parts <- character()
add <- function(...) parts <<- c(parts, paste0(...))
heading <- function(id, title) add('<h2 id="', id, '">', title, '</h2>')
para <- function(...) add("<p>", ..., "</p>")
fig <- function(name, caption) add('<figure><img src="plots/', name, '.png" alt="',
    html_escape(caption), '"><figcaption>', caption, '</figcaption></figure>')
table_link <- function(name, text) paste0('<a href="tables/', name, '.csv">', text, '</a>')
add('<!doctype html><html lang="zh-CN"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
    '<title>Assignment 3 客户细分与盈利分析</title><style>',
    'body{font-family:-apple-system,BlinkMacSystemFont,"PingFang SC","Microsoft YaHei",sans-serif;color:#23344d;background:#f5f7fa;margin:0;line-height:1.8}',
    'main{max-width:1140px;margin:32px auto;padding:42px 54px;background:white;border:1px solid #e1e7ee;border-radius:12px}',
    'h1{font-size:30px;line-height:1.35;color:#123859}h2{font-size:23px;margin-top:42px;padding-top:16px;border-top:1px solid #dce5ed;color:#123859}',
    'h3{font-size:18px;color:#234e70;margin-top:28px}p,li{font-size:16px}a{color:#166690}nav{background:#eef4f8;padding:16px 22px;border-radius:8px}',
    '.lead{font-size:18px}.note{border-left:4px solid #c9923d;padding:10px 18px;background:#fff8ed}.table-wrap{overflow:auto;margin:18px 0}',
    'table{border-collapse:collapse;width:100%;font-size:13px;line-height:1.55}td,th{border:1px solid #dce5ed;padding:9px 11px;text-align:left;vertical-align:top}',
    'th{background:#edf3f7;color:#163b56}tr:nth-child(even){background:#f9fbfd}figure{margin:26px 0}img{max-width:100%;height:auto}figcaption{font-size:13px;color:#56687b}',
    'th.text-column{min-width:170px}',
    'code,pre{font-family:Menlo,Consolas,monospace;font-size:13px}pre{background:#f0f4f7;padding:14px;overflow:auto}.meta{color:#63748a;font-size:14px}',
    '@media(max-width:700px){main{margin:0;padding:22px 18px;border:0}h1{font-size:25px}}',
    '@media print{body{background:white}main{margin:0;border:0;padding:0}nav{display:none}h2{break-after:avoid}figure,table{break-inside:avoid}a{color:inherit}}',
    '</style></head><body><main>')
add('<h1>Assignment 3<br>零售银行客户细分与盈利分析</h1>')
para('本报告完成作业 Part 1–4 的分析与结果解释。数据为 <code>Group_presentation.csv</code>，',
     '共 10,000 条记录。分析使用 R；原始数据保持不变。本阶段没有制作 PPT。')
add('<p class="meta">生成时间：', format(Sys.time(), '%Y-%m-%d %H:%M %Z'),
    ' · R ', getRversion(), ' · 原始文件 MD5：', source_md5, '</p>')
add('<nav>目录：<a href="#findings">主要发现</a> · <a href="#data">数据准备</a> · <a href="#eda">关系探索</a> · ',
    '<a href="#clusters">客户细分</a> · <a href="#models">预测模型</a> · <a href="#actions">商业建议</a> · ',
    '<a href="#limitations">限制</a> · <a href="#reproduce">复现与要求核对</a></nav>')

heading('findings', '1. 主要发现')
add('<ul>',

# ====================================================================
# 5.06 撰写发现与数据审计
# 来源：04_report.R:113
# 主要发现汇总关键数字及限制。数据准备展示字典、概括、异常筛查和年龄映射。引号里的长段是报告文字，不是要执行的命令。表格来自已计算对象，解释文字说明分析者的推理。
    '<li>收入为 0 的客户有 <strong>1,689 人（16.89%）</strong>，其中 1,576 人仍有交易。它可能混合真实零收入与未记录情况，不能仅从这份数据识别每个人的原因。</li>',
    '<li>主方案选择 <strong>', selected_k, ' 个客户群</strong>，全样本平均 silhouette 为 ', fmt(sil_summary$exact_mean, 3),
    '。这是有重叠的粗分群；收入处理的敏感性意味着分群不宜直接用于自动化个人决策。</li>',
    '<li>C', hp$Cluster, ' 的平均记录 CLV 为 $', fmt(hp$CLV_mean, 0), '，C', lp$Cluster, ' 为 $', fmt(lp$CLV_mean, 0),
    '。当前 Yield 的差异小得多，不能把客户价值与当前利润混为一谈。</li>',
    '<li>排除 Income 和 Yield 的 CLV 回归测试 R² 为 <strong>', fmt(clv_test$R2, 3), '</strong>，MAE 为 $', fmt(clv_test$MAE, 0),
    '。交易数、产品数、年龄的条件关联为正；这解释的是现有 CLV 标签。</li>',
    '<li>补充 Yield 模型测试 R² 为 <strong>', fmt(yield_test$R2, 3), '</strong>。它对当前个体利润的预测能力有限，不足以直接支持加费或差别待遇。</li>',
    '<li>年轻客户的 CLV 较低，但数据没有真实未来价值或旧公式，<strong>不能证明 30 岁以下客户被低估，更不能据此统一上调其 CLV</strong>。</li></ul>')

heading('data', '2. 数据准备与处理决策')
para('每行是一条客户记录，但没有客户 ID。全部 7 列均以整数存储，统计测量类型却不同；Cat_Age 是有序类别，不能因为编码是数字就当作连续变量。')
add(html_table(dictionary)); add(html_table(univariate[, c('variable', 'n', 'mean', 'sd', 'median', 'min', 'max', 'skewness')], 2))
para('数据没有显式缺失值、无限值或超出要求范围的年龄。存在两条与前面记录完全相同的值组合，但没有 ID 证明它们是重复客户，因此保留。产品数观察到 1–5，年龄组观察到 2–6；不能为没有出现的类别补造记录。')
para('Yield 可为负，实际有 4,368 人（43.68%）当前亏损，属于作业允许的业务状态。Income 的时间周期和具体币种没有说明，因此不自行称其为“年收入”或“月收入”。')
add('<h3>零收入与异常值</h3>')
para('零收入组平均交易数为 ', fmt(zero_profile$No_Trans_mean[zero_profile$group == '1'], 1),
    '，正收入组为 ', fmt(zero_profile$No_Trans_mean[zero_profile$group == '0'], 1),
    '。这表明零记录包含活跃客户，却不能证明其必为缺失。主分析保留原值，明确标注为“记录收入”，并比较年龄组正收入中位数填补、对数变换与排除收入的替代方案。')
add(html_table(outliers, 2))
para('IQR 超界只用于筛查。高收入、高盈利和多产品客户可能真实存在，不能机械删除。主聚类只对 Income 和 No_Trans 的上端 1% 封顶后标准化，原始列和全部客户保留；群体画像仍使用原始量纲。Tot_Prod 4–5 属于合法产品数量，即使被 IQR 标记也保留。')
para('主回归保留原始结果变量。另以训练集 99% 分位数封顶、排除训练集高 Cook 距离记录做敏感性分析，测试集始终保留原始结果。没有为了得到更漂亮的 R² 而删除难预测的测试客户。')
fig('01_distributions', '全范围分布显示 Income、Yield 与 CLV 的右偏。');
fig('02_outliers', '全范围箱线图；极端值被标记，但不自动认定为错误。')
add('<h3>年龄变量核对</h3>'); add(html_table(age_mapping, 0))
para('实际编码与年龄范围完全一致；Cat_Age=2 包含 18–35 岁，因此不能直接用该组代表 30 岁以下。专题分析另用 Age&lt;30。模型只使用 Age，Cat_Age 留作描述画像，避免重复计权或共线性。')
fig('05_zero_income', '零收入分布和交易活跃度；箱线图只隐藏离群点显示，没有删除记录。')

# ====================================================================
# 5.07 解释双变量证据
# 来源：04_report.R:140
# 报告插入 CLV 相关、两组差异和年龄段检验，并连接 CSV
# 证据。总体年龄线性相关弱，不等于分组形状没有规律。均值差与区间描述记录标签，无法对未知真实终身利润证明年轻客户被低估。

heading('eda', '3. 单变量与双变量探索')
para('先按 W3 做分布与频数，再根据 W4/W6 的变量类型选择检验。数值对数值用 Pearson；Spearman 用作对极端值敏感性的补充。两组均值使用 Welch t 检验，多组采用 Welch ANOVA，并同时保存课堂经典 ANOVA；方差检查用以中位数为中心的 Brown–Forsythe 检验。')
clv_corr <- cor_tests[cor_tests$y == 'CLV', c('x', 'method', 'r', 'p_holm')]
clv_corr$p_holm <- ptext(clv_corr$p_holm)
add(html_table(clv_corr, 3))
para('CLV 与 Income、No_Trans 和产品数存在关联。Yield 的 Pearson 与 Spearman 差异较大，说明极端值对线性相关系数影响明显。Age 与 CLV 的总体线性相关很弱，但年龄分组均值并非一条直线，因此不能据低相关就认定年龄不相关。')
fig('03_age_clv', '逐岁均值与分组分布用于发现非线性。左图显示至 CLV 的 99% 分位数，计算仍包含全部记录。')
fig('04_clv_associations', 'CLV 与其他变量的全范围散点图；高相关可能含有原 CLV 公式的机械联系。')
yr <- group_tests[group_tests$grouping == 'Under30' & group_tests$outcome == 'CLV', ]
para('30 岁以下有 ', fmt(yr$n1, 0), ' 人，平均 CLV $', fmt(yr$mean1, 0),
    '；其余客户平均 $', fmt(yr$mean0, 0), '。均值差（年轻减其他）为 $', fmt(yr$difference_1_minus_0, 0),
    '，95% CI [', fmt(yr$low95, 0), ', ', fmt(yr$high95, 0), ']，Welch p ', ptext(yr$p),
    '。年龄组也同时存在收入、产品数和交易差异，这不是低估的直接证据。')
av <- anova_results[, c('outcome', 'Welch_F', 'Welch_p', 'Brown_Forsythe_p', 'eta_squared')]
av$Welch_p <- ptext(av$Welch_p); av$Brown_Forsythe_p <- ptext(av$Brown_Forsythe_p)
add(html_table(av, 3))
para('多组 CLV 比较采用逐对 Welch t 检验并用 Holm 校正；这些是对记录值的差异检验。年龄组与零收入的卡方检验 Cramér’s V 为 ', fmt(chi_result$CramersV, 3),
    '，p ', ptext(chi_result$p), '，最小期望频数 ', fmt(chi_result$minimum_expected, 1), '。详细假设、效应量和 p 值见 ',
    table_link('01_welch_two_group_tests', '两组检验'), '、', table_link('01_CLV_age_posthoc_Welch_Holm', 'CLV 组间比较'),
    ' 与 ', table_link('01_zero_income_chisquare', '卡方结果'), '。大样本下统计显著不等于业务影响很大。')


# ====================================================================
# 5.08 解释分群选择
# 来源：04_report.R:162
# 展示输入理由、分数比较、两群与六群取舍和原量纲画像。文字明确承认群间重叠以及收入处理的敏感性。被动盈利帮助解释画像，并不能形成独立确认的因果细分。
heading('clusters', '4. k-means 客户细分')
para('聚类输入为 Tot_Prod（关系广度）、Income（记录收入）、No_Trans（活跃度）、Age（生命周期）。Yield 与 CLV 完全不参与形成群体，只用于事后画像。所有输入进行 z-score 标准化；Cat_Age 不重复进入。')
para('比较 k=2–8，每个解使用 50 个随机起点。由于数据中存在大量重复或接近的点，采用 R 的 Lloyd 更新算法以避免默认算法的 Quick-Transfer 警告。固定种子使结果可复现，并以其他种子验证稳定性。')
para('先用相同的 2,500 条记录估计 silhouette，再对最高的两个可行方案计算全体 10,000 条记录的精确 silhouette。将至少占 2% 的群体作为可操作性参考；这是分析者的选择，不是作业硬性要求。W7 提醒微小分差不值得过度解读：分数接近时，结合群体画像、规模和可执行性判断，简单方案是重要参考。')
add(html_table(candidates, 4)); fig('06_select_k', '同一抽样上的 silhouette 比较、候选解的精确复核及辅助 elbow 图。')
add('<h3>为什么选择六群而不是两群</h3>')
comparison_display <- profile_comparison[, c('k', 'exact_silhouette', 'smallest_n', 'largest_n', 'mean_age_range', 'median_yield_min', 'median_yield_max')]
names(comparison_display) <- c('群数', 'Silhouette', '最小群人数', '最大群人数', '各群均龄极差', '最低群中位 Yield', '最高群中位 Yield')
add(html_table(comparison_display, 3))
para('两群方案的精确 silhouette 为 0.3205，六群为 0.3163，差距约 0.0042。两群足以表达大致活跃程度，但其平均年龄仅约 45 和 48 岁，且合并了不同的产品和交易组合。六群保留了单产品高交易、年长单产品、较年轻低活跃等可解释群体，最小群有 470 人（4.7%），并非由少数极端客户组成。')
para('在六群画像中，单产品高活跃群与较年轻低活跃群的 Yield 中位数均为 −25，而年长单产品群为 118。这是选择六群的重要业务理由：它揭示了两群方案掩盖的当前盈利差异。代价是管理与执行更复杂，且整体 silhouette 并未改善，因此两群仍保留为简化备选。这里使用被动盈利变量帮助解释方案，后续群间检验属于探索性描述，不能充当独立确认。')
para('最终选择 k=', selected_k, '，精确 silhouette ', fmt(sil_summary$exact_mean, 3),
    '，负 silhouette 的客户比例 ', fmt(sil_summary$negative_share * 100, 1),
    '%。分群存在明显重叠，宜理解为粗分层；并非“清晰自然存在的几种人”。所有 k 的画像均已保存以便比较，不能只凭最高分解释群体。')
profile_display <- cluster_profile[, c('Cluster', 'name', 'n', 'share', 'Tot_Prod_mean', 'Income_mean', 'No_Trans_mean', 'Age_mean', 'Yield_mean', 'Yield_median', 'CLV_mean', 'negative_yield_rate')]
add(html_table(profile_display, 2))
para('表中的 share 和 negative_yield_rate 是 0–1 比例。完整表提供所有数值变量的均值、中位数和标准差；Cat_Age 按类别比例呈现，另保存产品数各档比例。',
    table_link('02_cluster_age_category_proportions', '年龄类别比例'), ' 覆盖原始变量中不能用连续均值解释的 Cat_Age。')
fig('07_cluster_profiles', '聚类中心为处理后的标准化值；报告中的业务画像为原始数据。')
fig('08_cluster_profitability', '均值与中位数并列，避免少数极端高利润客户掩盖典型客户状态。')
para('按人均 Yield 排序，C', cluster_profile$Cluster[profit_hi], ' 最高、C', cluster_profile$Cluster[profit_lo],
    ' 最低；按人均 CLV 排序，C', hp$Cluster, ' 最高、C', lp$Cluster, ' 最低。总利润还受群体规模影响：',
    'C', cluster_profile$Cluster[total_profit_hi], ' 贡献全部 Yield 的 ', fmt(cluster_profile$Yield_share[total_profit_hi] * 100, 1),
    '%。年长客户的当前盈利贡献尤其值得保留，不能因为其记录 CLV 低于高活跃群就降低服务重视程度。')
para('群体 Yield 差异 Welch p ', ptext(stage_yield$p), '，但 η² 只有 ', fmt(stage_yield$eta_squared, 5),
    '；CLV 的 η² 为 ', fmt(stage_clv$eta_squared, 3), '。因此这套细分对记录 CLV 的区分强于对当前利润的区分。主动输入本来就被用于形成群体，其显著性不能当成独立验证；被动 CLV 也受旧公式影响。')
add('<h3>稳定性与收入处理敏感性</h3>'); add(html_table(seed_stability, 4)); add(html_table(sensitivity, 4))
para('ARI（调整兰德指数）越接近 1，表示两种分群越一致；接近 0 或负值表示一致性低。固定处理方式下不同种子的稳定，与改变处理方式后的稳定，是两件不同的事。对数收入方案与主方案明显不同，排除收入也会改变分组，说明收入如何编码是重要决策。不同处理空间的 silhouette 不宜直接作为谁“更真实”的证明。')
para('主方案选择保留记录零值，是为了不假装知道缺失机制；上端封顶是为了减弱少量极端距离。该决定需要在业务上核实收入信息后重评，报告不将任何一个分群称为唯一正确答案。')


# ====================================================================
# 5.09 解释回归表现
# 来源：04_report.R:192
# 回归部分区分目标来源、数据切分、训练选择、测试误差和条件系数。系数描述其他已纳入预测变量固定时的模型变化；产品系数不是再卖一个产品就必然得到的收益。
heading('models', '5. 预测模型及核验')
add('<h3>为什么以 CLV 线性回归为主</h3>')
para('作业允许预测 Yield 或 CLV，但商业建议明确要求讨论 CLV 的关联因素。因此主模型解释记录 CLV，并以 Yield 回归补充当前盈利检查。选择线性回归以体现 W8 的系数解释、拟合与假设检查；数据中的主要关联可用较简单模型表达，没有为增加方法数量而加入 CART。')
para('CLV 是前任分析师计算的标签。主模型排除 Income 与 Yield，降低直接复现其公式的风险。No_Trans、Tot_Prod 等也可能参与原公式，因此这并不能消除目标构造问题。所有“驱动因素”在本报告中都指对现有标签的统计关联，不能解释为未来真实利润的因果驱动。')
para('按相同的 Tot_Prod、No_Trans、Age 组合分组切分，训练集 ', nrow(train), ' 行、测试集 ', nrow(test),
    ' 行，避免同样的主模型输入组合同时出现在两边。训练内做分组五折交叉验证；预处理敏感性所需的收入填补值与封顶值只由训练集估计。聚类使用全体客户，但聚类标签不进入预测模型，也不用于选择模型。')
para('比较线性年龄、二次年龄与 30 岁连续折线三种候选。选择仅依据训练内 CV；在最优 RMSE 的 1% 以内优先简单模型。这里保留 ', selected_name,
    '。这是一套内部验证，不是新银行、新时期或真实未来现金流上的外部验证；探索阶段已查看全体数据分布。')
add(html_table(cv_summary, 3)); add(html_table(fit_stats, 4)); add(html_table(test_metrics, 3))
para('相对训练均值基线，测试 RMSE 降低 ', fmt(100 * (1 - clv_test$RMSE / baseline_test$RMSE), 1),
    '%；MAE 降低 ', fmt(100 * (1 - clv_test$MAE / baseline_test$MAE), 1),
    '%。RMSE 仍较大，反映高价值尾部难以预测。测试指标属于训练模型；另存全数据重拟合模型用于最终解释，不把重拟合后的训练拟合度冒充测试表现。')
add('<h3>系数与关联因素</h3>'); add(html_table(primary_coef, 4))
if (selected_name == 'linear') para('训练模型为：CLV = ', fmt(coef(primary)[1], 2), ' + ', fmt(product_b$estimate, 2),
    ' × Tot_Prod + ', fmt(trans_b$estimate, 3), ' × No_Trans + ', fmt(age_b$estimate, 3), ' × (Age − 45)。')
para('控制模型中的其他变量后，每多一个产品与 CLV 高 $', fmt(product_b$estimate, 0),
    ' 相关；每多 100 次交易与 CLV 高 $', fmt(100 * trans_b$estimate, 0),
    ' 相关。这些是条件关联，不能用来承诺交叉销售或增加交易的实际增量利润。年龄系数是条件关系，不等同原始年龄组均值的走势。')
add(html_table(importance, 4)); para('用分块 partial R² 比较贡献，避免直接比较不同单位的原始系数。这里交易数最强，其次是产品数，再次是年龄；这是给定变量集合中的结果，不代表未观察因素不重要。')
fig('10_test_predictions', '测试集预测与实际记录 CLV；完整与局部视图同时保留。')
fig('11_adjusted_age_pattern', '保持产品数和交易不变后的年龄关联。阴影是均值估计不确定性，不是个人预测区间。')


# ====================================================================
# 5.10 解释限制与敏感性
# 来源：04_report.R:214
# 插入残差、VIF、替代训练处理、目标重构与年轻组条件差异，并展示当前 Yield
# 预测较弱。稳健标准误不修复极端值预测，也不证明独立性。这些限制决定建议应如何使用。
add('<h3>假设、异常点与敏感性</h3>'); add(html_table(diagnostics, 5)); add(html_table(primary_vif, 3))
para('VIF 较低，未显示严重共线性。残差右尾很长且存在异方差与影响点，不能宣称线性回归全部假设满足。采用 HC3 稳健标准误辅助系数推断，同时保留课堂常规标准误供比较。HC3 不会修复非线性、尾部预测误差、未知的独立性或因果识别问题。数据无客户 ID/时间信息，无法实证证明误差独立。')
fig('09_regression_diagnostics', 'W8 四类诊断图：残差、Q–Q、Scale–Location 与杠杆/影响点。')
add(html_table(sensitivity_metrics, 3))
para('上端封顶和移除训练影响点后，产品数、交易数和年龄系数方向仍可核对于 ',
    table_link('03_outlier_sensitivity_coefficients', '敏感性系数表'),
    '。这些方案不取代主结果：移除合法高价值客户会改变研究总体，且在原始测试集上的 RMSE 没有改善。')

add('<h3>CLV 衍生公式与年轻客户专题</h3>'); add(html_table(clv_diagnostics, 3))
para('将 Income 和 Yield 加回诊断模型后，测试 R² 升到 ', fmt(clv_diagnostics$R2[nrow(clv_diagnostics)], 3),
    '。这与“CLV 由其他变量计算”的描述相符，但不是找回旧公式的证明，更不是模型能预测真实未来价值的证据。该模型仅作目标来源诊断。')
add(html_table(young_compare, 4))
para('加入二次年龄项后，Under30 仍表现为负关联；再调整 Income 和 Yield，差异明显减弱。其大小依赖控制变量和模型形式。因此证据支持检查旧公式及年轻客户的输入质量，无法确定低估的幅度，也不支持直接给所有年轻客户统一加值。')

add('<h3>补充 Yield 回归</h3>'); add(html_table(yield_metrics, 3))
para('补充模型排除 CLV，避免把可能包含 Yield 的衍生变量拿来预测 Yield。其测试 R² 为 ', fmt(yield_test$R2, 3),
    '，说明可用变量解释当前利润的能力有限。在控制收入、年龄、产品数等因素后，交易数系数为 ', fmt(yield_trans_b$estimate, 3),
    '，95% HC3 CI [', fmt(yield_trans_b$HC3_low95, 3), ', ', fmt(yield_trans_b$HC3_high95, 3),
    ']。这一负关联提示调查服务成本，但数据没有交易渠道、费用和成本信息，不能断言交易导致亏损。')
fig('12_yield_diagnostics', '补充 Yield 模型也有明显极端残差，使用限制比 CLV 主模型更强。')


# ====================================================================
# 5.11 写入行动与复现说明
# 来源：04_report.R:235
# 循环展示每条建议的两类证据和 KPI，结尾汇总限制、运行命令、代码链接、课程对照及全部
# CSV。writeLines 将组装好的 HTML
# 写入文件。下一阶段会实际检查这些链接和输出是否存在。
heading('actions', '6. 商业建议与验证方式')
para('以下建议把群体画像与预测模型证据连接起来。它们是应通过业务试点验证的行动方向；数据没有具体产品持有类型、客户需求、交易渠道或成本，不能推出应向某个客户销售哪一个产品。')
for (i in seq_len(nrow(recommendations))) {
  rr <- recommendations[i, ]
  add('<h3>', i, '. ', rr$action, '</h3>')
  para('<strong>目标：</strong>', rr$target, '。<strong>群体证据：</strong>', rr$cluster_evidence, '。')
  para('<strong>模型证据：</strong>', rr$model_evidence, '。')
  para('<strong>行动：</strong>', rr$implementation, '<strong>验证：</strong>', rr$validation_KPI)
}

heading('limitations', '7. 结论的适用范围')
add('<ul><li>这是一份无采样说明、无客户 ID、无时间跨度的数据；不能证明代表银行全体客户，也无法验证真实生命周期结果。</li>',
    '<li>Income=0 的真实原因未知；极端值未获业务核实。所有处理均为可复查的分析选择。</li>',
    '<li>分群存在重叠且对收入处理敏感，不能把某一客户的群标签看作永久属性。</li>',
    '<li>CLV 为衍生标签，良好拟合可能来自旧公式结构；模型没有识别营销行动的因果收益。</li>',
    '<li>两个回归均存在长尾和异方差。测试集表现只描述本次内部切分，个体层面的预测尤其是极端高值不可靠。</li>',
    '<li>主张“年轻客户被低估”需要旧公式、纵向实际利润或其他可信价值基准；目前证据不足。</li></ul>')

heading('reproduce', '8. 复现、文件说明与作业要求核对')
para('在装有 R 的电脑上运行以下命令。只依赖随 R 通常安装的推荐包 cluster，当前机器已经具备；没有新增 R 包，也没有访问外部数据。脚本不安装软件、不覆盖原始 CSV。')
add('<pre>Rscript --vanilla run_analysis.R\nRscript --vanilla verify_results.R</pre>')
para('主入口会重新生成 tables、plots、models、data 和本 HTML。<code>models/regression.rds</code> 同时保留训练模型和全数据重拟合模型；<code>logs/sessionInfo.txt</code> 记录环境。验证结果见 <a href="logs/verification.txt">verification.txt</a>。')
add('<ul><li><a href="run_analysis.R">run_analysis.R</a>：主入口。</li>',
    '<li><a href="R/01_data_eda.R">01_data_eda.R</a>：数据检查与探索。</li>',
    '<li><a href="R/02_clusters.R">02_clusters.R</a>：聚类与稳定性。</li>',
    '<li><a href="R/03_models.R">03_models.R</a>：预测与诊断。</li>',
    '<li><a href="R/04_report.R">04_report.R</a>：用真实结果生成本报告。</li>',
    '<li><a href="verify_results.R">verify_results.R</a>：独立核验。</li></ul>')
add(html_table(course_mapping)); add(html_table(coverage))
para('补充方法均对应实际问题：分组 CV 防止相同输入组合跨训练/测试，HC3 针对异方差，ARI 揭示分群敏感性。没有引入与作业目标无关的复杂算法。课件中的方法按其统计适用条件使用，未机械套用所有检验。')
add('<h3>资料来源</h3><ul><li>group/assignment requirement.docx：四张作业要求与评分标准截图。</li>',
    '<li>group/Group_presentation.csv：唯一分析数据。</li>',
    '<li>W3、w4、W6、W7、W8 讲义和相关实验材料：描述统计、双变量分析、聚类和回归。</li>',
    '<li>本地 R 帮助：stats::lm、stats::kmeans、stats::t.test、stats::oneway.test、cluster::silhouette。</li></ul>')
add('<h3>全部结果表</h3><ul>')
for (f in sort(list.files(file.path(ROOT, 'tables'), pattern = '\\.csv$')))
  add('<li><a href="tables/', f, '">', f, '</a></li>')
add('</ul></main></body></html>')
writeLines(parts, file.path(ROOT, 'analysis_report.html'), useBytes = TRUE)
cat('Report generated: analysis_report.html\n')
# END ORIGINAL MODULE: 04_report.R
# 核验之前先关闭分析日志，并复制原代码以保留报告中的查看链接。这些副本供阅读；重建本目录请运行当前学习脚本。HTML
# 是原分析阶段历史报告，最终演示状态另见交付说明。
capture.output(sessionInfo(), file = file.path(ROOT, "logs", "sessionInfo.txt"))
sink(); close(log_con)
for (f in c("run_analysis.R", "verify_results.R"))
  file.copy(file.path(REFERENCE_DIR, f), file.path(ROOT, f), overwrite = TRUE)
for (f in list.files(file.path(REFERENCE_DIR, "R"), full.names = TRUE))
  file.copy(f, file.path(ROOT, "R", basename(f)), overwrite = TRUE)
# 只修正学习版生成报告的运行说明；计算和原始模块保持不变。请按学习版横幅运行，不使用历史原入口命令。

learning_html_path <- file.path(ROOT, "analysis_report.html")
learning_html <- paste(readLines(learning_html_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
learning_html <- gsub('<pre>Rscript --vanilla run_analysis.R\nRscript --vanilla verify_results.R</pre>', '<pre>Rscript --vanilla ../../Assignment3_Learning_Chinese.R</pre>', learning_html, fixed = TRUE)
learning_banner <- '<aside class="note"><strong>R learning edition / R 学习版</strong><p>From this output folder, rerun: <code>Rscript --vanilla ../../Assignment3_Learning_Chinese.R</code>. 从当前输出目录运行此命令即可完整复现并核验。Linked original scripts are historical reading references; do not run the copied originals here. 下方原脚本链接供对照阅读。The old no-slides statement records the earlier analysis stage; bilingual final PPTs are delivered separately. 原报告暂不做 PPT 的文字属于历史阶段，最终中英文 PPT 已另行提供。</p></aside>'
learning_html <- sub("<main>", paste0("<main>", learning_banner), learning_html, fixed = TRUE)
writeLines(learning_html, learning_html_path, useBytes = TRUE)

# BEGIN ORIGINAL MODULE: verify_results.R






# ====================================================================
# 6.01 独立检查
# 来源：verify_results.R:6
# 另开核验日志。check() 遇到失败立即停止，只有通过才累加计数。eq()
# 允许浮点计算的微小误差；readtab() 读取保存后的 CSV，避免只检查内存中同一个对象。
log <- file(file.path(ROOT, "logs", "verification.txt"), "wt")
sink(log, split = TRUE)
checks <- 0L
check <- function(condition, label) {
  if (!isTRUE(condition)) stop(paste("FAIL:", label))
  checks <<- checks + 1L; cat("PASS:", label, "\n")
}
eq <- function(a, b, tolerance = 1e-8) isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = tolerance))
readtab <- function(name) read.csv(file.path(ROOT, "tables", paste0(name, ".csv")), check.names = FALSE)

# ====================================================================
# 6.02 核对源数据与概括
# 来源：verify_results.R:15
# 重新读取原始数据，核对固定
# MD5、维度和关键人数。直接从原列重算均值、中位数并与导出表比较。这可以发现输入变化、错误筛选或导出问题。
raw <- read.csv(SOURCE)
check(identical(unname(tools::md5sum(SOURCE)),
                "f0476691df9c6f541114756ef40a56d4"), "Original CSV MD5 unchanged")
check(nrow(raw) == 10000 && ncol(raw) == 7 && !anyNA(raw), "Source dimensions and missingness")
check(sum(raw$Income == 0) == 1689 && sum(raw$Income == 0 & raw$No_Trans > 0) == 1576,
      "Zero-income and positive-transaction counts")
check(sum(raw$Yield < 0) == 4368 && sum(duplicated(raw)) == 2, "Negative profit and repeated-value counts")
ss <- readtab("01_univariate_summary")
for (v in ss$variable) {
  rr <- ss[ss$variable == v, ]
  check(eq(rr$mean, sum(raw[[v]]) / nrow(raw)) && eq(rr$median, median(raw[[v]])), paste("Raw summary", v))
}

# ====================================================================
# 6.03 核对聚类输入
# 来源：verify_results.R:27
# 加载保存的聚类对象，重新应用阈值与标准化，比较保存的均值和标准差。确认盈利结果未混入输入，且
# Lloyd 正常结束，再核对行号和归属。这是在检查模型形成过程，不只是看打印摘要。
cm <- readRDS(file.path(ROOT, "models", "clustering.rds"))
check(!any(c("Yield", "CLV", "Cat_Age") %in% cm$variables), "No outcome or redundant age category in clustering")
inp <- raw[cm$variables]
for (v in names(cm$caps)) inp[[v]] <- pmin(inp[[v]], cm$caps[v])
z <- scale(inp)
check(eq(colMeans(z), rep(0, ncol(z))) && eq(apply(z, 2, sd), rep(1, ncol(z))), "All clustering inputs standardised")
check(eq(attr(z, "scaled:center"), cm$center) && eq(attr(z, "scaled:scale"), cm$scale), "Saved scaling matches data")
check((is.null(cm$kmeans$ifault) || identical(as.integer(cm$kmeans$ifault), 0L)) &&
      is.finite(cm$kmeans$iter) && cm$kmeans$iter < 200, "Selected Lloyd k-means converged before iteration limit")
members <- readtab("02_cluster_membership")
check(identical(members$row_id, seq_len(nrow(raw))) && eq(members$Cluster, cm$cluster), "Every row assigned once and labels match model")

# ====================================================================
# 6.04 对账画像与中心
# 来源：verify_results.R:38
# 各群人数必须合计 10,000，份额合计
# 1。用原始数据重算群均值，按人数加权后必须还原总体均值。类别比例合计
# 1，总利润与原数据一致，标准化中心等于所分配记录的平均值。
pr <- readtab("02_cluster_profiles")
check(sum(pr$n) == nrow(raw) && eq(sum(pr$share), 1), "Cluster counts and shares reconcile")
check(eq(as.numeric(table(cm$cluster)), pr$n), "Cluster table matches assignments")
for (v in setdiff(names(raw), "Cat_Age")) {
  observed <- tapply(raw[[v]], cm$cluster, mean)
  check(eq(observed, pr[[paste0(v, "_mean")]]) && eq(weighted.mean(pr[[paste0(v, "_mean")]], pr$n), mean(raw[[v]])),
        paste("Original-scale cluster means reconcile:", v))
}
agep <- read.csv(file.path(ROOT, "tables", "02_cluster_age_category_proportions.csv"), row.names = 1, check.names = FALSE)
check(eq(rowSums(agep), rep(1, cm$selected_k)), "Cat_Age category proportions sum to one")
check(eq(pr$Yield_total, tapply(raw$Yield, cm$cluster, sum)) && eq(sum(pr$CLV_total), sum(raw$CLV)), "Cluster profit totals reconcile")
centroids <- rowsum(z, cm$cluster) / as.numeric(table(cm$cluster))
check(eq(centroids, cm$centers_relabelled), "Saved cluster centers match assigned observations")

# ====================================================================
# 6.05 核对选择依据
# 来源：verify_results.R:51
# 检查七个候选 k 存在且只有两个有完整
# silhouette，六群在公开容差内并达到最小规模规则，同时要求画像比较表存在。通过表示选择符合公开规则，不表示六群唯一最优。
kc <- readtab("02_k_comparison")
check(identical(as.integer(kc$k), 2:8) && sum(!is.na(kc$full_silhouette)) == 2, "Seven k candidates and two exact silhouette checks")
near_best <- kc$k[!is.na(kc$full_silhouette) & kc$full_silhouette >= max(kc$full_silhouette, na.rm = TRUE) - .01]
check(cm$selected_k == 6 && cm$selected_k %in% near_best && min(pr$n) >= .02 * nrow(raw),
      "Profile-reviewed k=6 is near-best statistically and all groups meet size criterion")
check(file.exists(file.path(ROOT, "tables", "02_shortlist_profile_comparison.csv")), "Candidate profile comparison recorded")


# ====================================================================
# 6.06 核对切分与模型来源
# 来源：verify_results.R:58
# 对照模型行号、保存数据和切分表。相同输入组合不能跨训练测试或交叉验证折。检查主输入排除风险变量，训练模型仅用训练行、重拟合用全部行。这些检查保护测试表现的含义。
model <- readRDS(file.path(ROOT, "models", "regression.rds"))
dat <- read.csv(file.path(ROOT, "data", "analysis_with_clusters.csv"))
split <- readtab("03_partition_assignment")
tr <- split$partition == "train"; te <- !tr
check(sum(tr) + sum(te) == nrow(raw) && !anyDuplicated(split$row_id), "Training and test split covers each row once")
check(!length(intersect(split$predictor_group[tr], split$predictor_group[te])), "Identical primary input patterns do not cross train/test")
check(all(tapply(split$cv_fold[tr], split$predictor_group[tr], function(x) length(unique(x))) == 1), "Identical primary patterns do not cross CV folds")
check(identical(dat$row_id[te], model$test_row_ids) && identical(dat$row_id[tr], model$train_row_ids), "Saved model and split rows agree")
check(!any(c("Income", "Yield", "Cat_Age", "Cluster") %in% all.vars(model$formula)[-1]), "Primary regression excludes provenance-risk variables and cluster labels")
check(nobs(model$training_model) == sum(tr) && nobs(model$final_model) == nrow(raw), "Training model distinct from all-data refit")

# ====================================================================
# 6.07 重算预测与误差
# 来源：verify_results.R:68
# 独立构建 X 并乘保存系数，而不是再次调用 predict() 来核对
# CLV。逐条比对预测，确认测试结果从未截尾。由误差向量重算
# RMSE、MAE、R²，并确认可部署基准只用训练结果。
pp <- readtab("03_test_predictions")
X <- model.matrix(delete.response(terms(model$training_model)), dat[te, ])
manual_prediction <- as.vector(X %*% coef(model$training_model))
check(eq(pp$CLV_pred, manual_prediction), "Saved CLV predictions equal independent X beta calculation")
check(eq(pp$CLV, raw$CLV[te]) && eq(pp$Yield, raw$Yield[te]), "Test outcomes were not capped or changed")
test_met <- readtab("03_primary_test_metrics")
e <- raw$CLV[te] - manual_prediction
check(eq(test_met$RMSE[2], sqrt(sum(e^2) / length(e))) && eq(test_met$MAE[2], sum(abs(e)) / length(e)), "Independent test RMSE and MAE")
check(eq(test_met$R2[2], 1 - sum(e^2) / sum((raw$CLV[te] - mean(raw$CLV[te]))^2)), "Independent test R squared")
baseline <- mean(raw$CLV[tr])
check(eq(test_met$RMSE[1], sqrt(mean((raw$CLV[te] - baseline)^2))), "Baseline uses training mean only")
check(all(is.finite(pp$CLV_pred)) && all(is.finite(pp$Yield_pred)), "All test predictions finite")

# ====================================================================
# 6.08 核对补充结果与训练规则
# 来源：verify_results.R:80
# 重算 Yield 预测及 RMSE，核对阈值仅来自训练行，再独立应用 1%
# 内选简单模型的交叉验证规则。验证结果遵守选择计划，而不是从测试误差挑模型。
ym <- readtab("03_Yield_test_metrics")
yp <- predict(model$yield_training_model, dat[te, ])
check(eq(yp, pp$Yield_pred) && eq(ym$RMSE[2], sqrt(mean((raw$Yield[te] - yp)^2))), "Independent Yield predictions and RMSE")
limits <- readtab("03_training_only_caps")
check(eq(limits$train_cap, c(quantile(raw$No_Trans[tr], .99), quantile(raw$CLV[tr], .99))), "Sensitivity caps derived from training only")
cv <- readtab("03_CV_model_comparison")
eligible <- cv[cv$pooled_RMSE <= min(cv$pooled_RMSE) * 1.01, ]
check(model$selected_name == eligible$model[order(eligible$parameters, eligible$pooled_RMSE)][1], "Primary model selected by recorded training CV rule")

# ====================================================================
# 6.09 独立核验 HC3
# 来源：verify_results.R:88
# 基础 R 的 dfbeta() 给出删除单条观察相关的系数变化。在这里的 OLS
# 恒等式下，各系数变化平方和等于 HC3
# 方差对角线。开方后比较，独立检验自写三明治函数。还检查区间是否包住点估计。
ct <- readtab("03_primary_coefficients_HC3")
delete_one <- dfbeta(model$training_model)
independent_hc3_se <- sqrt(colSums(delete_one^2))
check(eq(ct$HC3_se, independent_hc3_se, 1e-7), "HC3 standard errors checked against base R delete-one influence identity")
check(all(ct$HC3_low95 <= ct$estimate & ct$HC3_high95 >= ct$estimate), "Coefficient intervals ordered")


# ====================================================================
# 6.10 核对实际交付物
# 来源：verify_results.R:96
# 要求至少三条建议，每条都有分群和模型证据。检查报告与至少十二张非空 PNG，读取每张前八字节核验
# PNG 签名，而不是只信扩展名。这是文件完整性检查，不能代替实际看图。
rec <- readtab("04_business_recommendations")
check(nrow(rec) >= 3 && all(nchar(rec$cluster_evidence) > 0) && all(nchar(rec$model_evidence) > 0), "At least three recommendations with both evidence sources")
check(file.exists(file.path(ROOT, "analysis_report.html")), "Readable HTML report exists")
imgs <- list.files(file.path(ROOT, "plots"), full.names = TRUE, pattern = "[.]png$")
check(length(imgs) >= 12 && all(file.info(imgs)$size > 5000), "All diagnostic and analysis PNGs exist and are nonempty")
for (f in imgs) {
  con <- file(f, "rb"); signature <- readBin(con, "raw", n = 8); close(con)
  check(identical(signature, as.raw(c(137,80,78,71,13,10,26,10))), paste("Valid PNG signature:", basename(f)))
}

# ====================================================================
# 6.11 检查链接并结束
# 来源：verify_results.R:105
# 提取 HTML 的本地 href/src
# 路径并确认可打开，再查无效数字占位符，打印通过数量并关闭日志。本数据成功运行应报告 63
# 项通过。任何断言失败都应查原因，不能为了通过而删检查。
html <- paste(readLines(file.path(ROOT, "analysis_report.html"), warn = FALSE), collapse = "\n")
paths <- regmatches(html, gregexpr('(?:href|src)="([^"#]+)"', html, perl = TRUE))[[1]]
paths <- sub('^(?:href|src)="', "", paths, perl = TRUE); paths <- sub('"$', "", paths)
paths <- paths[!grepl("^https?://", paths)]
check(all(file.exists(file.path(ROOT, paths))), "Every local report link resolves")
check(!grepl("NaN|Inf</td>|undefined", html), "No invalid numeric placeholders in report")
cat("\n", checks, "checks passed.\n", sep = "")
cat("Verified:", format(Sys.time()), "\n")
sink(); close(log)
# END ORIGINAL MODULE: verify_results.R
# 完成后先看
# logs/verification.txt，再读结果表与图。原分析保留为正式参照，学习版负责解释与复现。交付核验摘要会报告数值
# CSV 是否完全一致。
