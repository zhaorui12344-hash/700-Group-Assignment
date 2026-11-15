# Generate a readable Chinese report from actual R objects, with linked evidence.
# No presentation slides are created. Plots use English to keep labels portable.
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
