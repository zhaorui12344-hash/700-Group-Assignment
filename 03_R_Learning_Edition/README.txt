R 学习版文件 / R Learning Edition

这两份文件是同一套完整分析的教学版本，区别是注释语言。
These are two teaching editions of the same complete analysis; only the comment language differs.

1. Assignment3_Learning_Chinese.R — 中文逐段讲解。
2. Assignment3_Learning_English.R — detailed English block-by-block explanations.

每份覆盖：路径与日志、R 基础语法、数据核查、单变量、双变量、k-means、稳定性、线性回归、交叉验证、诊断、结果报告和独立核验。
Each covers R syntax, data audit, univariate/bivariate analysis, k-means, stability, regression, cross-validation, diagnostics, reporting and independent verification.

运行任意一份即可 / Run either edition:
Rscript --vanilla Assignment3_Learning_Chinese.R
Rscript --vanilla Assignment3_Learning_English.R

RStudio：将工作目录设为本文件所在目录，打开其中一份 R 文件，点击 Source。
In RStudio, set the working directory to this folder, open one R file and click Source.
需要 R 和推荐包 cluster；通常安装 R 时已有。若缺少，运行 install.packages("cluster")。
Requires R and the recommended cluster package, normally installed with R. If absent, run install.packages("cluster").

输出：runs/Chinese/ 或 runs/English/。请先读 logs/verification.txt，再看 analysis_report.html、tables/、plots/。
Outputs: runs/Chinese/ or runs/English/. Start with logs/verification.txt, then analysis_report.html, tables/ and plots/.

目录说明 / Folder layout:
独立本地版：data/ 保存源 CSV，reference_original/ 保存未改动的参考代码。
分类仓库版：为避免重复，脚本自动复用 ../02_Assignment/group/ 的数据及 analysis/ 原代码，不另存副本。
Standalone local edition: data/ contains the CSV; reference_original/ contains unchanged reference code.
Classified repository: the script reuses ../02_Assignment/group/ data and analysis/ code without duplicate copies.
请保持所提供的目录结构 / Keep the supplied folder structure.

注意：运行会更新所选语言 runs 子目录中的旧输出，不会覆盖原分析或原始数据。
A rerun replaces prior outputs inside that language's runs folder only; original analysis and input data remain unchanged.
不要单独运行 reference_original 或 runs 中复制的原脚本；它们供阅读对照。请使用上面学习脚本命令。
Do not execute the reference scripts copied into reference_original or runs. Use the learning entry command above.

注释以外的原模块已做 R 语法树比较；除数据路径适配外，统计计算完全一致。
Executable module expressions were compared with the originals; only declared source-path adaptations differ.
英文文件仍包含原报告中的中文字串，这是保持原输出一致所需；教学注释使用英文。
The English script retains Chinese strings from the original report to preserve original outputs; teaching comments are English.
原 HTML 里“本阶段不做 PPT”是早期阶段记录；最终中英文 PPT 已单独交付。
The original HTML's no-slides statement records an earlier stage; final bilingual PPTs are delivered separately.
