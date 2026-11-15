from pathlib import Path
import textwrap,json,hashlib,shutil
from annotations import NOTES
B=Path(__file__).resolve().parent; OUT=B.parent; A=OUT.parent/'analysis'
INTRO={
'en':'''Assignment 3 — R learning edition (English comments)
Read one numbered block at a time. This is a complete executable script, not pseudocode.
It runs the same analyses, generates the original Chinese HTML report, and runs the same independent checks.
The other learning file differs in COMMENT LANGUAGE only. Do not run both to obtain two statistical methods.
Original variable names, CSV labels and report strings remain unchanged for exact result comparison.

RUN: Rscript --vanilla Assignment3_Learning_English.R
RStudio: open this file, set the working directory to this file's folder, then click Source.
Keep the supplied folder structure. Standalone copies use data/ and reference_original/; the classified repository reuses ../02_Assignment/group/ without duplicate files. Outputs go to runs/English/.
Prerequisite: R and its recommended cluster package; install.packages("cluster") only if missing.
This script does not install software or access the network. Full distance calculations need memory.

FIRST R CONCEPTS
# starts a comment; R ignores it. <- stores a value in a named object.
c() combines values; data.frame() is a rectangular table; list() can hold unlike objects.
d$Age extracts one named column. d[rows, columns] selects cells; a blank means all.
d[[v]] extracts the column whose name is stored in v. R positions start at 1.
== compares values; = names a function argument; ! reverses TRUE/FALSE; & means both.
%in% tests membership; ~ describes a model formula; ^ means a power; %*% is matrix multiplication.
NA means unavailable, not zero. is.na() checks it. NULL means absence of an object/component.
function() defines a reusable recipe; for() repeats steps; lapply() applies a recipe to each item.
A pair of braces groups steps. Run the entire balanced block, not half of a loop or function.
A seed fixes random choices for reproduction; it does not certify statistical truth.

READING ROUTE
0. Paths and logging. 1. Helpers. 2. Data/EDA (W3/W4/W6). 3. Clustering (W7).
4. Regression/validation (W8 plus justified extensions). 5. Evidence report. 6. Independent verification.
Start with the bilingual Word guide if z-score, confidence interval or R2 is unfamiliar.
The # SOURCE lines point back to the unchanged original source and its line number.
Within each module every line lies in a teaching block; several related operations may share a block.
''',
'zh':'''Assignment 3 — R 学习版（中文注释）
每次阅读一个编号板块。这是一份可完整运行的脚本，不是伪代码。
它执行相同分析、生成原中文版 HTML 报告，并完成同一套独立检查。
另一个学习文件仅注释语言不同，不代表另一套统计方法，不必为了分析重复运行两份。
原始变量名、CSV 标签及报告文字保持不变，方便逐项比对结果。

运行：Rscript --vanilla Assignment3_Learning_Chinese.R
RStudio：打开文件，把工作目录设为本文件所在目录，再点击 Source。
请保持所提供的目录结构。独立版使用旁边 data/ 与 reference_original/；分类仓库直接复用 ../02_Assignment/group/，避免重复文件。输出位于 runs/Chinese/。
需要 R 及推荐包 cluster；只有缺少时才用 install.packages("cluster") 安装。
本脚本不会安装软件或访问网络。完整距离计算需要一定内存。

先认识 R 语法
# 开头是注释，R 不执行；<- 把右边结果存入左边对象。
c() 组合数值，data.frame() 是矩形数据表，list() 可以容纳不同类型对象。
d$Age 取名为 Age 的列；d[行,列] 选区域，留空表示全部。
d[[v]] 取名称保存在 v 中的列；R 的位置从 1 开始计数。
== 是比较，= 常用于指定参数；! 表示取反，& 表示同时满足。
%in% 判断是否属于某个集合，~ 表示模型公式，^ 表示乘方，%*% 表示矩阵相乘。
NA 是不可用，不是零；is.na() 检查它。NULL 表示对象或部分不存在。
function() 定义可复用步骤，for() 重复操作，lapply() 对每项执行同一函数。
花括号把步骤包成整体；不要只运行半个循环或半个函数。
随机种子固定随机选择以便复现，不能保证结果就是真理。

阅读顺序
0 路径与日志；1 工具；2 数据与探索（W3/W4/W6）；3 分群（W7）；
4 回归与验证（W8 加有用途的补充）；5 证据报告；6 独立核验。
如果还不认识标准分数、置信区间或 R²，可先阅读配套 Word 学习指南。
每段的来源行号可以对照未改动的原始代码；每行都归属一个教学板块。
一个板块可包含多个紧密相关的操作，请连同输入与输出一起理解。
'''}
BOOT='''args <- commandArgs(trailingOnly = FALSE)
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
ROOT <- file.path(LEARN_DIR, "runs", "LANGDIR")
options(stringsAsFactors = FALSE, scipen = 5, warn = 1)
for (p in c("tables", "plots", "models", "logs", "data", "R"))
  dir.create(file.path(ROOT, p), recursive = TRUE, showWarnings = FALSE)
if (!requireNamespace("cluster", quietly = TRUE))
  stop("The recommended R package 'cluster' is required. Install it before running.")
log_con <- file(file.path(ROOT, "logs", "analysis_log.txt"), "wt")
sink(log_con, split = TRUE)
cat("Assignment 3 learning edition\\nStarted:", format(Sys.time()), "\\n")
'''
def comment(text,lang):
 width=96 if lang=='en' else 50
 return '\n'.join('# '+ln for para in text.split('\n') for ln in (textwrap.wrap(para,width=width,break_long_words=False,break_on_hyphens=False) or ['']))+'\n'
manifest=[]
for lang in ('en','zh'):
 langdir='English' if lang=='en' else 'Chinese'
 out=['#!/usr/bin/env Rscript\n',comment(INTRO[lang],lang)]
 out.append(comment('0. Locate this file, preserve the source and separate new outputs. Paths are relative to the script, not tied to one computer. sink records console output; split=TRUE also prints it. Do not interrupt halfway and rerun random blocks in a stale session.' if lang=='en' else '0. 找到脚本位置、保护源文件，并把新输出放进独立目录。路径相对脚本定位，不绑定某台电脑。sink 记录控制台输出，split=TRUE 同时显示。不要在半途打断后凭记忆乱序运行零散代码。',lang))
 out.append(BOOT.replace('LANGDIR',langdir))
 for index,file in enumerate(NOTES,1):
  if file=='verify_results.R':
   out.append(comment('Close the analysis log and preserve readable original-source links before checking the saved outputs. These copied original files are reference material; run this learning script to regenerate this folder. The HTML is the historical analysis-stage report; final presentation status is documented separately.' if lang=='en' else '核验之前先关闭分析日志，并复制原代码以保留报告中的查看链接。这些副本供阅读；重建本目录请运行当前学习脚本。HTML 是原分析阶段历史报告，最终演示状态另见交付说明。',lang))
   out.append('''capture.output(sessionInfo(), file = file.path(ROOT, "logs", "sessionInfo.txt"))
sink(); close(log_con)
for (f in c("run_analysis.R", "verify_results.R"))
  file.copy(file.path(REFERENCE_DIR, f), file.path(ROOT, f), overwrite = TRUE)
for (f in list.files(file.path(REFERENCE_DIR, "R"), full.names = TRUE))
  file.copy(f, file.path(ROOT, "R", basename(f)), overwrite = TRUE)
''')
   out.append(comment('Adapt only the generated learning-report instructions: the computations and original modules remain unchanged. Read the learning-edition banner instead of the historical original-runner commands.' if lang=='en' else '只修正学习版生成报告的运行说明；计算和原始模块保持不变。请按学习版横幅运行，不使用历史原入口命令。',lang))
   out.append("\nlearning_html_path <- file.path(ROOT, \"analysis_report.html\")\nlearning_html <- paste(readLines(learning_html_path, warn = FALSE, encoding = \"UTF-8\"), collapse = \"\\n\")\n")
   out.append('learning_html <- gsub(\'<pre>Rscript --vanilla run_analysis.R\\nRscript --vanilla verify_results.R</pre>\', \'<pre>Rscript --vanilla ../../Assignment3_Learning_'+langdir+'.R</pre>\', learning_html, fixed = TRUE)\n')
   out.append('learning_banner <- \'<aside class="note"><strong>R learning edition / R 学习版</strong><p>From this output folder, rerun: <code>Rscript --vanilla ../../Assignment3_Learning_'+langdir+'.R</code>. 从当前输出目录运行此命令即可完整复现并核验。Linked original scripts are historical reading references; do not run the copied originals here. 下方原脚本链接供对照阅读。The old no-slides statement records the earlier analysis stage; bilingual final PPTs are delivered separately. 原报告暂不做 PPT 的文字属于历史阶段，最终中英文 PPT 已另行提供。</p></aside>\'\n')
   out.append('learning_html <- sub("<main>", paste0("<main>", learning_banner), learning_html, fixed = TRUE)\nwriteLines(learning_html, learning_html_path, useBytes = TRUE)\n')
  src=A/('R/'+file if file!='verify_results.R' else file)
  lines=src.read_text().splitlines()
  # Only adapt source/output locations; all statistical expressions are preserved.
  if file=='01_data_eda.R':lines[1]='SOURCE <- normalizePath(SOURCE)'
  if file=='verify_results.R':
   lines[:5]=['']*5
   lines=[x.replace('file.path(ROOT, "..", "Group_presentation.csv")','SOURCE') for x in lines]
  out.append('\n# BEGIN ORIGINAL MODULE: '+file+'\n')
  notes={r[0]:r for r in NOTES[file]}
  for line_no,line in enumerate(lines,1):
   if line_no in notes:
    _,title,en,zh=notes[line_no]
    title=title.split(' / ')[0 if lang=='en' else 1]
    block_id=f'{index}.{list(notes).index(line_no)+1:02d}'
    out.append('\n'+comment('='*68+'\n'+block_id+' '+title+'\n'+('SOURCE: ' if lang=='en' else '来源：')+file+':'+str(line_no)+'\n'+(en if lang=='en' else zh),lang))
   if line.lstrip().startswith('#'):continue
   if lang=='zh':
    line=line.replace('# Row reference only; the source has no customer ID.','# 只是行号；源数据没有客户 ID。').replace('# fixed reference, not estimated from test data','# 固定参照值，不从测试集估计。')
   out.append(line+'\n')
  out.append('# END ORIGINAL MODULE: '+file+'\n')
 out.append(comment('Finished. Read logs/verification.txt first, then the tables and figures. Keep the original analysis as the authoritative reference; the learning edition explains and reproduces it. Exact numeric CSV equality is checked in the delivered validation summary.' if lang=='en' else '完成后先看 logs/verification.txt，再读结果表与图。原分析保留为正式参照，学习版负责解释与复现。交付核验摘要会报告数值 CSV 是否完全一致。',lang))
 p=OUT/('Assignment3_Learning_English.R' if lang=='en' else 'Assignment3_Learning_Chinese.R')
 p.write_text(''.join(out));print(p,len(p.read_text().splitlines()),'lines')
 for file,notes in NOTES.items():
  for i,r in enumerate(notes):manifest.append({'language':lang,'source':file,'start_line':r[0],'end_line':notes[i+1][0]-1 if i+1<len(notes) else len((A/('R/'+file if file!='verify_results.R' else file)).read_text().splitlines()),'title':r[1].split(' / ')[0 if lang=='en' else 1]})
(B/'commentary_map.json').write_text(json.dumps(manifest,ensure_ascii=False,indent=2))
