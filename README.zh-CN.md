# BUSINFO 700 · 小组作业 3

[English](README.md) | **简体中文**

客户细分与盈利分析：课堂资料、完整 R 分析、中英文学习材料及最终演示。

## 先从哪里看

- **学懂结果：**[中文学习指南](04_Learning_Guides/Assignment3_学习指南_中文版.docx) · [英文学习指南](04_Learning_Guides/Assignment3_Learning_Guide_English.docx)。各 20 课、29 页，从基础概念逐步讲到结果与限制。
- **阅读代码：**[中文详注 R](03_R_Learning_Edition/Assignment3_Learning_Chinese.R) · [英文详注 R](03_R_Learning_Edition/Assignment3_Learning_English.R)。每份包含 86 个教学板块，覆盖完整计算与核验；只需选择一种语言运行。
- **正式演示：**[中文版 PPT](05_Final_Presentations/Assignment3_最终演示_中文版.pptx) · [英文版 PPT](05_Final_Presentations/Assignment3_Final_Presentation_English.pptx)。每份 10 张主讲页、47 张技术附录，包含讲者备注和完整原始 R 代码。主讲约 10 分钟，附录供问答与技术讨论。
- **核对原分析：**[原始 R 入口](02_Assignment/group/analysis/run_analysis.R) · [结果报告](02_Assignment/group/analysis/analysis_report.html) · [详细结果表](02_Assignment/group/analysis/tables)。请下载仓库后，在浏览器打开 HTML 报告，以显示本地图表。

## 分类目录

| 文件夹 | 内容 |
|---|---|
| `01_Course_Materials` | W3、W4、W6、W7、W8 的全部原课堂 PPT、案例、练习与答案 |
| `02_Assignment` | 作业要求、唯一原始 CSV、原 R 代码与正式分析结果 |
| `03_R_Learning_Edition` | 中英文逐段注释 R、运行说明与核验记录 |
| `04_Learning_Guides` | 中英文零基础 Word 学习指南 |
| `05_Final_Presentations` | 仅保留最终中英文 PPT |
| `06_Preliminary_Analysis` | 建模之前的 Python 探索及其输出 |
| `07_Reproducibility_and_QA` | 文件清单、去重记录、核验结果和文档制作源文件 |

## 运行

在仓库根目录打开终端，选择以下一种学习版命令：

```sh
Rscript --vanilla 03_R_Learning_Edition/Assignment3_Learning_Chinese.R
Rscript --vanilla 03_R_Learning_Edition/Assignment3_Learning_English.R
```

需要 R 和推荐包 `cluster`。若缺少，在 R 中运行 `install.packages("cluster")`。脚本本身不安装软件。新输出位于 `03_R_Learning_Edition/runs/Chinese/` 或 `runs/English/`，已由 Git 忽略，不会覆盖正式分析。请保持目录结构：学习版直接复用 `02_Assignment/group/` 的数据和原代码，避免重复存放。

原分析可独立复现：

```sh
Rscript --vanilla 02_Assignment/group/analysis/run_analysis.R
Rscript --vanilla 02_Assignment/group/analysis/verify_results.R
```

## 核验与阅读边界

两份学习 R 都实际完成全流程，各通过 63 项检查；79 个结果表和 2 个数据 CSV 与原分析逐字节一致。除明确的数据路径适配外，原模块的可执行表达式保持一致。中英文指南和 PPT 均完成逐页渲染检查。

CLV 是数据中已有的估计标签，不是真实未来利润；分群与回归描述关联，不证明营销行动的因果收益。六群是画像与复杂度之间的取舍，两群仍是合理备选。

原分析报告和入口说明中的“本阶段不做 PPT”属于早期分析阶段记录。最终 PPT 现在位于 `05_Final_Presentations`。学习版报告顶部已提供正确的学习版运行命令。

## 去重与制作文件

保留全部独立课堂原件及现行作业成果。重复压缩包、旧打包副本、重复运行输出、预览截图、草稿和运行缓存不上传。[FILE_INDEX.csv](07_Reproducibility_and_QA/FILE_INDEX.csv) 列出文件及 SHA-256；[ORGANIZATION_LOG.csv](07_Reproducibility_and_QA/ORGANIZATION_LOG.csv) 说明来源与去重。英文版、中文版属于不同语言成品，不是重复副本。

`07_Reproducibility_and_QA` 中的文档制作脚本保留制作记录，部分依赖原制作工作区与工具环境；不属于统计分析运行入口。复现统计分析请使用上方 R 命令。
