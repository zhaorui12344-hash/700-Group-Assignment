# BUSINFO 700 · Group Assignment 3

**English** | [简体中文](README.zh-CN.md)

Customer segmentation and profitability: course materials, complete R analysis, bilingual learning materials and final presentations.

## Start here

- **Understand the results:** [English learning guide](04_Learning_Guides/Assignment3_Learning_Guide_English.docx) · [Chinese learning guide](04_Learning_Guides/Assignment3_学习指南_中文版.docx). Each has 20 lessons across 29 pages, progressing from basic concepts to findings and limitations.
- **Learn the code:** [English annotated R](03_R_Learning_Edition/Assignment3_Learning_English.R) · [Chinese annotated R](03_R_Learning_Edition/Assignment3_Learning_Chinese.R). Each has 86 teaching blocks covering the complete analysis and verification. Run either language edition.
- **Present:** [English PPT](05_Final_Presentations/Assignment3_Final_Presentation_English.pptx) · [Chinese PPT](05_Final_Presentations/Assignment3_最终演示_中文版.pptx). Each has 10 main slides, 47 backup slides and speaker notes, including complete original R code. The main presentation is planned for approximately 10 minutes; appendices support questions and technical discussion.
- **Inspect the original analysis:** [Original R entry point](02_Assignment/group/analysis/run_analysis.R) · [Results report](02_Assignment/group/analysis/analysis_report.html) · [Detailed result tables](02_Assignment/group/analysis/tables). Download the repository and open the HTML report locally in a browser to view its figures.

## Folder map

| Folder | Contents |
|---|---|
| `01_Course_Materials` | All supplied slides, cases, exercises and solutions for W3, W4, W6, W7 and W8 |
| `02_Assignment` | Assignment brief, single source CSV, original R code and authoritative analysis results |
| `03_R_Learning_Edition` | English and Chinese annotated R scripts, instructions and validation records |
| `04_Learning_Guides` | Beginner Word guides in English and Chinese |
| `05_Final_Presentations` | Final English and Chinese PPTs only |
| `06_Preliminary_Analysis` | Earlier Python exploration and its outputs |
| `07_Reproducibility_and_QA` | File inventory, deduplication log, validation and document-authoring sources |

## Run

From the repository root, run either learning edition:

```sh
Rscript --vanilla 03_R_Learning_Edition/Assignment3_Learning_English.R
Rscript --vanilla 03_R_Learning_Edition/Assignment3_Learning_Chinese.R
```

Requires R and its recommended `cluster` package. If absent, install it from R with `install.packages("cluster")`. The scripts do not install software. Outputs are isolated under `03_R_Learning_Edition/runs/English/` or `runs/Chinese/`, which Git ignores, and do not overwrite the authoritative analysis. Keep the directory structure: the learning scripts reuse the data and original code under `02_Assignment/group/` without duplicate copies.

Reproduce the original analysis independently:

```sh
Rscript --vanilla 02_Assignment/group/analysis/run_analysis.R
Rscript --vanilla 02_Assignment/group/analysis/verify_results.R
```

## Verification and interpretation

Both learning scripts completed execution and passed 63 checks each. All 79 result tables and two data CSVs match the original analysis byte for byte. Executable original modules match except for declared source-path adaptations. Both guides and decks were rendered and visually reviewed page by page.

Recorded CLV is an existing estimate, not observed future profit. Cluster differences and regression coefficients describe associations, not causal returns from marketing actions. Six clusters trade additional complexity for richer profiles; two remain a reasonable alternative.

The original report and entry instructions contain no-slides wording from the earlier analysis phase. Final PPTs now appear in `05_Final_Presentations`. Learning-generated reports include corrected rerun instructions at the top.

## Deduplication and authoring files

All unique original course materials and current assignment deliverables are retained. Duplicate archives, previous packaged copies, repeated run outputs, preview screenshots, intermediate drafts and runtime caches are omitted. [FILE_INDEX.csv](07_Reproducibility_and_QA/FILE_INDEX.csv) lists files and their SHA-256 hashes; [ORGANIZATION_LOG.csv](07_Reproducibility_and_QA/ORGANIZATION_LOG.csv) documents sources and deduplication. English and Chinese editions are distinct deliverables.

Document-authoring scripts in `07_Reproducibility_and_QA` preserve the production record and may depend on the original authoring workspace and runtime. Use the R entry commands above to reproduce the statistical analysis.
