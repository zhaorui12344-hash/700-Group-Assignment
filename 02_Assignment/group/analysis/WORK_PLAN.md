# Assignment 3 analysis implementation plan

> Execute inline with superpowers:executing-plans. The user has approved the full workflow and asks for uninterrupted autonomous execution. No further design approval is needed.

**Goal:** Complete and verify Assignment 3 Parts 1–4 in R, with reproducible outputs and a Chinese report, but no presentation slides.

**Architecture:** One entry script sources focused R modules for preparation, exploratory analysis, clustering, predictive modelling, verification and reporting. Original inputs remain unchanged. All findings are generated from actual model outputs.

**Tech stack:** Installed R 4.6.1, base/recommended R packages (stats, graphics, cluster). Add dependencies only if necessary.

**Spec:** ../assignment requirement.docx (four embedded screenshots, read in full); W3/W4/W6/W7/W8 lecture decks.

## Global constraints
- 10,000 source rows and 7 original variables; retain original CSV and document.
- Investigate ambiguous Income=0, outliers, Age/Cat_Age redundancy, and derived CLV including under-30 concerns.
- Cluster inputs exclude Yield and CLV; standardise numeric inputs; compare k, profile every original variable and give descriptive names.
- Explain and validate a linear regression or CART predicting CLV and/or Yield. Prioritise classroom linear regression unless evidence warrants otherwise.
- At least three recommendations link specific clusters and predictive-model evidence.
- R code and results must be suitable for future slide appendices; no PPT now.
- Do not mistake association, a constructed target, cluster separation or low p-values for causal or external validation.

## Review focus
1. Ambiguous zeros and legitimate negative profit must not be silently deleted or recoded as proven missing.
2. Target leakage, train/test preprocessing leakage and duplicated predictor rows across partitions must be prevented or transparently bounded.
3. Extreme observations, nonlinear age patterns, heteroscedasticity and misleading average profits must be checked.
4. All clusters, all original variables and all records must reconcile; cluster input significance is descriptive only.
5. Numeric claims in the report must match saved results; no unobserved future CLV or causal effects may be invented.

## Tasks
- [x] 1. Setup and EDA: create run_analysis.R and R/01_data_eda.R; audit schema, ranges, zeros, duplicates, univariate summaries, correlations, categorical comparisons and age-specific CLV patterns. Verify against the preliminary results and raw counts.
- [x] 2. Clustering: create R/02_clusters.R; compare k=2:8 using common sampled silhouettes, report exact silhouette for shortlisted solutions, profile and label the chosen solution, and compare preprocessing/seed alternatives. Verify exclusions, scaling, sizes and deterministic seeds.
- [x] 3. Prediction: create R/03_models.R; use a held-out test sample and training-only validation to compare parsimonious regression forms, assess derived-CLV sensitivity and a secondary Yield model, check residuals/VIF/influence and robust uncertainty. Verify split isolation, baseline comparisons and finite predictions.
- [x] 4. Findings: create R/04_report.R and analysis_report.html with linked plots/tables, evidence-linked recommendations, limitations, course mapping and requirement checklist. Inspect key plots and rendered report.
- [x] 5. Verification: create verify_results.R; independently recompute selected summaries and model metrics, check source hash and file inventory, rerun the final pipeline and compare deterministic tables. Obtain a fresh read-only review, resolve material findings, and deliver links.

## Execution ledger
- Environment: R and recommended packages cluster/rpart are already installed. Extra R packages are not needed for the initial implementation.
- Ruling: This is a standalone academic analysis folder, not a Git repository. Use an isolated analysis output directory rather than creating a repository/worktree or commits.
- Ruling: Use statistical invariants and independent numerical cross-checks rather than unit tests mirroring disposable analysis code; user asks for empirical verification.
- Ruling: Produce a local HTML analysis report with Chinese explanations and English plot labels, suitable for reading and later slide preparation.

## Final verification and review
- Completed in R 4.6.1 using base/recommended packages; no extra packages installed.
- Read-only independent reviewer checked numerical outputs, HC3 inference, split isolation and model choices. Addressed findings: Lloyd k-means may return NULL ifault (now check iteration/convergence correctly); added actual two-versus-six-cluster profile comparison; display small p-values as <0.001.
- Final k=6 retains meaningful age/product/activity groups, with exact silhouette 0.316279 versus k=2 at 0.320474 and smallest group 470. The report explains the tradeoff and exploratory status of outcome-guided interpretation.
- Independent verification: 63 checks passed, including source MD5, profile reconciliation, held-out metrics, HC3 and local artifact links.
- Complete clean rerun: 81 CSV outputs byte-identical. Final run has no warnings or errors.
- Visually inspected all 12 plots and rendered HTML. Fixed the adjusted-age figure caption overlap and the candidate-comparison table layout.
- W3/W4/W6/W7/W8 course mapping and all Parts 1–4 requirements documented in the report and CSV tables. No PPT produced.
- Superseded development models/logs and the temporary Rplots.pdf are preserved in logs/development_archive and excluded from the delivery ZIP.
