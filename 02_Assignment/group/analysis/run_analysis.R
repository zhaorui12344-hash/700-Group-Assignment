#!/usr/bin/env Rscript
# Assignment 3: complete, reproducible analysis. No slides are generated.
# Run: Rscript --vanilla /absolute/path/to/group/analysis/run_analysis.R
# Only base/recommended R packages are required (cluster ships with R).
args <- commandArgs(trailingOnly = FALSE)
entry <- sub("^--file=", "", args[grepl("^--file=", args)])
ROOT <- if (length(entry)) dirname(normalizePath(entry[1])) else normalizePath(getwd())
options(stringsAsFactors = FALSE, scipen = 5, warn = 1)
for (p in c("tables", "plots", "models", "logs", "data"))
  dir.create(file.path(ROOT, p), recursive = TRUE, showWarnings = FALSE)
log_con <- file(file.path(ROOT, "logs", "analysis_log.txt"), "wt")
sink(log_con, split = TRUE)
cat("Assignment 3 analysis\nStarted:", format(Sys.time()), "\n")
if (!requireNamespace("cluster", quietly = TRUE))
  stop("Install the recommended R package 'cluster' before running.")
source(file.path(ROOT, "R", "00_helpers.R"), encoding = "UTF-8")
for (module in c("01_data_eda.R", "02_clusters.R", "03_models.R", "04_report.R")) {
  cat("\nMODULE:", module, "\n")
  source(file.path(ROOT, "R", module), encoding = "UTF-8")
}
capture.output(sessionInfo(), file = file.path(ROOT, "logs", "sessionInfo.txt"))
cat("\nCompleted:", format(Sys.time()), "\n")
sink()
close(log_con)
