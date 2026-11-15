#!/usr/bin/env Rscript
# Independent numerical and file-integrity checks. Run after run_analysis.R.
args <- commandArgs(trailingOnly = FALSE)
entry <- sub("^--file=", "", args[grepl("^--file=", args)])
ROOT <- if (length(entry)) dirname(normalizePath(entry[1])) else normalizePath(getwd())
log <- file(file.path(ROOT, "logs", "verification.txt"), "wt")
sink(log, split = TRUE)
checks <- 0L
check <- function(condition, label) {
  if (!isTRUE(condition)) stop(paste("FAIL:", label))
  checks <<- checks + 1L; cat("PASS:", label, "\n")
}
eq <- function(a, b, tolerance = 1e-8) isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = tolerance))
readtab <- function(name) read.csv(file.path(ROOT, "tables", paste0(name, ".csv")), check.names = FALSE)
raw <- read.csv(file.path(ROOT, "..", "Group_presentation.csv"))
check(identical(unname(tools::md5sum(file.path(ROOT, "..", "Group_presentation.csv"))),
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
kc <- readtab("02_k_comparison")
check(identical(as.integer(kc$k), 2:8) && sum(!is.na(kc$full_silhouette)) == 2, "Seven k candidates and two exact silhouette checks")
near_best <- kc$k[!is.na(kc$full_silhouette) & kc$full_silhouette >= max(kc$full_silhouette, na.rm = TRUE) - .01]
check(cm$selected_k == 6 && cm$selected_k %in% near_best && min(pr$n) >= .02 * nrow(raw),
      "Profile-reviewed k=6 is near-best statistically and all groups meet size criterion")
check(file.exists(file.path(ROOT, "tables", "02_shortlist_profile_comparison.csv")), "Candidate profile comparison recorded")

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
ym <- readtab("03_Yield_test_metrics")
yp <- predict(model$yield_training_model, dat[te, ])
check(eq(yp, pp$Yield_pred) && eq(ym$RMSE[2], sqrt(mean((raw$Yield[te] - yp)^2))), "Independent Yield predictions and RMSE")
limits <- readtab("03_training_only_caps")
check(eq(limits$train_cap, c(quantile(raw$No_Trans[tr], .99), quantile(raw$CLV[tr], .99))), "Sensitivity caps derived from training only")
cv <- readtab("03_CV_model_comparison")
eligible <- cv[cv$pooled_RMSE <= min(cv$pooled_RMSE) * 1.01, ]
check(model$selected_name == eligible$model[order(eligible$parameters, eligible$pooled_RMSE)][1], "Primary model selected by recorded training CV rule")
ct <- readtab("03_primary_coefficients_HC3")
# Independent HC3 identity: sum of outer products of exact delete-one coefficient
# changes, returned by base R dfbeta(), equals the HC3 sandwich covariance.
delete_one <- dfbeta(model$training_model)
independent_hc3_se <- sqrt(colSums(delete_one^2))
check(eq(ct$HC3_se, independent_hc3_se, 1e-7), "HC3 standard errors checked against base R delete-one influence identity")
check(all(ct$HC3_low95 <= ct$estimate & ct$HC3_high95 >= ct$estimate), "Coefficient intervals ordered")

rec <- readtab("04_business_recommendations")
check(nrow(rec) >= 3 && all(nchar(rec$cluster_evidence) > 0) && all(nchar(rec$model_evidence) > 0), "At least three recommendations with both evidence sources")
check(file.exists(file.path(ROOT, "analysis_report.html")), "Readable HTML report exists")
imgs <- list.files(file.path(ROOT, "plots"), full.names = TRUE, pattern = "[.]png$")
check(length(imgs) >= 12 && all(file.info(imgs)$size > 5000), "All diagnostic and analysis PNGs exist and are nonempty")
for (f in imgs) {
  con <- file(f, "rb"); signature <- readBin(con, "raw", n = 8); close(con)
  check(identical(signature, as.raw(c(137,80,78,71,13,10,26,10))), paste("Valid PNG signature:", basename(f)))
}
html <- paste(readLines(file.path(ROOT, "analysis_report.html"), warn = FALSE), collapse = "\n")
paths <- regmatches(html, gregexpr('(?:href|src)="([^"#]+)"', html, perl = TRUE))[[1]]
paths <- sub('^(?:href|src)="', "", paths, perl = TRUE); paths <- sub('"$', "", paths)
paths <- paths[!grepl("^https?://", paths)]
check(all(file.exists(file.path(ROOT, paths))), "Every local report link resolves")
check(!grepl("NaN|Inf</td>|undefined", html), "No invalid numeric placeholders in report")
cat("\n", checks, "checks passed.\n", sep = "")
cat("Verified:", format(Sys.time()), "\n")
sink(); close(log)
