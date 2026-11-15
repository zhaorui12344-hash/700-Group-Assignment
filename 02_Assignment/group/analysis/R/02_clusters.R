# Part 2. W7: standardised k-means, k comparison, all-variable profiles, labels.
# Primary choice: retain ambiguous recorded zeros; cap only upper 1% of Income and
# No_Trans for distance calculations. This is not deletion or a claim of error.
cluster_vars <- c("Tot_Prod", "Income", "No_Trans", "Age")
stopifnot(!any(c("Yield", "CLV", "Cat_Age") %in% cluster_vars))
cluster_input <- d[cluster_vars]
caps <- sapply(cluster_input[c("Income", "No_Trans")], q, p = .99)
for (v in names(caps)) cluster_input[[v]] <- pmin(cluster_input[[v]], caps[v])
tab(data.frame(variable = names(caps), upper_cap = caps,
               changed_rows = sapply(names(caps), function(v) sum(d[[v]] > caps[v]))), "02_cluster_input_caps")
Z <- scale(cluster_input)
stopifnot(max(abs(colMeans(Z))) < 1e-10, max(abs(apply(Z, 2, sd) - 1)) < 1e-10)
tab(cor(cluster_input), "02_cluster_input_correlations", TRUE)
set.seed(7003)
sil_sample <- sort(sample(seq_len(nrow(d)), 2500))
sample_dist <- dist(Z[sil_sample, ])
fits <- list(); candidate_rows <- list()
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
candidates <- do.call(rbind, candidate_rows); rownames(candidates) <- NULL
# Shortlist the best two feasible scores; exact full-data silhouette confirms selection.
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
best <- max(candidates$full_silhouette, na.rm = TRUE)
near_best <- candidates$k[!is.na(candidates$full_silhouette) & candidates$full_silhouette >= best - .01]
# Analyst profile review: k=2 is a useful broad split but merges single-product
# high-transaction and older profitable customers. k=6 retains these distinctions,
# with every group >=470 records and exact silhouette only 0.0042 lower.
# This choice follows the assignment's explicit profile-based selection requirement.
PROFILE_REVIEW_K <- 6L
stopifnot(PROFILE_REVIEW_K %in% near_best)
selected_k <- PROFILE_REVIEW_K
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
km <- fits[[as.character(selected_k)]]
# Sort numeric labels by mean Age, then mean recorded Income, for reproducible narration.
raw_profile <- profile(d, km$cluster, numeric_vars)
order_old <- as.integer(raw_profile$group[order(raw_profile$Age_mean, raw_profile$Income_mean)])
cluster_id <- match(km$cluster, order_old)
d$Cluster <- factor(cluster_id, levels = seq_len(selected_k))
centers <- km$centers[order_old, , drop = FALSE]
rownames(centers) <- seq_len(selected_k)
cluster_profile <- profile(d, cluster_id, numeric_vars)
names(cluster_profile)[1] <- "Cluster"
cluster_profile$Yield_total <- as.numeric(tapply(d$Yield, d$Cluster, sum))
cluster_profile$CLV_total <- as.numeric(tapply(d$CLV, d$Cluster, sum))
cluster_profile$Yield_share <- cluster_profile$Yield_total / sum(d$Yield)
cluster_profile$CLV_share <- cluster_profile$CLV_total / sum(d$CLV)
# Names use observed distinguishing attributes, never unobserved products or occupations.
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
chosen_sil <- exact_sil[[as.character(selected_k)]]
sil_summary <- data.frame(k = selected_k, exact_mean = mean(chosen_sil[, 3]),
  negative_share = mean(chosen_sil[, 3] < 0), sample_n = length(sil_sample),
  selected_rule = "Profile-reviewed k=6 within 0.01 of best exact shortlist; all groups >=2%; preserve age/activity distinctions")
tab(sil_summary, "02_selected_quality")
rm(full_dist, exact_sil, sample_dist); invisible(gc())

# Stability of the same preprocessing under other random starts.
seed_stability <- do.call(rbind, lapply(c(11, 23, 47, 101, 2026), function(seed) {
  set.seed(seed); other <- kmeans(Z, selected_k, nstart = 30, iter.max = 200, algorithm = "Lloyd")
  stopifnot(kmeans_converged(other))
  data.frame(seed = seed, adjusted_rand = adjusted_rand(cluster_id, other$cluster), within_ss = other$tot.withinss)
}))
tab(seed_stability, "02_seed_stability")

# Sensitivity, not alternate hidden final answers: raw input; compressed income;
# treating ambiguous zeros as missing; omit Income entirely.
alternatives <- list(raw_no_caps = d[cluster_vars], capped_log_income = cluster_input,
                     zeros_as_missing_imputed = cluster_input,
                     exclude_income = cluster_input[setdiff(cluster_vars, "Income")])
alternatives$capped_log_income$Income <- log1p(alternatives$capped_log_income$Income)
positive_age_median <- tapply(d$Income[d$Income > 0], d$Cat_Age[d$Income > 0], median)
imputed <- pmin(positive_age_median[as.character(d$Cat_Age[d$Income == 0])], caps["Income"])
alternatives$zeros_as_missing_imputed$Income[d$Income == 0] <- imputed
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

# Passive outcomes only: differences describe sample profiles, not causal effects.
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
saveRDS(list(kmeans = km, selected_k = selected_k, centers_relabelled = centers,
  center = attr(Z, "scaled:center"), scale = attr(Z, "scaled:scale"), caps = caps,
  variables = cluster_vars, cluster = cluster_id, names = cluster_names, label_map = order_old),
  file.path(ROOT, "models", "clustering.rds"))
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
