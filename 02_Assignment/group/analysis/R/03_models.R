# Part 3. W8 multiple linear regression; related bivariate evidence is in Part 1.
# Primary target: recorded CLV, to address the explicit request for CLV drivers.
# Exclude Income and Yield from the primary model because CLV is derived and both
# are strongly associated with it. This reduces direct formula reconstruction but
# cannot turn a constructed target into independently observed lifetime profit.
# Secondary Yield model provides a check on current-profit recommendations.
d$Age_c <- d$Age - 45 # fixed reference, not estimated from test data
d$Age_c2 <- d$Age_c^2
d$Age_after30 <- pmax(d$Age - 30, 0)

# Put identical primary-predictor patterns in the same partition/fold. There is
# no customer ID; this does not establish that customers themselves are independent.
key <- paste(d$Tot_Prod, d$No_Trans, d$Age, sep = "|")
keys <- unique(key)
key_age <- d$Under30[match(keys, key)]
set.seed(37008)
test_keys <- unlist(lapply(split(keys, key_age), function(x) sample(x, floor(.2 * length(x)))), use.names = FALSE)
is_test <- key %in% test_keys
train <- d[!is_test, ]; test <- d[is_test, ]
stopifnot(!length(intersect(key[!is_test], key[is_test])))
train_keys <- unique(key[!is_test])
set.seed(37009)
fold_map <- sample(rep(1:5, length.out = length(train_keys)))
names(fold_map) <- train_keys
fold <- unname(fold_map[key[!is_test]])
split_table <- data.frame(row_id = d$row_id, partition = ifelse(is_test, "test", "train"),
  cv_fold = ifelse(is_test, NA_integer_, fold_map[key]), predictor_group = key)
tab(split_table, "03_partition_assignment")
tab(profile(d, ifelse(is_test, "test", "train"), numeric_vars), "03_partition_profiles")
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
# Training-only selection. A gain below 1% does not justify an extra age term.
best_cv <- min(cv_summary$pooled_RMSE)
acceptable <- cv_summary[cv_summary$pooled_RMSE <= best_cv * 1.01, ]
selected_name <- acceptable$model[order(acceptable$parameters, acceptable$pooled_RMSE)][1]
primary_formula <- formulas[[selected_name]]
primary <- lm(primary_formula, train)
stopifnot(!any(c("Income", "Yield", "Cat_Age", "Cluster") %in% all.vars(primary_formula)[-1]))
primary_coef <- coef_table(primary)
primary_vif <- vif_table(primary)
primary_summary <- summary(primary)
test_pred <- predict(primary, test)
baseline_test <- metrics(test$CLV, rep(mean(train$CLV), nrow(test)), "Training_mean_baseline")
test_metrics <- rbind(baseline_test, metrics(test$CLV, test_pred, paste0("Primary_", selected_name)))
tab(cv, "03_CV_fold_metrics"); tab(cv_summary, "03_CV_model_comparison")
tab(test_metrics, "03_primary_test_metrics"); tab(primary_coef, "03_primary_coefficients_HC3")
tab(primary_vif, "03_primary_VIF")
fit_stats <- data.frame(model = selected_name, train_n = nrow(train), test_n = nrow(test),
  R2_train = primary_summary$r.squared, adjusted_R2 = primary_summary$adj.r.squared,
  residual_SE = primary_summary$sigma, F = unname(primary_summary$fstatistic[1]),
  F_df1 = unname(primary_summary$fstatistic[2]), F_df2 = unname(primary_summary$fstatistic[3]),
  F_p = pf(primary_summary$fstatistic[1], primary_summary$fstatistic[2],
           primary_summary$fstatistic[3], lower.tail = FALSE))
tab(fit_stats, "03_primary_training_fit")
capture.output(summary(primary), file = file.path(ROOT, "models", "primary_summary.txt"))

# Complementary target-provenance diagnostic, not a competing final model chosen
# on test results. Do not present its high R2 as validation of true future value.
formula_reconstruction <- lm(CLV ~ Income + Yield + No_Trans + Tot_Prod + Age_c + Under30, train)
income_added <- lm(update(primary_formula, . ~ . + Income), train)
income_zero_model <- lm(update(primary_formula, . ~ . + Income + IncomeZero), train)
zero_imputed_train <- train; zero_imputed_test <- test
age_medians <- tapply(train$Income[train$Income > 0], train$Cat_Age[train$Income > 0], median)
for (nm in c("zero_imputed_train", "zero_imputed_test")) {
  part <- get(nm); z <- part$Income == 0
  part$Income[z] <- age_medians[as.character(part$Cat_Age[z])]
  assign(nm, part)
}
income_imputed <- lm(update(primary_formula, . ~ . + Income + IncomeZero), zero_imputed_train)
clv_diagnostics <- rbind(
  metrics(test$CLV, test_pred, "Primary_excludes_Income_Yield"),
  metrics(test$CLV, predict(income_added, test), "Diagnostic_add_Income"),
  metrics(test$CLV, predict(income_zero_model, test), "Diagnostic_Income_plus_zero_flag"),
  metrics(test$CLV, predict(income_imputed, zero_imputed_test), "Diagnostic_train_age_median_imputation"),
  metrics(test$CLV, predict(formula_reconstruction, test), "Diagnostic_include_Income_and_Yield"))
tab(clv_diagnostics, "03_CLV_target_provenance_sensitivity")
tab(coef_table(formula_reconstruction), "03_formula_reconstruction_coefficients")

# Focused under-30 investigation. The contrast cannot estimate true CLV bias:
# Income, product ownership and activity also differ across ages; future outcomes
# and the old formula are unavailable. A significant indicator is only a pattern.
young_adjusted <- lm(CLV ~ Tot_Prod + No_Trans + Age_c + Age_c2 + Under30, train)
young_adjusted_income <- lm(CLV ~ Tot_Prod + No_Trans + Age_c + Age_c2 + Under30 + Income + Yield, train)
young_compare <- rbind(
  cbind(specification = "Primary predictors plus quadratic age", coef_table(young_adjusted)[coef_table(young_adjusted)$term == "Under30", ]),
  cbind(specification = "Also adjust Income and Yield (derived-target diagnostic)",
        coef_table(young_adjusted_income)[coef_table(young_adjusted_income)$term == "Under30", ]))
tab(young_compare, "03_under30_adjusted_association")

# W8 residual diagnostics. HC3 standard errors address heteroscedasticity for
# coefficient inference, not nonlinearity, nonnormal prediction tails or causality.
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

# Sensitivity checks keep test outcomes on the original dollar scale. No test
# record is discarded, and all fitted caps come from the training sample only.
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

# Relative contribution: squared partial association (partial R2). For an age
# block, remove both age terms together; avoid comparing unstandardised betas.
blocks <- list(products = "Tot_Prod", transactions = "No_Trans", age = intersect(
  c("Age_c", "Age_c2", "Age_after30"), attr(terms(primary), "term.labels")))
importance <- do.call(rbind, lapply(names(blocks), function(nm) {
  remain <- setdiff(attr(terms(primary), "term.labels"), blocks[[nm]])
  reduced <- lm(reformulate(remain, response = "CLV"), train)
  data.frame(block = nm, partial_R2_train = (deviance(reduced) - deviance(primary)) / deviance(reduced),
    incremental_R2_train = (deviance(reduced) - deviance(primary)) / sum((train$CLV - mean(train$CLV))^2))
}))
importance <- importance[order(-importance$partial_R2_train), ]; tab(importance, "03_predictor_contributions")

# Secondary current Yield regression; never use CLV as a predictor because its
# unknown formula may already contain Yield. Separate CV selects its age shape.
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

# Use identical train/test split for subgroup error inspection, not model tuning.
predictions <- data.frame(row_id = test$row_id, Cluster = test$Cluster, Under30 = test$Under30,
  CLV = test$CLV, CLV_pred = test_pred, Yield = test$Yield, Yield_pred = yield_pred)
tab(predictions, "03_test_predictions")
subgroup_metrics <- do.call(rbind, lapply(split(seq_len(nrow(test)), test$Cluster), function(ii)
  metrics(test$CLV[ii], test_pred[ii], paste0("Cluster_", as.character(test$Cluster[ii[1]])))))
tab(subgroup_metrics, "03_CLV_test_error_by_cluster")
tab(do.call(rbind, lapply(split(seq_len(nrow(test)), test$Under30), function(ii)
  metrics(test$CLV[ii], test_pred[ii], paste0("Under30_", test$Under30[ii[1]])))), "03_CLV_test_error_by_under30")

# All-data refit for final interpretation/future reproduction; performance claims
# above remain exclusively from the model fitted on the training partition.
final_model <- lm(primary_formula, d)
final_coef <- coef_table(final_model)
tab(final_coef, "03_final_all_data_coefficients_HC3")
saveRDS(list(training_model = primary, final_model = final_model, formula = primary_formula,
  selected_name = selected_name, test_row_ids = test$row_id, train_row_ids = train$row_id,
  cv_fold = fold, target_provenance_diagnostic = formula_reconstruction,
  yield_training_model = yield_fit, train_limits = train_limits), file.path(ROOT, "models", "regression.rds"))
write.csv(d, file.path(ROOT, "data", "analysis_with_clusters.csv"), row.names = FALSE)

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
