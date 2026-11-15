# Part 1. W3 univariate, W4 two-group/categorical, W6 correlations/ANOVA.
SOURCE <- normalizePath(file.path(ROOT, "..", "Group_presentation.csv"))
source_md5 <- unname(tools::md5sum(SOURCE))
d <- read.csv(SOURCE, check.names = FALSE)
original_vars <- c("Tot_Prod", "Income", "No_Trans", "Cat_Age", "Age", "Yield", "CLV")
numeric_vars <- setdiff(original_vars, "Cat_Age")
stopifnot(identical(names(d), original_vars), nrow(d) == 10000,
          all(vapply(d, is.numeric, logical(1))), !anyNA(d), all(is.finite(as.matrix(d))),
          all(d$Age >= 18 & d$Age <= 99), all(d$Tot_Prod %in% 0:5),
          all(d$Cat_Age %in% 1:6), all(d$Income >= 0), all(d$No_Trans >= 0))
d$row_id <- seq_len(nrow(d)) # Row reference only; the source has no customer ID.
d$Under30 <- as.integer(d$Age < 30)
d$IncomeZero <- as.integer(d$Income == 0)
d$AgeBand <- cut(d$Age, c(17, 29, 35, 50, 64, 79, 100),
                 labels = c("18-29", "30-35", "36-50", "51-64", "65-79", "80-99"))
age_mapping <- aggregate(Age ~ Cat_Age, d, function(x) c(n = length(x), min = min(x), max = max(x)))
age_mapping <- data.frame(Cat_Age = age_mapping$Cat_Age, age_mapping$Age)
tab(age_mapping, "01_age_category_mapping")
expected_cat <- ifelse(d$Age <= 35, 2L, ifelse(d$Age <= 50, 3L,
                  ifelse(d$Age <= 64, 4L, ifelse(d$Age <= 79, 5L, 6L))))
stopifnot(all(d$Cat_Age == expected_cat))
audit <- data.frame(check = c("source_rows", "source_columns", "explicit_missing_cells",
  "duplicate_value_rows_beyond_first", "zero_income_n", "zero_income_with_transactions_n",
  "negative_yield_n", "age_mapping_mismatches", "original_md5"),
  value = c(nrow(d), length(original_vars), sum(is.na(d)), sum(duplicated(d[original_vars])),
    sum(d$IncomeZero), sum(d$IncomeZero == 1 & d$No_Trans > 0), sum(d$Yield < 0),
    sum(d$Cat_Age != expected_cat), source_md5))
tab(audit, "01_data_audit"); print(audit)
dictionary <- data.frame(variable = original_vars,
  type = c("Discrete count", "Numeric; ambiguous zero", "Discrete count", "Ordinal category",
           "Numeric years", "Numeric profit; negatives valid", "Derived numeric estimate"),
  meaning = c("Total products owned, permitted 0-5", "Recorded income; period/unit not specified",
    "Transaction count; period not specified", "Observed categories 2-6; category 1 absent",
    "Age in years, 18-99", "Current profit in dollars, may be negative",
    "Expected lifetime value in dollars; unknown existing formula"))
tab(dictionary, "01_data_dictionary")
univariate <- data.frame(variable = numeric_vars,
  do.call(rbind, lapply(d[numeric_vars], summary_numeric)), row.names = NULL)
tab(univariate, "01_univariate_summary")
for (v in c("Tot_Prod", "Cat_Age")) {
  f <- as.data.frame(table(d[[v]])); names(f) <- c("value", "n")
  f$proportion <- f$n / nrow(d); tab(f, paste0("01_frequency_", v))
}
outliers <- do.call(rbind, lapply(numeric_vars, function(v) {
  x <- d[[v]]; iqr <- IQR(x); lo <- q(x, .25) - 1.5 * iqr; hi <- q(x, .75) + 1.5 * iqr
  data.frame(variable = v, lower = lo, upper = hi, below = sum(x < lo), above = sum(x > hi),
             cap99 = q(x, .99), over_cap99 = sum(x > q(x, .99)))
}))
tab(outliers, "01_outlier_screening")
for (v in c("Income", "Yield", "CLV", "No_Trans"))
  tab(d[order(-d[[v]])[1:10], c("row_id", original_vars)], paste0("01_top10_", v))
zero_profile <- profile(d, d$IncomeZero, numeric_vars)
age_profile <- profile(d, d$AgeBand, numeric_vars)
young_profile <- profile(d, d$Under30, numeric_vars)
tab(zero_profile, "01_zero_income_profile"); tab(age_profile, "01_age_band_profile")
tab(young_profile, "01_under30_profile")
tab(profile(d, d$Age, numeric_vars), "01_exact_age_profile")

# Pearson captures linear association; Spearman is an outlier-resistant rank check.
cor_tests <- do.call(rbind, lapply(combn(numeric_vars, 2, simplify = FALSE), function(v) {
  do.call(rbind, lapply(c("pearson", "spearman"), function(method) {
    test <- cor.test(d[[v[1]]], d[[v[2]]], method = method, exact = FALSE)
    data.frame(x = v[1], y = v[2], method = method, r = unname(test$estimate), p = test$p.value)
  }))
}))
cor_tests$p_holm <- p.adjust(cor_tests$p, "holm")
tab(cor_tests, "01_correlations_tests")
tab(cor(d[numeric_vars]), "01_pearson_matrix", TRUE)
tab(cor(d[numeric_vars], method = "spearman"), "01_spearman_matrix", TRUE)
tab(cor(d[-which.max(d$Yield), numeric_vars]), "01_without_max_yield_correlation", TRUE)

# W4: two independent groups. Welch t-test avoids assuming equal variances.
group_tests <- do.call(rbind, lapply(c("Under30", "IncomeZero"), function(g) {
  do.call(rbind, lapply(c("CLV", "Yield", "Income", "No_Trans", "Tot_Prod"), function(v) {
    a <- d[d[[g]] == 1, v]; b <- d[d[[g]] == 0, v]; tt <- t.test(a, b)
    pooled_sd <- sqrt(((length(a) - 1) * var(a) + (length(b) - 1) * var(b)) /
                       (length(a) + length(b) - 2))
    data.frame(grouping = g, outcome = v, n1 = length(a), n0 = length(b),
      mean1 = mean(a), mean0 = mean(b), difference_1_minus_0 = mean(a) - mean(b),
      low95 = tt$conf.int[1], high95 = tt$conf.int[2], t = unname(tt$statistic),
      df = unname(tt$parameter), p = tt$p.value, standardized_difference = (mean(a) - mean(b)) / pooled_sd)
  }))
}))
group_tests$p_holm <- p.adjust(group_tests$p, "holm")
tab(group_tests, "01_welch_two_group_tests")
chi <- chisq.test(table(d$AgeBand, d$IncomeZero))
chi_result <- data.frame(comparison = "AgeBand x IncomeZero", statistic = unname(chi$statistic),
  df = unname(chi$parameter), p = chi$p.value, minimum_expected = min(chi$expected),
  CramersV = sqrt(unname(chi$statistic) / nrow(d)))
tab(chi_result, "01_zero_income_chisquare")
tab(prop.table(table(d$AgeBand, d$IncomeZero), 1), "01_zero_income_by_age", TRUE)

# W6: classical ANOVA is shown for coursework; Welch is primary when unequal variances.
anova_results <- do.call(rbind, lapply(c("CLV", "Yield", "Income", "No_Trans"), function(v) {
  y <- d[[v]]; g <- d$AgeBand
  welch <- oneway.test(y ~ g, var.equal = FALSE)
  classic <- summary(aov(y ~ g))[[1]]
  deviations <- abs(y - ave(y, g, FUN = median))
  lev <- summary(aov(deviations ~ g))[[1]]
  ssb <- sum(tapply(y, g, length) * (tapply(y, g, mean) - mean(y))^2)
  data.frame(outcome = v, classic_F = classic[1, "F value"], classic_p = classic[1, "Pr(>F)"],
    Welch_F = unname(welch$statistic), Welch_df1 = welch$parameter[1], Welch_df2 = welch$parameter[2],
    Welch_p = welch$p.value, Brown_Forsythe_p = lev[1, "Pr(>F)"], eta_squared = ssb / sum((y - mean(y))^2))
}))
tab(anova_results, "01_age_band_anova")
posthoc <- pairwise.t.test(d$CLV, d$AgeBand, pool.sd = FALSE, p.adjust.method = "holm")
tab(posthoc$p.value, "01_CLV_age_posthoc_Welch_Holm", TRUE)

plot_file("01_distributions", {
  par(mfrow = c(2, 3))
  for (v in numeric_vars) hist(d[[v]], breaks = 45, main = v, xlab = v,
    col = "#4E7896", border = "white")
}, 1800, 1100)
plot_file("02_outliers", {
  par(mfrow = c(2, 2))
  for (v in c("Income", "No_Trans", "Yield", "CLV"))
    boxplot(d[[v]], main = paste(v, "- full range"), col = "#AAC7D8", ylab = v)
}, 1500, 1100)
plot_file("03_age_clv", {
  par(mfrow = c(1, 2))
  smoothScatter(d$Age, d$CLV, ylim = c(0, q(d$CLV, .99)), xlab = "Age", ylab = "CLV ($)",
    main = "Age and CLV (vertical view to 99th percentile)")
  abline(v = 30, lty = 2, col = "#A94F21")
  a <- aggregate(CLV ~ Age, d, mean); lines(a$Age, a$CLV, col = "#D85B36", lwd = 2)
  boxplot(CLV ~ AgeBand, d, outline = FALSE, col = "#AAC7D8", xlab = "Age band", ylab = "CLV ($)",
    main = "CLV by age band (outlier points hidden)")
}, 1700, 850)
plot_file("04_clv_associations", {
  par(mfrow = c(2, 2))
  for (v in c("Income", "No_Trans", "Tot_Prod", "Yield"))
    plot(d[[v]], d$CLV, pch = 16, cex = .4, col = adjustcolor("#2E6F91", .2),
         xlab = v, ylab = "CLV ($)", main = paste("CLV vs", v))
}, 1600, 1100)
plot_file("05_zero_income", {
  par(mfrow = c(1, 2))
  z <- tapply(d$IncomeZero, d$AgeBand, mean)
  barplot(z * 100, col = "#4E7896", ylab = "Income recorded as zero (%)", xlab = "Age band", ylim = c(0, max(z) * 115))
  boxplot(No_Trans ~ IncomeZero, d, outline = FALSE, names = c("Positive income", "Zero income"),
    col = "#AAC7D8", ylab = "Transactions", main = "Activity despite zero recorded income")
}, 1500, 800)
write.csv(d, file.path(ROOT, "data", "analysis_data.csv"), row.names = FALSE)
cat("EDA complete. No source records deleted. Source MD5:", source_md5, "\n")
