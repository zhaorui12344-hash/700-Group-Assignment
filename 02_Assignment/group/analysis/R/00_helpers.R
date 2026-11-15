# Shared small helpers. No package installation and no source-data mutation.
tab <- function(x, name, row.names = FALSE) {
  write.csv(x, file.path(ROOT, "tables", paste0(name, ".csv")), row.names = row.names,
            na = "", fileEncoding = "UTF-8")
  invisible(x)
}
plot_file <- function(name, expr, width = 1500, height = 1000) {
  png(file.path(ROOT, "plots", paste0(name, ".png")), width = width, height = height,
      res = 150, type = if (Sys.info()[["sysname"]] == "Darwin") "quartz" else "cairo")
  on.exit(dev.off())
  par(mar = c(4.2, 4.5, 3.2, 1.5), las = 1, col.axis = "#334155",
      col.lab = "#172B4D", col.main = "#172B4D", family = "sans")
  force(expr)
}
fmt <- function(x, digits = 2) formatC(x, digits = digits, format = "f", big.mark = ",")
ptext <- function(p) ifelse(is.na(p), "NA", ifelse(p < .001, "<0.001", sprintf("%.4f", p)))
q <- function(x, p) as.numeric(quantile(x, p, na.rm = TRUE, names = FALSE))
skew <- function(x) { x <- x[is.finite(x)]; mean((x - mean(x))^3) / mean((x - mean(x))^2)^1.5 }
summary_numeric <- function(x) c(n = length(x), missing = sum(is.na(x)), mean = mean(x),
  sd = sd(x), min = min(x), p01 = q(x, .01), q25 = q(x, .25), median = median(x),
  q75 = q(x, .75), p99 = q(x, .99), max = max(x), skewness = skew(x),
  zeros = sum(x == 0), negative = sum(x < 0))
profile <- function(dat, grp, vars) do.call(rbind, lapply(sort(unique(grp)), function(g) {
  part <- dat[grp == g, , drop = FALSE]
  out <- data.frame(group = as.character(g), n = nrow(part), share = nrow(part) / nrow(dat))
  for (v in vars) {
    out[[paste0(v, "_mean")]] <- mean(part[[v]])
    out[[paste0(v, "_median")]] <- median(part[[v]])
    out[[paste0(v, "_sd")]] <- sd(part[[v]])
  }
  out$zero_income_rate <- mean(part$Income == 0)
  out$negative_yield_rate <- mean(part$Yield < 0)
  out
}))
adjusted_rand <- function(a, b) {
  ct <- table(a, b); choose2 <- function(x) x * (x - 1) / 2
  n2 <- choose2(sum(ct)); obs <- sum(choose2(ct))
  ea <- sum(choose2(rowSums(ct))); eb <- sum(choose2(colSums(ct)))
  expected <- ea * eb / n2; den <- (ea + eb) / 2 - expected
  if (den == 0) return(1)
  (obs - expected) / den
}
kmeans_converged <- function(fit, iter_max = 200L) {
  # Lloyd does not populate ifault; an absent field is not an error code.
  (is.null(fit$ifault) || identical(as.integer(fit$ifault), 0L)) &&
    length(fit$iter) == 1L && is.finite(fit$iter) && fit$iter < iter_max
}
metrics <- function(y, pred, name) data.frame(model = name, n = length(y),
  RMSE = sqrt(mean((y - pred)^2)), MAE = mean(abs(y - pred)),
  R2 = 1 - sum((y - pred)^2) / sum((y - mean(y))^2), mean_error = mean(pred - y))
hc3 <- function(model) {
  X <- model.matrix(model); bread <- solve(crossprod(X))
  meat <- crossprod(X, X * (resid(model) / (1 - hatvalues(model)))^2)
  bread %*% meat %*% bread
}
coef_table <- function(model) {
  b <- coef(model); se <- sqrt(diag(hc3(model))); crit <- qt(.975, df.residual(model))
  ss <- coef(summary(model))
  data.frame(term = names(b), estimate = b, classical_se = ss[, 2],
    classical_p = ss[, 4], HC3_se = se, HC3_t = b / se,
    HC3_p = 2 * pt(-abs(b / se), df.residual(model)),
    HC3_low95 = b - crit * se, HC3_high95 = b + crit * se, row.names = NULL)
}
vif_table <- function(model) {
  X <- model.matrix(model)[, -1, drop = FALSE]
  data.frame(term = colnames(X), VIF = vapply(seq_len(ncol(X)), function(j) {
    if (ncol(X) == 1) return(1)
    1 / (1 - summary(lm(X[, j] ~ X[, -j, drop = FALSE]))$r.squared)
  }, numeric(1)))
}
html_escape <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
html_table <- function(x, digits = 3) {
  if (nrow(x) == 0) return("<p>No rows</p>")
  text_cols <- !vapply(x, is.numeric, logical(1))
  x[] <- lapply(names(x), function(nm) {
    z <- x[[nm]]
    if (is.numeric(z) && grepl("(^p$|_p$|^p_holm$)", nm)) return(html_escape(ptext(z)))
    if (is.numeric(z)) format(round(z, digits), trim = TRUE, scientific = FALSE, big.mark = ",") else html_escape(z)
  })
  paste0('<div class="table-wrap"><table><thead><tr>',
    paste0(ifelse(text_cols, '<th class="text-column">', '<th>'), html_escape(names(x)), "</th>", collapse = ""),
    "</tr></thead><tbody>", paste(apply(x, 1, function(z)
      paste0("<tr>", paste0("<td>", z, "</td>", collapse = ""), "</tr>")), collapse = ""),
    "</tbody></table></div>")
}
