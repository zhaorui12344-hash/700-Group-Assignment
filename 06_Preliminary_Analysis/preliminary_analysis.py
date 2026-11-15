"""Exploratory checks only; no clustering or regression is fitted.

Run with Python containing pandas and numpy. The original CSV is
read-only. Outputs go to preliminary_outputs next to this script. This Python
spike is preparation; the assignment's final analysis must include R code.
"""
from pathlib import Path
import hashlib
import json
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parent
SOURCE = ROOT.parent / "02_Assignment" / "group" / "Group_presentation.csv"
OUT = ROOT / "preliminary_outputs"
OUT.mkdir(exist_ok=True)
df = pd.read_csv(SOURCE)
expected = ["Tot_Prod", "Income", "No_Trans", "Cat_Age", "Age", "Yield", "CLV"]
assert df.columns.tolist() == expected, df.columns.tolist()
assert all(pd.api.types.is_numeric_dtype(df[c]) for c in expected)
metric = [c for c in expected if c != "Cat_Age"]

summary = df[metric].describe(percentiles=[.01, .05, .25, .5, .75, .95, .99]).T
summary["skewness"] = df.skew()
summary["zero_n"] = df.eq(0).sum()
summary["negative_n"] = df.lt(0).sum()
summary["missing_n"] = df.isna().sum()
summary.to_csv(OUT / "univariate_summary.csv")

flags = {}
for c in expected:
    q1, q3 = df[c].quantile([.25, .75])
    lo, hi = q1 - 1.5 * (q3 - q1), q3 + 1.5 * (q3 - q1)
    flags[c] = {"lower_fence": lo, "upper_fence": hi,
                "below_n": int(df[c].lt(lo).sum()),
                "above_n": int(df[c].gt(hi).sum())}
# IQR flags are screening flags, not automatic evidence of invalid records.
pd.DataFrame(flags).T.to_csv(OUT / "iqr_screening.csv")

for c in ["Cat_Age", "Tot_Prod"]:
    f = df[c].value_counts(dropna=False).sort_index().rename("n").to_frame()
    f["percent"] = 100 * f.n / len(df)
    f.to_csv(OUT / f"frequency_{c}.csv")

df.groupby("Cat_Age").Age.agg(["count", "min", "max", "nunique"]).to_csv(OUT / "age_category_ranges.csv")
pd.crosstab(df.Age, df.Cat_Age).to_csv(OUT / "age_category_crosstab.csv")

def grouped_profile(group, filename):
    result = df.groupby(group)[metric].agg(["count", "mean", "median", "std"])
    result.to_csv(OUT / filename)

grouped_profile(df.Income.eq(0).rename("income_is_zero"), "zero_income_profile.csv")
grouped_profile(df.Age.lt(30).rename("under_30"), "under30_profile.csv")
grouped_profile(df.Cat_Age, "age_group_profile.csv")
df.assign(income_is_zero=df.Income.eq(0), loss_making=df.Yield.lt(0)).groupby("Age").agg(
    n=("CLV", "size"), mean_clv=("CLV", "mean"), median_clv=("CLV", "median"),
    mean_yield=("Yield", "mean"), median_yield=("Yield", "median"),
    mean_income=("Income", "mean"), zero_income_rate=("income_is_zero", "mean"),
    loss_making_rate=("loss_making", "mean"), mean_products=("Tot_Prod", "mean"),
    mean_transactions=("No_Trans", "mean")).to_csv(OUT / "profile_by_exact_age.csv")

for name, part in {"all": df, "under30": df[df.Age < 30],
                   "30plus": df[df.Age >= 30], "positive_income": df[df.Income > 0]}.items():
    for method in ["pearson", "spearman"]:
        part[metric].corr(method=method).to_csv(OUT / f"correlation_{name}_{method}.csv")

# Descriptive sensitivity checks only, not a decision to delete any customer.
df.drop(df.Yield.idxmax())[metric].corr().to_csv(OUT / "correlation_without_max_yield_sensitivity.csv")
for c in ["Income", "Yield", "CLV"]:
    df.nlargest(10, c).to_csv(OUT / f"largest_{c}_records.csv", index_label="source_row_index_zero_based")

audit = {
    "source_sha256": hashlib.sha256(SOURCE.read_bytes()).hexdigest(),
    "rows": len(df), "columns": expected, "dtypes": df.dtypes.astype(str).to_dict(),
    "semantic_types": {"Cat_Age": "ordered category; use frequencies, not a continuous predictor",
                       "Tot_Prod": "discrete count", "No_Trans": "discrete count",
                       "Age": "age in years", "Income": "numeric; zero meaning uncertain",
                       "Yield": "numeric current profit; negatives valid", "CLV": "derived numeric lifetime value"},
    "missing": df.isna().sum().to_dict(), "infinite_values": int(np.isinf(df).sum().sum()),
    "exact_duplicate_rows_beyond_first": int(df.duplicated().sum()),
    "note_duplicates": "No customer ID is available; identical values do not prove duplicate customers.",
    "age_outside_18_99": int((~df.Age.between(18, 99)).sum()),
    "products_outside_0_5": int((~df.Tot_Prod.between(0, 5)).sum()),
    "age_categories_outside_1_6": int((~df.Cat_Age.between(1, 6)).sum()),
    "ages_mapped_to_multiple_categories": int(df.groupby("Age").Cat_Age.nunique().gt(1).sum()),
    "income_zero_n": int(df.Income.eq(0).sum()),
    "income_zero_percent": float(100 * df.Income.eq(0).mean()),
    "zero_income_with_positive_transactions_n": int((df.Income.eq(0) & df.No_Trans.gt(0)).sum()),
    "negative_yield_n": int(df.Yield.lt(0).sum()),
    "negative_yield_percent": float(100 * df.Yield.lt(0).mean()),
    "negative_clv_n": int(df.CLV.lt(0).sum()),
    "no_models_fitted": True,
    "caveats": ["Income=0 is ambiguous; no imputation or deletion has been performed.",
                "CLV is a derived estimate with unknown formula, not observed lifetime value.",
                "A lower CLV in young customers alone cannot establish underestimation.",
                "Negative Yield is permitted by the assignment; it is not a data error.",
                "No units or time period for Income are assumed beyond the supplied requirement."]
}
(OUT / "audit.json").write_text(json.dumps(audit, indent=2), encoding="utf-8")

for c in metric:
    counts, edges = np.histogram(df[c].dropna(), bins=30)
    pd.DataFrame({"bin_left": edges[:-1], "bin_right": edges[1:], "n": counts}).to_csv(
        OUT / f"histogram_{c}.csv", index=False)
print(json.dumps(audit, indent=2))
print(summary.round(3).to_string())
