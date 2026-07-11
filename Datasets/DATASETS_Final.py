"""
Generate V2 datasets from V1 imputed datasets.

This is the Python counterpart to `Datasets/DATASETS_Final.R`.

Input:
    Datasets/V1/data_complete_baseline_V1.csv
    Datasets/V1/mean_mode_imputed_dataset_V1.csv
    Datasets/V1/knn_imputed_data_k5_V1.csv
    Datasets/V1/knn_imputed_data_k10_V1.csv
    Datasets/V1/mice_imputation_data1_V1.csv
    Datasets/V1/mice_imputation_data2_V1.csv

Output:
    Datasets/V2/data_complete_baseline.csv
    Datasets/V2/mean_mode_imputed_dataset.csv
    Datasets/V2/knn_imputed_data_k5.csv
    Datasets/V2/knn_imputed_data_k10.csv
    Datasets/V2/mice_imputation_data1.csv
    Datasets/V2/mice_imputation_data2.csv

The script mirrors the R workflow:
    1. Replace age == "Prefer not to state" with the mode age.
    2. Recode gender == "Other / Prefer not to state" as "Other".
    3. Convert income == "Prefer not to state" to missing.
    4. Impute the remaining income values using mode, KNN, and MICE-like
       iterative imputation.
"""

from __future__ import annotations

from pathlib import Path
import re

import numpy as np
import pandas as pd
from sklearn.experimental import enable_iterative_imputer  # noqa: F401
from sklearn.impute import IterativeImputer, KNNImputer
from sklearn.preprocessing import OrdinalEncoder


BASE_PATH = Path("Datasets/V1")
OUTPUT_PATH = Path("Datasets/V2")

INCOME_LEVELS = [
    "Under $20,000",
    "$20,000 - $39,999",
    "$40,000 - $59,999",
    "$60,000 - $79,999",
    "$80,000 - $99,999",
    "Over $100,000",
]


INPUT_FILES = {
    "baseline": "data_complete_baseline_V1.csv",
    "knn_k5": "knn_imputed_data_k5_V1.csv",
    "knn_k10": "knn_imputed_data_k10_V1.csv",
    "meanmode": "mean_mode_imputed_dataset_V1.csv",
    "mice1": "mice_imputation_data1_V1.csv",
    "mice2": "mice_imputation_data2_V1.csv",
}

OUTPUT_FILES = {
    "baseline": "data_complete_baseline.csv",
    "knn_k5": "knn_imputed_data_k5.csv",
    "knn_k10": "knn_imputed_data_k10.csv",
    "meanmode": "mean_mode_imputed_dataset.csv",
    "mice1": "mice_imputation_data1.csv",
    "mice2": "mice_imputation_data2.csv",
}


def r_style_column_name(name: str) -> str:
    """Approximate R's check.names behavior for compatibility with old scripts."""
    cleaned = re.sub(r"[^0-9A-Za-z_.]", ".", name)
    if re.match(r"^[0-9]", cleaned):
        cleaned = f"X{cleaned}"
    return cleaned


def make_unique(names: list[str]) -> list[str]:
    seen: dict[str, int] = {}
    unique_names = []

    for name in names:
        if name not in seen:
            seen[name] = 0
            unique_names.append(name)
        else:
            seen[name] += 1
            unique_names.append(f"{name}.{seen[name]}")

    return unique_names


def apply_r_style_column_names(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    out.columns = make_unique([r_style_column_name(col) for col in out.columns])
    return out


def mode_value(series: pd.Series) -> object:
    modes = series.dropna().mode()
    if modes.empty:
        raise ValueError(f"Cannot compute mode for empty column {series.name!r}.")
    return modes.iloc[0]


def load_datasets() -> dict[str, pd.DataFrame]:
    return {
        name: pd.read_csv(BASE_PATH / filename)
        for name, filename in INPUT_FILES.items()
    }


def replace_age_with_mode(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    if "age" not in out.columns:
        return out

    age_without_special = out["age"].replace("Prefer not to state", np.nan)
    age_mode = mode_value(age_without_special)
    out["age"] = out["age"].replace("Prefer not to state", age_mode)
    return out


def clean_gender(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    if "gender" in out.columns:
        out["gender"] = out["gender"].replace("Other / Prefer not to state", "Other")
    return out


def income_prefer_not_to_missing(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    if "income" in out.columns:
        out["income"] = out["income"].replace("Prefer not to state", np.nan)
    return out


def final_special_category_cleanup(df: pd.DataFrame) -> pd.DataFrame:
    out = replace_age_with_mode(df)
    out = clean_gender(out)
    out = income_prefer_not_to_missing(out)
    return out


def impute_income_mode(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    if "income" in out.columns:
        out["income"] = out["income"].fillna(mode_value(out["income"]))
    return out


def encode_for_numeric_imputation(df: pd.DataFrame) -> tuple[np.ndarray, list[str], dict[str, OrdinalEncoder]]:
    """Encode a mixed dataframe as numeric values for KNN/iterative imputation."""
    encoded_parts = []
    columns = []
    encoders = {}

    for col in df.columns:
        series = df[col]

        if pd.api.types.is_numeric_dtype(series):
            encoded_parts.append(series.astype(float).to_numpy().reshape(-1, 1))
            columns.append(col)
            continue

        values = series.astype("object").where(series.notna(), np.nan).to_numpy().reshape(-1, 1)
        encoder = OrdinalEncoder(
            handle_unknown="use_encoded_value",
            unknown_value=np.nan,
            encoded_missing_value=np.nan,
        )

        if col == "income":
            categories = [INCOME_LEVELS]
            encoder = OrdinalEncoder(
                categories=categories,
                handle_unknown="use_encoded_value",
                unknown_value=np.nan,
                encoded_missing_value=np.nan,
            )

        encoded = encoder.fit_transform(values)
        encoded_parts.append(encoded.astype(float))
        columns.append(col)
        encoders[col] = encoder

    return np.hstack(encoded_parts), columns, encoders


def decode_income(values: np.ndarray) -> list[str]:
    income_codes = np.rint(values).astype(int)
    income_codes = np.clip(income_codes, 0, len(INCOME_LEVELS) - 1)
    return [INCOME_LEVELS[i] for i in income_codes]


def impute_income_with_numeric_matrix(
    df: pd.DataFrame,
    imputer: KNNImputer | IterativeImputer,
) -> pd.DataFrame:
    if "income" not in df.columns or df["income"].isna().sum() == 0:
        return df.copy()

    out = df.copy()
    income_missing = out["income"].isna().to_numpy()
    matrix, columns, _ = encode_for_numeric_imputation(out)

    imputed_matrix = imputer.fit_transform(matrix)
    income_idx = columns.index("income")
    out.loc[income_missing, "income"] = decode_income(imputed_matrix[income_missing, income_idx])

    return out


def impute_income_knn(df: pd.DataFrame, n_neighbors: int) -> pd.DataFrame:
    imputer = KNNImputer(n_neighbors=n_neighbors)
    return impute_income_with_numeric_matrix(df, imputer)


def impute_income_iterative(df: pd.DataFrame, random_state: int) -> pd.DataFrame:
    imputer = IterativeImputer(
        max_iter=20,
        random_state=random_state,
        sample_posterior=True,
        initial_strategy="most_frequent",
    )
    return impute_income_with_numeric_matrix(df, imputer)


def write_dataset(df: pd.DataFrame, filename: str) -> None:
    OUTPUT_PATH.mkdir(parents=True, exist_ok=True)
    out = apply_r_style_column_names(df)
    out.to_csv(OUTPUT_PATH / filename, index=False)


def summarize_missing(datasets: dict[str, pd.DataFrame]) -> pd.DataFrame:
    rows = []
    for dataset_name, df in datasets.items():
        for col, n_missing in df.isna().sum().items():
            if n_missing > 0:
                rows.append(
                    {
                        "dataset": dataset_name,
                        "column": col,
                        "n_missing": int(n_missing),
                    }
                )
    return pd.DataFrame(rows)


def main() -> None:
    datasets = load_datasets()
    cleaned = {
        name: final_special_category_cleanup(df)
        for name, df in datasets.items()
    }

    print("Missing values after special-category cleanup:")
    missing_summary = summarize_missing(cleaned)
    print(missing_summary if not missing_summary.empty else "No missing values")

    write_dataset(cleaned["baseline"], OUTPUT_FILES["baseline"])

    meanmode_v2 = impute_income_mode(cleaned["meanmode"])
    write_dataset(meanmode_v2, OUTPUT_FILES["meanmode"])

    knn_k5_v2 = impute_income_knn(cleaned["knn_k5"], n_neighbors=5)
    write_dataset(knn_k5_v2, OUTPUT_FILES["knn_k5"])

    knn_k10_v2 = impute_income_knn(cleaned["knn_k10"], n_neighbors=10)
    write_dataset(knn_k10_v2, OUTPUT_FILES["knn_k10"])

    mice1_v2 = impute_income_iterative(cleaned["mice1"], random_state=123)
    write_dataset(mice1_v2, OUTPUT_FILES["mice1"])

    mice2_v2 = impute_income_iterative(cleaned["mice2"], random_state=456)
    write_dataset(mice2_v2, OUTPUT_FILES["mice2"])

    print(f"Generated V2 datasets in: {OUTPUT_PATH}")


if __name__ == "__main__":
    main()
