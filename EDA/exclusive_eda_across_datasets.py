"""
Exclusive EDA across the six final V2 datasets.

This is the Python script counterpart to `exclusive_eda_across_datasets.ipynb`.

Run from the project root:

    python EDA/exclusive_eda_across_datasets.py

Inputs:
    Datasets/V2/data_complete_baseline.csv
    Datasets/V2/knn_imputed_data_k5.csv
    Datasets/V2/knn_imputed_data_k10.csv
    Datasets/V2/mean_mode_imputed_dataset.csv
    Datasets/V2/mice_imputation_data1.csv
    Datasets/V2/mice_imputation_data2.csv

Outputs:
    EDA/outputs/v2_*.png
    EDA/outputs/v2_dataset_summary.csv
    EDA/outputs/v2_missing_summary.csv
"""

from __future__ import annotations

from pathlib import Path
import warnings

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
from sklearn.metrics import pairwise_distances
from sklearn.preprocessing import StandardScaler

warnings.filterwarnings("ignore", category=RuntimeWarning, module="sklearn.utils.extmath")

PROJECT_ROOT = Path(__file__).resolve().parents[1]
DATA_DIR = PROJECT_ROOT / "Datasets" / "V2"
OUTPUT_DIR = PROJECT_ROOT / "EDA" / "outputs"

DATASET_PATHS = {
    "baseline": DATA_DIR / "data_complete_baseline.csv",
    "knn_k5": DATA_DIR / "knn_imputed_data_k5.csv",
    "knn_k10": DATA_DIR / "knn_imputed_data_k10.csv",
    "mean_mode": DATA_DIR / "mean_mode_imputed_dataset.csv",
    "mice1": DATA_DIR / "mice_imputation_data1.csv",
    "mice2": DATA_DIR / "mice_imputation_data2.csv",
}

DATASET_LABELS = {
    "baseline": "Baseline",
    "knn_k5": "KNN k=5",
    "knn_k10": "KNN k=10",
    "mean_mode": "Mean/Mode",
    "mice1": "MICE 1",
    "mice2": "MICE 2",
}

DATASET_ORDER = list(DATASET_PATHS)

COLUMN_ALIASES = {
    "work_time_main": ["WorkTimeInSeconds...9", "WorkTimeInSeconds...1"],
    "multiplier": ["multiplier"],
    "amount": ["amount"],
    "work_time_followup": ["WorkTimeInSeconds...17", "WorkTimeInSeconds...7"],
    "political_views": [
        "Q3_1 On a scale of 0 to 100, how would you describe your political views?",
        "Q3_1.On.a.scale.of.0.to.100..how.would.you.describe.your.political.views.",
    ],
    "religious_orientation": [
        "Q8_1 On a scale of 0 to 100, how would you describe your religious orientation?",
        "Q8_1.On.a.scale.of.0.to.100..how.would.you.describe.your.religious.orientation.",
    ],
    "age": ["age"],
    "gender": ["gender"],
    "income": ["income"],
    "vote_2016": [
        "Q5 In the 2016 Presidential election, who did you vote for?",
        "Q5.In.the.2016.Presidential.election..who.did.you.vote.for.",
    ],
    "state": ["Q9 What State do you live in?", "Q9.What.State.do.you.live.in."],
    "party": ["Q7 Do you consider yourself a:", "Q7.Do.you.consider.yourself.a."],
    "religious_attendance": [
        "Q8 Aside from weddings and funerals, how often do you attend religious services?",
        "Q8.Aside.from.weddings.and.funerals..how.often.do.you.attend.religious.services.",
    ],
    "batch": ["batch"],
}

NUMERIC_VARIABLES = [
    "work_time_main",
    "multiplier",
    "amount",
    "work_time_followup",
    "political_views",
    "religious_orientation",
]

CATEGORICAL_VARIABLES = [
    "age",
    "gender",
    "income",
    "vote_2016",
    "state",
    "party",
    "religious_attendance",
    "batch",
]


def resolve_column(df: pd.DataFrame, variable: str) -> str | None:
    for candidate in COLUMN_ALIASES[variable]:
        if candidate in df.columns:
            return candidate
    return None


def load_datasets() -> dict[str, pd.DataFrame]:
    missing_files = [str(path) for path in DATASET_PATHS.values() if not path.exists()]
    if missing_files:
        raise FileNotFoundError(
            "Missing expected V2 dataset files:\n" + "\n".join(missing_files)
        )
    return {name: pd.read_csv(path) for name, path in DATASET_PATHS.items()}


def collect_long_variable(
    datasets: dict[str, pd.DataFrame],
    variable: str,
    numeric: bool = False,
) -> pd.DataFrame:
    frames = []
    for dataset_name in DATASET_ORDER:
        df = datasets[dataset_name]
        col = resolve_column(df, variable)
        if col is None:
            continue

        values = df[col]
        if numeric:
            values = pd.to_numeric(values, errors="coerce")

        frames.append(
            pd.DataFrame(
                {
                    "dataset": DATASET_LABELS[dataset_name],
                    "dataset_key": dataset_name,
                    "value": values,
                }
            )
        )

    if not frames:
        return pd.DataFrame(columns=["dataset", "dataset_key", "value"])

    out = pd.concat(frames, ignore_index=True)
    out["dataset"] = pd.Categorical(
        out["dataset"],
        categories=[DATASET_LABELS[x] for x in DATASET_ORDER],
        ordered=True,
    )
    return out


def numeric_matrix(df: pd.DataFrame) -> pd.DataFrame:
    parts = {}
    for variable in NUMERIC_VARIABLES:
        col = resolve_column(df, variable)
        if col is not None:
            parts[variable] = pd.to_numeric(df[col], errors="coerce")
    return pd.DataFrame(parts)


def save_fig(filename: str) -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    plt.savefig(OUTPUT_DIR / filename, dpi=200, bbox_inches="tight")
    plt.close()


def write_summary_tables(datasets: dict[str, pd.DataFrame]) -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    summary = pd.DataFrame(
        [
            {
                "dataset": DATASET_LABELS[name],
                "file": DATASET_PATHS[name].name,
                "rows": len(df),
                "columns": df.shape[1],
                "total_missing": int(df.isna().sum().sum()),
            }
            for name, df in datasets.items()
        ]
    )
    summary.to_csv(OUTPUT_DIR / "v2_dataset_summary.csv", index=False)

    missing_rows = []
    for dataset_name in DATASET_ORDER:
        counts = datasets[dataset_name].isna().sum()
        for col, n_missing in counts[counts > 0].items():
            missing_rows.append(
                {
                    "dataset": DATASET_LABELS[dataset_name],
                    "column": col,
                    "n_missing": int(n_missing),
                }
            )

    pd.DataFrame(missing_rows).to_csv(OUTPUT_DIR / "v2_missing_summary.csv", index=False)


def plot_numeric_distributions(datasets: dict[str, pd.DataFrame]) -> None:
    for variable in NUMERIC_VARIABLES:
        plot_data = collect_long_variable(datasets, variable, numeric=True).dropna()
        if plot_data.empty:
            continue

        grid = sns.displot(
            data=plot_data,
            x="value",
            col="dataset",
            col_order=[DATASET_LABELS[x] for x in DATASET_ORDER],
            col_wrap=3,
            bins=30,
            color="steelblue",
            facet_kws={"sharex": False, "sharey": False},
            height=3,
            aspect=1.25,
        )
        grid.set_titles("{col_name}")
        grid.set_axis_labels(variable, "Count")
        grid.fig.suptitle(f"Distribution Across V2 Datasets: {variable}", y=1.03)
        save_fig(f"v2_numeric_distribution_{variable}.png")


def plot_numeric_boxplots(datasets: dict[str, pd.DataFrame]) -> None:
    for variable in NUMERIC_VARIABLES:
        plot_data = collect_long_variable(datasets, variable, numeric=True).dropna()
        if plot_data.empty:
            continue

        plt.figure(figsize=(10, 4.5))
        sns.boxplot(
            data=plot_data,
            x="dataset",
            y="value",
            order=[DATASET_LABELS[x] for x in DATASET_ORDER],
            color="lightsteelblue",
        )
        sns.stripplot(
            data=plot_data.sample(min(len(plot_data), 1200), random_state=123),
            x="dataset",
            y="value",
            order=[DATASET_LABELS[x] for x in DATASET_ORDER],
            color="black",
            alpha=0.15,
            size=2,
        )
        plt.title(f"{variable}: Distribution Across Six V2 Datasets")
        plt.xlabel("")
        plt.ylabel(variable)
        plt.xticks(rotation=25)
        plt.tight_layout()
        save_fig(f"v2_numeric_boxplot_{variable}.png")


def plot_categorical_counts(datasets: dict[str, pd.DataFrame]) -> None:
    for variable in CATEGORICAL_VARIABLES:
        plot_data = collect_long_variable(datasets, variable, numeric=False).dropna()
        if plot_data.empty:
            continue

        plot_data["value"] = plot_data["value"].astype(str)
        grid = sns.catplot(
            data=plot_data,
            x="value",
            col="dataset",
            col_order=[DATASET_LABELS[x] for x in DATASET_ORDER],
            col_wrap=3,
            kind="count",
            color="darkorange",
            sharex=False,
            sharey=False,
            height=3.2,
            aspect=1.3,
        )
        grid.set_titles("{col_name}")
        grid.set_axis_labels(variable, "Count")
        grid.fig.suptitle(f"Category Counts Across V2 Datasets: {variable}", y=1.03)
        for ax in grid.axes.flatten():
            ax.tick_params(axis="x", rotation=45)
        save_fig(f"v2_categorical_counts_{variable}.png")


def plot_correlation_heatmaps(datasets: dict[str, pd.DataFrame]) -> None:
    fig, axes = plt.subplots(2, 3, figsize=(18, 10))
    axes = axes.flatten()

    for ax, dataset_name in zip(axes, DATASET_ORDER):
        corr_df = numeric_matrix(datasets[dataset_name]).corr()
        sns.heatmap(
            corr_df,
            vmin=-1,
            vmax=1,
            center=0,
            cmap="coolwarm",
            square=True,
            linewidths=0.5,
            annot=True,
            fmt=".2f",
            ax=ax,
            cbar=dataset_name == DATASET_ORDER[-1],
        )
        ax.set_title(DATASET_LABELS[dataset_name])

    fig.suptitle("Numeric Correlations Across Six V2 Datasets", y=1.02)
    plt.tight_layout()
    save_fig("v2_correlation_heatmaps_all.png")


def plot_voting_behavior(datasets: dict[str, pd.DataFrame]) -> None:
    vote_data = collect_long_variable(datasets, "vote_2016", numeric=False).dropna()
    if vote_data.empty:
        return

    vote_data["value"] = vote_data["value"].astype(str)
    plt.figure(figsize=(12, 5))
    sns.countplot(
        data=vote_data,
        x="dataset",
        hue="value",
        order=[DATASET_LABELS[x] for x in DATASET_ORDER],
    )
    plt.title("Voting Behavior Across Six V2 Datasets")
    plt.xlabel("")
    plt.ylabel("Count")
    plt.xticks(rotation=25)
    plt.legend(title="2016 vote", bbox_to_anchor=(1.02, 1), loc="upper left")
    plt.tight_layout()
    save_fig("v2_voting_behavior_comparison.png")


def scaled_numeric_matrix(df: pd.DataFrame) -> pd.DataFrame:
    num_df = numeric_matrix(df).dropna()
    if num_df.empty:
        return num_df

    scaled = StandardScaler().fit_transform(num_df)
    return pd.DataFrame(scaled, columns=num_df.columns, index=num_df.index)


def order_distance_matrix(distance_matrix: np.ndarray) -> np.ndarray:
    from scipy.cluster.hierarchy import leaves_list, linkage
    from scipy.spatial.distance import squareform

    condensed = squareform(distance_matrix, checks=False)
    linkage_matrix = linkage(condensed, method="average")
    order = leaves_list(linkage_matrix)
    return distance_matrix[np.ix_(order, order)]


def plot_vat_like_heatmaps(
    datasets: dict[str, pd.DataFrame],
    max_rows: int = 300,
) -> None:
    rng = np.random.default_rng(123)
    fig, axes = plt.subplots(2, 3, figsize=(15, 9))
    axes = axes.flatten()

    for ax, dataset_name in zip(axes, DATASET_ORDER):
        scaled = scaled_numeric_matrix(datasets[dataset_name])
        if len(scaled) > max_rows:
            chosen_idx = rng.choice(scaled.index.to_numpy(), size=max_rows, replace=False)
            scaled = scaled.loc[chosen_idx]

        distance_matrix = pairwise_distances(scaled, metric="euclidean")
        ordered_distance = order_distance_matrix(distance_matrix)

        sns.heatmap(
            ordered_distance,
            cmap="coolwarm",
            xticklabels=False,
            yticklabels=False,
            cbar=False,
            ax=ax,
        )
        ax.set_title(DATASET_LABELS[dataset_name])

    fig.suptitle("VAT-Like Numeric Distance Patterns Across Six V2 Datasets", y=1.02)
    plt.tight_layout()
    save_fig("v2_vat_like_distance_heatmaps_all.png")


def main() -> None:
    sns.set_theme(style="whitegrid")
    datasets = load_datasets()

    write_summary_tables(datasets)
    plot_numeric_distributions(datasets)
    plot_numeric_boxplots(datasets)
    plot_categorical_counts(datasets)
    plot_correlation_heatmaps(datasets)
    plot_voting_behavior(datasets)
    plot_vat_like_heatmaps(datasets)

    print(f"EDA outputs saved to: {OUTPUT_DIR}")


if __name__ == "__main__":
    main()
