import matplotlib.pyplot as plt
import pandas as pd
import sys

def main(input_file, output_file):
    df = pd.read_csv(input_file)
    rules = sorted(df["nr_removed_rules"].unique())

    data = {}

    for model in df["analysis_type"].unique():
        model_df = df[df["analysis_type"] == model]
        
        # sort by nr_removed_rules to align with rules
        model_df = model_df.sort_values("nr_removed_rules")
        
        data[model] = model_df["time"].tolist()

    plt.rcParams.update({
        "font.family": "serif",
        "font.size": 11,
        "axes.labelsize": 12,
        "axes.titlesize": 13,
        "legend.fontsize": 9,
        "xtick.labelsize": 10,
        "ytick.labelsize": 10,
        "lines.linewidth": 1.8,
        "lines.markersize": 4
    })

    fig, ax = plt.subplots(figsize=(6,4))

    colors = plt.get_cmap("tab10").colors

    for i, (model, runtimes) in enumerate(data.items()):
        ax.plot(rules, runtimes, marker="o", label=model, color=colors[i+1])

    ax.set_xlabel("Number of rules in working set")
    ax.set_ylabel("Runtime (s)")
    ax.grid(True, linestyle="--", linewidth=0.5, alpha=0.6)

    ax.set_yscale("log")

    ax.legend(frameon=False, ncol=2)

    fig.tight_layout()

    fig.savefig(output_file + ".pdf")
    fig.savefig(output_file + ".png", dpi=300)

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python3 plot.py input.csv output")
        sys.exit(1)
    main(sys.argv[1], sys.argv[2])