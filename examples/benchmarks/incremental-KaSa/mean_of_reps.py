import pandas as pd

def main(input_file, output_file):
    df = pd.read_csv(input_file)
    group_cols = [col for col in df.columns if col not in ["rep", "time"]]
    grouped = (
        df.groupby(group_cols, as_index=False)
          .agg({"time": "mean"})
    )
    group_cols2 = [col for col in grouped.columns if col not in ["substep_name", "time"]]
    grouped2 = (
        grouped.groupby(group_cols2, as_index=False)
          .agg({"time": "sum"})
    )
    grouped2.to_csv(output_file, index=False)

if __name__ == "__main__":
    main("examples/benchmarks/incremental-KaSa/output/experiments_output.csv", "examples/benchmarks/incremental-KaSa/output/experiments_output_mean.csv")