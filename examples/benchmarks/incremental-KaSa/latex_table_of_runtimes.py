"""
latex_table_of_runtimes.py

Usage:
    python3 latex_table_of_runtimes.py input.csv output.tex

Assumptions about CSV columns (no header):
    0: model (string)
    1: nr_rules (int or string)
    2: analysis_type (like "1_full")
    3: step_name     (like "1_init")
    4: nr_removed_rules  (ignored)
    5: time (float)

The script writes a LaTeX table that summarizes the benchmark results.
"""
import sys
import csv
from collections import defaultdict, OrderedDict

def latex_escape(s: str) -> str:
    # minimal escaping for LaTeX special chars
    replace = {
        '&': r'\&',
        '%': r'\%',
        '$': r'\$',
        '#': r'\#',
        '_': r'\_',
        '{': r'\{',
        '}': r'\}',
        '~': r'\textasciitilde{}',
        '^': r'\^{}',
    }
    for k, v in replace.items():
        s = s.replace(k, v)
    return s

def format_time(t):
    # compact formatting: up to 3 significant digits
    try:
        f = float(t)
    except Exception:
        return latex_escape(str(t))
    return "{:.3g}".format(f)

def main(inp_path, out_path):
    # data[model][analysis_name][step_name] = time
    data = defaultdict(lambda: defaultdict(dict))
    model_nr_rules = {}

    with open(inp_path, newline='') as f:
        reader = csv.reader(f)
        next(reader, None) # skip first row (headers)
        for row in reader:
            if not row or all(not c.strip() for c in row):
                continue
            if len(row) < 6:
                # skip malformed rows
                continue
            model = row[0].strip()
            nr_rules = row[1].strip()
            analysis_type = row[2].strip()
            step_name = row[3].strip()
            time_val = row[5].strip()

            data[model][analysis_type][step_name] = time_val
            # keep nr_rules (if inconsistent across rows for same model, keep first seen)
            if model not in model_nr_rules:
                model_nr_rules[model] = nr_rules

    total_step_count = 3
    col_spec = "l c " + " ".join(["c"] * total_step_count)

    # Build LaTeX
    lines = []
    lines.append(r"\documentclass{article}")
    lines.append(r"\usepackage{booktabs}")
    lines.append(r"\usepackage[margin=1in,landscape]{geometry}")
    lines.append(r"\begin{document}")
    lines.append(r"\begin{table}[ht]")
    lines.append(r"\centering")
    lines.append(r"\small")
    lines.append(r"\begin{tabular}{" + col_spec + "}")
    lines.append(r"\toprule")

    # header row
    lines.append(r"\textbf{Model} & \textbf{Nr. of rules} & \bfseries\shortstack{initial\\analysis} &  \bfseries\shortstack{add\\rules} & \bfseries\shortstack{disable\\rules}\\")
    lines.append(r"\midrule")

    for model in data.keys():
        row_elems = []
        row_elems.append(r"\texttt{" + latex_escape(model) + "}")
        row_elems.append(latex_escape(model_nr_rules.get(model, "")))
        analysis_items = [("1_full",["1_init"]), ("2_incremental",["1_init"]), ("3_decremental",["4_disable"])]
        for a, steps in analysis_items:
            for s in steps:
                val = data[model].get(a, {}).get(s, "")
                if val == "":
                    row_elems.append("")  # empty cell if missing
                else:
                    row_elems.append(format_time(val))
        lines.append(" & ".join(row_elems) + r" \\")
    lines.append(r"\bottomrule")
    lines.append(r"\end{tabular}")
    lines.append(r"\caption{Timing table}")
    lines.append(r"\end{table}")
    lines.append(r"\end{document}")

    # write output
    with open(out_path, "w") as outf:
        outf.write("\n".join(lines))

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python3 latex_table_of_runtimes.py input.csv output.tex")
        sys.exit(1)
    main(sys.argv[1], sys.argv[2])
