"""
latex_table_of_runtimes.py

Usage:
    python3 latex_table_of_runtimes.py input.csv output.tex

Assumptions about CSV columns (no header):
    0: model (string)
    1: nr_rules (int or string)
    2: analysis_type (like "1_full")
    3: step_name     (like "1_initialization")
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

def parse_prefix_name(field: str):
    """
    Parse fields like "1_full" or "10_somename" into (index:int, name:str).
    If no underscore present, return (large_index, field).
    """
    if '_' in field:
        idx_str, name = field.split('_', 1)
        try:
            idx = int(idx_str)
        except ValueError:
            idx = 10**9
        return idx, name
    return 10**9, field

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
    # track analysis order and step order
    analysis_idx_map = {}  # analysis_name -> idx
    step_idx_map = defaultdict(dict)  # analysis_name -> {step_name: idx}

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

            a_idx, a_name = parse_prefix_name(analysis_type)
            s_idx, s_name = parse_prefix_name(step_name)

            # if same step name appears multiple times with different indices, keep the smallest index
            analysis_idx_map[a_name] = min(analysis_idx_map.get(a_name, 10**9), a_idx)
            step_idx_map[a_name][s_name] = min(step_idx_map[a_name].get(s_name, 10**9), s_idx)

            data[model][a_name][s_name] = time_val
            # keep nr_rules (if inconsistent across rows for same model, keep first seen)
            if model not in model_nr_rules:
                model_nr_rules[model] = nr_rules

    total_step_count = 10
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

    # First header row
    lines.append(r"\textbf{Model} & \textbf{Nr. of rules} & \multicolumn{3}{c}{\textbf{full}} & \multicolumn{3}{c}{\textbf{partial}} & \multicolumn{4}{c}{\textbf{incremental}} \\")
    lines.append(r"\cmidrule(lr){3-5}")
    lines.append(r"\cmidrule(lr){6-8}")
    lines.append(r"\cmidrule(lr){9-12}")
    # Second header row
    lines.append(r"& & \bfseries\shortstack{Domain\\initialization} & \bfseries\shortstack{Initial\\state} & \bfseries\shortstack{Fixpoint} & \bfseries\shortstack{Domain\\initialization} & \bfseries\shortstack{Initial\\state} & \bfseries\shortstack{Fixpoint} & \bfseries\shortstack{Domain\\initialization} & \bfseries\shortstack{Initial\\state} & \bfseries\shortstack{Fixpoint} & \bfseries\shortstack{Disable\\rules} \\")
    lines.append(r"\midrule")

    for model in sorted(data.keys()):
        row_elems = []
        row_elems.append(r"\texttt{" + latex_escape(model) + "}")
        row_elems.append(latex_escape(model_nr_rules.get(model, "")))
        analysis_items = [("full",["initialization", "initial_state", "fixpoint"]), ("partial",["initialization", "initial_state", "fixpoint"]), ("incremental",["initialization", "initial_state", "fixpoint", "disable"])]
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
