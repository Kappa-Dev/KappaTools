"""
latex_table_compare_ws.py

Usage:
    python3 latex_table_compare_ws.py input.csv output.tex

Assumptions about CSV columns: analysis_type,step_name,nr_removed_rules,time
    0: model_name (like "tgf_v19")
    1: model_name_again (like "tgf_v19")
    2: nr_removed_rules  (int)
    3: time (float)

The script writes a LaTeX table that summarizes the benchmark results.
"""
import sys
import csv
from collections import defaultdict

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
    return "{:.4g}".format(f)

def main(inp_path, out_path):
    # data[nr_removed_rules][model_name] = time
    data = defaultdict(lambda: defaultdict(dict))
    with open(inp_path, newline='') as f:
        reader = csv.reader(f)
        next(reader, None) # skip first row (headers)
        for row in reader:
            if not row or all(not c.strip() for c in row):
                continue
            if len(row) < 4:
                # skip malformed rows
                continue
            test_instance = row[2].strip()
            step_name = row[1].strip()
            time_val = row[3].strip()

            data[test_instance][step_name] = time_val

    all_steps = data["0"].keys()
    total_step_count = len(all_steps)
    col_spec = "c " + " ".join(["c"] * total_step_count)

    # Build LaTeX
    lines = []
    lines.append(r"\centering")
    lines.append(r"\small")
    lines.append(r"\begin{tabular}{" + col_spec + "}")
    lines.append(r"\toprule")
    header = r"\bfseries\shortstack{Nr. of rules\\in working set} "
    for step in all_steps:
        header += r"& \texttt{" + latex_escape(step) + "} "
    header += r"\\"
    lines.append(header)
    lines.append(r"\midrule")

    for test_instance in sorted(data.keys()):
        row_elems = []
        row_elems.append(latex_escape(test_instance))
        analysis_items = all_steps
        for s in analysis_items:
            val = data[test_instance].get(s, "")
            if val == "":
                row_elems.append("")  # empty cell if missing
            else:
                row_elems.append(format_time(val))
        lines.append(" & ".join(row_elems) + r" \\")
    lines.append(r"\bottomrule")
    lines.append(r"\end{tabular}")

    # write output
    with open(out_path, "w") as outf:
        outf.write("\n".join(lines))

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python3 latex_table_of_runtimes.py input.csv output.tex")
        sys.exit(1)
    main(sys.argv[1], sys.argv[2])
