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

def html_escape(s: str) -> str:
    # minimal escaping for LaTeX special chars
    replace = {
        '\\': ' '
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

    # sort by number of rules in the model
    sorted_data = sorted(data.keys(), key=lambda k: int(model_nr_rules.get(k, "0")))

    step_names = [r"analysis", r"initial\\analysis", r"disable\\rules", r"add\\a rule"]
    analysis_items = [("1_full",["1_init"]), ("2_decremental",["1_init", "4_disable"]), ("3_incremental",["1_init"])]

    total_step_count = len(step_names)
    col_spec = "l c " + " ".join(["c"] * total_step_count)

    # Build LaTeX
    lines = []
    lines.append(r"\centering")
    lines.append(r"\small")
    lines.append(r"\begin{tabular}{" + col_spec + "}")
    lines.append(r"\toprule")
    # First header row
    lines.append(r"\textbf{Model} & \textbf{Nr. of rules} & \multicolumn{1}{c}{\textbf{non-incremental}} & \multicolumn{3}{c}{\textbf{incremental}} \\")
    lines.append(r"\cmidrule(lr){3-3}")
    lines.append(r"\cmidrule(lr){4-6}")
    # Second header row
    header = r"& & \bfseries\shortstack{" + r"} & \bfseries\shortstack{".join(step_names) + r"}\\"
    lines.append(header)
    lines.append(r"\midrule")

    for model in sorted_data:
        row_elems = []
        row_elems.append(r"\texttt{" + latex_escape(model) + "}")
        row_elems.append(latex_escape(model_nr_rules.get(model, "")))
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
    # write output
    with open(out_path, "w") as outf:
        outf.write("\n".join(lines))

    # Build HTML
    lines = []
    lines.append("<table>")
    # First header row
    lines.append("  <tr>")
    lines.append("    <th rowspan=\"2\">Model</th>")
    lines.append("    <th rowspan=\"2\">Nr. of rules</th>")
    lines.append("    <th colspan=\"1\">non-incremental</th>")
    lines.append("    <th colspan=\"3\">incremental</th>")
    lines.append("  </tr>")
    # Second header row
    header = "  <tr>\n    <th>" + "</th>\n    <th>".join([html_escape(step) for step in step_names]) + "</th>\n  </tr>"
    lines.append(header)

    for model in sorted_data:
        row_elems = []
        row_elems.append(model)
        row_elems.append(model_nr_rules.get(model, ""))
        for a, steps in analysis_items:
            for s in steps:
                val = data[model].get(a, {}).get(s, "")
                if val == "":
                    row_elems.append("")  # empty cell if missing
                else:
                    row_elems.append(format_time(val))
        lines.append("  <tr>\n    <td>" + "</td>\n    <td>".join(row_elems) + "</td>\n  </tr>")
    lines.append("</table>")
    # write output
    with open(out_path[:-4] + ".html", "w") as outf:
        outf.write("\n".join(lines))

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python3 latex_table_of_runtimes.py input.csv output.tex")
        sys.exit(1)
    main(sys.argv[1], sys.argv[2])
