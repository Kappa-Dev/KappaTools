let string_of_un_op logger op =
  let format = Loggers.get_encoding_format logger in
  match op with
  | Operator.UMINUS->
    begin
      match
        format
      with
      | Loggers.SBML -> "<minus/>"
      | Loggers.Octave | Loggers.Matlab ->
        "-"
      | Loggers.Maple
      | Loggers.Json
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.LOG ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<log/>"
      | Loggers.Octave | Loggers.Matlab ->
        "log"
      | Loggers.Maple
      | Loggers.Json
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.SQRT ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<root/>"
      | Loggers.Octave | Loggers.Matlab ->
        "sqrt"
      | Loggers.Maple
      | Loggers.Json
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.EXP ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<exp/>"
      | Loggers.Octave | Loggers.Matlab ->
        "exp"
      | Loggers.Maple
      | Loggers.Json
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.SINUS ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<sin/>"
      | Loggers.Octave | Loggers.Matlab ->
        "sin"
      | Loggers.Maple
      | Loggers.Json
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.COSINUS ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<cos/>"
      | Loggers.Octave | Loggers.Matlab ->
        "cos"
      | Loggers.Maple
      | Loggers.Json
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.TAN ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<tan/>"
      | Loggers.Octave | Loggers.Matlab ->
        "tan"
      | Loggers.Maple
      | Loggers.Json
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.INT ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<floor/>"
      | Loggers.Octave | Loggers.Matlab ->
        "floor"
      | Loggers.Maple
      | Loggers.Json
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end

let string_of_compare_op logger op =
  let format = Loggers.get_encoding_format logger in
  match op with
  | Operator.EQUAL ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<eq/>"
      | Loggers.Octave | Loggers.Matlab ->
        "=="
      | Loggers.Maple
      | Loggers.Json
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.DIFF ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<neq/>"
      | Loggers.Octave | Loggers.Matlab ->
        "!="
      | Loggers.Maple
      | Loggers.Json
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.SMALLER ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<lt/>"
      | Loggers.Octave | Loggers.Matlab ->
        "<"
      | Loggers.Maple
      | Loggers.Json
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.GREATER ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<gt/>"
      | Loggers.Octave | Loggers.Matlab ->
        ">"
      | Loggers.Maple
      | Loggers.Json
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end

let string_of_bin_op logger op =
  let format = Loggers.get_encoding_format logger in
  match op with
  | Operator.MODULO ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<rem/>"
      | Loggers.Octave | Loggers.Matlab -> "mod"
      | Loggers.Maple
      | Loggers.Json
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.MAX ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<max/>"
      | Loggers.Octave | Loggers.Matlab -> "max"
      | Loggers.Maple
      | Loggers.Json
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.MIN ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<min/>"
      | Loggers.Octave | Loggers.Matlab -> "min"
      | Loggers.Maple
      | Loggers.Json
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.DIV ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<divide/>"
      | Loggers.Octave | Loggers.Matlab -> "/"
      | Loggers.Maple
      | Loggers.Json
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.SUM ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<plus/>"
      | Loggers.Octave | Loggers.Matlab -> "+"
      | Loggers.Maple
      | Loggers.Json
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.MINUS ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<minus/>"
      | Loggers.Octave | Loggers.Matlab -> "-"
      | Loggers.Maple
      | Loggers.Json
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.MULT ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<times/>"
      | Loggers.Octave | Loggers.Matlab -> "*"
      | Loggers.Maple
      | Loggers.Json
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.POW ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<power/>"
      | Loggers.Octave | Loggers.Matlab -> "**"
      | Loggers.Maple
      | Loggers.Json
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end

let string_of_bool_op logger op =
  let format = Loggers.get_encoding_format logger in
  match op with
  | Operator.AND ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<and/>"
      | Loggers.Octave | Loggers.Matlab -> "&"
      | Loggers.Maple
      | Loggers.Json
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.OR ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<or/>"
      | Loggers.Octave | Loggers.Matlab -> "|"
      | Loggers.Maple
      | Loggers.Json
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
