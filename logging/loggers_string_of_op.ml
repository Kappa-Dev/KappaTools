let string_of_un_op logger op =
  let format = Loggers.get_encoding_format logger in
  match op with
  | Operator.UMINUS->
    begin
      match
        format
      with
      | Loggers.SBML -> "<minus/>"
      | Loggers.DOTNET
      | Loggers.Octave | Loggers.Matlab
      | Loggers.Maple | Loggers.Mathematica -> "-"
      | Loggers.Json
      | Loggers.DOT
      | Loggers.Matrix | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.LOG ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<log/>"
      | Loggers.DOTNET
      | Loggers.Octave | Loggers.Matlab
      | Loggers.Maple | Loggers.Mathematica
        -> "log"
      | Loggers.Json
      | Loggers.DOT
      | Loggers.Matrix | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.SQRT ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<root/>"
      | Loggers.DOTNET
      | Loggers.Octave | Loggers.Matlab | Loggers.Maple | Loggers.Mathematica
        ->
        "sqrt"
      | Loggers.Json
      | Loggers.DOT
      | Loggers.Matrix | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.EXP ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<exp/>"
      | Loggers.DOTNET
      | Loggers.Octave | Loggers.Matlab | Loggers.Maple | Loggers.Mathematica ->
        "exp"
      | Loggers.Json
      | Loggers.DOT
      | Loggers.Matrix | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.SINUS ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<sin/>"
      | Loggers.DOTNET
      | Loggers.Octave | Loggers.Matlab | Loggers.Maple | Loggers.Mathematica
        -> "sin"
      | Loggers.Json
      | Loggers.DOT
      | Loggers.Matrix | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.COSINUS ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<cos/>"
      | Loggers.DOTNET
      | Loggers.Octave | Loggers.Matlab | Loggers.Maple | Loggers.Mathematica
        ->
        "cos"
      | Loggers.Json
      | Loggers.DOT
      | Loggers.Matrix | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.TAN ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<tan/>"
      | Loggers.DOTNET
      | Loggers.Octave | Loggers.Matlab | Loggers.Maple | Loggers.Mathematica  ->
        "tan"
      | Loggers.Json
      | Loggers.DOT
      | Loggers.Matrix | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML
      | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.INT ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<floor/>"
      | Loggers.DOTNET
      | Loggers.Octave | Loggers.Matlab | Loggers.Maple | Loggers.Mathematica ->
        "floor"
      | Loggers.Json
      | Loggers.DOT
      | Loggers.Matrix
      | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML | Loggers.HTML_Tabular
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
      | Loggers.DOTNET
      | Loggers.Maple | Loggers.Mathematica -> "="
      | Loggers.Json
      | Loggers.DOT
      | Loggers.Matrix
      | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.DIFF ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<neq/>"
      | Loggers.DOTNET
      | Loggers.Octave | Loggers.Matlab | Loggers.Maple | Loggers.Mathematica
        ->
        "!="
      | Loggers.Json
      | Loggers.DOT
      | Loggers.Matrix
      | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.SMALLER ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<lt/>"
      | Loggers.DOTNET
      | Loggers.Octave | Loggers.Matlab | Loggers.Maple | Loggers.Mathematica
        ->
        "<"
      | Loggers.Json
      | Loggers.Matrix
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.GREATER ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<gt/>"
      | Loggers.DOTNET
      | Loggers.Octave | Loggers.Matlab | Loggers.Maple | Loggers.Mathematica
        ->
        ">"
      | Loggers.Json
      | Loggers.Matrix
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML | Loggers.HTML_Tabular
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
      | Loggers.DOTNET
      | Loggers.Octave | Loggers.Matlab | Loggers.Maple
      | Loggers.Mathematica  -> "mod"
      | Loggers.Json
      | Loggers.Matrix
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.MAX ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<max/>"
      | Loggers.DOTNET
      | Loggers.Octave | Loggers.Matlab | Loggers.Maple | Loggers.Mathematica
        -> "max"
      | Loggers.Json
      | Loggers.Matrix
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.MIN ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<min/>"
      | Loggers.DOTNET
      | Loggers.Octave | Loggers.Matlab | Loggers.Maple | Loggers.Mathematica
        ->
        "min"
      | Loggers.Json
      | Loggers.Matrix
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.DIV ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<divide/>"
      | Loggers.DOTNET
      | Loggers.Maple | Loggers.Mathematica | Loggers.Octave | Loggers.Matlab -> "/"
      | Loggers.Json
      | Loggers.Matrix
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.SUM ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<plus/>"
      | Loggers.Maple | Loggers.Mathematica
      | Loggers.DOTNET
      | Loggers.Octave | Loggers.Matlab -> "+"
      | Loggers.Json
      | Loggers.Matrix
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.MINUS ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<minus/>"
      | Loggers.Maple | Loggers.Mathematica
      | Loggers.DOTNET
      | Loggers.Octave | Loggers.Matlab -> "-"
      | Loggers.Json
      | Loggers.Matrix
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.MULT ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<times/>"
      | Loggers.Maple | Loggers.Mathematica
      | Loggers.DOTNET
      | Loggers.Octave | Loggers.Matlab -> "*"
      | Loggers.Json
      | Loggers.Matrix
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.POW ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<power/>"
      | Loggers.Maple | Loggers.Mathematica
      | Loggers.DOTNET
      | Loggers.Octave | Loggers.Matlab -> "**"
      | Loggers.Json
      | Loggers.Matrix
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end

let string_of_bin_bool_op logger op =
  let format = Loggers.get_encoding_format logger in
  match op with
  | Operator.AND ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<and/>"
      | Loggers.DOTNET
      | Loggers.Maple | Loggers.Mathematica
      | Loggers.Octave | Loggers.Matlab -> "&"
      | Loggers.Json
      | Loggers.Matrix
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
  | Operator.OR ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<or/>"
      | Loggers.DOTNET
      | Loggers.Maple | Loggers.Mathematica
      | Loggers.Octave | Loggers.Matlab -> "|"
      | Loggers.Matrix
      | Loggers.Json
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end

let string_of_un_bool_op logger op =
  let format = Loggers.get_encoding_format logger in
  match op with
  | Operator.NOT ->
    begin
      match
        format
      with
      | Loggers.SBML -> "<not/>"
      | Loggers.DOTNET
      | Loggers.Maple | Loggers.Mathematica
      | Loggers.Octave | Loggers.Matlab -> "!"
      | Loggers.Json
      | Loggers.Matrix
      | Loggers.DOT
      | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML | Loggers.HTML_Tabular
      | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ""
    end
