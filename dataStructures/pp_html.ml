let graph_page title deps header core f =
  let dependency f t =
    Format.fprintf f "<script src=\"%s\" charset=\"utf-8\"></script>" t in
  let () = Format.fprintf f "@[<v><!doctype html>@,@,<html>@," in
  let () = Format.fprintf f "@[<v 2><head>@,<meta charset=\"utf-8\">@," in
  let () = Format.fprintf f "<title>%t</title>@," title in
  let () = Pp.list ~trailing:Pp.space Pp.space dependency f deps in
  let () = Format.fprintf f "%t@]@,</head>@," header in
  let () = Format.fprintf f "@[<v 2><body>@,<h1>%t</h1>@," title in
  let () = Format.fprintf f "<svg width=960 height=600><g/></svg>@," in
  Format.fprintf f "%t@]@,</body>@,</html>@]@." core
