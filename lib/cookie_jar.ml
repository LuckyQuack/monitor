type t = (string, string) Hashtbl.t

let create () : t = Hashtbl.create 8

let update (jar : t) (set_cookie_values : string list) : unit =
  List.iter (fun raw ->
    let pair = match String.split_on_char ';' raw with
      | first :: _ -> String.trim first
      | []         -> raw
    in
    match String.index_opt pair '=' with
    | None     -> ()
    | Some idx ->
      let k = String.sub pair 0 idx |> String.trim in
      let v = String.sub pair (idx + 1) (String.length pair - idx - 1) in
      if k <> "" then Hashtbl.replace jar k v
  ) set_cookie_values

let header (jar : t) : string option =
  if Hashtbl.length jar = 0 then None
  else
    Some (Hashtbl.fold
      (fun k v acc ->
        let pair = k ^ "=" ^ v in
        if acc = "" then pair else acc ^ "; " ^ pair)
      jar "")
