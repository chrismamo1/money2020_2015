open Humane_re

{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
}}

type perkele = {
  location: string;
  category_name: string;
  title: string;
}

module Hack2020_app =
  Eliom_registration.App (
    struct
      let application_name = "hack2020"
    end)

let main_service =
  Eliom_service.App.service ~path:["search"] ~get_params:(Eliom_parameter.string "q") ()

let nvget str key =
  Str.(find_matches (regexp @@ key ^ "=\\(.*\\)&")) str
  |> List.hd

let parse_nv str =
  let os = open_out "test.out" in
  let n =
    nvget str "searchResult(.*).@count"
    |> Str.(find_matches (regexp "\\([0-9]+\\)"))
    |> List.hd
    |> int_of_string in
  let rval = Array.make n { location= ""; title = ""; category_name = ""; } in
  for i = 0 to n - 1 do
    rval.(i) <-
      { location =
          Printf.sprintf "searchResult(0).item(%d).location(0)" i
          |> nvget str;
        title =
          Printf.sprintf "searchResult(0).item(%d).title(0)" i
          |> nvget str;
        category_name =
          Printf.sprintf "searchResult(0).item(%d).primaryCategory(0).categoryName(0)" i
          |> nvget str;
      }
  done;
  ignore(output_string os (string_of_int n));
  ignore(close_out os);
  rval;;

let () =
  Eliom_registration.Html_text.register
    ~service:main_service
    (fun s () ->
      let open Nethttp_client.Convenience in
      let resp = http_get ("http://svcs.ebay.com/services/search/FindingService/v1?OPERATION-NAME=findItemsAdvanced&SERVICE-VERSION=1.11.0&SECURITY-APPNAME=Money202-d72e-424c-88c4-2aaf8a598da6&RESPONSE-DATA-FORMAT=NV&keywords=" ^ s) in
      let items =
        parse_nv resp
        |> Array.to_list
        |> List.map (fun i -> "<li>" ^ i.category_name ^ i.location ^ i.title ^ "</li>")
      in
      Lwt.return ("<ul>" ^ (List.fold_left (^) "" items) ^ "</ul>")
        (*Eliom_tools.F.html
           ~title:"hack2020"
           ~css:[["css";"hack2020.css"]]
           Html5.F.(body [
             h2 [items];
           ])*))
