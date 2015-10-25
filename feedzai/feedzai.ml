open Core.Std

let sandbox_url = "api.sbx.gomo.do"

let production_url = "https://api.gomo.do"

type api_url = {
  env: string; (** environment *)
  api_v: string; (** api version *)
  res: string; (** resource *)
  op: string; (** operation *)
}

let get_timestamp () =
  let time = Time.now () in
  let time' = Float.modf @@ Time.to_float time in
  let seconds = Float.to_int @@ Float.Parts.integral time' in
  let millis = Float.to_int @@ 1000.0 *. Float.Parts.fractional time' in
  seconds + millis

(** e.g.
 * [ build_query sandbox_url "YiiModo/api_v2" "people" "register"
 *)
let build_request_url url =
  "https://" ^ url.env ^ "/" ^ url.api_v ^ "/" ^ url.res ^ "/" ^ url.op

let send_query req_url req_body key secret =
  let uri = "/" ^ req_url.api_v ^ "/" ^ req_url.res ^ "/" ^ req_url.op in
  let timestamp = get_timestamp () in
  let sigv = "MODO1" in
  let sigkey =
    let hasher = Cryptokit.MAC.hmac_sha256 (sigv ^ secret) in
    hasher#add_string (string_of_int timestamp);
    hasher#result
  in
  let signature =
    let hasher = Cryptokit.MAC.hmac_sha256 sigkey in
    hasher#add_string((string_of_int timestamp) ^ "&" ^ uri ^ "&" ^ req_body);
    hasher#result
  in
  let auth_head = Printf.(sprintf "%s key=%s, sig=%s" sigv key signature) in
  let headers =
    let x = Cohttp.Header.init () in
    begin
      ignore(Cohttp.Header.add x "Accept" "*/*");
      ignore(Cohttp.Header.add x "X-Modo-Date" (string_of_int timestamp));
      ignore(Cohttp.Header.add x "Authorization" auth_head);
      ignore(Cohttp.Header.add x "X-Modo-Date" (string_of_int timestamp));
      ignore(Cohttp.Header.add x "Content-Length" (string_of_int @@ String.length req_body));
      ignore(Cohttp.Header.add x "Content-Type" "application/x-www-form-urlencoded");
    end;
    x
  in
  Cohttp_lwt_unix.Client.post ~body:(Cohttp_lwt_body.(`String req_body)) ~headers:headers (Uri.of_string @@ build_request_url req_url)


