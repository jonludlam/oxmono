(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Eio integration for httpz - response writing and connection handling *)

open Base

module I16 = Stdlib_stable.Int16_u
let[@inline] i16 x = I16.of_int x
let[@inline] to_int x = I16.to_int x

(** {1 Response Writing} *)

(** Write response headers to buffer using typed Header_name.t.
    Returns header length. *)
let rec write_headers_loop buf off (headers : Httpz_server.Route.resp_header list) =
  match headers with
  | [] -> off
  | (name, value) :: rest ->
      let off = Httpz.Res.write_header_name buf ~off name value in
      write_headers_loop buf off rest

type content_length_mode =
  | Known of int              (* Content-Length: N *)
  | Chunked                   (* Transfer-Encoding: chunked *)
  | Omit                      (* No content-length header (valid for HEAD per RFC 7230) *)

let write_response_headers buf ~off ~keep_alive ~content_length_mode version ~status
    ~(headers : Httpz_server.Route.resp_header list) =
  let off = Httpz.Res.write_status_line buf ~off status version in
  let off = write_headers_loop buf off headers in
  let off = match content_length_mode with
    | Known len -> Httpz.Res.write_content_length buf ~off len
    | Chunked -> Httpz.Res.write_header_name buf ~off Httpz.Header_name.Transfer_encoding "chunked"
    | Omit -> off  (* No content-length header for HEAD responses where we don't know the size *)
  in
  let off = Httpz.Res.write_connection buf ~off ~keep_alive in
  let off = Httpz.Res.write_crlf buf ~off in
  to_int off

(** {1 Connection State} *)

(** Response buffer size - 64KB for headers *)
let response_buffer_size = 65536

type 'a conn = {
  flow : 'a Eio.Net.stream_socket;
  read_buf : bytes;     (* For httpz parser *)
  write_buf : bytes;    (* For httpz response writer *)
  read_cs : Cstruct.t;  (* For Eio reading - separate buffer, we blit to read_buf *)
  mutable read_len : int;
  mutable keep_alive : bool;
}

let create_conn flow =
  {
    flow;
    read_buf = Bytes.create Httpz.buffer_size;
    write_buf = Bytes.create response_buffer_size;
    read_cs = Cstruct.create Httpz.buffer_size;
    read_len = 0;
    keep_alive = true;
  }

(** {1 Response Writing with Eio} *)

(** Create a respond function that writes directly to the connection.
    This is the CPS callback passed to dispatch - no intermediate resp record.

    For HEAD requests per {{:https://datatracker.ietf.org/doc/html/rfc7230#section-3.3.2}
    RFC 7230 Section 3.3.2}: "A server MAY send a Content-Length header field in a
    response to a HEAD request; a server MUST NOT send Content-Length in such a
    response unless its field-value equals the decimal number of octets that would
    have been sent in the payload body of a response if the same request had used
    the GET method."

    When body is Empty for HEAD (meaning we skipped generation), we omit Content-Length
    since we don't know the actual GET body size. When body is String/Bigstring for HEAD,
    we include Content-Length since we have the actual body. *)
let make_respond conn ~is_head ~keep_alive version ~status ~headers body =
  let buf = conn.write_buf in
  match body with
  | Httpz_server.Route.Empty ->
      (* For HEAD with Empty body, we don't know the real content length, so omit it.
         Per RFC 7230 3.3.2, Content-Length MAY be omitted for HEAD responses. *)
      let content_length_mode = if is_head then Omit else Known 0 in
      let header_len = write_response_headers buf ~off:(i16 0) ~keep_alive
        ~content_length_mode version ~status ~headers in
      Eio.Flow.write conn.flow [Cstruct.of_bytes conn.write_buf ~off:0 ~len:header_len]

  | Httpz_server.Route.String body_str ->
      (* We have the actual body, so Content-Length is accurate even for HEAD *)
      let header_len = write_response_headers buf ~off:(i16 0) ~keep_alive
        ~content_length_mode:(Known (String.length body_str)) version ~status ~headers in
      if is_head then
        (* HEAD: send headers with correct Content-Length but no body *)
        Eio.Flow.write conn.flow [Cstruct.of_bytes conn.write_buf ~off:0 ~len:header_len]
      else
        Eio.Flow.write conn.flow [
          Cstruct.of_bytes conn.write_buf ~off:0 ~len:header_len;
          Cstruct.of_string body_str;
        ]

  | Httpz_server.Route.Bigstring { buf = body_buf; off; len } ->
      (* We have the actual body, so Content-Length is accurate even for HEAD *)
      let header_len = write_response_headers buf ~off:(i16 0) ~keep_alive
        ~content_length_mode:(Known len) version ~status ~headers in
      if is_head then
        Eio.Flow.write conn.flow [Cstruct.of_bytes conn.write_buf ~off:0 ~len:header_len]
      else
        Eio.Flow.write conn.flow [
          Cstruct.of_bytes conn.write_buf ~off:0 ~len:header_len;
          Cstruct.of_bigarray body_buf ~off ~len;
        ]

  | Httpz_server.Route.Stream { length; iter } ->
      (* For streams: use known length if available, omit for HEAD without length, chunked otherwise *)
      let content_length_mode = match length, is_head with
        | Some len, _ -> Known len
        | None, true -> Omit
        | None, false -> Chunked
      in
      let header_len = write_response_headers buf ~off:(i16 0) ~keep_alive
        ~content_length_mode version ~status ~headers in
      Eio.Flow.write conn.flow [Cstruct.of_bytes conn.write_buf ~off:0 ~len:header_len];
      if is_head then
        (* HEAD: headers only, skip streaming body *)
        ()
      else begin
        (* Stream chunks - if no content-length, use chunked encoding *)
        match length with
        | Some _ ->
            (* Known length - just write chunks directly *)
            iter (fun chunk -> Eio.Flow.write conn.flow [Cstruct.of_string chunk])
        | None ->
            (* Chunked encoding *)
            iter (fun chunk ->
              let len = String.length chunk in
              if len > 0 then begin
                let hex = Printf.sprintf "%x\r\n" len in
                Eio.Flow.write conn.flow [
                  Cstruct.of_string hex;
                  Cstruct.of_string chunk;
                  Cstruct.of_string "\r\n";
                ]
              end);
            (* Final chunk *)
            Eio.Flow.write conn.flow [Cstruct.of_string "0\r\n\r\n"]
      end

(** Send error response *)
let send_error conn status message version =
  let buf = conn.write_buf in
  let off = Httpz.Res.write_status_line buf ~off:(i16 0) status version in
  let off = Httpz.Res.write_header_name buf ~off Httpz.Header_name.Content_type "text/plain" in
  let off = Httpz.Res.write_content_length buf ~off (String.length message) in
  let off = Httpz.Res.write_connection buf ~off ~keep_alive:conn.keep_alive in
  let off = Httpz.Res.write_crlf buf ~off in
  let header_len = to_int off in
  Eio.Flow.write conn.flow
    [
      Cstruct.of_bytes conn.write_buf ~off:0 ~len:header_len;
      Cstruct.of_string message;
    ]

(** {1 Buffer Operations} *)

(** Read more data into buffer *)
let read_more conn =
  if conn.read_len >= Httpz.buffer_size then `Buffer_full
  else
    let available = Httpz.buffer_size - conn.read_len in
    let cs = Cstruct.sub conn.read_cs conn.read_len available in
    match Eio.Flow.single_read conn.flow cs with
    | n ->
        (* Blit from cstruct to bytes so httpz parser can access it *)
        Cstruct.blit_to_bytes cs 0 conn.read_buf conn.read_len n;
        conn.read_len <- conn.read_len + n;
        `Ok n
    | exception End_of_file -> `Eof

(** Shift buffer contents to remove processed data *)
let shift_buffer conn consumed =
  if consumed > 0 && consumed < conn.read_len then begin
    Bytes.blit ~src:conn.read_buf ~src_pos:consumed ~dst:conn.read_buf ~dst_pos:0 ~len:(conn.read_len - consumed);
    conn.read_len <- conn.read_len - consumed
  end
  else if consumed >= conn.read_len then conn.read_len <- 0

(** {1 Request Handling} *)

(** Handle one request. Returns `Continue, `Close, or `Need_more *)
let handle_request conn ~routes ~on_request =
  (* Create string view of bytes for parsing *)
  let buf = conn.read_buf in
  let len = conn.read_len in
  let len16 = i16 len in
  let #(status, req, headers) =
    Httpz.parse buf ~len:len16 ~limits:Httpz.default_limits
  in
  let body_off = to_int req.#body_off in
  let version = req.#version in
  match status with
  | Httpz.Buf_read.Complete ->
      let meth = req.#meth in
      (* Parse target once - used for both logging and dispatch *)
      let target = Httpz.Target.parse buf req.#target in
      let path_span = Httpz.Target.path target in
      let path_str = Httpz.Span.to_string buf path_span in
      (* Update keep_alive before dispatch *)
      conn.keep_alive <- req.#keep_alive;
      (* Check if this is a HEAD request for body suppression *)
      let is_head = Poly.equal meth Httpz.Method.Head in
      (* Create respond function and track status for logging *)
      let logged_status = ref Httpz.Res.Success in
      let respond ~status ~headers body =
        logged_status := status;
        make_respond conn ~is_head ~keep_alive:conn.keep_alive version ~status ~headers body
      in
      (* Dispatch - respond is called directly by handler *)
      let matched = Httpz_server.Route.dispatch buf ~meth ~target ~headers routes ~respond in
      if not matched then begin
        logged_status := Httpz.Res.Not_found;
        Httpz_server.Route.not_found respond
      end;
      (* Call request callback for logging etc *)
      on_request ~meth ~path:path_str ~status:!logged_status;
      (* Calculate consumed bytes *)
      let body_span = Httpz.Req.body_span ~len:len16 req in
      let body_span_len = Httpz.Span.len body_span in
      let body_span_off = Httpz.Span.off body_span in
      let consumed =
        if body_span_len > 0 then body_span_off + body_span_len else body_off
      in
      shift_buffer conn consumed;
      if conn.keep_alive then `Continue else `Close
  | Httpz.Buf_read.Partial -> `Need_more
  | Httpz.Buf_read.Headers_too_large | Httpz.Buf_read.Content_length_overflow ->
      conn.keep_alive <- false;
      send_error conn Httpz.Res.Payload_too_large "Payload Too Large"
        Httpz.Version.Http_1_1;
      `Close
  | Httpz.Buf_read.Bare_cr_detected | Httpz.Buf_read.Ambiguous_framing ->
      conn.keep_alive <- false;
      send_error conn Httpz.Res.Bad_request "Bad Request" Httpz.Version.Http_1_1;
      `Close
  | Httpz.Buf_read.Missing_host_header ->
      conn.keep_alive <- false;
      send_error conn Httpz.Res.Bad_request "Missing Host Header"
        Httpz.Version.Http_1_1;
      `Close
  | _ ->
      conn.keep_alive <- false;
      send_error conn Httpz.Res.Bad_request "Bad Request" Httpz.Version.Http_1_1;
      `Close

(** Send payload too large error and close connection *)
let send_payload_too_large conn =
  conn.keep_alive <- false;
  send_error conn Httpz.Res.Payload_too_large "Payload Too Large"
    Httpz.Version.Http_1_1

(** Handle connection loop *)
let handle_connection conn ~routes ~on_request =
  let handle_read_result ~continue = function
    | `Eof -> ()
    | `Buffer_full -> send_payload_too_large conn
    | `Ok _ -> continue ()
  in
  let rec loop () =
    if conn.read_len = 0 then
      handle_read_result ~continue:loop (read_more conn)
    else
      match handle_request conn ~routes ~on_request with
      | `Continue -> loop ()
      | `Close -> ()
      | `Need_more -> handle_read_result ~continue:loop (read_more conn)
  in
  loop ()

(** Handle a single client connection *)
let handle_client ~routes ~on_request ~on_error flow _addr =
  let conn = create_conn flow in
  try handle_connection conn ~routes ~on_request
  with exn -> on_error exn
