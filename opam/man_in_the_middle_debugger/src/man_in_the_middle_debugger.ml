open Core
open Async
include Man_in_the_middle_debugger_intf

let name ~from ~to_ = Info.of_string (sprintf "%s -> %s" from to_)

module Make (Protocol : Protocol) = struct
  module Peer = Peer

  let wrap_reader ~my_name ~peer_name ~f reader =
    let pipe_r, pipe_w = Pipe.create () in
    let angstrom_exit_status =
      let%bind result =
        Angstrom_async.parse_many
          Protocol.parser_
          (fun message ->
            f `Received message;
            Pipe.write_without_pushback pipe_w (Protocol.to_string message);
            return ())
          reader
      in
      let%bind () = Reader.close reader in
      Pipe.close pipe_w;
      return result
    in
    let%map outer_reader = Reader.of_pipe (name ~from:peer_name ~to_:my_name) pipe_r in
    don't_wait_for
      (let%bind () = Reader.close_finished outer_reader in
       let%bind () = Reader.close reader in
       let%bind () = Pipe.closed pipe_w in
       Pipe.close_read pipe_r;
       return ());
    outer_reader, `Stopped_reading angstrom_exit_status
  ;;

  let wrap_writer ~my_name ~peer_name ~f writer =
    (* Because readers and writers are associated with file descriptors, to produce one
       you need a file descriptor, so we need [Unix.pipe] where one end is used by the
       outer reader/writer and the other is connected to the inner reader/writer via a
       [Pipe.t] (which is *not* associated with a file descriptor - it's purely
       in-process). In the case of [wrap_reader], the [Reader.of_pipe] call is doing the
       [Unix.pipe] behind the scenes. *)
    let%map `Reader reader_fd, `Writer writer_fd =
      Unix.pipe (name ~from:my_name ~to_:peer_name)
    in
    let reader = Reader.create reader_fd in
    let outer_writer = Writer.create writer_fd in
    let angstrom_exit_status =
      let%bind result =
        Angstrom_async.parse_many
          Protocol.parser_
          (fun message ->
            f `Sent message;
            match Writer.is_closed writer || Fd.is_closed (Writer.fd writer) with
            | true -> return ()
            | false ->
              Writer.write writer (Protocol.to_string message);
              Writer.flushed writer)
          reader
      in
      let%bind () = Reader.close reader in
      let%bind () = Writer.close writer in
      return result
    in
    outer_writer, `Stopped_writing angstrom_exit_status
  ;;

  let wrap_connection_to_peer { Peer.reader; writer; name = peer_name } ~my_name ~f =
    let%map () = return ()
    and reader, stopped_reading = wrap_reader ~my_name ~peer_name ~f reader
    and writer, stopped_writing = wrap_writer ~my_name ~peer_name ~f writer in
    reader, writer, stopped_reading, stopped_writing
  ;;

  let connect_peers_and_listen ~peer1 ~peer2 ~f =
    let%bind ( peer2_reader
             , peer2_writer
             , `Stopped_reading stopped_reading_from_peer2
             , `Stopped_writing stopped_writing_to_peer2 )
      =
      wrap_connection_to_peer
        peer2
        ~f:(fun dir message ->
          let dir =
            match dir with
            | `Received -> `Peer_2_to_1
            | `Sent -> `Peer_1_to_2
          in
          f dir message)
        ~my_name:peer1.Peer.name
    in
    let%bind () =
      let done_sending ~from_reader ~to_writer =
        let to_pipe = Writer.pipe to_writer in
        let%bind () = Reader.transfer from_reader to_pipe in
        let%bind () = Reader.close from_reader in
        let%bind (`Ok | `Reader_closed) = Pipe.upstream_flushed to_pipe in
        Writer.close to_writer
      in
      [ done_sending ~from_reader:peer1.reader ~to_writer:peer2_writer
      ; done_sending ~from_reader:peer2_reader ~to_writer:peer1.writer
      ]
      |> Deferred.all_unit
    in
    let%map stopped_reading_from_peer2 and stopped_writing_to_peer2 in
    `Peer1 stopped_writing_to_peer2, `Peer2 stopped_reading_from_peer2
  ;;
end
