open! Base

let size_header_length = 8
let bin_write_size_header = Bin_prot.Write.bin_write_int_64bit

let%template[@kind k = (value, float32, float64, bits32, bits64, word)] bin_dump
  (type a : k)
  ?(header = false)
  (writer : a Bin_prot.Type_class.writer)
  (v : a)
  =
  let buf, pos, pos_len =
    let v_len = writer.size v in
    if header
    then (
      let tot_len = v_len + size_header_length in
      let buf = Bin_prot.Common.create_buf tot_len in
      let pos = bin_write_size_header buf ~pos:0 v_len in
      buf, pos, pos + v_len)
    else (
      let buf = Bin_prot.Common.create_buf v_len in
      buf, 0, v_len)
  in
  let pos = writer.write buf ~pos v in
  if pos = pos_len
  then buf
  else failwith "Bin_prot.Utils.bin_dump: size changed during writing"
;;
