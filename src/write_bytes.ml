(* NOTE:
   $ runo src/write_binary.ml out/hello.txt
   $ xxd out/hello.txt
   00000000: 4865 6c6c 6f2c 2077 6f72 6c64 21         Hello, world! *)

let () : unit =
  if Array.length Sys.argv <> 2 then
    exit 0;
  let hex : int array = [|
    0x48;
    0x65;
    0x6c;
    0x6c;
    0x6f;
    0x2c;
    0x20;
    0x77;
    0x6f;
    0x72;
    0x6c;
    0x64;
    0x21;
  |] in
  let path : string = Sys.argv.(1) in
  let file : out_channel = open_out_bin path in
  let n : int = Array.length hex in
  let buffer : bytes = Bytes.create n in
  for i = 0 to (n - 1) do
    Bytes.set_uint8 buffer i hex.(i);
  done;
  output_bytes file buffer;
  close_out file
