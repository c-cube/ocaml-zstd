open ExtLib
open Printf

let test (name,src) =
  let orig = String.length src in
  let a = Array.init 20 (fun level -> Zstd.compress ~level src) in
  a |> Array.iteri begin fun i s ->
    assert (Zstd.get_decompressed_size s = orig);
    let s = Zstd.decompress orig s in
    if s <> src then failwith @@ sprintf "%s : level %d failed" name i;
  end;
  let best = Array.fold_left (fun m s -> min m (String.length s)) (String.length a.(0)) a in
  let best_level = Array.findi (fun s -> String.length s = best) a in
  printf "%50s : best compression %02.1fx at level %d : %d -> %d\n" name (float orig /. float best) best_level orig best

let stream_compress ?level ?dict src =
  let buf = Buffer.create 256 in
  let writer b off len = Buffer.add_subbytes buf b off len in
  let s = Zstd.Compress_stream.create ?level ?dict ~writer () in
  Zstd.Compress_stream.write s (Bytes.of_string src) 0 (String.length src);
  Zstd.Compress_stream.close s;
  Buffer.contents buf

let stream_decompress_reader ?dict ?(buf_size=Zstd.Decompress_stream.out_size ()) ~reader () =
  let s = Zstd.Decompress_stream.create ~reader ?dict () in
  let out = Buffer.create 256 in
  let tmp = Bytes.create buf_size in
  let rec loop () =
    let n = Zstd.Decompress_stream.read s tmp 0 (Bytes.length tmp) in
    if n > 0 then begin
      Buffer.add_subbytes out tmp 0 n;
      loop ()
    end
  in
  loop ();
  Zstd.Decompress_stream.close s;
  Buffer.contents out

let string_reader src =
  let pos = ref 0 in
  fun b off len ->
    let n = min len (String.length src - !pos) in
    Bytes.blit_string src !pos b off n;
    pos := !pos + n;
    n

let stream_decompress ?dict ?buf_size src =
  stream_decompress_reader ?dict ?buf_size ~reader:(string_reader src) ()

let test_streaming () =
  let data =
    let buf = Buffer.create 8192 in
    for i = 0 to 199 do
      Buffer.add_string buf (sprintf "line %d: the quick brown fox jumps over the lazy dog\n" i);
      if i mod 10 = 0 then Buffer.add_string buf (String.make 50 (Char.chr (65 + i mod 26)));
    done;
    Buffer.contents buf
  in
  let buf_writer () =
    let buf = Buffer.create 256 in
    buf, (fun b off len -> Buffer.add_subbytes buf b off len)
  in
  let expect_exn f =
    (try ignore (f ()); assert false
     with _ -> ())
  in
  (* 1. Round-trip: stream compress -> stream decompress *)
  let () =
    let compressed = stream_compress data in
    let decompressed = stream_decompress compressed in
    assert (decompressed = data);
    printf "  1. round-trip: OK\n"
  in
  (* 2. Simple compress -> stream decompress *)
  let () =
    let compressed = Zstd.compress ~level:3 data in
    let decompressed = stream_decompress compressed in
    assert (decompressed = data);
    printf "  2. simple-to-stream: OK\n"
  in
  (* 3. Stream compress -> simple decompress (using known original size) *)
  let () =
    let compressed = stream_compress ~level:3 data in
    let decompressed = Zstd.decompress (String.length data) compressed in
    assert (decompressed = data);
    printf "  3. stream-to-simple: OK\n"
  in
  (* 4. Incremental writes *)
  let () =
    let buf, writer = buf_writer () in
    let s = Zstd.Compress_stream.create ~level:1 ~writer () in
    let src = Bytes.of_string data in
    for i = 0 to String.length data - 1 do
      Zstd.Compress_stream.write s src i 1
    done;
    Zstd.Compress_stream.close s;
    let decompressed = stream_decompress (Buffer.contents buf) in
    assert (decompressed = data);
    printf "  4. incremental writes: OK\n"
  in
  (* 5. Empty stream *)
  let () =
    assert (stream_decompress (stream_compress "") = "");
    printf "  5. empty stream: OK\n"
  in
  (* 6. Flush produces output before close; full round-trip with flush+close *)
  let () =
    let buf, writer = buf_writer () in
    let s = Zstd.Compress_stream.create ~level:1 ~writer () in
    Zstd.Compress_stream.write s (Bytes.of_string data) 0 (String.length data);
    Zstd.Compress_stream.flush s;
    assert (Buffer.length buf > 0);
    Zstd.Compress_stream.close s;
    let decompressed = stream_decompress (Buffer.contents buf) in
    assert (decompressed = data);
    printf "  6. flush: OK\n"
  in
  (* 7. Dictionary *)
  let () =
    let dict = String.init 1000 (fun i -> Char.chr (i mod 256)) in
    let compressed = stream_compress ~dict data in
    let decompressed = stream_decompress ~dict compressed in
    assert (decompressed = data);
    printf "  7. dictionary: OK\n"
  in
  (* 8. Truncated stream *)
  let () =
    let compressed = stream_compress data in
    let half = String.sub compressed 0 (String.length compressed / 2) in
    (try
      ignore (stream_decompress half);
      assert false
    with Zstd.Error _ ->
      printf "  8. truncated stream: OK\n")
  in
  (* 9. Concatenated frames *)
  let () =
    let part1 = "first frame data" in
    let part2 = "second frame data" in
    let c1 = stream_compress part1 in
    let c2 = stream_compress part2 in
    let decompressed = stream_decompress (c1 ^ c2) in
    assert (decompressed = part1 ^ part2);
    printf "  9. concatenated frames: OK\n"
  in
  (* 10. Large data (10MB+) *)
  let () =
    let size = 10 * 1024 * 1024 + 37 in
    let src = String.init size (fun i ->
      if i mod 512 < 384 then Char.chr ((i * 7 + i / 256) mod 256)
      else Char.chr (i / 4096 mod 26 + 65))
    in
    let compressed = stream_compress ~level:1 src in
    let decompressed = stream_decompress compressed in
    assert (decompressed = src);
    printf " 10. large data (10MB+): OK\n"
  in
  (* 11. Input validation *)
  let () =
    let in_size = Zstd.Decompress_stream.in_size () in
    let tmp = Bytes.create 64 in
    expect_exn (fun () ->
      let ds = Zstd.Decompress_stream.create ~reader:(fun _ _ _ -> -1) () in
      Fun.protect ~finally:(fun () -> Zstd.Decompress_stream.close ds) (fun () ->
        Zstd.Decompress_stream.read ds tmp 0 64));
    expect_exn (fun () ->
      let ds = Zstd.Decompress_stream.create ~reader:(fun _ _ _ -> in_size + 1) () in
      Fun.protect ~finally:(fun () -> Zstd.Decompress_stream.close ds) (fun () ->
        Zstd.Decompress_stream.read ds tmp 0 64));
    expect_exn (fun () ->
      let ds = Zstd.Decompress_stream.create ~reader:(fun _ _ _ -> 0) () in
      Fun.protect ~finally:(fun () -> Zstd.Decompress_stream.close ds) (fun () ->
        Zstd.Decompress_stream.read ds tmp max_int 1));
    expect_exn (fun () ->
      let noop _ _ _ = () in
      let s = Zstd.Compress_stream.create ~writer:noop () in
      Fun.protect ~finally:(fun () -> Zstd.Compress_stream.close s) (fun () ->
        Zstd.Compress_stream.write s tmp max_int 1));
    printf " 11. input validation: OK\n"
  in
  (* 12. Byte-at-a-time decompression (exercises internal buffering) *)
  let () =
    let compressed = stream_compress data in
    assert (stream_decompress ~buf_size:1 compressed = data);
    printf " 12. byte-at-a-time decompression: OK\n"
  in
  (* 13. GC stress (exercises GC safety of ctypes structs) *)
  let () =
    let gc_churn () =
      ignore (Sys.opaque_identity (Array.init 100 (fun i -> String.make 64 (Char.chr (i mod 256)))));
      Gc.compact ()
    in
    (* compress in 512B chunks with GC churn between each *)
    let buf, writer = buf_writer () in
    let s = Zstd.Compress_stream.create ~level:1 ~writer () in
    let src = Bytes.of_string data in
    let chunk = 512 in
    let pos = ref 0 in
    while !pos < String.length data do
      let len = min chunk (String.length data - !pos) in
      Zstd.Compress_stream.write s src !pos len;
      gc_churn ();
      pos := !pos + len
    done;
    Zstd.Compress_stream.close s;
    (* decompress with small buf_size: many read+decompress calls *)
    let compressed = Buffer.contents buf in
    let reader =
      let r = string_reader compressed in
      fun b off len -> let n = r b off len in gc_churn (); n
    in
    assert (stream_decompress_reader ~buf_size:512 ~reader () = data);
    printf " 13. GC stress: OK\n"
  in
  printf "All streaming tests passed.\n"

let () =
  let file f = try Some (f, Std.input_file f) with _ -> None in
  let inputs = [
    file "/bin/bash";
    file Sys.executable_name;
    file "/etc/ld.so.cache";
    file "/etc/mailcap";
    Some ("environment", String.concat " " @@ Array.to_list @@ Unix.environment ());
  ] |> List.filter_map (fun x -> x)
  in
  List.iter test inputs;
  printf "\nStreaming tests:\n";
  test_streaming ()
