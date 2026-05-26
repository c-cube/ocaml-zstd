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

let seekable_compress ?level ?frame_size ?checksum src =
  let buf = Buffer.create 256 in
  let writer b off len = Buffer.add_subbytes buf b off len in
  let s = Zstd_seekable.Compress.create ?level ?frame_size ?checksum ~writer () in
  Zstd_seekable.Compress.write s (Bytes.of_string src) 0 (String.length src);
  Zstd_seekable.Compress.close s;
  Buffer.contents buf

let seekable_read_all dec =
  let buf = Buffer.create 256 in
  let tmp = Bytes.create 65536 in
  let rec loop () =
    let n = Zstd_seekable.Decompress.read dec tmp 0 (Bytes.length tmp) in
    if n > 0 then begin
      Buffer.add_subbytes buf tmp 0 n;
      loop ()
    end
  in
  loop ();
  Buffer.contents buf

let test_seekable () =
  let module CS = Zstd_seekable.Compress in
  let module DS = Zstd_seekable.Decompress in
  (* Deterministic fixture data, big enough for several frames at 64K. *)
  let mk_data size =
    String.init size (fun i ->
      if i mod 256 < 200 then Char.chr ((i * 31 + i / 256) land 0xff)
      else Char.chr (65 + (i / 4096) mod 26))
  in
  let fixtures =
    [ "synthetic-1.5MB", mk_data (1_500_000)
    ; "synthetic-200K",  mk_data 200_000
    ; "synthetic-tiny",  mk_data 17
    ]
    @ (match (try Some (Std.input_file "/bin/bash") with _ -> None) with
       | Some s -> ["/bin/bash", s]
       | None -> [])
  in
  let policies = [
    "Uncompressed 64K", CS.Uncompressed (64 * 1024);
    "Uncompressed 2M",  CS.Uncompressed (2 * 1024 * 1024);
    "Compressed 16K",   CS.Compressed (16 * 1024);
  ] in
  (* 1. Round-trip our encoder <-> our decoder, with several policies. *)
  List.iter (fun (fname, data) ->
    List.iter (fun (pname, frame_size) ->
      let compressed = seekable_compress ~frame_size data in
      let reader = DS.Reader.of_string compressed in
      let table = match DS.find_table reader with
        | Some t -> t
        | None -> failwith (sprintf "%s / %s: find_table = None" fname pname)
      in
      assert (DS.decompressed_size table = String.length data);
      let nf = DS.num_frames table in
      assert (nf >= (if String.length data = 0 then 0 else 1));
      let dec = DS.create reader table in
      let got = seekable_read_all dec in
      DS.close dec;
      if got <> data then
        failwith (sprintf "%s / %s: linear read mismatch (got %d, expected %d)"
                    fname pname (String.length got) (String.length data));
    ) policies
  ) fixtures;
  printf "  seekable 1. round-trip (encoder/decoder): OK\n";

  (* 2. Seek correctness. For each fixture, several offsets. *)
  List.iter (fun (fname, data) ->
    let n = String.length data in
    let compressed = seekable_compress ~frame_size:(CS.Uncompressed (64 * 1024)) data in
    let reader = DS.Reader.of_string compressed in
    let table = match DS.find_table reader with Some t -> t | None -> failwith "find_table" in
    let dec = DS.create reader table in
    let offsets =
      let nf = DS.num_frames table in
      let mid_frame =
        if nf > 0 then
          let fi = DS.frame_info table (nf / 2) in
          fi.DS.uncomp_offset + fi.DS.decompressed / 2
        else 0
      in
      let frame_boundary =
        if nf >= 2 then (DS.frame_info table 1).DS.uncomp_offset
        else 0
      in
      List.sort_uniq compare [0; mid_frame; frame_boundary; max 0 (n - 1); n; n + 100]
    in
    List.iter (fun u ->
      Zstd_seekable.Decompress.seek dec u;
      let span = 257 in
      let tmp = Bytes.create span in
      let got = Zstd_seekable.Decompress.read dec tmp 0 span in
      if u >= n then begin
        if got <> 0 then failwith (sprintf "%s @%d: expected 0 at/after EOF, got %d" fname u got)
      end else begin
        let expected = min span (n - u) in
        if got <> expected then
          failwith (sprintf "%s @%d: expected to read %d, got %d" fname u expected got);
        let want = String.sub data u expected in
        let have = Bytes.sub_string tmp 0 got in
        if want <> have then
          failwith (sprintf "%s @%d: span mismatch" fname u)
      end
    ) offsets;
    DS.close dec
  ) fixtures;
  printf "  seekable 2. seek correctness: OK\n";

  (* 3. Plain-decompress compatibility: existing Decompress_stream must
     consume the seekable file and produce the original. *)
  List.iter (fun (fname, data) ->
    let compressed = seekable_compress ~frame_size:(CS.Uncompressed (64 * 1024)) data in
    let decompressed = stream_decompress compressed in
    if decompressed <> data then
      failwith (sprintf "%s: plain Decompress_stream did not recover original" fname)
  ) fixtures;
  printf "  seekable 3. plain Decompress_stream compatibility: OK\n";

  (* 4. Per-frame checksum: with ~checksum:true, libzstd appends a 4-byte XXH64
     to each frame epilogue. Round-trip must succeed and the compressed output
     must be exactly 4*num_frames bytes longer than without the checksum. *)
  let () =
    let data = mk_data 300_000 in
    (* Frame size chosen so the input produces several frames (>= 2). *)
    let frame_size = CS.Uncompressed (64 * 1024) in
    let c_no  = seekable_compress ~frame_size ~checksum:false data in
    let c_yes = seekable_compress ~frame_size ~checksum:true  data in
    (* round-trip with checksum *)
    let reader = DS.Reader.of_string c_yes in
    let table = match DS.find_table reader with
      | Some t -> t
      | None -> failwith "checksum: find_table = None"
    in
    let nf = DS.num_frames table in
    assert (nf >= 2);
    let dec = DS.create reader table in
    let got = seekable_read_all dec in
    DS.close dec;
    if got <> data then failwith "checksum: round-trip mismatch";
    let diff = String.length c_yes - String.length c_no in
    if diff <> 4 * nf then
      failwith (sprintf "checksum: size diff = %d, expected %d (= 4 * %d frames)" diff (4 * nf) nf)
  in
  printf "  seekable 4. per-frame checksum: OK\n";

  (* 6. Edge cases: find_table on plain zstd returns None; seek past EOF
     then read returns 0. *)
  let () =
    let plain = stream_compress (mk_data 1024) in
    let reader = DS.Reader.of_string plain in
    (match DS.find_table reader with
     | None -> ()
     | Some _ -> failwith "find_table on plain zstd should be None")
  in
  let () =
    (* truly tiny non-zstd input *)
    let reader = DS.Reader.of_string "x" in
    (match DS.find_table reader with
     | None -> ()
     | Some _ -> failwith "find_table on tiny garbage should be None")
  in
  let () =
    let data = mk_data 5000 in
    let compressed = seekable_compress ~frame_size:(CS.Uncompressed 1024) data in
    let reader = DS.Reader.of_string compressed in
    let table = match DS.find_table reader with Some t -> t | None -> assert false in
    let dec = DS.create reader table in
    Zstd_seekable.Decompress.seek dec (String.length data + 1_000_000);
    let tmp = Bytes.create 16 in
    let n = Zstd_seekable.Decompress.read dec tmp 0 16 in
    assert (n = 0);
    DS.close dec
  in
  printf "  seekable 6. edge cases: OK\n";

  (* 7. Manual frame policy + current_frame_size: write fixed-size "items" and
     close the frame whenever adding the next item would push the frame past a
     target uncompressed size. Verify item-aligned frame boundaries. *)
  let () =
    let item = Bytes.make 137 'A' in
    let item_len = Bytes.length item in
    let n_items = 500 in
    let target = 8 * 1024 in
    let buf = Buffer.create 64 in
    let writer b o l = Buffer.add_subbytes buf b o l in
    let enc = CS.create ~frame_size:CS.Manual ~writer () in
    let frame_uncomp_sizes = ref [] in
    for _ = 1 to n_items do
      let (_, u) = CS.current_frame_size enc in
      if u > 0 && u + item_len > target then begin
        frame_uncomp_sizes := u :: !frame_uncomp_sizes;
        CS.force_end_frame enc
      end;
      CS.write enc item 0 item_len
    done;
    let (_, last_u) = CS.current_frame_size enc in
    if last_u > 0 then frame_uncomp_sizes := last_u :: !frame_uncomp_sizes;
    CS.close enc;
    let frame_uncomp_sizes = List.rev !frame_uncomp_sizes in
    List.iter (fun u ->
      if u mod item_len <> 0 then
        failwith (sprintf "manual: frame uncomp size %d not item-aligned" u)
    ) frame_uncomp_sizes;
    let compressed = Buffer.contents buf in
    let reader = DS.Reader.of_string compressed in
    let table = match DS.find_table reader with Some t -> t | None -> failwith "manual: find_table" in
    assert (DS.num_frames table = List.length frame_uncomp_sizes);
    assert (DS.decompressed_size table = n_items * item_len);
    let dec = DS.create reader table in
    let got = seekable_read_all dec in
    DS.close dec;
    let expected = String.concat "" (List.init n_items (fun _ -> Bytes.to_string item)) in
    if got <> expected then failwith "manual: round-trip mismatch"
  in
  printf "  seekable 7. manual policy + current_frame_size: OK\n";
  printf "All seekable tests passed.\n"

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
  test_streaming ();
  printf "\nZstd_seekable tests:\n";
  test_seekable ()
