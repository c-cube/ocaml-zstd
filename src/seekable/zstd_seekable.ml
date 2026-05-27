
(* This module assumes a 64-bit OCaml runtime: the u32 byte arithmetic below
   would overflow on large u32 values. *)
let () = assert (Sys.word_size >= 64)

(* ----- Seekable format constants ------------------------------------------ *)

let seekable_magic = 0x8F92EAB1
let skippable_magic = 0x184D2A5E
let footer_size = 9    (* Number_Of_Frames u32 + Descriptor u8 + Magic u32 *)
let entry_size_no_checksum = 8     (* Compressed_Size u32 + Decompressed_Size u32 *)
let entry_size_with_checksum = 12  (* + Frame_Checksum u32, not validated by reader *)

let u32_le_of_bytes b off =
  let b0 = Char.code (Bytes.unsafe_get b off) in
  let b1 = Char.code (Bytes.unsafe_get b (off + 1)) in
  let b2 = Char.code (Bytes.unsafe_get b (off + 2)) in
  let b3 = Char.code (Bytes.unsafe_get b (off + 3)) in
  b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24)

let bytes_set_u32_le b off v =
  if v < 0 || v > 0xffff_ffff then
    raise (Zstd.Error (Printf.sprintf "Seekable: value %d does not fit in u32" v));
  Bytes.unsafe_set b off (Char.unsafe_chr (v land 0xff));
  Bytes.unsafe_set b (off + 1) (Char.unsafe_chr ((v lsr 8) land 0xff));
  Bytes.unsafe_set b (off + 2) (Char.unsafe_chr ((v lsr 16) land 0xff));
  Bytes.unsafe_set b (off + 3) (Char.unsafe_chr ((v lsr 24) land 0xff))

module Compress = struct
  module Compress_engine = Zstd.Internal.Compress_engine

  type frame_size_policy =
    | Uncompressed of int
    | Compressed of int
    | Manual

  type t = {
    engine: Compress_engine.t;
    user_writer: bytes -> int -> int -> unit;
    policy: frame_size_policy;
    (** per-frame counters. [comp_in_frame] is updated from the wrapper
        writer that sits between the engine and the user. *)
    comp_in_frame: int ref;
    uncomp_in_frame: int ref;
    (** collected entries: concatenation of (comp_u32 ; uncomp_u32),
        8 bytes per entry. *)
    entries: Buffer.t;
  }

  let create ?level ?(frame_size=Uncompressed (2 * 1024 * 1024)) ?(checksum=false) ~writer () =
    (match frame_size with
     | Uncompressed n | Compressed n ->
       if n <= 0 then invalid_arg "Seekable.Compress.create: frame_size must be > 0"
     | Manual -> ());
    let comp_in_frame = ref 0 in
    let uncomp_in_frame = ref 0 in
    let wrapped_writer b off len =
      comp_in_frame := !comp_in_frame + len;
      writer b off len
    in
    (* [?checksum] enables libzstd's per-frame XXH64, not the seektable checksum *)
    let engine = Compress_engine.create ?level ~checksum ~writer:wrapped_writer () in
    { engine; user_writer = writer; policy = frame_size;
      comp_in_frame; uncomp_in_frame;
      entries = Buffer.create 64 }

  let is_closed s = Compress_engine.is_closed s.engine

  let force_end_frame s =
    if Compress_engine.is_closed s.engine then
      raise (Zstd.Error "stream is closed");
    if !(s.uncomp_in_frame) = 0 then ()
    else begin
      Compress_engine.end_frame s.engine;
      let entry = Bytes.create entry_size_no_checksum in
      bytes_set_u32_le entry 0 !(s.comp_in_frame);
      bytes_set_u32_le entry 4 !(s.uncomp_in_frame);
      Buffer.add_bytes s.entries entry;
      s.comp_in_frame := 0;
      s.uncomp_in_frame := 0
    end

  let should_end_frame s =
    match s.policy with
    | Uncompressed n -> !(s.uncomp_in_frame) >= n
    | Compressed n -> !(s.comp_in_frame) >= n
    | Manual -> false

  let current_frame_size s = (!(s.comp_in_frame), !(s.uncomp_in_frame))

  let write s buf off len =
    if Compress_engine.is_closed s.engine then
      raise (Zstd.Error "stream is closed");
    if off < 0 || len < 0 || off > Bytes.length buf - len then
      invalid_arg "Seekable.Compress.write";
    if len = 0 then ()
    else begin
      (* Walk the input in policy-friendly chunks so the frame-size
         policy is consulted at frequent boundaries rather than only at
         the end of a (potentially huge) [write] call. The bound is the
         policy target (when known) so any single chunk can at most fill
         the remainder of the current frame; we then end the frame and
         move on. *)
      let chunk_cap = match s.policy with
        | Uncompressed n -> n
        | Compressed n -> n
        | Manual -> len
      in
      let pos = ref 0 in
      while !pos < len do
        let remaining = len - !pos in
        let chunk = min remaining (max 1 chunk_cap) in
        Compress_engine.write s.engine buf (off + !pos) chunk;
        s.uncomp_in_frame := !(s.uncomp_in_frame) + chunk;
        pos := !pos + chunk;
        if should_end_frame s then force_end_frame s
      done
    end

  (* The seek table is metadata appended outside normal zstd frame. It must
     bypass the wrapped writer so it does not get counted into
     [comp_in_frame]. *)
  let write_seek_table s =
    let n = Buffer.length s.entries / entry_size_no_checksum in
    let entries_size = n * entry_size_no_checksum in
    let total_payload = entries_size + footer_size in
    (* skippable frame header: magic + Frame_Size = total_payload *)
    let header = Bytes.create 8 in
    bytes_set_u32_le header 0 skippable_magic;
    bytes_set_u32_le header 4 total_payload;
    s.user_writer header 0 8;
    (* entries *)
    let entries = Buffer.to_bytes s.entries in
    if Bytes.length entries > 0 then
      s.user_writer entries 0 (Bytes.length entries);
    (* footer: Number_Of_Frames u32 + Seek_Table_Descriptor u8 + Seekable_Magic u32 *)
    let footer = Bytes.create footer_size in
    bytes_set_u32_le footer 0 n;
    Bytes.unsafe_set footer 4 (Char.unsafe_chr 0); (* no checksum, no reserved bits *)
    bytes_set_u32_le footer 5 seekable_magic;
    s.user_writer footer 0 footer_size

  let close s =
    if Compress_engine.is_closed s.engine then ()
    else begin
      if !(s.uncomp_in_frame) > 0 then force_end_frame s;
      (* Use [close_no_drain] (rather than [close]) so a never-written
         engine does not emit a spurious empty zstd frame after the seek
         table footer — that would push the last 9 bytes of the file
         inside an empty frame and break the seekable invariant.
         [Fun.protect] guarantees the engine is freed even if the user
         writer raises mid-table. *)
      Fun.protect
        ~finally:(fun () -> Compress_engine.close_no_drain s.engine)
        (fun () -> write_seek_table s)
    end
end

module Decompress = struct
  module Reader = struct
    type t = {
      read   : bytes -> int -> int -> int;
      seek   : int -> unit;
      length : unit -> int;
    }

    let of_string src =
      let pos = ref 0 in
      let len = String.length src in
      {
        read = (fun b off n ->
          let avail = len - !pos in
          let k = min n avail in
          if k > 0 then Bytes.blit_string src !pos b off k;
          pos := !pos + k;
          k);
        seek = (fun p ->
          if p < 0 || p > len then invalid_arg "Seekable.Decompress.Reader.of_string: seek out of bounds";
          pos := p);
        length = (fun () -> len);
      }

    let of_in_channel ic =
      let read b off n =
        let rec loop got =
          if got = n then got
          else
            let r = Stdlib.input ic b (off + got) (n - got) in
            if r = 0 then got
            else loop (got + r)
        in
        loop 0
      in
      {
        read;
        seek = (fun p -> Stdlib.seek_in ic p);
        length = (fun () -> Stdlib.in_channel_length ic);
      }
  end

  type table = {
    n_frames: int;
    comp_offsets: int array;
      (** cumulative compressed offsets, length = n_frames + 1; per-frame
          compressed size = [comp_offsets.(i+1) - comp_offsets.(i)]. *)
    uncomp_offsets: int array;
    total_comp: int; (** sum of frame compressed sizes (excludes seek table) *)
    total_uncomp: int;
  }

  let num_frames t = t.n_frames
  let decompressed_size t = t.total_uncomp
  let compressed_size t = t.total_comp

  type frame_info = {
    compressed : int;
    decompressed : int;
    comp_offset : int;
    uncomp_offset : int;
  }

  let frame_info t i =
    if i < 0 || i >= t.n_frames then invalid_arg "Seekable.Decompress.frame_info";
    {
      compressed = t.comp_offsets.(i + 1) - t.comp_offsets.(i);
      decompressed = t.uncomp_offsets.(i + 1) - t.uncomp_offsets.(i);
      comp_offset = t.comp_offsets.(i);
      uncomp_offset = t.uncomp_offsets.(i);
    }

  let pp_frame_info fmt fi =
    Format.fprintf fmt
      "{@[compressed=%d; decompressed=%d;@ comp_offset=%d; uncomp_offset=%d@]}"
      fi.compressed fi.decompressed fi.comp_offset fi.uncomp_offset

  (* Debug-only: prints every frame, no truncation. For tables with very many
     frames this can produce a lot of output. *)
  let pp_table fmt t =
    Format.fprintf fmt "@[<hov>";
    for i = 0 to t.n_frames - 1 do
      if i > 0 then Format.fprintf fmt "@ ";
      pp_frame_info fmt (frame_info t i)
    done;
    Format.fprintf fmt "@]"

  let read_exact (r : Reader.t) pos len =
    r.seek pos;
    let b = Bytes.create len in
    let i = ref 0 in
    try
      while !i < len do
        let n = r.read b !i (len - !i) in
        if n = 0 then raise Exit;
        i := !i + n
      done;
      Some b
    with Exit -> None

  let[@inline] ( let* ) o f = match o with None -> None | Some x -> f x
  let[@inline] guard b = if b then Some () else None

  let find_table (r : Reader.t) : table option =
    let total_len = r.length () in
    let* () = guard (total_len >= footer_size) in
    let* footer = read_exact r (total_len - footer_size) footer_size in
    let* () = guard (u32_le_of_bytes footer 5 = seekable_magic) in
    let n = u32_le_of_bytes footer 0 in
    let* () = guard (n >= 0) in
    let desc = Char.code (Bytes.unsafe_get footer 4) in
    let entry_size =
      if desc land 0x80 <> 0 then entry_size_with_checksum
      else entry_size_no_checksum
    in
    let entries_size = n * entry_size in
    let frame_size = entries_size + footer_size in
    let skippable_start = total_len - frame_size - 8 in
    let* () = guard (skippable_start >= 0) in
    let* hdr = read_exact r skippable_start 8 in
    let* () = guard (u32_le_of_bytes hdr 0 = skippable_magic
                     && u32_le_of_bytes hdr 4 = frame_size) in
    let* entries =
      if entries_size = 0 then Some Bytes.empty
      else read_exact r (skippable_start + 8) entries_size
    in
    let comp_offsets = Array.make (n + 1) 0 in
    let uncomp_offsets = Array.make (n + 1) 0 in
    let cc = ref 0 and cu = ref 0 in
    for i = 0 to n - 1 do
      let off = i * entry_size in
      let cs = u32_le_of_bytes entries off in
      let us = u32_le_of_bytes entries (off + 4) in
      comp_offsets.(i) <- !cc;
      uncomp_offsets.(i) <- !cu;
      cc := !cc + cs;
      cu := !cu + us
    done;
    comp_offsets.(n) <- !cc;
    uncomp_offsets.(n) <- !cu;
    (* Sanity: total compressed frames + skippable frame header
       + entries + footer must equal total_len. *)
    let* () = guard (!cc + 8 + entries_size + footer_size = total_len) in
    Some {
      n_frames = n;
      comp_offsets; uncomp_offsets;
      total_comp = !cc;
      total_uncomp = !cu;
    }

  module E = Zstd.Internal.Decompress_engine

  type t = {
    engine: E.t;
    reader: Reader.t;
    scratch: bytes;
    table: table;
    mutable uncomp_pos: int;       (* next uncompressed byte to emit, absolute *)
    mutable skip_remaining: int;   (* bytes still to discard from decoded output *)
    mutable comp_pos: int;         (* current absolute read position in source *)
    comp_end: int;                 (* offset at which user data ends (= total_comp) *)
  }

  let create (reader : Reader.t) table =
    let engine = E.create () in
    let scratch = Bytes.create (E.in_capacity engine) in
    (* [reader.seek 0] runs after [E.create] has registered its GC
       finaliser. If [seek] raises (closed fd, IO error), tear down the
       engine synchronously so we don't leak a dctx into the finaliser. *)
    try
      reader.seek 0;
      { engine; reader; scratch; table;
        uncomp_pos = 0; skip_remaining = 0;
        comp_pos = 0; comp_end = table.total_comp }
    with exn ->
      E.close engine;
      raise exn

  let is_closed s = E.is_closed s.engine
  let close s = E.close s.engine

  (* Largest i such that uncomp_offsets[i] <= u, with i < n_frames. *)
  let find_frame_for_offset (t : table) u =
    let n = t.n_frames in
    let lo = ref 0 and hi = ref (n - 1) in
    while !lo < !hi do
      let mid = (!lo + !hi + 1) / 2 in
      if t.uncomp_offsets.(mid) <= u then lo := mid
      else hi := mid - 1
    done;
    !lo

  let seek s u =
    if E.is_closed s.engine then raise (Zstd.Error "stream is closed");
    if u < 0 then invalid_arg "Seekable.Decompress.seek: negative offset";
    let t = s.table in
    E.session_reset s.engine;
    if u >= t.total_uncomp || t.n_frames = 0 then begin
      s.uncomp_pos <- t.total_uncomp;
      s.skip_remaining <- 0;
      s.comp_pos <- s.comp_end
    end else begin
      let frame = find_frame_for_offset t u in
      s.reader.seek t.comp_offsets.(frame);
      s.comp_pos <- t.comp_offsets.(frame);
      s.uncomp_pos <- u;
      s.skip_remaining <- u - t.uncomp_offsets.(frame)
    end

  let read s buf off len =
    if E.is_closed s.engine then raise (Zstd.Error "stream is closed");
    if off < 0 || len < 0 || off > Bytes.length buf - len then
      invalid_arg "Seekable.Decompress.read";
    if len = 0 then 0
    else
      (* Tear down the engine on any error from the reader callback, from
         the truncation/no-progress raises below, or from libzstd via
         [E.step]. Without this, exceptions leak the dctx until GC and
         trigger the 'closing in GC finalizer' warning. *)
      try
        let t = s.table in
        let total = ref 0 in
        let continue = ref true in
        while !total < len && !continue do
          if s.uncomp_pos >= t.total_uncomp then continue := false
          else if E.pending_output s.engine > 0 then begin
            (* Discard up to [skip_remaining] before copying anything. *)
            if s.skip_remaining > 0 then begin
              let dropped = E.discard s.engine s.skip_remaining in
              s.skip_remaining <- s.skip_remaining - dropped
            end;
            let cap = min (len - !total) (t.total_uncomp - s.uncomp_pos) in
            let n = E.drain s.engine buf (off + !total) cap in
            s.uncomp_pos <- s.uncomp_pos + n;
            total := !total + n
          end else begin
            (* Refill, bounded by the table's user-data extent — must never
               read past it into the trailing seek table. *)
            if E.needs_input s.engine then begin
              let want = min (E.in_capacity s.engine) (s.comp_end - s.comp_pos) in
              if want = 0 then begin
                (* All compressed bytes the table promised are consumed.
                   If libzstd still wants more input ([last_ret <> 0])
                   the final frame is truncated relative to the table. *)
                if E.last_ret s.engine <> 0 then
                  raise (Zstd.Error "truncated compressed data");
                continue := false
              end else begin
                let n = s.reader.read s.scratch 0 want in
                if n <= 0 then raise (Zstd.Error "unexpected end of compressed input");
                E.push_input s.engine s.scratch 0 n;
                s.comp_pos <- s.comp_pos + n
              end
            end;
            if !continue then begin
              let (consumed, produced) = E.step s.engine in
              (* [(0, 0)] with [last_ret = 0] just means libzstd is at a
                 clean frame boundary and the next frame's magic hasn't
                 been read yet — don't mistake it for a stall. *)
              if produced = 0 && consumed = 0 && E.last_ret s.engine <> 0 then
                raise (Zstd.Error "decompression made no progress")
            end
          end
        done;
        !total
      with exn ->
        E.close s.engine;
        raise exn
end
