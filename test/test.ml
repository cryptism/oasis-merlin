

(** Run tests for OASIS
    @author Joe Whittles
  *)


open OUnit2
open OASISTest
open OASISTypes
open Fl_metascanner
open OASISFindlib
open TestCommon


let extract_timings () =
  let log_fn j =
    Filename.concat OUnitUtils.buildir
      (Printf.sprintf "oUnit-OASIS-%s.log" (OUnitUtils.shardf j))
  in
  let j = ref 0 in
  let timings = Hashtbl.create 13 in
  let () = ignore "(*(*" in
  let rex = Pcre.regexp "Time spent in '(.*)': (.*)s" in
  let total_time = ref 0.0 in
  let () =
    while Sys.file_exists (log_fn !j) do
      let chn = open_in (log_fn !j) in
        incr j;
        try
          while true do
            let ln = input_line chn in
              try
                let substr = Pcre.exec ~rex ln in
                let name = Pcre.get_substring substr 1 in
                let time = float_of_string (Pcre.get_substring substr 2) in
                let count', time' =
                  try
                    Hashtbl.find timings name
                  with Not_found ->
                    0, 0.0
                in
                  total_time := !total_time +. time;
                  Hashtbl.replace timings name (count' + 1, time' +. time)
              with Not_found ->
                ()
          done
        with End_of_file ->
          close_in chn
    done
  in
  let output_dir = "../dist" in
  let output_fn = Filename.concat output_dir "timings.csv" in
  if Sys.file_exists output_dir && Sys.is_directory output_dir then begin
    let chn = open_out output_fn in
      Printf.fprintf chn "name,time,count\n";
      Hashtbl.iter
        (fun name (count, time) ->
           Printf.fprintf chn "%S,%f,%d\n" name time count)
        timings;
      close_out chn
  end;
  try
    ignore(Sys.getenv "TIMINGS");
    Hashtbl.iter
      (fun name (count, time) ->
         Printf.printf
           "Time spent in '%s':\n  % 7.2fs (% 3d time, % 6.2f%%, %5.2fs/call)\n"
           name time count ((time /. !total_time) *. 100.0)
           (time /. (float_of_int count)))
      timings;
    Printf.printf "Total time accounted: %fs\n" !total_time
  with Not_found ->
    ()

let tests =
  let test_of_vector (nm, oasis_str, tests) =
    nm >::
      (fun test_ctxt ->
        let fn, _ =
          bracket_tmpfile ~prefix:"oasis-merlin-" ~suffix:".meta" test_ctxt
        in (* Parse string to get OASIS package *)
        let pkg =
          OASISParse.from_string
            ~ctxt: oasis_ctxt
            oasis_str
        in
        let t = (OASISMerlin.generator pkg.schema_data) in
        let chn = (open_out fn) in
        let fmt = Format.formatter_of_out_channel chn in
        OASISMerlin.pp_print_merlin pkg t fmt;
        (*
        List.fold_left
          (fun former_merlin (k, vs) -> ())
          ()
          tests;*)
        close_out chn;
      )
  in
  "OASISMerlin" >:::
    (List.map test_of_vector
      ["test-for-presence",
       "\
OASISFormat:  0.1
Name:         foo
Version:      0.0.1
Synopsis:     foo
License:      LGPL
Authors:      me
Plugins: Merlin
XMerlinEnable: true
Library bar
  Path:    src
  BuildDepends: odn, oasis
Library baz
  Path:    test
  BuildDepends: odn", ["S", ["src"]; "PKG",["odn";"oasis"]];
       "test-extra-lines",
       "\
OASISFormat:  0.1
Name:         foo
Version:      0.0.1
Synopsis:     foo
License:      LGPL
Authors:      me
Plugins: Merlin
XMerlinEnable: true
XMerlinExtraLines: S a_source\n
                   EXT an_extension\n
                   FLG a_flag

Executable bill
  Path:  bob
  MainIs: main.ml
  BuildDepends: oasis, quux", ["S", ["bob"]; "PKG",["oasis";"quux"]];
       "test-exclusion",
           "\
OASISFormat:  0.1
Name:         foo
Version:      0.0.1
Synopsis:     foo
License:      LGPL
Authors:      me
Plugins: Merlin
XMerlinEnable: true
XMerlinExcludes: bar, baz
Library bar
  Path:    src
  BuildDepends: bbb, quux
Library baz
  Path:    test
  BuildDepends: oasis
Executable bill
  Path:  bob
  MainIs: main.ml
  BuildDepends: odn, quux", ["S", ["src"]; "PKG",["odn";"quux"]];])

let () =
  let () = OASISMerlin.init ()
  in
  run_test_tt_main ~exit ("OASIS-Merlin">:::[tests]);
  extract_timings ()
