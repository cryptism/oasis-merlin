(** Merlin generator
    @author Joe Whittles
  *)

open OASISGettext
open OASISTypes
open OASISValues
open OASISFileTemplate
open OASISPlugin
open OASISSchema
open Format

type t =
  {
    enable:       bool;
    extra_lines:  string list;
    excludes:     string list;
  }

let plugin =
  `Extra, "Merlin", Some (OASISVersion.version_of_string "0.1")

let self_id, all_id = Extra.create plugin

let pivot_data = data_new_property plugin

let generator =
  let new_field nm =
    new_field OASISPackage.schema all_id nm
  in

  let excludes =
    new_field
      "Excludes"
      ~default:[]
      (comma_separated string_not_empty)
      (fun () -> s_ "List of sections to exclude from generation")
      pivot_data (fun _ t -> t.excludes)
  in

  let enable =
    new_field
      "Enable"
      ~default:true
      boolean
      (fun () -> s_ "Enable .merlin file generation.")
      pivot_data (fun _ t -> t.enable)
  in

  let extra_lines =
    new_field
      "ExtraLines"
      ~default:[]
      (newline_separated string_not_empty)
      (fun () -> s_ "Extra lines to add to .merlin")
      pivot_data (fun _ t -> t.extra_lines)
  in

    fun data ->
      {
        enable      = enable data;
        extra_lines = extra_lines data;
        excludes    = excludes data;
      }

module StringSet = Set.Make (String);;

let pp_print_merlin pkg t fmt =
  if not t.enable then ()
  else
    let all_build_sections =
      List.fold_left
         (fun acc ->
           function
           | Library (_, bs, _)
           | Object (_, bs, _)
           | Executable (_, bs, _) ->
              bs :: acc
           | Flag (_)
           | SrcRepo(_)
           | Test(_)
           | Doc(_) -> acc)
         []
         pkg.sections
    in

    let path_set =
      List.fold_left
        (fun acc bs -> StringSet.add bs.bs_path acc)
        StringSet.empty
        all_build_sections
    in

    let pp_print_extra_lines =
      List.iter
        (fun dir -> fprintf fmt "%s\n" dir)
        t.extra_lines
    in

    let pp_print_dir dir mp set =
      StringSet.iter
        (fun s -> fprintf fmt "%s %s\n" dir (mp s))
        set
    in

    let all_pkg_set =
      List.fold_left
         (fun acc bs ->
           StringSet.union
             acc
             (StringSet.of_list
                (List.map
                   (function
                    | FindlibPackage(lib, _ ) -> lib
                    | InternalLibrary(nm) -> nm)
                   bs.bs_build_depends)))
         StringSet.empty
         all_build_sections
    in
    pp_print_dir
      "S"
      (fun p -> p ^ "/*")
      path_set;
    pp_print_dir
      "B"
      (fun p -> "_build/" ^ p ^ "/*")
      path_set;
    pp_print_dir
      "PKG"
      (fun d -> d)
      all_pkg_set;
    pp_print_extra_lines;;

let main ctxt pkg =
  (* construct map *)
  (** All sections that contains a build_section *)
  let t = generator pkg.schema_data in
  let buff = Buffer.create 13 in
  let fmt = Format.formatter_of_buffer buff in
  pp_print_merlin pkg t fmt;
  OASISPlugin.add_file
    (template_of_string_list
       ~ctxt:ctxt.OASISPlugin.ctxt
       ~template:true
       ".merlin"
       comment_sh
       (OASISString.split_newline
          ~do_trim:false
          (Buffer.contents buff)))
      ctxt

let init () =
  Extra.register_act self_id main;
  register_generator_package all_id pivot_data generator;
