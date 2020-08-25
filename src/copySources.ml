(** This module is the body implementation for the [copy-sources] subcommand. *)

open Logs.Logger (struct
  let str = __MODULE__
end)


let process_file () : unit =

  (* TODO: make idiomatic Cmdliner :-(  *)
  let filename_elf =
    match !AnalyseGlobals.elf with Some s -> s | None -> fatal "no --elf option\n"
  in

  let src_target_dir =
    match !AnalyseGlobals.src_target_dir with Some s -> s | None -> fatal "no --src-target-dir option\n"
  in

  let test = AnalyseUtils.time "parse_elf_file" AnalyseElf.parse_elf_file filename_elf in

  let ds : Dwarf.dwarf_static = test.AnalyseElfTypes.dwarf_static in

  let evaluated_line_info : Dwarf.evaluated_line_info = ds.Dwarf.ds_evaluated_line_info in

  let open Dwarf in

  let lnhs : line_number_header list = List.map fst evaluated_line_info in

  let lnh_comp_dirs : string list = List.sort_uniq compare (List.filter_map (fun lnh -> lnh.lnh_comp_dir) lnhs) in

  let comp_dir =
    match lnh_comp_dirs with
    | [] -> fatal "no comp_dir"
    | [comp_dir] -> comp_dir
    | _ -> fatal "multiple distinct comp_dir's: %s" (String.concat " " lnh_comp_dirs) in

  let files_of_lnh lnh : (string(*lnh_include_directory*)*string(*lnfe_path*)) list =
    List.map
      (function lnfe ->
         ((let dir = (Nat_big_num.to_int lnfe.lnfe_directory_index) in
           if dir=0 then
             ""
           else
             Byte_sequence.string_of_byte_sequence (List.nth lnh.lnh_include_directories (dir-1))),
          Byte_sequence.string_of_byte_sequence lnfe.lnfe_path))
      lnh.lnh_file_entries in

  let files = List.concat_map files_of_lnh lnhs in
     
  (*List.iter (function (dir,path) -> Printf.printf "%s  %s\n" dir path) files ; *)

  let copies = List.map (function (dir,path) ->
                           let source = Filename.concat comp_dir (Filename.concat dir path) in
                           let target = Filename.concat src_target_dir (Filename.concat dir path) in 
                           let target_dir = Filename.dirname target in
                           [source; target; target_dir]) files in


  (*Printf.printf "%s" (Dwarf.pad_rows (  ["source=";  "target=";  "targetdir="]::copies));*)


  let sys_command s =
    if !AnalyseGlobals.copy_sources_dry_run then
      Printf.printf "dry run: %s\n" s
    else 
      let exit_code = Sys.command s in 
      if exit_code <> 0 then fatal "sys_command %s failed with exit code %d" s exit_code else () in

  let copy [source; target; target_dir] = 
    sys_command ("mkdir -p " ^ Filename.quote target_dir);
    sys_command ("cp -a " ^ Filename.quote source ^ " " ^ Filename.quote target) in

  List.iter copy copies;


  ()
