open Cmdliner
open Config.CommonOpt

open Logs.Logger (struct
  let str = __MODULE__
end)


let run_prog elfname name objdump_d branchtables =
  match Analyse.Utils.read_file_lines "src/analyse/html-preamble-insts.html" with
  | Error _ -> ()
  | Ok lines -> Array.iter (function s -> Printf.printf "%s\n" s) lines
  ;
  base "Running with rd %s in %s" name elfname;
  base "Loading %s" elfname;
  let dwarf = Dw.of_file elfname in
  let elf = dwarf.elf in
  let func =
    Dw.get_func_opt ~name dwarf
    |> Option.value_fun ~default:(fun () -> fail "Function %s wasn't found in %s" name elfname)
  in
  let api = Dw.Func.get_api func in
  base "API %t" (Pp.top Arch.pp_api api);
  base "Loading ABI";
  let abi = Arch.get_abi api in
  Trace.Cache.start @@ Arch.get_isla_config ();
  base "Computing entry state";
  let start = Init.state () |> State.copy ~elf |> State.init_sections ~addr_size:Arch.address_size |> abi.init in
  base "Loading %s for Analyse" elfname;
  let analyse_test = Analyse.Elf.parse_elf_file elfname in
  base "Analysing %s for Analyse" elfname;
  let analyse_analysis = Analyse.Collected.mk_analysis analyse_test objdump_d branchtables in
  let print_analyse_instruction pc =
    let pc = Elf.Address.to_sym pc in
    let index = analyse_analysis.index_of_address pc in
    let instr = analyse_analysis.instructions.(index) in
    Analyse.Pp.pp_instruction Analyse.Types.Html (*Ascii*) analyse_test analyse_analysis 0 index
      instr
  in
  (* base "Entry state:\n%t" Pp.(topi State.pp start); *)
  match func.sym with
  | None -> fail "Function %s exists in DWARF data but do not have any code" name
  | Some sym ->
      let endpred = Block_lib.gen_endpred () in
      let runner = Runner.of_dwarf dwarf in
      let block = Block_lib.make ~runner ~start:sym.addr ~endpred in
      base "Start running";
      let tree = Block_lib.run ~every_instruction:true block start in
      base "Ended running, start pretty printing";
      (* This table will contain the state diff to print at each pc with a message *)
      (* let instr_data : (Elf.Address.t, string * State.t * State.Reg.t list) Hashtbl.t =
        Hashtbl.create 100
      in
      let get_footprint pc =
        Runner.get_normal_opt runner pc |> Option.fold ~none:[] ~some:Trace.Instr.footprint
      in *)
      State.Tree.iter
        (fun a st ->
          match a with
          | Block_lib.Start -> ()
          | Block_lib.BranchAt pc -> 
              let last_pc = st.last_pc in
              if Elf.Address.(last_pc + 4 <> pc) then
                Printf.printf "\nJUMP from %t:\n " Pp.(top Elf.Address.pp last_pc);
              print_string (print_analyse_instruction pc);
              print_endline "BRANCH!";
          | Block_lib.NormalAt pc ->
              let last_pc = st.last_pc in
              if Elf.Address.(last_pc + 4 <> pc) then
                Printf.printf "\nJUMP from %t:\n " Pp.(top Elf.Address.pp last_pc);
              print_string (print_analyse_instruction pc);
          | Block_lib.End _ -> ())
        tree;
  match Analyse.Utils.read_file_lines "src/analyse/html-postamble.html" with
  | Error _ -> ()
  | Ok lines -> Array.iter (function s -> Printf.printf "%s\n" s) lines


let elf =
  let doc = "ELF file from which to pull the code" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"ELF_FILE" ~doc)

let func =
  let doc = "Symbol name of the function to run" in
  Arg.(value & pos 1 string "main" & info [] ~docv:"FUNCTION" ~doc)

let objdump_d =
  let doc = "File containing result of objdump -d" in
  Arg.(required & opt (some non_dir_file) None & info ["objdump-d"] ~docv:"OBJDUMP_FILE" ~doc)

let branch_table =
  let doc = "File containing branch table base addresses and sizes" in
  Arg.(
    (* required *)
    value & opt (some non_dir_file) None & info ["branch-tables"] ~docv:"BRANCH_TABLES_FILE" ~doc)

let term =
  Term.(
    CmdlinerHelper.func_options comopts run_prog
    $ elf $ func $ objdump_d $ branch_table)

let info =
  let doc =
    "Run main of relocatable file"
  in
  Cmd.(info "run-rel-prog" ~doc ~exits)

let command = (term, info)
        