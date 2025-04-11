open Cmdliner
open Config.CommonOpt

open Logs.Logger (struct
  let str = __MODULE__
end)

let test return_register exit_register name =
  let tree = Func.get_state_tree ~elf:name ~name:"main" ~init:(State.init_sections ~addr_size:Arch.address_size) ~every_instruction:false ()
    ~breakpoints:["UND.abort"; "UND.exit"]
  in
  debug "%t" (Pp.top (State.Tree.pp_all Block_lib.pp_label) tree);
  let pc = Arch.pc () in
  let ret = State.Reg.of_string return_register in
  let ext = State.Reg.of_string exit_register in
  State.Tree.iter (fun l st ->
    let found_symread = ref false in
    st.read_vars |> Vec.iter Fun.(State.Tval.exp %> Ast.Manip.exp_iter_var (fun v ->
      match v with
      | State.Var.ReadVar _ -> found_symread := true; warn "State contains symbolic read variable:\n %t" (Pp.top State.Var.pp v)
      | _ -> ()
    ));
    if !found_symread then
      warn "State:\n%t" (Pp.top State.pp st);
    if State.is_possible st then
      match l with
      | Block_lib.End _ -> (
          let pc_exp = State.get_reg_exp st pc in
          let ret_exp = match (try
              Some (State.Exp.expect_sym_address pc_exp)
            with
              _ -> None
            ) with
              | Some pc_addr ->
                if pc_addr = Elf.Address.{ section="UND.abort"; offset=0 } then
                  fail "abort called from %t" (Pp.top Elf.Address.pp st.last_pc)
                else if pc_addr <> Elf.Address.{ section="UND.exit"; offset=0 } then
                  fail "finished at weird address %t" (Pp.top Elf.Address.pp pc_addr)
                else
                  State.get_reg_exp st ext
              | None ->
                State.get_reg_exp st ret (* Symbolic pc = returned from main *)
          in
          let ret_val = ret_exp |> Exp.ConcreteEval.eval |> Exp.Value.expect_bv |> BitVec.to_int in
          if ret_val <> 0 then
            fail "non-zero return code %d" ret_val;
      )
      | _ -> ()
  ) tree;
  base "Success"

let elf =
  let doc = "ELF file from which to pull the code" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"ELF_FILE" ~doc)

let return_register =
  let doc = "The name of the register containing the return value of main function" in
  Arg.(value & opt string "R0" & info ["r"] ~docv:"RETURN_REGISTER" ~doc)

let exit_register =
  let doc = "The name of the register containing the argument to exit function" in
  Arg.(value & opt string "R0" & info ["e"] ~docv:"EXIT_REGISTER" ~doc)
  

let term =
  Term.(
    CmdlinerHelper.func_options comopts test
    $ return_register $ exit_register $ elf)

let info =
  let doc =
    "Test run relocatable file\
    
    Test succeeds if all possble outcomes result in the program exiting with\
    with code 0"
  in
  Cmd.(info "test-rel-prog" ~doc ~exits)

let command = (term, info)
