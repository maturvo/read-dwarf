
open Printf;;

type natural = Nat_big_num.num

let pp_addr (a:natural) = Ml_bindings.hex_string_of_big_int_pad8 a
             
type test =
  {
   elf_file: Elf_file.elf_file;
   symbol_map: Elf_file.global_symbol_init_info;
   segments: Elf_interpreted_segment.elf64_interpreted_segment list;
   e_entry: natural;
   e_machine: natural;
   dwarf_static: Dwarf.dwarf_static;
   dwarf_semi_pp_frame_info:  (natural (*address*) * string (*cfa*) * (string*string) (*register rules*) list) list;
  }

(** ********************************************************************* *)
(** **   use linksem to parse ELF file and extract DWARF info          ** *)
(** ********************************************************************* *)
  
let parse_file (filename:string) : test = 
  (* call ELF analyser on file *)
  let info = (Sail_interface.populate_and_obtain_global_symbol_init_info filename) in 
  
  let
    ((elf_file: Elf_file.elf_file),
     (elf_epi: Sail_interface.executable_process_image),
     (symbol_map: Elf_file.global_symbol_init_info))
    =
    begin match info with
    | Error.Fail s -> Warn.fatal "populate_and_obtain_global_symbol_init_info: %s" s
    | Error.Success x -> x
    end
  in
  
  (* Printf.printf "%s\n" (Sail_interface.string_of_executable_process_image elf_epi);*)

  Debug.print_string "elf segments etc\n";
  begin match elf_epi, elf_file with
  | (Sail_interface.ELF_Class_32 _, _)  -> Warn.fatal "%s" "cannot handle ELF_Class_32"
  | (_, Elf_file.ELF_File_32 _)  -> Warn.fatal "%s" "cannot handle ELF_File_32"
  | (Sail_interface.ELF_Class_64 (segments,e_entry,e_machine), Elf_file.ELF_File_64 f1) ->
     (* remove all the auto generated segments (they contain only 0s) *)
     let segments =
       Lem_list.mapMaybe
         (fun (seg, prov) -> if prov = Elf_file.FromELF then Some seg else None)
         segments
     in
     let ds =
       match Dwarf.extract_dwarf_static (Elf_file.ELF_File_64 f1) with
       | None -> Warn.fatal "%s" "extract_dwarf_static failed"
       | Some ds ->
(*          Debug.print_string2 (Dwarf.pp_analysed_location_data ds.Dwarf.ds_dwarf ds.Dwarf.ds_analysed_location_data);
          Debug.print_string2 (Dwarf.pp_evaluated_frame_info ds.Dwarf.ds_evaluated_frame_info);*)
          ds
     in
     let dwarf_semi_pp_frame_info = Dwarf.semi_pp_evaluated_frame_info ds.ds_evaluated_frame_info in 
     let test =
       { elf_file    = elf_file;
         symbol_map  = symbol_map (*@ (symbols_for_stacks !Globals.elf_threads)*);
         segments    = segments;
         e_entry     = e_entry;
         e_machine   = e_machine;
         dwarf_static= ds;
         dwarf_semi_pp_frame_info = dwarf_semi_pp_frame_info;
       } in
     test
  end
  

(** ********************************************************************* *)
(** **          pretty-print source lines for an address               ** *)
(** ********************************************************************* *)

(* 'safe_open_in filename f' will open filename, pass it to f and cloth
 * the channel at the end or when an exception is raised
 *)
let safe_open_in (filename: string) (f: in_channel -> 'a) : 'a =
    let chan = open_in filename in
    let res = try f chan with e -> close_in chan; raise e in
    close_in chan;
    res

type 'a ok_or_fail = Ok of 'a | MyFail of string    

let read_source_file (name: string) : string array ok_or_fail = 
  let read_lines chan =
    let lines = ref [] in
    let () =
      try
        while true do lines := (input_line chan) :: !lines done
      with
      | End_of_file -> ()
    in
    !lines |> List.rev |> Array.of_list
  in
  match safe_open_in name read_lines with
  | lines -> Ok lines
  | exception Sys_error s ->
     MyFail (Printf.sprintf "read_source_file Sys_error \"%s\"\n" s)

let rec read_source_file2 (names: string list) (err_acc:string) : string array ok_or_fail =
  match names with
  | [] -> MyFail ("not found:\n"^err_acc)
  | name::names' ->
     match read_source_file name with
     | Ok lines -> Ok lines
     | MyFail err -> read_source_file2 names' (err_acc^err)
                 
let source_file_cache = ref ([] : (string * (string array option)) list)

let source_line file n1 =
  let access_lines lines n =
    if n < 0 || n >= Array.length lines then
      Some (sprintf "line out of range: %i vs %i" n (Array.length lines))
    else
      Some lines.(n) in

  let n = n1 -1 in
  match (try Some (List.assoc file !source_file_cache) with Not_found -> None) with
  | Some (Some lines) -> access_lines lines n
  | Some (None) -> None
  | None ->
     let filenames = match !Globals.dwarf_source_dirs with
     | [] -> [file]
     | _ -> List.map (fun s -> Filename.concat s file) !Globals.dwarf_source_dirs in
     match read_source_file2 filenames "" with
     | Ok lines ->
        source_file_cache := (file, Some lines) :: !source_file_cache;
        access_lines lines n
     | MyFail s ->
(*        source_file_cache := (file, None) :: !source_file_cache;
          None *)
         source_file_cache := (file, None) :: !source_file_cache;
         (Warn.nonfatal "%s" s; None)

let pp_source_line so =
  match so with
  | Some s -> s (*" (" ^ s ^ ")"*)
  | None -> "file not found"

let pp_dwarf_source_file_lines m ds (pp_actual_line: bool) (a: natural) : string option =
  let sls = Dwarf.source_lines_of_address ds a in
  match sls with
  | [] -> None
  | _ ->
     Some
       (String.concat
          ", "
          (List.map
             (fun (s,n,lnr) ->
               s ^ ":" ^ Nat_big_num.to_string n ^ if pp_actual_line then pp_source_line (source_line s (Nat_big_num.to_int n))  else ""
             )
             sls
          )
       )


(** ********************************************************************* *)
(** **          look up address in ELF symbol table                    ** *)
(** ********************************************************************* *)

let elf_symbols_of_address (test:test) (addr:natural) : string list =
  List.filter_map
    (fun (name, (typ, size, address, mb, binding)) ->
      if address=addr then Some name else None)
    test.symbol_map


(** ********************************************************************* *)
(** **          look up address in frame info                          ** *)
(** ********************************************************************* *)

let aof ((a:natural),(cfa:string),(regs:(string*string) list)) = a
  
let rec f (aof:'b->natural) (a:natural) (last: 'b option) (bs:'b list) : 'b option =
  match (last,bs) with
  | (None,   [])        -> None
  | (Some b',[])        -> if Nat_big_num.greater_equal a (aof b') then Some b' else None
  | (None,    b''::bs') -> f aof a (Some b'') bs'
  | (Some b', b''::bs') ->
     if Nat_big_num.less a (aof b') then None
     else if Nat_big_num.greater_equal a (aof b') && Nat_big_num.less a (aof b'') then Some b'
     else f aof a (Some b'') bs'

  
let pp_frame_info (test:test) (addr:natural) : string =
  (* assuming the dwarf_semi_pp_frame_info has monotonically increasing addresses - always true? *)
  match f aof addr None test.dwarf_semi_pp_frame_info with
  | None -> "<no frame info for this address>\n"
  | Some ((a:natural),(cfa:string),(regs:(string*string) list)) -> 
     pp_addr a ^ " " ^ "CFA:" ^ cfa ^ " " ^ String.concat " " (List.map (function (rname,rinfo)->rname^":"^rinfo) regs) ^"\n"

(** ********************************************************************* *)
(** **          pull disassembly out of an objdump -d file             ** *)
(** ********************************************************************* *)

let objdump_lines : (natural * (natural * string)) list option ref = ref None

let init_objdump () =     
  match !Globals.objdump_d with
  | None -> ()
  | Some filename ->
     match read_source_file filename with
     | MyFail s ->
        Warn.fatal2 "%s\ncouldn't read objdump-d file: \"%s\"\n" s filename
     | Ok lines ->
        let parse_line (s:string) : (natural*(natural*string)) option =
(*          if String.length s >=9 && s.[8] = ':' then *)
          match Scanf.sscanf s " %x: %x%n" (fun a -> fun i -> fun n -> (a,i,n)) with
          | (a,i,n) ->
              let s' = String.sub s n (String.length s - n) in
              Some (Nat_big_num.of_int a, (Nat_big_num.of_int i, s'))
          | exception _ -> None
(*          else
            None*)
        in
        objdump_lines := Some (List.filter_map parse_line (Array.to_list lines))

let lookup_objdump_lines (a: natural) : (natural*string) option =
  match !objdump_lines with
  | Some lines -> 
     List.assoc_opt a lines
  | None ->
     None

    

(** ********************************************************************* *)
(** **          pretty-print the result                                ** *)
(** ********************************************************************* *)

let pp_test test = 
  init_objdump ();

  (* pull out instructions from text section, assuming 4-byte insns *)
  let (p,addr,bs) = Dwarf.extract_text test.elf_file in
  let instructions : (natural * natural) list = Dwarf.instructions_of_byte_list addr bs [] in

  let pp_instruction ((addr:natural),(i:natural)) =

    (* the elf symbols at this address, if any *)
    String.concat ""
      (List.map
         (fun (s:string) ->
           pp_addr addr
           ^ " <" ^ s ^">:\n")
         (elf_symbols_of_address test addr))
    
    (* the address and (hex) instruction *)
    ^ pp_addr addr ^ ":  " ^ pp_addr i 
    (* the dissassembly from objdump, if it exists *)
    ^ begin match lookup_objdump_lines addr with
      | Some (i',s) ->
         if i=i' then 
           s
         else
           Warn.fatal2 "instruction mismatch - linksem: %s vs objdump: %s\n"  (pp_addr i) (pp_addr i')
      | None ->
         ""
      end
    ^ "\n"
    (* the frame info for this address *)
    ^ pp_frame_info test addr
    (* the source file lines (if any) associated to this address *)
    ^ begin match pp_dwarf_source_file_lines () test.dwarf_static true addr with Some s -> s^"\n" | None -> "" end
    (* the variables whose location ranges include this address *)
    ^ let als (*fald*) = Dwarf.filtered_analysed_location_data test.dwarf_static addr in
      Dwarf.pp_analysed_location_data test.dwarf_static.ds_dwarf als

    ^ "\n"
  in

  String.concat "" (List.map pp_instruction instructions)





(** ********************************************************************* *)
(** **          top-level                                              ** *)
(** ********************************************************************* *)
    

let process_file (filename:string) : unit =
  let test = parse_file filename in
  printf "%s" (pp_test test)
    
