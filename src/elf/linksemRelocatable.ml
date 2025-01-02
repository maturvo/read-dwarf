(* TODO header *)

type sym_addr = string * Z.t

(* Like in linksem, but address is section+offset, and with a writable flag *)
type symbol = string * (Z.t * Z.t * sym_addr * Byte_sequence_wrapper.byte_sequence * Z.t) * bool

type global_symbol_init_info = symbol list

open Elf_symbol_table
open Elf_interpreted_section

let get_elf64_file_global_symbol_init f : global_symbol_init_info Error.error =
  let secs = f.Elf_file.elf64_file_interpreted_sections in
  Error.bind (Elf_file.get_elf64_file_symbol_table f) (fun (symtab, strtab) ->
    List.filter_map (
      fun entry ->
        let name = Uint32_wrapper.to_bigint entry.elf64_st_name in
        let addr_offset = Uint64_wrapper.to_bigint entry.elf64_st_value in
        let size = Uint64_wrapper.to_bigint entry.elf64_st_size in
        let shndx = Uint32_wrapper.to_int entry.elf64_st_shndx in
        let typ  = Elf_symbol_table.extract_symbol_type entry.elf64_st_info in
        let bnd  = Elf_symbol_table.extract_symbol_binding entry.elf64_st_info in
        Option.map (
          fun section ->
            let addr = (section.elf64_section_name_as_string, addr_offset) in
            let data = if Byte_sequence.length0 section.elf64_section_body = Z.zero then
              Error.return (Byte_sequence.zeros size)
            else
              Byte_sequence.offset_and_cut addr_offset size section.elf64_section_body
            in
            Error.bind data (fun data ->
            Error.bind (String_table.get_string_at name strtab) (fun str ->
              let write = Elf_file.flag_is_set Elf_section_header_table.shf_write section.elf64_section_flags in
              Error.return (str, (typ, size, addr, data, bnd), write)
            ))
        ) (List.nth_opt secs shndx)
    ) symtab |> Error.mapM Fun.id
  )