open Logs.Logger (struct
  let str = __MODULE__
end)

type t = Elf.Relocations.target

type segment = string * (int * int) (* mapping the name of a segment to the range of the relocation value *)

let pp_opcode_with_segments (b, r) =
  match r with
  | None -> Pp.(!^"#x" ^^ BytesSeq.ppint b)
  | Some (Elf.Relocations.AArch64 rtype) ->
      let bits = BytesSeq.getbvle ~size:32 b 0 in
      Pp.(
        match rtype with
        | Abi_aarch64_symbolic_relocation.Data640 -> fatal "Data64 relocation not allowed for instruction"
        | Abi_aarch64_symbolic_relocation.Data320 -> fatal "Data32 relocation not allowed for instruction"
        | Abi_aarch64_symbolic_relocation.ADRP -> 
            BitVec.pp_smt (BitVec.extract 31 31 bits)  
            ^^ !^" x0:2 " ^^
            BitVec.pp_smt (BitVec.extract 24 28 bits)
            ^^ !^" x1:19 " ^^
            BitVec.pp_smt (BitVec.extract 0 4 bits)
        | Abi_aarch64_symbolic_relocation.ADD -> 
            BitVec.pp_smt (BitVec.extract 22 31 bits)  
            ^^ !^" x0:12 " ^^
            BitVec.pp_smt (BitVec.extract 0 9 bits)
        | Abi_aarch64_symbolic_relocation.LDST b -> (* TODO different width loads, alignment *) 
            BitVec.pp_smt (BitVec.extract (22-b) 31 bits)  
            ^^ !^" x0:" ^^ int (12-b) ^^ !^" " ^^
            BitVec.pp_smt (BitVec.extract 0 9 bits)
        | Abi_aarch64_symbolic_relocation.CALL -> 
            BitVec.pp_smt (BitVec.extract 26 31 bits)  
            ^^ !^" x0:26 "
      )

(* for interpreting the segments *)
let segments_of_reloc: t -> segment list = function
| Elf.Relocations.AArch64 Abi_aarch64_symbolic_relocation.Data640 -> fatal "invalid relocation for instructions (Data64)"
| Elf.Relocations.AArch64 Abi_aarch64_symbolic_relocation.Data320 -> fatal "invalid relocation for instructions (Data32)"
| Elf.Relocations.AArch64 Abi_aarch64_symbolic_relocation.ADRP -> ["x0", (0, 1); "x1", (2, 20)] (* or absolute? ["x0", (12, 13); "x1", (14, 32)] *)
| Elf.Relocations.AArch64 Abi_aarch64_symbolic_relocation.ADD -> ["x0", (0, 11)]
| Elf.Relocations.AArch64 Abi_aarch64_symbolic_relocation.LDST b -> ["x0", (0, 11-b)] (* TODO depends on load size *) (* or absolute? ["x0", (2, 11)] *)
| Elf.Relocations.AArch64 Abi_aarch64_symbolic_relocation.CALL -> ["x0", (0, 25)] (* or absolute? ["x0", (2, 27)] *)