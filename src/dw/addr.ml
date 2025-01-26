include Elf.Address

let of_sym : Sym.t -> t = function
| Dwarf.Offset (section, offset) -> { section; offset = Z.to_int offset }
| _ -> Raise.fail "expected section+offset"

