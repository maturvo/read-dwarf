type t = {
  section : string;
  offset: int;
}

let pp addr = Pp.(!^(addr.section) ^^ !^"+" ^^ ptr addr.offset)

let of_linksem (section, offset) = { section; offset = Z.to_int offset }

let (+) addr offset = { section = addr.section; offset = addr.offset + offset }