(** This is the user configuration module.
    It is pulled from the root config.ml file or the default_config.ml file

    All configuration option are specified here.
*)

(* Please ensure the documentation of this file matches the documentation in default_config.ml *)

(** Default Z3 command. (Can be overidden with Z3_PATH and --z3) *)
val z3 : string

(** Default Isla command. (Can be overidden with ISLA_CLIENT and --isla) *)
val isla_client : string

(** Default ir file for isla. (Can be overidden with ISLA_ARCH and -a/--arch) *)
val arch_file : string

(** The Architecture module. They are in src/archs. *)
module Arch : ArchSig.S

(** Whether to enable backtraces or not *)
val enable_backtrace : bool

(** Whether to enable internal unit test and the test test sub-command *)
val enable_tests : bool

(** The toolchain to use when working with aarch64 architecture (GNU Prefix) *)
val aarch64_toolchain : string