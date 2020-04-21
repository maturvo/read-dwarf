(** This module is about calling external processes, and helper functions for doing that *)

open Logs.Logger (struct
  let str = "Cmd"
end)

open Protect
open Unix

type cmd = string array

(** Thrown when a call to cmd resulted in non-zero exit status *)
exception Crash of cmd * process_status

let _ =
  Printexc.register_printer (function
    | Crash (cmd, code) -> Some PP.(sprint @@ array string cmd ^^ blank 1 ^^ statusi code)
    | _ -> None)

(** Check the return status and throw Chrash if input is noe WEXITED 0 *)
let check_status cmd = function
  | WEXITED code when code = 0 -> ()
  | WSIGNALED code when code < 0 -> warn "Signal %d should not exist: ????\n" code
  | code -> raise (Crash (cmd, code))

(** Close input channel for channel openned with open_process_in.
    Throw Crash if the process did not return 0 *)
let closing_in channel cmd () = check_status cmd @@ close_process_in channel

(** Close output channel for channel openned with open_process_out.
    Throw Crash if the process did not return 0 *)
let closing_out channel cmd () = check_status cmd @@ close_process_out channel

(** Close channels for channel openned with open_process.
    Throw Crash if the process did not return 0 *)
let closing channels cmd () = check_status cmd @@ close_process channels

(** Call the [which] shell command to get a executable position from its name.
    May throw Crash if the command fails*)
let which arg =
  debug "Calling which on %s" arg;
  let channel = open_process_in @@ "which " ^ arg in
  protect (fun () -> input_line channel) @@ closing_in channel [|"which"; arg|]

(** Expand an executable path using which if it's not already a full path *)
let get_full_path (s : string) : string =
  match s.[0] with
  | '/' | '.' -> s
  | _ -> (
      try which s
      with _ ->
        warn "Trying to get path for %s with `which` and failed" s;
        s
    )

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Pipe input } *)

(** Call cmd and then call cont to read the output of the command *)
let input (cmd : cmd) (cont : in_channel -> 'a) : 'a =
  cmd.(0) <- get_full_path cmd.(0);
  let output = open_process_args_in cmd.(0) cmd in
  protect (fun () -> cont output) @@ closing_in output cmd

(** Call a command and read its output in a string *)
let input_string (cmd : string array) : string = input cmd Files.input_string

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Simple command } *)

(** Call cmd. That's it. Check the status is valid with {!check_status}*)
let cmd (cmd : cmd) : unit =
  cmd.(0) <- get_full_path cmd.(0);
  let output = open_process_args_in cmd.(0) cmd in
  closing_in output cmd ()

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Pipe output } *)

(** Call cmd and then call cont on the output pipe *)
let output (cmd : cmd) (output : out_channel -> unit) : 'a =
  cmd.(0) <- get_full_path cmd.(0);
  let input = open_process_args_out cmd.(0) cmd in
  protect (fun () -> output input) @@ closing_out input cmd

(** Call a command and read its output in a string *)
let output_string (cmd : string array) s : unit = output cmd (fun o -> output_string o s)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Command input and output } *)

(** Call the command provided, then call the first continuation to send
    a message and then call the second continuation to get the answer.
    Then it close the child process properly.
*)
let io (cmd : cmd) (out_cont : out_channel -> unit) (in_cont : in_channel -> 'a) : 'a =
  cmd.(0) <- get_full_path cmd.(0);
  (* PP.(println @@ array string cmd); *)
  let (output, input) = open_process_args cmd.(0) cmd in
  let process () =
    out_cont input;
    flush input;
    in_cont output
  in
  protect process @@ closing (output, input) cmd

let _ =
  Tests.add_test "Cmd.io.cat" (fun () ->
      let output oc = Printf.fprintf oc "test string\n" in
      let input ic = input_line ic in
      let a = io [|"cat"|] output input in
      a = "test string")

(** This submodule manages another process as request server, trough pipes like Z3*)
module IOServer = struct
  (** The type a pipe server *)
  type t = { cmd : cmd; input : in_channel; output : out_channel }

  (** State the pipe server using commands *)
  let start (cmd : cmd) : t =
    cmd.(0) <- get_full_path cmd.(0);
    let (input, output) = open_process_args cmd.(0) cmd in
    { cmd; input; output }

  (** Stop the pipe server *)
  let stop (t : t) = close_process (t.input, t.output) |> check_status t.cmd
end

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Socket management } *)

(** This submodule is to manage another process as request server, like isla *)
module Server = struct
  (** This type holds all the data about the server subprocess *)
  type t = { name : string; pid : int; socket : file_descr; sock_path : string }

  (** Make a new socket, bind it to a temporary file name using the name provided
      and then listen to it *)
  let make_socket name =
    let sock_fd = Unix.(socket PF_UNIX SOCK_STREAM 0) in
    let sock_path =
      Filename.concat (Filename.get_temp_dir_name ())
        (Printf.sprintf "%s-%d.socket" name (Random.bits ()))
    in
    info "New socket : %s" sock_path;
    Unix.(bind sock_fd (ADDR_UNIX sock_path));
    at_exit (fun () -> try Unix.unlink sock_path with Unix_error _ -> ());
    Unix.listen sock_fd 1;
    (sock_fd, sock_path)

  (** Start the server with provided name and wait for it to connect to the socket.
      Then build the {!Server.t} object.
      the second argument must take a socket name and give a valid command line to call
      the server subprocess.
  *)
  let start name (cmdf : string -> cmd) : t =
    let (sock_fd, sock_path) = make_socket name in
    let cmd = cmdf sock_path in
    cmd.(0) <- get_full_path cmd.(0);
    let pid = Unix.(create_process cmd.(0) cmd stdin stdout stderr) in
    let (sock_fd, _) = Unix.accept sock_fd in
    info "Socket connected to %s" name;
    { name; pid; socket = sock_fd; sock_path }

  (** Stop the cut the connection, wait for the subprocess to die and then delete the socket *)
  let stop server =
    info "Closing connection with %s" server.name;
    shutdown server.socket SHUTDOWN_ALL;
    ignore @@ waitpid [] server.pid;
    Unix.unlink server.sock_path;
    info "Connection with %s closed" server.name

  (** [read_exact sock size] reads exactly size bytes on sock and return them as a Bytes *)
  let read_exact sock_fd exact =
    let rec read_exact_offset buff ofs =
      let nbytes = Unix.read sock_fd buff ofs (exact - ofs) in
      let ofs = ofs + nbytes in
      if ofs = exact then ()
      else if ofs > exact then failwith "Read too many bytes"
      else read_exact_offset buff ofs
    in
    let buff = Bytes.create exact in
    read_exact_offset buff 0;
    buff

  (** Read a string from the server with the 32 bit size then content format *)
  let read_string server =
    let sock_fd = server.socket in
    let header = read_exact sock_fd 4 in
    let length = Int32.to_int (Bytes.get_int32_le header 0) in
    let body = read_exact sock_fd length in
    let str = Bytes.unsafe_to_string body in
    debug "Read socket string \"%s\" from %s" str server.name;
    str

  (** Read a single byte from the server*)
  let read_byte server : int =
    let sock_fd = server.socket in
    let header = read_exact sock_fd 1 in
    let c = Char.code (Bytes.get header 0) in
    debug "Read byte \"%d\" from %s" c server.name;
    c

  (** Send a string to the server with the 32 bit size then content format *)
  let write_string server str =
    debug "Writing \"%s\" to %s" str server.name;
    let sock_fd = server.socket in
    let len = String.length str in
    let msg = Bytes.create (len + 4) in
    Bytes.set_int32_le msg 0 (Int32.of_int len);
    Bytes.blit_string str 0 msg 4 len;
    let _ = Unix.write sock_fd msg 0 (len + 4) in
    ()
end
