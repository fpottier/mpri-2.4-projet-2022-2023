let map = List.map
open Printf
open PPrintExtra

(* ------------------------------------------------------------------------- *)

(* Parse the command line. *)

type command =
  | Undefined
  | ReadFile of (* filename: *) string
  | Random

let command =
  ref Undefined

type processing =
  | ForwardModeOnly
  | ForwardModeAndReverseMode

let processing =
  ref ForwardModeAndReverseMode

let budget =
  ref 5

let mark =
  ref false

let usage =
  sprintf "Usage: %s <options> <filename>\n" Sys.argv.(0)

let resilient =
  ref false

let show_surface =
  ref false

let show_linear =
  ref false

let test_forward, test_unzip, test_reverse =
  ref false, ref false, ref false

let verbose =
  ref false

let highlight_style =
  ref LinearPrinter.Ansi

let highlight_styles = [
  "text", LinearPrinter.Text;
  "color", LinearPrinter.Ansi;
  "none", LinearPrinter.No_highlight;
]

let highlight_styles_descr =
  Printf.sprintf "[ %s ]"
    (String.concat " | "
       (List.map fst highlight_styles))

let set_highlight_style str =
  match List.assoc str highlight_styles with
  | style -> highlight_style := style
  | exception _ ->
    Printf.ksprintf (fun msg -> raise (Arg.Bad msg))
      "--highlight-style <style>: use one of %s"
      highlight_styles_descr

let () =
  Arg.parse (Arg.align [
    "--budget", Arg.Set_int budget, "<int> sets a random generation budget";
    "--forward-mode", Arg.Unit (fun () -> processing := ForwardModeOnly),
                      " perform just forward-mode AD";
    "--mark", Arg.Set mark, " produce marks in output (internal use)";
    "--resilient", Arg.Set resilient, " continue after a test fails";
    "--reverse-mode", Arg.Unit (fun () -> processing := ForwardModeAndReverseMode),
                      " perform reverse-mode AD";
    "--show-surface", Arg.Set show_surface, " show the Surface program";
    "--show-linear",  Arg.Set show_linear,  " show the Linear program";
    "--test-forward", Arg.Set test_forward, " test forward mode on the fly";
    "--test-unzip", Arg.Set test_unzip, " test unzipping on the fly";
    "--test-reverse", Arg.Set test_reverse, " test reverse mode on the fly";
    "--highlight-style", Arg.String set_highlight_style,
      "<style> choose error highlight style among " ^ highlight_styles_descr;
    "--verbose", Arg.Set verbose, " show progress messages";
  ]) (fun name -> command := ReadFile name) usage

let fail format =
  ksprintf (fun s -> fprintf stderr "%s%!" s; exit 1) format

let budget =
  !budget

let command =
  match !command with
  | Undefined | Random ->
      Random
  | ReadFile _ ->
      !command

let mark =
  !mark

let processing =
  !processing

let resilient =
  !resilient

let show_surface =
  !show_surface

let show_linear =
  !show_linear

let test_forward, test_unzip, test_reverse =
  !test_forward, !test_unzip, !test_reverse

let verbose =
  !verbose

(* ------------------------------------------------------------------------- *)

(* Helpers. *)

let info msg prog =
  if verbose then printf "%s\n" msg;
  prog

(* ------------------------------------------------------------------------- *)

(* If we are reading and processing a single file, then we want to stop as
   soon as an exception is raised, and display a message.

   If we are performing random testing, then we may either want to stop at
   the first exception, as above, or continue and report the number of
   failures at the end. *)

(* [failures] counts the failures that we have encountered. When we face a
   failure, we abandon the current (randomly generated) program. *)

let failures =
  ref 0

exception Abandon

let maybe_exit msg =
  if resilient then begin
    (* In resilient mode, increment the failure counter and abandon this
       program. *)
    incr failures;
    raise Abandon
  end
  else begin
    (* In normal mode, display an error message and stop everything. *)
    eprintf "%s%!" msg;
    exit 1
  end

let print_failures (final : bool) =
  if resilient then
    printf "Failures (%s): %d\n%!"
      (if final then "total" else "so far")
      !failures

(* ------------------------------------------------------------------------- *)

(* Reading, lexing and parsing a source file. *)

let read filename : Surface.prog =
  let _text, lexbuf = MenhirLib.LexerUtil.read filename in
  try
    SurfaceParser.prog SurfaceLexer.token lexbuf
  with SurfaceParser.Error ->
    let startp = Lexing.lexeme_start_p lexbuf
    and endp = Lexing.lexeme_end_p lexbuf in
    fail "%sSyntax error.\n" (MenhirLib.LexerUtil.range (startp, endp))

(* -------------------------------------------------------------------------- *)

(* Marks help delimit code fragments in our printed output. *)

let delimit (olabel : string option) (f : unit -> unit) =
  match mark, olabel with
  | true, Some label ->
      printf "(* %s *)\n" label;
      f();
      printf "(* %s *)\n" label
  | _ ->
      f()

(* -------------------------------------------------------------------------- *)

(* Type-checking and evaluation at the Surface level, with progress messages. *)

module S = struct

  open Surface
  open SurfaceTypeChecker
  open SurfacePrinter

  let check olabel (prog : prog) : prog =
    match infer prog with
    | _fenv ->
        if verbose then printf "The program is well-typed.\n%!";
        if show_surface then
          delimit olabel (fun () -> print (print_program prog));
        prog
    | exception Failure msg ->
        maybe_exit msg

  let test_reverse_mode prog prog' =
    if test_reverse then begin try
      Test.test_reverse_mode prog prog'
    with Test.TestFailure msg ->
      maybe_exit msg
    end;
    prog'

  let print prog' =
    match command with
    | ReadFile _ ->
        print (print_program prog')
    | _ ->
        ()

end

(* -------------------------------------------------------------------------- *)

(* Type-checking and evaluation at the Linear level, with progress messages. *)

module L = struct

  open Linear
  open LinearHelp
  open LinearTypeChecker
  open LinearPrinter

  let check olabel mode (prog : prog) : prog =
    let prog = annotate_prog prog in
    match check mode prog with
    | prog ->
        (* We normally do not show the program *before* type-checking. This
           program can contain [unknown] type annotations, which the
           type-checker replaces with inferred types. We show the program
           *after* type-checking. *)
        if verbose then printf "The program is well-typed.\n%!";
        if show_linear then
          delimit olabel (fun () -> print (print_program prog));
        prog
    | exception LinearTypeChecker.Error (range, msg) ->
        (* If the program is ill-typed, then we show the program *before*
           type-checking. *)
        let ranges =
          Option.fold ~none:RangeSet.empty ~some:RangeSet.singleton range in
        let highlight = {
          ranges;
          style = !highlight_style;
        } in
        Printf.ksprintf maybe_exit "%s%s.\n%s"
          (match range with
           | Some (Source range) -> MenhirLib.LexerUtil.range range
           | Some (Virtual _) | None -> "")
          msg
          (to_string (print_program ~highlight prog))

  let progress olabel msg mode prog =
    prog
    |> info msg
    |> check olabel mode

  let simplify olabel prog =
    prog
    |> Freshen.transform
    |> progress None "Freshened." `Strict
    |> Normalize.transform
    |> progress None "Normalized." `Strict
    |> Simplify.transform
    |> progress olabel "Simplified." `Strict

  let resurface_silently (prog : Linear.prog) : Surface.prog =
    prog
    |> Forget.transform
    |> Simplify.transform
    |> Linear2Surface.translate
    |> Compress.transform
    |> S.check (Some "FMAD_RESURFACED")

  let resurface (prog : Linear.prog) : Surface.prog =
    prog
    |> Forget.transform
    |> Simplify.transform
    |> Linear2Surface.translate
    |> Compress.transform
    |> info "Back up to Surface."
    |> S.check (Some "RMAD_RESURFACED")

  let test_forward_mode prog prog' =
    if test_forward then begin try
      Test.test_forward_mode prog (resurface_silently prog')
    with Test.TestFailure msg ->
      maybe_exit msg
    end;
    prog'

  let test_unzip fs prog prog' =
    if test_unzip then begin try
      Test.test_unzip fs prog prog'
    with Test.TestFailure msg ->
      maybe_exit msg
    end;
    prog'

end

(* -------------------------------------------------------------------------- *)

(* The main pipeline. *)

(* This pipeline consists of two halves: first, everything up to and including
   forward-mode AD; then, reverse-mode AD. *)

(* The first half of the pipeline. *)

let half1 (sprog : Surface.prog) : Linear.prog =

  (* Type-check and evaluate the Surface program. *)
  sprog
  |> S.check (Some "ORIG_SURFACE")

  (* Convert it to Linear, type-check, evaluate. *)
  |> Surface2Linear.transform
  |> L.simplify (Some "ORIG_LINEAR")
  |> L.progress None "Down to Linear." `Strict

  (* Perform forward-mode AD, type-check, evaluate. *)
  (* Forward-mode AD can produce code where the linearity discipline is
     violated, so we must be lenient at this point. *)
  |> ForwardMode.transform
  |> L.progress None "Forward-mode AD done." `Lenient

  (* Enforce linearity. *)
  |> DupDropInsertion.transform
  |> L.progress None "Dups and drops inserted." `Strict

  (* Normalize, simplify. *)
  |> L.simplify (Some "FMAD_LINEAR")

  (* Test that the transformed program at this point satisfies its
     specification. *)
  |> L.test_forward_mode sprog

(* The second half of the pipeline. *)

let half2 (sprog : Surface.prog) (prog : Linear.prog) : Surface.prog =

  (* Record the names of the functions [f] in the Surface program. *)
  let fs = Surface.names sprog in
  (* The derivatives [df] are the functions that we will want to unzip. *)
  let dfs = map NamingConventions.derivative fs in
  (* The functions [ldf] and [cdf], produced by unzipping, are the functions
     that we will want to transpose. *)
  let ldfs = map NamingConventions.linear dfs
  and cdfs = map NamingConventions.combined dfs in

  prog

  (* Unzip every derivative function [df], producing [udf], [ldf], and [cdf]. *)
  |> Unzip.transform dfs
  |> L.progress None "Unzipped." `Strict

  (* Normalize, simplify again. *)
  |> L.simplify (Some "UNZIPPED_LINEAR")

  (* Test whether the unzipped program satisfies its specification. *)
  |> L.test_unzip dfs prog

  (* Transpose all [ldf] and [cdf] functions. *)
  |> Transpose.transform (ldfs @ cdfs)
  |> L.progress None "Transposed." `Strict

  (* Normalize, simplify again. *)
  |> L.simplify (Some "TRANSPOSED_LINEAR")

  (* Transport back to Surface. *)
  |> L.resurface

  (* Test that the transformed program satisfies its specification. *)
  |> S.test_reverse_mode sprog

(* The second half of the pipeline is executed only in --reverse-mode. *)

let half2 (sprog : Surface.prog) (prog : Linear.prog) : Surface.prog =
  match processing with
  | ForwardModeOnly ->
      L.resurface prog
  | ForwardModeAndReverseMode ->
      half2 sprog prog

(* The complete pipeline. *)

let pipeline (sprog : Surface.prog) =
  sprog
  |> half1
  |> half2 sprog
  (* If the command is [ReadFile _], print the transformed program. *)
  |> S.print

(* -------------------------------------------------------------------------- *)

(* The number of attempts per level of budget is defined by the fixed
   following function. We want to run many different tests at small
   sizes and fewer tests at large sizes. *)

let attempts budget =
  if budget < 5 then
    1000
  else if budget < 10 then
    500
  else if budget < 30 then
    100
  else if budget < 60 then
    50
  else
    10

(* -------------------------------------------------------------------------- *)

(* Running the main pipeline. *)

let main () =
  match command with
  | Undefined ->
      assert false
  | ReadFile filename ->
      (* If a file name is specified on the command line, read this file
         and feed it into the pipeline. *)
      read filename
      |> pipeline
  | Random ->
      assert (budget >= 0);
      (* If no file name is specified on the command line, repeatedly generate
         programs of increasing sizes and feed them into the pipeline. *)
      for budget = 0 to budget do
        let attempts = attempts budget in
        for _ = 1 to attempts do
          try
            SurfaceGenerator.generate budget
            |> pipeline
          with Abandon ->
            ()
        done;
        printf "Done running %d tests at budget %d.\n%!" attempts budget;
        print_failures false
      done

(* -------------------------------------------------------------------------- *)

(* Run the main pipeline and handle exceptions. *)

let () =
  try
    main();
    print_failures true
  with e ->
    print_failures true;
    printf "%s\n" (Printexc.to_string e);
    Printexc.print_backtrace stdout;
    exit 1
