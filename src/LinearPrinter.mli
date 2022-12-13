open PPrint
open Linear

(** a [highlight] is information for highlighting
    certain parts of the printed program. *)
type highlight = {
  ranges: RangeSet.t;
    (** what parts of the program to highlight *)
  style: highlight_style;
    (** how to highlight them *)
}

and highlight_style =
  | Text (** include a text marker *)
  | Ansi (** bold+red using Ansi escape codes *)
  | No_highlight (** no visible highlight *)

(**[print_program ?highlight] converts a Linear program to a PPrint document.
   [highlight] provides an optional set of ranges to highlight in the printed program.
*)
val print_program : ?highlight:highlight -> prog -> document
