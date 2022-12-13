(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)


(* use ANSI color codes, see https://en.wikipedia.org/wiki/ANSI_escape_code *)
type color =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

type style =
  | FG of color (* foreground *)
  | BG of color (* background *)
  | Bold
  | Reset

let ()  =
  ignore [Black; Red; Green; Yellow; Blue; Magenta; Cyan; White];
  ignore [FG Black; BG Black; Bold; Reset];
  ()

let ansi_of_color = function
  | Black -> "0"
  | Red -> "1"
  | Green -> "2"
  | Yellow -> "3"
  | Blue -> "4"
  | Magenta -> "5"
  | Cyan -> "6"
  | White -> "7"

let code_of_style = function
  | FG c -> "3" ^ ansi_of_color c
  | BG c -> "4" ^ ansi_of_color c
  | Bold -> "1"
  | Reset -> "0"

let ansi_of_style_l l =
  let s = match l with
    | [] -> code_of_style Reset
    | [s] -> code_of_style s
    | _ -> String.concat ";" (List.map code_of_style l)
  in
  "\x1b[" ^ s ^ "m"

let of_styles = ansi_of_style_l
