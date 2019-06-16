type kbd_key = S of char (* single key button *)
             | Z of float (* wide button *)

let dvorak_kbd_data = [
  [ S('`'); S('1');  S('2'); S('3'); S('4'); S('5'); S('6'); S('7'); S('8'); S('9'); S('0'); S('['); S(']');  Z(2.0) ];
  [ Z(1.5); S('\''); S(','); S('.'); S('p'); S('y'); S('f'); S('g'); S('c'); S('r'); S('l'); S('/'); S('=');  Z(1.5) ];
  [ Z(2.);  S('a');  S('o'); S('e'); S('u'); S('i'); S('d'); S('h'); S('t'); S('n'); S('s'); S('-'); S('\\'); Z(1.) ];
  [ Z(1.5); S('<');  S(';'); S('q'); S('j'); S('k'); S('x'); S('b'); S('m'); S('w'); S('v'); S('z');          Z(2.5) ];
]

open Js_of_ocaml
open Js_of_ocaml_tyxml

module Html = Dom_html

let create_keyboard_svg layout =
  let open Tyxml_js in
  let open Svg in
  let keys = layout
             |> List.mapi (fun row cols ->
                 (List.fold_left (fun (col, prev_cols) key ->
                      let w = match key with
                        | S(_) -> 1.
                        | Z(w) -> w
                      in
                      let rect' = rect ~a:[a_x (col *. 40., Some `Px);
                                           a_y (float_of_int(row * 40), Some `Px);
                                           a_width (w *. 40., Some `Px);
                                           a_height (40., Some `Px);
                                          ] [] in
                        (col +. w), rect' :: prev_cols
                    ) (0., []) cols
                 )
                 |> snd
                 |> List.rev
               )
             |> List.flatten
  in
    svg ~a:[a_width (800., Some `Px); a_height (300., Some `Px);]
      keys

let create_rect () =
  let open Tyxml_js in
  let my_svg = [%svg{| <svg width="800" height="400"></svg> |}] in
  let onclick (ev) =
    print_endline "clicked";
    Js.Opt.iter ev##.target (fun t -> t##.style##.fill := Js.string "blue");
    false in
  let my_rect = Svg.(
      rect ~a:[a_width (200., Some `Px);
               a_height (100., Some `Px);
               a_onclick onclick;
              ] []
    ) in
    Dom.appendChild (Svg.toelt my_svg) (Svg.toelt my_rect);
    my_svg


let start () =
  Dom.appendChild
    (Dom_html.getElementById "container")
    (Tyxml_js.Svg.toelt (create_keyboard_svg dvorak_kbd_data))

let _ =
  Html.window##.onload :=
    Html.handler (fun _ ->
        ignore (start ());
        Js._false
      )
