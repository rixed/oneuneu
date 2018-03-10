open Batteries
open Option.Infix
open Ogli
open Ogli_view

let debug = true
let f2i = int_of_float
let i2f = float_of_int

let f2s v = Printf.sprintf "%+.4g" v
let i2s = string_of_int

(* Activation Functions *)

let sq x = x *. x
let sigmoid x = 1. /. ((exp ~-.x) +. 1.)

type transfer = Sigmoid | TanHyp | Linear [@@ppp PPP_JSON]
let string_of_transfer = function
  | Sigmoid -> "sigmoid"
  | TanHyp  -> "hyp.tang."
  | Linear  -> "linear"

let transfer = function
  | Sigmoid -> sigmoid
  | TanHyp  -> tanh
  | Linear  -> fun x -> x (* TODO: configurable attenuation factor or Linear1|Linear2 etc... *)

let transfer' = function
  | Sigmoid -> fun o -> o *. (1. -. o)
  | TanHyp  -> fun o -> 1. -. sq o
  | Linear  -> fun _ -> 1.

let x_range = function
  | Sigmoid -> Some (~-.1.732, 1.732)
  | TanHyp  -> Some (~-.1.688, 1.688)
  | Linear  -> None

let y_range = function
  | Sigmoid -> Some (0., 1.)
  | TanHyp  -> Some (~-.1., 1.)
  | Linear  -> None

let scale range (xmin, xmax) x =
  match range with
  | None -> x
  | Some (mi, ma) ->
    ((x -. xmin) /. (xmax -. xmin)) *. (ma -. mi) +. mi

let scale_rev range (xmin, xmax) i =
  match range with
  | None -> i
  | Some (mi, ma) ->
    ((i -. mi) /. (ma -. mi)) *. (xmax -. xmin) +. xmin

let scale_input func = scale (x_range func)
let scale_input_rev func = scale_rev (x_range func)
let scale_output func = scale (y_range func)
let scale_output_rev func = scale_rev (y_range func)

(* Inputs/Outputs neurons *)

(* Simplest kind of input: we input a value of some field in the CSV. *)
type io =
  { id : int ;
    (* which columns we are considering: *)
    col : int Param.t ;
    (* (> 0), how many back in time should we look - useful to have at the
     * same time several selected past values: *)
    lag : int Param.t ;
    (* How many past values to average with the current one: *)
    avg : int Param.t ;
    (* Precomputed lagged/averaged (but not diffed) values from the CSV: *)
    csv_values : float array ;
    (* Extremums values of the diffed csv_values *)
    mutable diff_extremums : float * float }

let sort_io =
  List.fast_sort (fun io1 io2 -> compare io1.id io2.id)

let find_io ios id =
  List.find (fun io -> io.id = id) ios

let need_save = Param.make "need save" false
let set_need_save () = Param.set need_save true

let inputs = Param.make ~on_update:[ set_need_save ] "inputs" []
let outputs = Param.make ~on_update:[ set_need_save ] "outputs" []

let max_lag = ref 0
let update_max_lag () =
  max_lag :=
    List.fold_left (fun m i ->
      max m (i.lag.value + i.avg.value)
    ) 0 inputs.Param.value
let () = Param.on_update inputs update_max_lag


let box_selection = Param.make "box selection" None

(* UI layout *)

module Layout =
struct
  let text_line_height = 16

  let inputs_height = 3 * text_line_height

  let neuron_radius = 12 (* either width or height *)

  let screen_width = Param.make "screen width" 320
  let screen_height = Param.make "screen height" 200

  (* Manual Layout:
   *
   * We have several horizontal zones: one for the inputs, one for the
   * neural net, one for the output and one for the result: *)

  (* Initial values will be updated according to actual content: *)
  let input_width = Param.make "input width" 130
  let neural_net_height = Param.make "neural net height" 300
  let layer_adder_height = Param.make "layer neurons adder height" 70
  let details_height = Param.make "neuron details height" 200
  let outputs_height = Param.make "outputs height" 50
  let output_width = Param.make "output width" 130
  let result_height = Param.make "result height" (8 * text_line_height)

  (* We have a control-column on the left with various widgets: *)
  let control_column_width = Param.make "controls width" 200

  (* TODO add visibility flags for each components in the layout so that we can
   * disable some display when screen becomes too small *)
  let () =
    let resize () =
      Param.set neural_net_height
        (screen_height.value - inputs_height - outputs_height.value - result_height.value)
    in
    Param.on_update screen_width resize ;
    Param.on_update screen_height resize

  let () =
    let resize () =
      let nb_inputs = List.length inputs.value in
      Param.set input_width
        (if nb_inputs = 0 then 0 else
         (screen_width.value - control_column_width.value) / nb_inputs)
    in
    Param.on_update inputs resize ;
    Param.on_update screen_width resize

  let () =
    let resize () =
      let nb_outputs = List.length outputs.value in
      Param.set output_width
        (if nb_outputs = 0 then 0 else
         (screen_width.value - control_column_width.value) / nb_outputs)
    in
    Param.on_update outputs resize ;
    Param.on_update screen_width resize
end

(* Some generic widget renderers *)

module Widget =
struct
  let rect ?on_drag_start ?on_drag_stop ?on_drag ?on_click ?on_sub_click ?on_hover color ~x ~y ~width ~height =
    let poly = Path.rect (pi x y) (pi (x + width) (y + height)) |>
               Algo.poly_of_path ~res:K.one (* unused *) in
    Ogli_render.shape_of_polys ?on_drag_start ?on_drag_stop ?on_drag ?on_click ?on_sub_click ?on_hover [ color, [ poly ] ] Point.origin []

  let todo ~x ~y ~width ~height =
    let color = C.rand C.one in
    rect color ~x ~y ~width ~height

  let text ?on_click text ~x ~y ~width ~height =
    (* TODO: clip text by width/height. shape.bbox should be a clip_box (with a flag to tell when clip is not necessary?) *)
    ignore width ; ignore height ;
    let position = pi x y in
    let color = C.black in
    let size = float_of_int Layout.text_line_height in
    Ogli_render.shape_of_text ~move_to_lower_left:true ?on_click color size text position []

  let button ?on_click ?(selected=false) label ~x ~y ~width ~height =
    let color =
      if selected then c 0.5 0.9 0.5 else c 0.8 0.8 0.8 in
    group [
      rect ?on_click color ~x ~y ~width ~height ;
      text ?on_click label ~x ~y ~width ~height ]

  let click_left_right ~x ~width pos =
    let dx = (K.to_float pos.(0) -. i2f x) /. i2f width in
    if dx < 0.45 then -1 else
    if dx > 0.55 then +1 else 0

  let simple_select ?(wrap=false) options param ~x ~y ~width ~height =
    let max_idx = Array.length options - 1 in
    if max_idx < 0 then
      button "(empty)" ~x ~y ~width ~height
    else fun_of param (fun selected ->
      let cur_idx, label =
        try
          let i = Array.findi (fun (k, _) -> k = selected) options in
          i, snd options.(i)
        with Not_found -> -1, "invalid" in
      let on_click _ pos =
        if cur_idx = -1 then
          (* initial value was invalid *)
          Param.set param (fst options.(0))
        else
          let new_idx =
            cur_idx + click_left_right ~x ~width pos in
          let new_idx =
            if new_idx <= max_idx then new_idx else
              if wrap then 0 else max_idx in
          let new_idx =
            if new_idx >= 0 then new_idx else
              if wrap then max_idx else 0 in
          if new_idx <> cur_idx then
            Param.set param (options.(new_idx) |> fst)
      in
      let arrow_w = 10 and label = "  "^ label in
      [ button label ~on_click ~x ~y ~width ~height ;
        (if wrap || cur_idx > 0 then
          text "‹" ~x ~y ~width:arrow_w ~height
        else group []) ;
        (if wrap || cur_idx < max_idx then
          text "›" ~x:(x + width - arrow_w) ~y ~width:arrow_w ~height
        else group []) ])

  let int_select ?(min=0) ?(max=max_int) ?(wrap=false) param ~x ~y ~width ~height =
    if min > max then
      button "(no choice)" ~x ~y ~width ~height
    else fun_of param (fun selected ->
      let on_click _ pos =
        let new_val =
          if selected < min || selected > max then min else
          selected + click_left_right ~x ~width pos in
        let new_val =
          if new_val <= max then new_val else
            if wrap then min else max in
        let new_val =
          if new_val >= min then new_val else
            if wrap then max else min in
        Param.set param new_val
      in
      let label = "  "^ i2s selected
      and arrow_w = 10 in
      [ button label ~on_click ~x ~y ~width ~height ;
        (if wrap || selected > min then
          text "‹" ~x ~y ~width:arrow_w ~height
        else group []) ;
        (if wrap || selected < max then
          text "›" ~x:(x + width - arrow_w) ~y ~width:arrow_w ~height
        else group []) ])

  let heightmap map ~x ~y ~width ~height =
    let polys = ref [] in
    for yi = 0 to Array.length map - 1 do
      let height = height / Array.length map in
      let y = y + yi * height in
      for xi = 0 to Array.length map.(yi) - 1 do
        let width = width / Array.length map.(yi) in
        let x = x + xi * width in
        let h = map.(yi).(xi) |> min 1. |> max 0. in
        let color = c h h h in
        let poly = Path.rect (pi x y) (pi (x + width) (y + height)) |>
                   Algo.poly_of_path ~res:K.one (* unused *) in
        polys := (color, [ poly ]) :: !polys
      done
    done ;
    Ogli_render.shape_of_polys !polys Point.origin []
end

(* Reading the CSV file providing the training data *)

module CSV =
struct
  type t =
    { columns : string array ;
      extremums : (float * float) array ;
      lines : float array array ;
      predictions : float option array array ;
      shuffling : int array ;
      mutable shuffling_idx : int ;
      mutable min_tot_err : float array (* for just 2 values *);
      mutable idx : int (* current line to read values from *) ;
      name : string }

  let random_columns nb_cols =
    Array.init nb_cols (fun c -> "column "^ i2s c)

  let make columns lines name =
    let nb_lines = Array.length lines
    and nb_cols = Array.length columns  in
    let extremums = Array.map (fun v -> v, v) lines.(0) in
    for l = 1 to nb_lines - 1 do
      for c = 0 to Array.length lines.(l) - 1 do
        extremums.(c) <- min (fst extremums.(c)) lines.(l).(c),
                         max (snd extremums.(c)) lines.(l).(c)
      done
    done ;
    Format.printf "CSV %s columns: @[" name ;
    for c = 0 to nb_cols - 1 do
      let min, max = extremums.(c) in
      Format.printf "%s:%f->%f@;" columns.(c) min max ;
      if min = max then Format.printf "@,XXX NO VARIATION XXX@,"
    done ;
    Format.printf "@]@." ;
    let predictions =
      Array.init nb_lines (fun _ -> Array.create nb_cols None)
    and shuffling = Array.init nb_lines (fun i -> i) in
    Array.shuffle shuffling ;
    { columns ; lines ; predictions ; min_tot_err = [| max_float ; max_float |] ;
      name ; extremums ; shuffling ; shuffling_idx = 0 ; idx = shuffling.(0) }

  let reset csv =
    for l = 0 to Array.length csv.predictions - 1 do
      for c = 0 to Array.length csv.predictions.(l) - 1 do
        csv.predictions.(l).(c) <- None
      done
    done ;
    csv.min_tot_err <- [| max_float ; max_float |]

  let random_walk () =
    let nb_cols = 4 + Random.int 4
    and nb_lines = 40 + Random.int 100 in
    let columns = random_columns nb_cols in
    let lines =
      Array.init nb_lines (fun _ ->
        Array.init nb_cols (fun _ -> Random.float 1.)) in
    for l = 1 to nb_lines - 1 do
      for c = 0 to nb_cols - 1 do
        lines.(l).(c) <- lines.(l-1).(c) +. lines.(l).(c)
      done
    done ;
    let name = Printf.sprintf "random_%dx%d" nb_cols nb_lines in
    make columns lines name

  let columns_of_header h =
    String.lchop h |>
    String.nsplit ~by:"," |>
    Array.of_list

  (* Append various times to the columns *)
  let append_times field_num to_secs columns lines =
    let open Unix in
    let infos =
      [| "seconds", (fun tm -> tm.tm_sec) ;
         "minutes", (fun tm -> tm.tm_min) ;
         "hour", (fun tm -> tm.tm_hour) ;
         "day of month", (fun tm -> tm.tm_mday) ;
         "month", (fun tm -> tm.tm_mon + 1) ;
         "year", (fun tm -> tm.tm_year + 1900) ;
         "day of week", (fun tm -> tm.tm_wday) ;
         "day of year", (fun tm -> tm.tm_yday + 1) ;
         "week-end", (fun tm -> if tm.tm_wday = 0 || tm.tm_wday = 6 then 1 else 0) ;
         "sunday", (fun tm -> if tm.tm_wday = 0 then 1 else 0) ;
         "monday", (fun tm -> if tm.tm_wday = 1 then 1 else 0) ;
         "tuesday", (fun tm -> if tm.tm_wday = 2 then 1 else 0) ;
         "wednesday", (fun tm -> if tm.tm_wday = 3 then 1 else 0) ;
         "thursday", (fun tm -> if tm.tm_wday = 4 then 1 else 0) ;
         "friday", (fun tm -> if tm.tm_wday = 5 then 1 else 0) ;
         "saturday", (fun tm -> if tm.tm_wday = 6 then 1 else 0) |] in
    let columns =
      Array.map fst infos |>
      Array.append columns
    and lines =
      Array.map (fun line ->
        let tm = line.(field_num) *. to_secs |>
                 localtime in
        Array.map (fun (_, f) -> i2f (f tm)) infos |>
        Array.append line
        ) lines
    in
    columns, lines

  let load fname minutes_f seconds_f =
    let lines = File.lines_of fname |> Array.of_enum in
    if Array.length lines < 1 then invalid_arg "Empty CSV file" ;
    let nb_cols = String.nsplit ~by:"," lines.(0) |> List.length in
    let columns, first_line =
      if lines.(0).[0] = '#' then
        columns_of_header lines.(0), 1
      else
        random_columns nb_cols, 0 in
    let nb_lines = Array.length lines - first_line in
    let my_float_of_string s =
      try float_of_string s
      with Failure _ ->
        Format.printf "Error: cannot parse %S@." s ;
        0. in
    let lines =
      Array.init nb_lines (fun i ->
        let l = lines.(first_line + i) in
        String.nsplit ~by:"," l |>
        List.map my_float_of_string |>
        Array.of_list) in
    let columns, lines =
      if seconds_f >= 0 then append_times seconds_f 1. columns lines else
      if minutes_f >= 0 then append_times minutes_f 60. columns lines else
      columns, lines in
    make columns lines fname

  let rec next min_line csv =
    assert (min_line < Array.length csv.lines) ;
    (* Iter over each line before looping *)
    csv.shuffling_idx <- csv.shuffling_idx + 1 ;
    if csv.shuffling_idx >= Array.length csv.shuffling then (
      Array.shuffle csv.shuffling ;
      csv.shuffling_idx <- 0 ;
    ) ;
    let l = csv.shuffling.(csv.shuffling_idx) in
    if l < min_line then next min_line csv else
    csv.idx <- l

  let get_value csv col lag avg idx =
    if avg = 0 then (* fast path *)
      let idx = Lr44.pos_mod (idx - lag) (Array.length csv.lines) in
      csv.lines.(idx).(col)
    else
      let rec sum i s =
        if i < 0 then s else
          let idx = Lr44.pos_mod (idx - lag -i) (Array.length csv.lines) in
          sum (i - 1) (s +. csv.lines.(idx).(col)) in
      sum avg 0. /. i2f avg

  let predict csv lag col v =
    let idx = Lr44.pos_mod (csv.idx - lag) (Array.length csv.lines) in
    csv.predictions.(idx).(col) <- Some v

  let render ~col csv test_set_sz min_line ~x ~y ~width ~height =
    (* Draw a simple bar for each prediction/value. When we have several
     * values per pixel draw the bounding box. *)
    let vmi, vma = csv.extremums.(col) in
    (* Enlarge to account for bad predictions: *)
    let vmi, vma =
      let l = (vma -. vmi) *. 0.1 in
      vmi -. l, vma +. l in
    let y_ratio = i2f (height - 1) /. (vma -. vmi) in
    let x_of_line l =
      let nb_lines = Array.length csv.lines - min_line - 1 in
      x + ((i2f l *. i2f (width - 1) /. i2f nb_lines) |> f2i)
    and y_of_v v =
      y + ((v -. vmi) *. y_ratio |> f2i |> max 0 |> min (height - 2*Layout.text_line_height - 1))
    in
    let rec loop naive_err tot_err skipped polys last_x y_lims limit line =
      if line >= limit then naive_err, tot_err, skipped, polys else
      let naive_err =
        if line = 0 then naive_err else
          naive_err +. abs_float (csv.lines.(line).(col) -. csv.lines.(line-1).(col)) in
      match csv.predictions.(line).(col) with
      | None ->
          (* Just skip *)
          loop naive_err tot_err (skipped + 1) polys last_x y_lims limit (line + 1)
      | Some v0 ->
        let v1 = csv.lines.(line).(col) in
        let v0, v1 =
          if v0 <= v1 then v0, v1 else v1, v0 in
        let tot_err = tot_err +. v1 -. v0 in
        let x = x_of_line line in
        (match y_lims with
        | None ->
            (* Start a new box *)
            loop naive_err tot_err skipped polys x (Some (v0, v1)) limit (line + 1)
        | Some (pv0, pv1) ->
            if x = last_x then
              (* If we are on the same x then grow the box: *)
              loop naive_err tot_err skipped polys last_x (Some (min v0 pv0, max v1 pv1)) limit (line + 1)
            else (
              (* Otherwise draw the prev box and start a new one: *)
              let y0 = y_of_v pv0 and y1 = y_of_v pv1 in
              let (++) = Poly.insert_after in
              let p =
                Poly.empty ++
                pi last_x y0 ++
                pi x y0 ++
                pi x y1 ++
                pi last_x y1 in
              let polys = p :: polys in
              loop naive_err tot_err skipped polys x (Some (v0, v1)) limit (line + 1)))
    in
    let partial_graph start stop color set_name set_num =
      let naive_err, tot_err, skipped, polys = loop 0. 0. 0 [] ~-1 None stop start in
      let err_ratio err nbp = err /. i2f nbp in
      let title = set_name ^": " in
      let title = title ^ (
        let set_size = stop - start in
        if set_size = 0 then "empty set" else (
          ( (* Wait before we have visited at least half the data before recording min error: *)
            if skipped > Array.length csv.lines / 2 then "" else
              let e = err_ratio tot_err (set_size - skipped) in
              if e < csv.min_tot_err.(set_num) then csv.min_tot_err.(set_num) <- e ;
              "err="^ f2s e ^" min="^ f2s csv.min_tot_err.(set_num)
          ) ^" naïve="^ f2s (err_ratio naive_err set_size))) in
      group [
        Ogli_render.shape_of_polys (List.map (fun p -> color, [ p ]) polys) Point.origin [] ;
        Widget.text title ~x:(x_of_line start) ~y:(y + height - (set_num + 1) * Layout.text_line_height) ~width ~height:Layout.text_line_height ]
    in
    group [
      partial_graph min_line (min_line + test_set_sz) (c 0.4 0.75 0.9) "test" 0 ;
      partial_graph (min_line + test_set_sz) (Array.length csv.lines) (c 0.7 0.7 0.7) "train" 1 ]
end

let msaa = ref true
let double_buffer = ref false
let test_name = ref ""

let csv =
  let csv_file = ref ""
  and seconds_field = ref ~-1
  and minutes_field = ref ~-1
  in
  Arg.(parse [
    "--csv", Set_string csv_file, "CSV file to use" ;
    "--timestamp", Set_int seconds_field, "Use this field number (starting at 0) as source of EPOCH seconds" ;
    "--minutes", Set_int minutes_field, "Use this field number (starting at 0) as source of EPOCH minutes" ;
    "--name", Set_string test_name, "Name to use for saving/loading the results (default to CSV file name)" ;
    "--no-msaa", Clear msaa, "Disable MSAA" ;
    "--double-buffer", Set double_buffer, "Force double-buffer" ]
    (fun str -> raise (Bad ("Unknown argument "^ str)))
    "Neural Network Playground for timeseries prediction") ;
  if !seconds_field >= 0 && !minutes_field >= 0 then (
    Printf.eprintf "You cannot use both minutes and timestamp\n" ;
    exit 1) ;
  if !csv_file = "" then CSV.random_walk ()
  else CSV.load !csv_file !minutes_field !seconds_field

(* List of options suitable for select widgets: *)
let csv_fields =
  Array.fold_lefti (fun lst i v ->
    let mi, ma = csv.extremums.(i) in
    if mi < ma then (i, v) :: lst else lst
  ) [] csv.columns |>
  Array.of_list |>
  Array.rev

(* Simple graphs to display evolution of values: *)

module Graph =
struct
  type t =
    { values : float array ;
      title : string ;
      mutable last_val : float ;
      mutable next_idx : int ;
      mutable max : float ;
      mutable min : float ;
      mutable accum : float ;
      mutable nb_accums : int ;
      mutable nb_shrinks : int }

  let compress g =
    let len = Array.length g.values in
    for i = 0 to len/2 - 1 do
      g.values.(i) <- max g.values.(i*2) g.values.(i*2+1)
    done ;
    g.next_idx <- len/2 ;
    g.nb_shrinks <- g.nb_shrinks + 1

  let push g v =
    g.last_val <- v ;
    if g.next_idx >= Array.length g.values then compress g ;
    g.min <- min g.min v ;
    g.max <- max g.max v ;
    g.accum <- g.accum +. v ;
    g.nb_accums <- g.nb_accums + 1 ;
    if g.nb_accums >= 1 lsl g.nb_shrinks then (
      g.values.(g.next_idx) <- g.accum  /. i2f g.nb_accums ;
      g.next_idx <- g.next_idx + 1 ;
      g.accum <- 0. ;
      g.nb_accums <- 0)

  let make title len =
    { values = Array.make len 0. ; title ;
      last_val = 0. ; next_idx = 0 ; max = 0. ; min = 0. ;
      accum = 0. ; nb_accums = 0 ; nb_shrinks = 0 }

  let reset g =
    for i = 0 to Array.length g.values - 1 do
      g.values.(i) <- 0. ;
    done ;
    g.last_val <- 0. ;
    g.next_idx <- 0 ;
    g.nb_shrinks <- 0 ;
    g.nb_accums <- 0 ;
    g.accum <- 0. ;
    g.max <- 0. ;
    g.min <- 0.

  let reset_param p =
    reset p.Param.value ;
    Param.change p

  let render g ~x ~y ~width ~height =
    let len = Array.length g.values in
    let title =
      let txt = g.title ^":"^ f2s g.last_val in
      Widget.text txt ~x ~y:(y + height - Layout.text_line_height) ~width ~height:Layout.text_line_height in
    let subtitle =
      if g.nb_shrinks > 0 then
        let txt = "Shrunk "^ i2s g.nb_shrinks ^" times" in
        Widget.text txt ~x ~y:(y + height - 2 * Layout.text_line_height) ~width ~height:Layout.text_line_height
      else
        group [] in
    let bg = Widget.rect (c 0.9 0.9 0.9) ~x ~y ~width ~height in
    let v2y v = if g.max = g.min then 0. else
      let v' = (v -. g.min) /. (g.max -. g.min) in
      v' ** 0.1 *. i2f height in
    let t2x t =
      i2f width *. i2f t /. i2f len in
    let axis =
      let yy = v2y 0. in
      if yy >= 0. && f2i yy < height then
        Widget.rect C.black ~x ~y:(y + f2i yy) ~width ~height:1
      else group [] in
    let tick = group [] (* TODO *) in
    let plot =
      let point i =
        let v = g.values.(i) in
        pf (t2x i) (v2y v) in
      let rec append i incr limit poly =
        if i = limit then poly else
          let poly = Poly.insert_after poly (point i) in
          append (i + incr) incr limit poly
      in
      if g.next_idx > 0 then
        let poly = Poly.insert_after Poly.empty (pf (t2x 0) (v2y 0.)) in
        let poly = Poly.insert_after poly (pf (t2x (g.next_idx - 1)) (v2y 0.)) in
        let poly = append (g.next_idx - 1) ~-1 ~-1 poly in
        try
          Ogli_render.shape_of_polys [ c 0.6 0.6 0.6, [ poly ] ] (pi x y) []
        with e ->
          Format.printf "Cannot render graph as poly@ %a@ because of %s at %s@."
            Poly.print poly
            (Printexc.to_string e)
            (Printexc.get_backtrace ()) ;
          group []
      else
        group []
    in
    group [ bg ; plot ; axis ; tick ; title ; subtitle ]
end

(* Neurons & Neural Net (UI only, see neuron.ml for actual neurons simulation *)

module Neuron =
struct
  (* Input and output neurons position and parameters depend on inputs/outputs controls (for instance, an input for recent values can have
   * from 1 to N input neurons, changing when the input config is changed. so we recompute the list of input/outputs neurons whenever those
   * controllers are updated, conservatively. Neurons position is allowed to change while the neuron id and internal states do not change,
   * though. *)

  type layer = Input | Hidden | Output [@@ppp PPP_JSON]

  let io_map_sz = 20

  type dendrit =
    { source : t ;
      mutable weight : float ;
      mutable prev_gradient : float ;
      mutable prev_weight_adj : float (* for momentum *) ;
      mutable learn_gain : float (* When using individual learn rates *) }
  and axon =
    { dest : t ;
      dendrit : dendrit }
  and t =
    { id : int ; (* TODO: We could probably get away with physical equality for neurons. *)
      layer : layer ;
      mutable output : float ;
      mutable dE_dOutput : float ;
      mutable func : transfer ;
      mutable io_key : int ; (* either input number of layer number *)
      mutable dendrits : dendrit list ;
      mutable axons : axon list ;
      mutable selected : bool ;
      (* relative to the net window (right column) so it eventually depends
       * on screen size *)
      position : Point.t Param.t ;
      (* An array of io_map_sz ^2 values output by this neuron for a range
       * of input values. *)
      io_map : float array array }

  let make_position id pos =
    Param.make ("position of neuron#"^ i2s id) pos

  let make_io_map () =
    Array.init io_map_sz (fun _ -> Array.make io_map_sz 0.)

  let io_maps_update = Param.make "io_map should be refreshed" ()

  type ser_dendrit =
    { ser_source : int ; ser_weight : float ;
      ser_prev_weight_adj : float [@ppp_default 0.] ;
      ser_learn_gain : float [@ppp_default 1.] } [@@ppp PPP_JSON]
  type ser_axon =
    { ser_dest : int ; ser_dendrit : int } [@@ppp PPP_JSON]
  type serialized_neuron =
    { ser_id : int ; ser_layer : layer ;
      ser_output : float ; ser_func : transfer ;
      ser_io_key : int ; ser_dendrits : ser_dendrit list ;
      ser_axons : ser_axon list } [@@ppp PPP_JSON]

  let serialize_dendrit d =
    { ser_source = d.source.id ;
      ser_weight = d.weight ;
      ser_prev_weight_adj = d.prev_weight_adj ;
      ser_learn_gain = d.learn_gain }
  let serialize_axon n a =
    { ser_dest = a.dest.id ;
      ser_dendrit = List.findi (fun _ d -> d.source == n) a.dest.dendrits |> fst }
  let serialize n =
    { ser_id = n.id ; ser_layer = n.layer ; ser_output = n.output ;
      ser_func = n.func ; ser_io_key = n.io_key ;
      ser_dendrits = List.map serialize_dendrit n.dendrits ;
      ser_axons = List.map (serialize_axon n) n.axons }
  (* We must unserialize all neurons first, then all dendrits, then all axons. *)
  let unserialize_dendrit neurons s =
    { weight = s.ser_weight ; prev_weight_adj = s.ser_prev_weight_adj ;
      prev_gradient = 0. (* restart from start of mini-batch *) ;
      learn_gain = s.ser_learn_gain ;
      source = Array.find (fun n -> n.id = s.ser_source) neurons }
  let unserialize_axon neurons s =
    let dest = Array.find (fun n -> n.id = s.ser_dest) neurons in
    { dest ;
      dendrit = List.nth dest.dendrits s.ser_dendrit }
  let unserialize0 s =
    { id = s.ser_id ; layer = s.ser_layer ; output = s.ser_output ;
      dE_dOutput = 0. ; func = s.ser_func ; io_key = s.ser_io_key ;
      dendrits = [] ; axons = [] ; selected = false ;
      position = make_position s.ser_id Point.origin ;
      io_map = make_io_map () }
  let unserialize_dendrits neurons n s =
    n.dendrits <- List.map (unserialize_dendrit neurons) s.ser_dendrits
  let unserialize_axons neurons n s =
    n.axons <- List.map (unserialize_axon neurons) s.ser_axons

  let random_weight () =
    let init_weight_amplitude = 1. in
    (Random.float 1. -. 0.5) *. init_weight_amplitude

  let dendrit source =
    { source ; weight = random_weight () ; prev_gradient = 0. ;
      prev_weight_adj = 0. ; learn_gain = 1. }

  let id_seq = ref 0
  let make io_key layer position =
    incr id_seq ;
    { id = !id_seq ; dendrits = [] ; axons = [] ; io_key ; layer ;
      selected = false ; position = make_position !id_seq position ;
      func = Sigmoid ; output = 0. ; dE_dOutput = 0. ;
      io_map = make_io_map () }

  let reset n =
    List.iter (fun d ->
      d.weight <- random_weight () ;
      d.prev_weight_adj <- 0. ;
      d.prev_gradient <- 0.
    ) n.dendrits ;
    n.dE_dOutput <- 0. ;
    Array.iter (fun y -> Array.iteri (fun i _ -> y.(i) <- 0.) y) n.io_map

  (* List of neurons, that we recompute (conservatively) whenever
   * the net is changed. *)
  let neurons = Param.make ~on_update:[ set_need_save ] "neurons" [||]
  (* we cannot really ~on_update:set_need_save because we update neurons for
   * non-savable changes such as selection changes *)

  (* Sort according to layer for signal propagation *)
  let compare n1 n2 =
    let order_of_layer = function
      Input -> 0 | Hidden -> 1 | Output -> 2 in
    match Int.compare (order_of_layer n1.layer)
                      (order_of_layer n2.layer) with
    | 0 ->
        (match Int.compare n1.io_key n2.io_key with
        | 0 -> Int.compare n1.id n2.id
        | c -> c)
    | c -> c

  let sort_neurons = Array.fast_sort compare

  (* This changes whenever the selection changes *)
  let selection_generation = Param.make "selection generation" 0

  (* Also track if we have selected only one neuron: *)
  let selected = Param.make "selected neuron" None
  let () =
    Param.on_update selection_generation (fun () ->
      let single_selected =
        let rec loop c i =
          if i >= Array.length neurons.value then c
          else if neurons.value.(i).selected then
            if c <> ~-1 then ~-1 else loop i (i + 1)
          else
            loop c (i + 1) in
        loop ~-1 0 in
      Param.set selected (
        if single_selected >= 0 then
          Some neurons.value.(single_selected)
        else None))

  (* Return true if there was some change *)
  let iter_all f =
    for i = 0 to Array.length neurons.value - 1 do
      f neurons.value.(i)
    done

  let iter_only layer f =
    iter_all (fun n -> if n.layer = layer then f n)

  let iter_all_but layer f =
    iter_all (fun n -> if n.layer <> layer then f n)

  let iter_back_all f =
    for i = Array.length neurons.value - 1 downto 0 do
      f neurons.value.(i)
    done

  let iter_back_but layer f =
    iter_back_all (fun n -> if n.layer <> layer then f n)

  let fold_all f u =
    Array.fold_left f u neurons.value

  let fold_only layer f u =
    fold_all (fun u n -> if n.layer = layer then f u n else u) u

  let fold_all_but layer f u =
    fold_all (fun u n -> if n.layer = layer then u else f u n) u

  let disconnect ~from ~to_ =
    from.axons <-
      List.filter (fun a -> a.dest.id <> to_.id) from.axons ;
    from.dendrits <-
      List.filter (fun d -> d.source.id <> to_.id) from.dendrits

  let disconnect_from others from =
    List.iter (fun to_ -> disconnect ~from:from ~to_:to_) others

  (* Given the list of io controllers, compute the
   * new set of IO neurons (keeping as much as we can from the
   * previous ones. Called only when we are pretty sure something has changed
   * so that's OK to call Param.set every times: *)
  let rearrange_io_neurons layer controllers io_width =
    (* Prepare an empty result. *)
    let res = ref [] in
    (* Also we will remove neurons from the current list as we process the
     * neurons: *)
    let old =
      fold_only layer (fun s n -> Map.add n.io_key n s) Map.empty in
    let old = ref old in
    (* Process each neuron per controller. *)
    (* We need to take the controllers in order we display them for x
     * calculation: *)
    sort_io controllers |>
    List.fold_left (fun tot_width (io : io) ->
      let controller_id = io.id in
      let position =
        pi (tot_width + io_width / 2)
           (if layer = Output then Layout.neuron_radius
            else Layout.neural_net_height.value - Layout.neuron_radius) in
      let io_key = controller_id in
      (match Map.extract io_key !old with
      (* If absent, create a new one and add it to the result list. *)
      | exception Not_found ->
        let new_neuron = make io_key layer position in
        res := new_neuron :: !res
      (* If present, update its position, remove it from the old list and
       * add it to the result. *)
      | neuron, old' ->
        if not (Point.eq neuron.position.value position) then
          Param.set neuron.position position ;
        old := old' ;
        res := neuron :: !res) ;
      tot_width + io_width
    ) 0 |> ignore ;
    (* Rebuild the resulting neurons array: *)
    let new_neurons =
      Array.append
        (* All neurons from other layers: *)
        (Array.filter (fun n -> n.layer <> layer) neurons.value)
        (* The new neurons for that layer: *)
        (Array.of_list !res) in
    sort_neurons new_neurons ;
    Param.set neurons new_neurons ;
    (* Then delete all the dendrits and axons from any neuron
     * to anything still in the old map: *)
    iter_all (fun n ->
      n.dendrits <-
        List.filter (fun d ->
          (* Beware that io_key is not only unique within a layer: *)
          not (d.source.layer = layer && Map.mem d.source.io_key !old)
        ) n.dendrits ;
      n.axons <-
        List.filter (fun a ->
          not (a.dest.layer = layer && Map.mem a.dest.io_key !old)
        ) n.axons)

  (* Similarly, rearrange hidden neurons layers *)
  let rearrange_hidden_neurons () =
    (* Start by getting all the defined layers: *)
    let layers =
      fold_all (fun m n ->
        if n.layer <> Hidden then m else
        Map.modify_def (0, []) n.io_key
                       (fun (c, l) -> c+1, n::l) m
      ) Map.empty in
    let nb_layers = Map.cardinal layers in
    (* Reorder the layer depths: *)
    let dy = i2f Layout.neural_net_height.value /. i2f (nb_layers + 1) in
    let rec loop new_depth rem_layers =
      if not (Map.is_empty rem_layers) then
        let (_old_depth, (nb_neurons, neurons)), rem_layers =
          Map.pop_min_binding rem_layers in
        let y = i2f Layout.neural_net_height.value -. i2f (new_depth + 1) *. dy in
        (* TODO: offset hidden layers according to where their cnx come from/goes to *)
        let neurons = List.fast_sort (fun n1 n2 -> Int.compare n1.id n2.id) neurons in
        let xmima =
          let extr xmima n =
            let x = K.to_float n.position.value.(0) in
            match xmima with
            | None -> Some (x, x)
            | Some (xmi, xma) ->
                Some (min xmi x, max xma x) in
          List.fold_left (fun xmima n ->
            (*let xmima =*) List.fold_left (fun xmima d ->
              extr xmima d.source
            ) xmima n.dendrits (*in
            List.fold_left (fun xmima a ->
              extr xmima a.dest
            ) xmima n.axons*)
          ) None neurons in
        let xmi, xma =
          xmima |? (0., i2f Layout.(screen_width.value - control_column_width.value)) in
        let dx = (xma -. xmi) /. i2f (nb_neurons + 1) in
        List.iteri (fun i n ->
          n.io_key <- new_depth ;
          let disp = 7. and a = i2f n.id in
          let position = pf (xmi +. i2f (i + 1) *. dx +. disp *. cos a)
                            (y +. disp *. sin a) in
          if not (Point.eq position n.position.value) then
            Param.set n.position position
        ) neurons ;
        loop (new_depth + 1) rem_layers
    in
    loop 0 layers

  let rearrange_input_neurons () = rearrange_io_neurons Input inputs.value Layout.input_width.value
  let rearrange_output_neurons () = rearrange_io_neurons Output outputs.value Layout.output_width.value
  let rearrange_all_neurons () =
    (* Must be input then hidden then output, so that to layout a layers we
     * can use positions of previous one: *)
    rearrange_input_neurons () ;
    rearrange_hidden_neurons () ;
    rearrange_output_neurons ()

  let () =
    Param.on_update Layout.neural_net_height rearrange_all_neurons ;
    Param.on_update Layout.screen_width rearrange_all_neurons ;
    Param.on_update Layout.control_column_width rearrange_all_neurons ;
    Param.on_update inputs rearrange_all_neurons ;
    Param.on_update Layout.input_width rearrange_all_neurons ;
    Param.on_update outputs rearrange_output_neurons ;
    Param.on_update Layout.output_width rearrange_output_neurons

  let unselect_all () =
    iter_all (fun n -> n.selected <- false) ;
    Param.incr selection_generation ;
    Param.change neurons

  let hovered = Param.make "hovered neuron" None

  let touch_hovered () = (* Used to redraw the neuron details *)
    Option.may (fun _ -> Param.change hovered) hovered.value

  let neuron_shape, neuron_bbox =
    let circle =
      Path.circle (K.of_int Layout.neuron_radius) |>
      Algo.poly_of_path ~res:K.one in
    Ogli_render.of_polys [ circle ],
    Algo.bbox [ circle ]

  let render t ~x ~y =
    let orig = pi x y in
    fun_of hovered (fun hvd -> [ (* Because we change the color according to this *)
      fun_of t.position (fun position -> [
        let is_hovered =
          match hvd with
          | Some ht when ht.id = t.id -> true
          | _ -> false in
        let color =
          if t.selected then c 0.7 0.7 0.7
                        else c 0.4 0.4 0.4 in
        if is_hovered then color.(1) <- 0.9 ;
        let render = neuron_shape ~color in
        let on_hover _ =
          if not is_hovered then
            Param.set hovered (Some t)
        and on_click shifted _ =
          if shifted then (
            t.selected <- not t.selected
          ) else (
            unselect_all () ;
            t.selected <- true) ;
          Param.change neurons ;
          Param.incr selection_generation
        in
        let s = Ogli_shape.make ~on_hover ~on_click
                  ~position:Point.Infix.(position +~ orig) render neuron_bbox in
        Ogli_view.shape s [] ]) ])

  let render_dendrit weight ~from ~to_ ~x ~y =
    let open Point.Infix in
    let orig = pi x y in
    fun_of from.position (fun from_pos -> [
      fun_of to_.position (fun to_pos -> [
        let src = from_pos +~ orig
        and dst = to_pos +~ orig in
        let dy = K.half (K.sub dst.(1) src.(1)) in
        let ctrl = p K.zero dy in
        let dendrit_width = min 10. (0.75 +. abs_float weight) in
        let poly = Path.start src |>
                   Path.bezier_to dst
                     [ src +~ ctrl ; dst -~ ctrl ] |>
                   Algo.line_of_path ~res:K.one ~width:(K.of_float dendrit_width) in
        fun_of hovered (fun hvd -> [
          let color =
            if weight >= 0. then c 0.3 0.2 0.95
                            else c 0.9 0.2 0.3 in
          let color =
            match hvd with
            | Some n when n.id = from.id || n.id = to_.id ->
                C.add color (c 0.05 0.8 0.05)
            | _ -> color in
          Ogli_render.shape_of_polys [ color, [ poly ] ] Point.origin [] ]) ]) ])

  let connect_pair from to_ =
    let already_connected =
      List.exists (fun d -> d.source == from)
        to_.dendrits in
    if not already_connected then (
      let new_d = dendrit from in
      to_.dendrits <- new_d :: to_.dendrits ;
      from.axons <- { dest = to_ ; dendrit = new_d } :: from.axons ;
    )

  let connect_from_to from to_ =
    List.iter (fun from ->
      List.iter (connect_pair from) to_
    ) from

  let add_how_many = Param.make "#neurons to be added" 1
  let add_where = Param.make "where to add next neurons" 0

  let render_layer_adder ~x ~y ~width ~height =
    ignore width ;
    (* Just a line of buttons to add some neurons. *)
    fun_of neurons (fun ns -> [
      let min_layer, max_layer, empty =
        fold_only Hidden (fun (mi, ma, _) n ->
          min mi n.io_key,
          max ma n.io_key,
          false)
          (0, 0, true) in
      let min_layer, max_layer =
        if empty then 0, 0 else
        min_layer - 1, max_layer + 1 in
      let add _ _ =
        let new_neurons =
          let new_nb_neurons =
            Array.length ns + add_how_many.value in
          Array.init new_nb_neurons (fun i ->
            if i < Array.length ns then ns.(i) else
            (* Positions will be rearranged in rearrange_hidden_neurons *)
            (* For hidden neurons controller_id is the layer depth: *)
            make add_where.value Hidden Point.origin) in
        sort_neurons new_neurons ;
        Param.incr add_where ;
        Param.set neurons new_neurons ;
        rearrange_hidden_neurons ()
      and add_bias _ _ =
        let bias = make min_layer Hidden Point.origin in
        bias.output <- 1. ;
        iter_all_but Input (connect_pair bias) ;
        let new_neurons = Array.append neurons.value [| bias |] in
        sort_neurons new_neurons ;
        Param.set neurons new_neurons ;
        rearrange_hidden_neurons ()
      in
      fun_of Layout.control_column_width (fun control_width ->
        let add_w = 30 and add_label_w = 60
        and add_y = y + height - Layout.text_line_height in
        let add_sel_w = (control_width - add_w - add_label_w) / 3 in
        [ (* Buttons to add a new layer: *)
          Widget.text "Add" ~x ~y:add_y ~width:add_w ~height:Layout.text_line_height ;
          Widget.int_select add_how_many ~min:1 ~x:(x + add_w) ~y:add_y ~width:add_sel_w ~height:Layout.text_line_height ;
          Widget.text "in layer" ~x:(x + add_w + add_sel_w) ~y:add_y ~width:add_label_w ~height:Layout.text_line_height ;
          Widget.int_select add_where ~min:min_layer ~max:max_layer ~x:(x + add_w + add_sel_w + add_label_w) ~y:add_y ~width:add_sel_w ~height:Layout.text_line_height ;
          Widget.button "ok" ~on_click:add ~x:(x + add_w + add_sel_w + add_label_w + add_sel_w) ~y:add_y ~width:add_sel_w ~height:Layout.text_line_height ;
          Widget.button "Add a bias" ~on_click:add_bias ~x ~y:(add_y - Layout.text_line_height) ~width ~height:Layout.text_line_height ;
          (* Now buttons to connect/delete the selection: *)
          fun_of selection_generation (fun _ ->
            (* If we have input and hiddens, connect all inputs to all hiddens;
             * if we have hiddens and output, connect all hiddens to all outputs;
             * if we have only hiddens, connect them fully according to their Y;
             * otherwise, we do nothing. *)
            let sel_ins, sel_hids, sel_outs, sels =
              fold_all (fun (ins, hids, outs, all as lsts) n ->
                if not n.selected then lsts else
                  match n.layer with
                  | Input -> n :: ins, hids, outs, n :: all
                  | Hidden -> ins, n :: hids, outs, n :: all
                  | Output -> ins, hids, n :: outs, n :: all
              ) ([], [], [], []) in
            let connect from to_ _ _ =
              connect_from_to from to_ ;
              Param.none box_selection ;
              rearrange_hidden_neurons () ;
              unselect_all ()
            and connect_per_layers sel _ _ =
              (* group by layer *)
              let by_y =
                List.fold_left (fun s n ->
                  Map.modify_opt n.io_key (function
                    | None -> Some [ n ]
                    | Some lst -> Some (n :: lst)) s
                ) Map.empty sel in
              let keys = Map.keys by_y |> Array.of_enum in
              Array.fast_sort Int.compare keys ;
              for i = 0 to Array.length keys - 2 do
                let from = Map.find keys.(i) by_y
                and to_ = Map.find keys.(i + 1) by_y in
                connect_from_to from to_
              done ;
              rearrange_hidden_neurons () ;
              Param.none box_selection ;
              unselect_all ()
            and connect_fully sel _ _ =
              List.iter (fun from ->
                List.iter (fun to_ ->
                  if from.io_key < to_.io_key then
                    connect_from_to [ from ] [ to_ ]
                ) sel
              ) sel ;
              rearrange_hidden_neurons () ;
              Param.none box_selection ;
              unselect_all ()
            and disconnect_all sels _ _ =
              List.iter (fun n -> disconnect_from sels n) sels ;
              rearrange_hidden_neurons () ;
              Param.none box_selection ;
              unselect_all ()
            and del shifted _ = (* Delete all hidden neurons that are selected *)
              if shifted then (
                let to_del, to_keep =
                  Array.partition (fun n -> n.layer = Hidden && n.selected) ns in
                let to_del =
                  Array.fold_left (fun s n -> n :: s) [] to_del in
                sort_neurons to_keep ;
                Param.set neurons to_keep ;
                iter_all (disconnect_from to_del) ;
                Param.incr selection_generation ;
                rearrange_hidden_neurons ())
            and set_transfer transfer _ _ =
              List.iter (fun n -> n.func <- transfer) sels ;
              Param.change hovered
            and set_bias v _ _ =
              List.iter (fun n -> n.output <- v) sels ;
              Param.change hovered
            in
            let w = 40 in
            let lab_w = control_width - 3 * w in
            [ (* Connect *)
              (let on_click =
                if sel_ins <> [] && sel_hids <> [] && sel_outs =  [] then Some (connect sel_ins sel_hids) else
                if sel_ins =  [] && sel_hids <> [] && sel_outs <> [] then Some (connect sel_hids sel_outs) else
                if sel_ins <> [] && sel_hids =  [] && sel_outs <> [] then Some (connect sel_ins sel_outs) else
                None in
              if on_click = None then group [] else
                Widget.button "Connect" ?on_click ~x ~y:(add_y - 1 * Layout.text_line_height) ~width:control_width ~height:Layout.text_line_height) ;
              if sel_ins <> [] || sel_hids = [] || sel_outs <> [] then group [] else group [
                Widget.button "Connect layers" ~on_click:(connect_per_layers sel_hids) ~x ~y:(add_y - 1 * Layout.text_line_height) ~width:(control_width/2) ~height:Layout.text_line_height ;
                Widget.button "Connect all" ~on_click:(connect_fully sel_hids) ~x:(control_width/2) ~y:(add_y - 1 * Layout.text_line_height) ~width:(control_width/2) ~height:Layout.text_line_height ] ;
              (* Disconnect *)
              (if sels = [] then group [] else
                Widget.button "Disconnect" ~on_click:(disconnect_all sels) ~x ~y:(add_y - 2 * Layout.text_line_height) ~width:control_width ~height:Layout.text_line_height) ;
              (* Set transfer function *)
              (if sels = [] then group [] else group [
                Widget.text "Transfer:" ~x ~y:(add_y - 3 * Layout.text_line_height) ~width:lab_w ~height:Layout.text_line_height ;
                Widget.button "sig." ~on_click:(set_transfer Sigmoid) ~x:(lab_w + 0 * w) ~y:(add_y - 3 * Layout.text_line_height) ~width:w ~height:Layout.text_line_height ;
                Widget.button "tan." ~on_click:(set_transfer TanHyp) ~x:(lab_w + 1 * w) ~y:(add_y - 3 * Layout.text_line_height) ~width:w ~height:Layout.text_line_height ;
                Widget.button "lin." ~on_click:(set_transfer Linear) ~x:(lab_w + 2 * w) ~y:(add_y - 3 * Layout.text_line_height) ~width:w ~height:Layout.text_line_height ]) ;
              (* Also offer to set a bias (for hidden neurons only) *)
              (if sel_ins <> [] || sel_outs <> [] || sel_hids == []
                then group [] else group [
                Widget.text "Output:" ~x ~y:(add_y - 4 * Layout.text_line_height) ~width:lab_w ~height:Layout.text_line_height ;
                Widget.button "+1" ~on_click:(set_bias 1.) ~x:(lab_w + 0 * w) ~y:(add_y - 4 * Layout.text_line_height) ~width:w ~height:Layout.text_line_height ;
                Widget.button " 0" ~on_click:(set_bias 0.) ~x:(lab_w + 1 * w) ~y:(add_y - 4 * Layout.text_line_height) ~width:w ~height:Layout.text_line_height ;
                Widget.button "-1" ~on_click:(set_bias ~-.1.) ~x:(lab_w + 2 * w) ~y:(add_y - 4 * Layout.text_line_height) ~width:w ~height:Layout.text_line_height ]) ;
              (* Delete *)
              (if sel_hids = [] then group [] else
                Widget.button "Delete!" ~on_click:del ~x ~y:(add_y - 5 * Layout.text_line_height) ~width:control_width ~height:Layout.text_line_height) ]) ]) ])

  (* Display a "cursor" where we are about to ad neurons: *)
  let render_adder_line ~x ~y ~width ~height =
    fun_of add_where (fun layer -> [
      fun_of neurons (fun _ -> (* so that we update this bar when the net is changed *)
        (* Slow but robust: look for the Y coordinates just above/below that layer: *)
        let y_above, y_below =
          fold_only Hidden (fun (y_above, y_below as prev) n ->
            if n.io_key < layer then
              min y_above (K.to_int n.position.value.(1)), y_below
            else if n.io_key > layer then
              y_above, max y_below (K.to_int n.position.value.(1))
            else prev
          ) (height, 0) in
        assert (y_above >= y_below) ;
        let y_avg = (y_above + y_below) / 2 in
        let height = 10 in
        [ Widget.rect (c 0.95 0.94 1.) ~x ~y:(y + y_avg - height/2) ~width ~height ]) ])

  (* Dendrits depends on all the lists: *)
  let render_dendrits ~x ~y =
    fun_of neurons (fun _ ->
      fold_all (fun lst neuron ->
        List.fold_left (fun lst d ->
          render_dendrit d.weight ~from:d.source ~to_:neuron ~x ~y :: lst
        ) lst neuron.dendrits) [])

  (* coordinates are relative to the neuralnet window *)
  let render_neurons ~x ~y =
    fun_of neurons (fun _ ->
      fold_all (fun res neuron ->
        render neuron ~x ~y :: res) [])

  let render_csv_value n ~x ~y ~width ~height =
    match n.layer with
    | Input ->
      fun_of inputs (fun ios ->
        let io = find_io ios n.io_key in
        (* TODO: fun_of io.col? *)
        let txt = csv.columns.(io.col.value) ^": "^ f2s io.csv_values.(csv.idx) in
        [ Widget.text txt ~x ~y ~width ~height ])
    | Output ->
      fun_of outputs (fun ios ->
        let io = find_io ios n.io_key in
        (* TODO: fun_of io.col? *)
        let t' = scale_output_rev n.func io.diff_extremums n.output in
        (* now we have the diff back to CSV proportions. Compute the actual value: *)
        let undiffed = t' +. io.csv_values.(csv.idx - 1) in
        let txt = f2s undiffed ^" for "^ f2s io.csv_values.(csv.idx)  in
        [ Widget.text txt ~x ~y ~width ~height ])
    | Hidden -> group []

  let input_from_csv () =
    iter_only Input (fun n ->
      let io = find_io inputs.value n.io_key in
      let x = io.csv_values.(csv.idx)
      and extr = csv.extremums.(io.col.value) in
      let o = scale_input n.func extr x in
      (* Note that we do *not* go through the transfer function itself.
       * We just use its type to scale. If we did, then for sigmoids the
       * output would be 0..1 which would be biased right from the start.
       * So better not.
       * Therefore, set the transfer function of the input neurons just
       * to control the scaling. If you mix different transfer function
       * in your network you are on your own. *)
      n.output <- o)

  let forward_propagation () =
    iter_all (fun n ->
      (* If there is no dendrit keep current output (for biases and input neurons) *)
      if n.dendrits <> [] then
        n.output <-
          List.fold_left (fun s d ->
            s +. d.source.output *. d.weight
          ) 0. n.dendrits |>
          transfer n.func)

  let selected_for_axis = Param.make "selected IO for IO map" (~-1, ~-1)
  (* Make sure we have selected some axes as soon as we have inputs,
   * and that we do not use deleted inputs: *)
  let () = Param.on_update inputs (fun () ->
    let ix, iy = selected_for_axis.value
    and cx, cy = (* candidate values *)
      match List.rev inputs.value with
      | [] -> ~-1, ~-1
      | [ i ] -> i.id, i.id
      | i1 :: i2 :: _ -> i1.id, i2.id
    and valid id =
      id >= 0 && List.exists (fun (i : io) -> i.id = id) inputs.value
    in
    let ix = if valid ix then ix else cx
    and iy = if valid iy then iy else
             if ix <> cy then cy else cx in
    if (ix, iy) <> selected_for_axis.value then
      Param.set selected_for_axis (ix, iy))

  let refresh_io_map () =
    (* Grab the inputs of interest: *)
    let idx, idy = selected_for_axis.value in
    (* Now grab the input neurons, and set all other input neurons
     * to 0: *)
    match fold_only Input (fun (nx, ny as prev) n ->
        if n.io_key <> idx && n.io_key <> idy then (
          n.output <- 0. ;
          prev
        ) else (
          (if n.io_key = idx then Some n else nx),
          (if n.io_key = idy then Some n else ny))
      ) (None, None) with
    | None, _ | _, None -> ()
    | Some nx, Some ny ->
      (* Now iter from one extremum to the other for inx and iny: *)
      let default_range = ~-.1., 1. in
      let miny, maxy = x_range ny.func |? default_range  (* yes x_range (should be input_range) *)
      and minx, maxx = x_range nx.func |? default_range  in
      for y = 0 to io_map_sz - 1 do
        ny.output <-
          miny +. (maxy -. miny) *. i2f y /. i2f (io_map_sz - 1) ;
        for x = 0 to io_map_sz - 1 do
          nx.output <-
            minx +. (maxx -. minx) *. i2f x /. i2f (io_map_sz - 1) ;
          (* Propagate *)
          forward_propagation () ;
          (* Record the value for all neurons *)
          iter_all (fun n ->
            let vmin, vmax = y_range n.func |? default_range in
            n.io_map.(y).(x) <- (n.output -. vmin) /. (vmax -. vmin))
        done
      done ;
      Param.change io_maps_update

  type direction = Vertical | Horizontal
  (* [width] and [height] are the coordinate of the whole surrounding rectangle
   * and therefore are the same for both directions. *)
  let render_io_choice but_width but_height ~dir ~x ~y ~width ~height =
    fun_of inputs (fun inputs -> [
      fun_of selected_for_axis (fun (sel_x, sel_y) ->
        let nb_inputs = List.length inputs in
        let dx, dy =
          match dir with
          | Vertical -> let d = height / (nb_inputs + 1) in 0, d
          | Horizontal -> let d = width / (nb_inputs + 1) in d, 0 in
        let rec loop buts x y i = function
          | [] -> buts
          | (input : io) :: inputs ->
              let selected = input.id =
                (match dir with Vertical -> sel_y | _ -> sel_x) in
              let on_click =
                if selected then None else Some (fun _ _ ->
                  (match dir with
                  | Vertical ->
                      Param.set selected_for_axis (sel_x, input.id)
                  | Horizontal ->
                      Param.set selected_for_axis (input.id, sel_y)) ;
                  refresh_io_map ()) in
              let but = Widget.button ~selected ?on_click (i2s i) ~x ~y ~width:but_width ~height:but_height in
              loop (but :: buts) (x + dx) (y - dy) (i + 1) inputs in
        let x = x + dx and y = y + nb_inputs * dy in
        loop [] x y 1 (List.rev inputs)) ])

  let render_io_map n ~x ~y ~width ~height =
    let but_width = 15 and but_height = Layout.text_line_height in
    group [
      render_io_choice but_width but_height ~dir:Vertical ~x ~y ~width ~height ;
      render_io_choice but_width but_height ~dir:Horizontal ~x ~y ~width ~height ;
      (* io_map is an array of floats between 0 and 1, we must represent
       * it as a heightmap. *)
      fun_of io_maps_update (fun () -> [
        Widget.heightmap n.io_map ~x:(x + but_width) ~y:(y + but_height)
          ~width:(width - but_width) ~height:(height - but_height) ]) ]

  let detailed_neuron hvd sel =
    if hvd <> None then hvd else sel

  let render_neuron_details ~x ~y ~width ~height =
    (* TODO: also a map of one input to another displaying the output of this neuron according to those inputs.
     * Requires that we stop the simulation every N steps, then run a `recording` simulation phase during which
     * we run all possible inputs for the two globally selected, and for each neuron record their output (without
     * back propagating of course!). Then we restart the sim as normal.
     * Those maps can also be used to display the neuron instead of the big dot. *)
    let render_details_of_neuron n =
      [ Widget.text ("Neuron "^ i2s n.id ^(if n.layer = Hidden then "("^ i2s n.io_key ^")" else "")) ~x ~y:(y + height - 1 * Layout.text_line_height) ~width:(width/2) ~height:Layout.text_line_height ;
        Widget.text (i2s (List.length n.dendrits) ^"/"^ i2s (List.length n.axons) ^"cnx") ~x:(x + width/2) ~y:(y + height - 1 * Layout.text_line_height) ~width:(width/2) ~height:Layout.text_line_height ;
        Widget.text (string_of_transfer n.func) ~x ~y:(y + height - 2 * Layout.text_line_height) ~width:(width/3) ~height:Layout.text_line_height ;
        Widget.text ("dE/dO="^ f2s n.dE_dOutput) ~x:(x + width/3) ~y:(y + height - 2 * Layout.text_line_height) ~width:(width/2) ~height:Layout.text_line_height ;
        Widget.text ("Last Output:"^ f2s n.output) ~x ~y:(y + height - 3 * Layout.text_line_height) ~width ~height:Layout.text_line_height ;
        render_csv_value n ~x ~y:(y + height - 4 * Layout.text_line_height) ~width ~height:Layout.text_line_height ;
        render_io_map n ~x ~y ~width ~height:(height - 4 * Layout.text_line_height) ];
    in
    fun_of hovered (fun hvd -> [
      fun_of selected (fun sel ->
        match detailed_neuron hvd sel with
        | None -> []
        | Some n -> render_details_of_neuron n) ])

  let render_all ~x ~y ~width ~height =
    fun_of Layout.control_column_width (fun control_width -> [
      fun_of Layout.neural_net_height (fun net_height -> [
        fun_of Layout.layer_adder_height (fun adder_height -> [
          (* A widget to add layers in the control column: *)
          render_layer_adder ~x ~y:(y + net_height - adder_height) ~width:control_width ~height:adder_height ;
          fun_of Layout.details_height (fun details_height -> [
            render_neuron_details ~x ~y ~width:control_width ~height:details_height ]) ]) ]) ;
      (* Now the neural network itself: *)
      render_adder_line ~x:(x + control_width) ~y ~width:(width - control_width) ~height ;
      render_dendrits ~x:(x + control_width) ~y ;
      render_neurons ~x:(x + control_width) ~y ])
end

(* IO Controllers *)

module IO =
struct
  let controller_header title ~on_del ~x ~y ~width ~height =
    let del_width = 3 * 10 in
    group
      [ Widget.text ~x ~y ~width:(width - del_width) ~height:Layout.text_line_height title ;
        Widget.button ~x:(x + width - del_width) ~y ~width:del_width ~height:height ~on_click:on_del "[X!]" ]

  let field_selector t ~x ~y ~width ~height =
    let text_width = 40 in
    group [
      Widget.text ~x ~y ~width:text_width ~height "Field:" ;
      Widget.simple_select csv_fields t.col ~x:(x + text_width) ~y ~width:(width - text_width) ~height ]

  let lag_selector t ~x ~y ~width ~height =
    let text_width = 40 in
    group [
      Widget.text ~x ~y ~width ~height "Lag:" ;
      Widget.int_select t.lag ~max:(Array.length csv.lines - 1 - t.avg.value) ~x:(x + text_width) ~y ~width:(width - text_width) ~height ]

  let avg_selector t ~x ~y ~width ~height =
    let text_width = 40 in
    group [
      Widget.text ~x ~y ~width ~height "Avg:" ;
      Widget.int_select t.avg ~max:(Array.length csv.lines - 1 - t.lag.value) ~x:(x + text_width) ~y ~width:(width - text_width) ~height ]

  let render title ios_param t ~x ~y ~width ~height =
    (* Every input has a common header with some controls for
     * deleting it for instance *)
    let on_del shifted _ =
      if shifted then (
        List.remove_if (fun io -> io.id = t.id) ios_param.Param.value |>
        Param.set ios_param)
    in
    group [
      controller_header title ~on_del ~x ~y:(y + height - 1 * Layout.text_line_height) ~width ~height:Layout.text_line_height;
      field_selector t ~x ~y:(y + height - 2 * Layout.text_line_height) ~width ~height:Layout.text_line_height ;
      lag_selector t ~x ~y:(y + height - 3 * Layout.text_line_height) ~width:(width/2) ~height:Layout.text_line_height ;
      avg_selector t ~x:(x + width/2) ~y:(y + height - 3 * Layout.text_line_height) ~width:(width/2) ~height:Layout.text_line_height ]

  let id_seq = ref 0

  let precomp io () =
    for i = 0 to Array.length io.csv_values - 1 do
      io.csv_values.(i) <-
        CSV.get_value csv io.col.value io.lag.value io.avg.value i
    done ;
    (* Compute the exrtremums on this column. *)
    let diff i = io.csv_values.(i) -. io.csv_values.(i - 1) in
    io.diff_extremums <- diff 1, diff 1 ;
    for i = 2 to Array.length io.csv_values - 1 do
      let d = diff i in
      let mi, ma = io.diff_extremums in
      io.diff_extremums <- min mi d, max ma d
    done

  let make_ id col lag avg =
    let io =
      { id ;
        col = Param.make ~on_update:[ set_need_save ] "some IO CSV column" col ;
        lag = Param.make ~on_update:[ set_need_save ; update_max_lag ] "some IO CSV lag" lag ;
        avg = Param.make ~on_update:[ set_need_save ; update_max_lag ] "some IO CSV avg" avg ;
        csv_values = Array.make (Array.length csv.lines) 0. ;
        diff_extremums = 0., 0. } in
    precomp io () ;
    Param.on_update io.col (precomp io) ;
    Param.on_update io.lag (precomp io) ;
    Param.on_update io.avg (precomp io) ;
    io

  let make () =
    incr id_seq ;
    make_ !id_seq 0 0 0

  (* Avoid functionals in the serialization: *)
  type serialized_io =
    { ser_id : int ; ser_col : int ; ser_lag : int [@ppp_default 0] ; ser_avg : int [@ppp_default 0] } [@@ppp PPP_JSON]
  let serialize io =
    { ser_id = io.id ; ser_col = io.col.value ; ser_lag = io.lag.value ; ser_avg = io.avg.value }
  let unserialize s =
    make_ s.ser_id s.ser_col s.ser_lag s.ser_avg

  let render_io_adder what ios_param ~x ~y ~width ~height =
    let on_click _ _ = Param.cons ios_param (make ()) in
    Widget.button ("Add a new "^ what) ~on_click ~x ~y:(y + height - Layout.text_line_height) ~width ~height:Layout.text_line_height

  let render_all title ios_param io_width ~x ~y ~width ~height =
    ignore width ;
    (* Draw the input adder on the control column: *)
    fun_of Layout.control_column_width (fun control_width ->
      [ (* The input adder: *)
        render_io_adder title ios_param ~x ~y ~width:control_width ~height ;
        fun_of ios_param (fun ios ->
          (* We want to display them from oldest to newest: *)
          sort_io ios |>
          List.fold_left (fun (x, children) io ->
            let child =
              render title ios_param io ~x ~y ~width:io_width ~height
            in
            x + io_width, child :: children) (x + control_width, []) |>
          snd)])
end

module Simulation =
struct
  let is_running = Param.make "simulation is running" false
  let rem_steps = ref 0
  let nb_steps = ref 0
  let nb_batches = Param.make "nb batches" 0
  let minibatch_steps = ref 0
  let nb_steps_update = Param.make "nb_steps should be refreshed" () (* TODO: Param.make ~update_every:16 ? *)
  let tot_err_history = 350
  let tot_err_graph = Param.make "total error graph" (Graph.make "Error" tot_err_history)

  let adjust_dendrits learn_rate momentum n =
    let open Neuron in
    List.iter (fun d ->
      let de_dw = d.source.output *. n.dE_dOutput in
      let adj = momentum *. d.prev_weight_adj -. learn_rate *. de_dw in
      d.weight <- d.weight +. adj ;
      d.prev_weight_adj <- adj
    ) n.dendrits

  let cap v mi ma =
    if v < mi then mi else
    if v > ma then ma else v

  let adjust_dendrits_individually learn_rate n =
    let open Neuron in
    List.iter (fun d ->
      let de_dw = d.source.output *. n.dE_dOutput in
      let new_learn_gain =
        if d.prev_gradient *. de_dw >= 0. then d.learn_gain +. 0.05
        else d.learn_gain *. 0.95 in
      let new_learn_gain = cap new_learn_gain 0.1 10. in
      d.learn_gain <- new_learn_gain ;
      let adj = -. learn_rate *. d.learn_gain *. de_dw in
      d.weight <- d.weight +. adj ;
      d.prev_weight_adj <- adj ;
      d.prev_gradient <- de_dw ;
    ) n.dendrits

  let adjust_dendrits_individually_with_momentum learn_rate momentum n =
    let open Neuron in
    List.iter (fun d ->
      let de_dw = d.source.output *. n.dE_dOutput in
      let new_learn_gain =
        if d.prev_weight_adj *. de_dw <= 0. then d.learn_gain +. 0.05
        else d.learn_gain *. 0.95 in
      let new_learn_gain = cap new_learn_gain 0.1 10. in
      d.learn_gain <- new_learn_gain ;
      let adj = momentum *. d.prev_weight_adj -. learn_rate *. d.learn_gain *. de_dw in
      d.weight <- d.weight +. adj ;
      d.prev_weight_adj <- adj ;
      d.prev_gradient <- de_dw
    ) n.dendrits

  (* Assuming we know how n's children influence the error (dE_dOutput),
   * compute its own output influences the error. *)
  let propagate_err_backward n =
    let open Neuron in
    n.dE_dOutput <-
      transfer' n.func n.output *.
      List.fold_left (fun s a ->
        assert (compare n a.dest < 0) ;
        s +. a.dendrit.weight *. a.dest.dE_dOutput
      ) 0. n.axons ;
    assert (Float.compare nan n.dE_dOutput <> 0)

  let set_output_and_err do_train =
    let open Neuron in
    iter_only Output (fun n ->
      let io = find_io outputs.value n.io_key in
      let target = io.csv_values.(csv.idx) -. io.csv_values.(csv.idx - 1)
      and extr = io.diff_extremums in
      (* Same remark as in forward_propagation regarding the
       * transfer function of output neurons. *)
      let r = scale_output_rev n.func extr n.output in
      if do_train then (
        let err = (r -. target) /. (snd extr -. fst extr) in
        n.dE_dOutput <- n.dE_dOutput +. err ;
        assert (Float.compare nan n.dE_dOutput <> 0)) ;
      if io.avg.value = 0 then ( (* TODO: predict for avg <> 0 *)
        let undiffed = r +. io.csv_values.(csv.idx - 1) in
        CSV.predict csv io.lag.value io.col.value undiffed)
    )

  let back_propagation nb_samples =
    let open Neuron in
    (* Scale down the dE_dOutput of the error: *)
    let s = 1. /. i2f nb_samples in
    let tot_err =
      fold_only Output (fun e n ->
        n.dE_dOutput <- s *. n.dE_dOutput ;
        e +. sq n.dE_dOutput
      ) 0. in
    (* Now we can back-propagate to the hidden layer *)
    iter_back_but Output propagate_err_backward ;
    tot_err

  let momentum = Param.make ~on_update:[ set_need_save ] "momentum" 0.
  let minibatch_size = Param.make ~on_update:[ set_need_save ] "minibatch size" 1
  let test_set_size = Param.make ~on_update:[ set_need_save ] "test set size" 0
  let learn_rate = Param.make ~on_update:[ set_need_save ] "learning speed" 0.03
  let auto_learn_rate = Param.make ~on_update:[ set_need_save ] "automatic learning rate" 0.0001
  type learning_algo = FixedGlobal | AutoAdapt | AutoAdaptPerWeight [@@ppp PPP_JSON]
  let learning_algo = Param.make ~on_update:[ set_need_save ] "learning algo" FixedGlobal
  let prev_tot_err = ref None

  let adjust_weights momentum tot_err =
    let open Neuron in
    match learning_algo.value with
    | FixedGlobal ->
        iter_all (adjust_dendrits learn_rate.value momentum)
    | AutoAdapt ->
        Option.may (fun prev_tot_err ->
          let new_learn_rate =
            if tot_err > prev_tot_err +. 1e-10 then
              cap (auto_learn_rate.value *. 0.5) 1e-10 10.
            else if tot_err < prev_tot_err then
              cap (auto_learn_rate.value *. 1.01) 1e-10 10.
            else auto_learn_rate.value in
          if auto_learn_rate.value <> new_learn_rate then
            Param.set auto_learn_rate new_learn_rate ;
          Format.printf "prev_tot_err=%g, tot_err=%g, auto_learn_rate=%g@."
            prev_tot_err tot_err auto_learn_rate.value
        ) !prev_tot_err ;
        iter_all (adjust_dendrits auto_learn_rate.value momentum) ;
        prev_tot_err := Some tot_err
    | AutoAdaptPerWeight when momentum > 0. ->
        iter_all (adjust_dendrits_individually_with_momentum learn_rate.value momentum)
    | AutoAdaptPerWeight ->
        iter_all (adjust_dendrits_individually learn_rate.value)

  let step () =
    if is_running.value then (
      (* 1 is for being able to diff; !max_lag is actually the max of
       * lag + avg. We want twice that so that, again, we can diff. *)
      CSV.next (1 + 2 * !max_lag) csv ;
      Neuron.input_from_csv () ;

      Neuron.forward_propagation () ;
      let do_train = csv.idx >= !max_lag + test_set_size.value in
      set_output_and_err do_train ;
      (* Begin with accumulating the error into dE_dOutput of the output
       * nodes. While at it, also save the prediction in the CSV. *)

      (* If the line was from the training set we merely skip updating the
       * weights *)
      if do_train then (
        incr minibatch_steps ;
        if !minibatch_steps >= minibatch_size.value then (
          let tot_err = back_propagation !minibatch_steps in
          adjust_weights momentum.value tot_err ;
          minibatch_steps := 0 ;
          Param.incr nb_batches ;
          Graph.push tot_err_graph.value tot_err ;
          Param.change tot_err_graph ;
          (* Refresh the dendrits from time to time: *)
          if nb_batches.value land 7 = 0 then
            Param.change Neuron.neurons) ;
      ) ;

      decr rem_steps ;
      incr nb_steps ;
      if !rem_steps = 0 then Param.set is_running false ;
      (* Refresh params now and then: *)
      if !nb_steps land 31 = 0 || !rem_steps = 0 then (
        Param.change nb_steps_update ;
        Neuron.touch_hovered () ;
        set_need_save ()) ;
      if !nb_steps land 63 = 0 || !rem_steps = 0 then
        Neuron.refresh_io_map ())
end

module LoadSave =
struct
  type t =
    { io_id_seq : int [@ppp_default 0] ;
      neuron_id_seq : int [@ppp_default 0] ;
      nb_steps : int [@ppp_default 0] ;
      nb_batches : int [@ppp_default 0] ;
      minibatch_size : int [@ppp_default 1] ;
      test_set_size : int [@ppp_default 0] ;
      momentum : float [@ppp_default 0.] ;
      learn_rate : float [@ppp_default 0.03] ;
      auto_learn_rate : float [@ppp_default 0.0001] ;
      learning_algo : Simulation.learning_algo [@ppp_default Simulation.FixedGlobal] ;
      inputs : IO.serialized_io list ;
      outputs : IO.serialized_io list ;
      neurons : Neuron.serialized_neuron array } [@@ppp PPP_JSON]

  let make () =
    { io_id_seq = !IO.id_seq ;
      neuron_id_seq = !Neuron.id_seq ;
      nb_steps = !Simulation.nb_steps ;
      nb_batches = Simulation.nb_batches.value ;
      minibatch_size = Simulation.minibatch_size.value ;
      test_set_size = Simulation.test_set_size.value ;
      momentum = Simulation.momentum.value ;
      learn_rate = Simulation.learn_rate.value ;
      auto_learn_rate = Simulation.auto_learn_rate.value ;
      learning_algo = Simulation.learning_algo.value ;
      inputs = List.map IO.serialize inputs.value ;
      outputs = List.map IO.serialize outputs.value ;
      neurons = Array.map Neuron.serialize Neuron.neurons.value }

  let save_info oc =
    make () |>
    PPP.to_string t_ppp |>
    Printf.fprintf oc "%s\n"

  let load_info ic =
    let t = BatIO.read_all ic |> PPP.of_string_exc t_ppp in
    IO.id_seq := t.io_id_seq ;
    Neuron.id_seq := t.neuron_id_seq ;
    Simulation.nb_steps := t.nb_steps ;
    Param.set Simulation.nb_batches t.nb_batches ;
    Param.set Simulation.minibatch_size t.minibatch_size ;
    Param.set Simulation.test_set_size t.test_set_size ;
    Param.set Simulation.learn_rate t.learn_rate ;
    Param.set Simulation.auto_learn_rate t.auto_learn_rate ;
    Param.set Simulation.learning_algo t.learning_algo ;
    Param.set Simulation.momentum t.momentum ;
    Param.set inputs (List.map IO.unserialize t.inputs) ;
    Param.set outputs (List.map IO.unserialize t.outputs) ;
    let neurons = Array.map Neuron.unserialize0 t.neurons in
    Array.iter2 (Neuron.unserialize_dendrits neurons) neurons t.neurons ;
    Array.iter2 (Neuron.unserialize_axons neurons) neurons t.neurons ;
    Neuron.sort_neurons neurons ;
    Param.set Neuron.neurons neurons

  let file_prefix =
    if !test_name = "" then csv.name ^".save."
    else !test_name ^"."

  let save _ _ =
    let fname n = file_prefix ^ i2s n
    and file_exists fname =
      let open Unix in
      try ignore (stat fname) ; true
      with Unix_error (ENOENT, _, _) -> false
    in
    let rec save n =
      let f = fname n in
      if file_exists f then save (n + 1)
      else (
        if debug then Format.printf "Saving configuration in %s@." f ;
        File.with_file_out ~mode:[`create] f save_info ;
        Param.set need_save false
      )
    in
    save 1

  let load shifted _ =
    if shifted || Array.length Neuron.neurons.value = 0 then (
      let f, _ =
        Sys.readdir "." |>
        Array.fold_left (fun (_, best_n as prev) fname ->
          if String.starts_with fname file_prefix then
            let plen = String.length file_prefix in
            try let n = String.sub fname plen (String.length fname - plen) |>
                        int_of_string in
                if n > best_n then fname, n else prev
            with _ -> prev
          else prev) ("", 0) in
      if f = "" then
        Format.printf "Cannot find any save file for this CSV@."
      else (
        Format.printf "Loading configuration from %s@." f ;
        File.with_file_in f load_info ;
        Param.set need_save false ;
        Param.incr Neuron.selection_generation ;
        Simulation.minibatch_steps := 0 ;
        Simulation.prev_tot_err := None ;
        Param.change Layout.neural_net_height ; (* trigger the rearrangement of neurons *)
        Graph.reset_param Simulation.tot_err_graph))
end

let render_result_controls ~x ~y ~width ~height =
  let learning_options =
    [| Simulation.FixedGlobal, "fixed" ; Simulation.AutoAdapt, "Automatic" ;
       Simulation.AutoAdaptPerWeight, "Individual" |]
  and learn_rate_options =
    [| 0.000_001, "0.000,001" ; 0.000_01, "0.000,01" ; 0.000_1, "0.000,1" ;
       0.001, "0.001" ; 0.003, "0.003" ; 0.01, "0.01" ; 0.03, "0.03" ;
       0.1, "0.1" ; 0.3, "0.3" ; 1., "1" ; 3., "3" ; 10., "10" |]
  and momentum_options =
    [| 0., "0" ; 0.1, "0.1" ; 0.5, "0.5" ;
       0.9, "0.9" ; 0.95, "0.95" ; 0.99, "0.99" |]
  and minibatch_options =
    Array.filter (fun (v, _n) ->
      v <= Array.length csv.lines
    ) [| 1, "1" ; 10, "10" ; 100, "100" ; 1_000, "1,000" ; 10_000, "10,000" ;
         100_000, "100,000" ; 1_000_000, "1,000,000" ; 10_000_000, "10,000,000" ;
         Array.length csv.lines, "full-batch" |]
  and test_set_size_options =
    let p n =
      let l = Array.length csv.lines in (n*l + l/2) / 100 in
    [| 0, "online" ; p 5, "5%" ; p 10, "10%" ; p 25, "25%" ; p 50, "50%" |]
  and label_w = 105 in [
  fun_of need_save (fun need_save ->
    if need_save then
      [ Widget.button "Save" ~on_click:LoadSave.save ~x ~y:(y + height - 1 * Layout.text_line_height) ~width:(width / 2) ~height:Layout.text_line_height ]
    else []) ;
  Widget.button ("Load"^ (if Array.length Neuron.neurons.value > 0 then "!" else "")) ~on_click:LoadSave.load ~x:(width / 2) ~y:(y + height - 1 * Layout.text_line_height) ~width:(width / 2) ~height:Layout.text_line_height ;
  fun_of Simulation.is_running (fun is_running ->
    let run_for n _ _ =
      Param.set Simulation.is_running (n > 0) ;
      Simulation.rem_steps := n
    and reset shifted _ =
      if shifted then (
        Neuron.iter_all Neuron.reset ;
        Graph.reset_param Simulation.tot_err_graph ;
        Param.change Neuron.neurons ;
        CSV.reset csv ;
        Simulation.nb_steps := 0 ;
        Simulation.minibatch_steps := 0 ;
        Simulation.prev_tot_err := None ;
        Param.set Simulation.auto_learn_rate 0.0001 ;
        Param.change Simulation.nb_steps_update ;
        Param.set Simulation.nb_batches 0)
    and y = y + height - 2 * Layout.text_line_height and height = Layout.text_line_height in
    if is_running then [
      Widget.button "Pause" ~on_click:(run_for 0) ~x ~y ~width ~height
    ] else [
        Widget.button "Run" ~on_click:(run_for max_int) ~x ~y ~width:(width/3) ~height ;
        Widget.button "Step" ~on_click:(run_for 1) ~x:(width/3) ~y ~width:(width/3) ~height ;
        Widget.button "Reset!" ~on_click:reset ~x:(2 * width/3) ~y ~width:(width/3) ~height ]) ;
  fun_of Simulation.nb_steps_update (fun () -> [
    Widget.text ("Steps: "^ i2s !Simulation.nb_steps) ~x ~y:(y + height - 3 * Layout.text_line_height) ~width:(width/2) ~height:Layout.text_line_height ]) ;
  fun_of Simulation.nb_batches (fun nbb -> [
    Widget.text ("Batches: "^ i2s nbb) ~x:(width/2) ~y:(y + height - 3 * Layout.text_line_height) ~width:(width/2) ~height:Layout.text_line_height ]) ;

  Widget.text "Test Set Size:" ~x ~y:(y + height - 4 * Layout.text_line_height) ~width:label_w ~height:Layout.text_line_height ;
  Widget.simple_select test_set_size_options Simulation.test_set_size ~x:(x + label_w) ~y:(y + height - 4 * Layout.text_line_height) ~width:(width - label_w) ~height:Layout.text_line_height ;
  Widget.text "Minibatch Size:" ~x ~y:(y + height - 5 * Layout.text_line_height) ~width:label_w ~height:Layout.text_line_height ;
  Widget.simple_select minibatch_options Simulation.minibatch_size ~x:(x + label_w) ~y:(y + height - 5 * Layout.text_line_height) ~width:(width - label_w) ~height:Layout.text_line_height ;
  Widget.text "Learning:" ~x ~y:(y + height - 6 * Layout.text_line_height) ~width:label_w ~height:Layout.text_line_height ;
  Widget.simple_select learning_options Simulation.learning_algo ~x:(x + label_w) ~y:(y + height - 6 * Layout.text_line_height) ~width:(width - label_w) ~height:Layout.text_line_height ;
  fun_of Simulation.learning_algo (function
    | Simulation.(FixedGlobal | AutoAdaptPerWeight) -> [
        Widget.text "Learn Rate:" ~x ~y:(y + height - 7 * Layout.text_line_height) ~width:label_w ~height:Layout.text_line_height ;
        Widget.simple_select learn_rate_options Simulation.learn_rate ~x:(x + label_w) ~y:(y + height - 7 * Layout.text_line_height) ~width:(width - label_w) ~height:Layout.text_line_height ]
    | Simulation.AutoAdapt -> [
        Widget.text "Learn Rate:" ~x ~y:(y + height - 7 * Layout.text_line_height) ~width:label_w ~height:Layout.text_line_height ;
        fun_of Simulation.auto_learn_rate (fun rate -> [
          Widget.text (f2s rate) ~x:(x + label_w) ~y:(y + height - 7 * Layout.text_line_height) ~width:(width - label_w) ~height:Layout.text_line_height ]) ]) ;
  Widget.text "Momentum:" ~x ~y:(y + height - 8 * Layout.text_line_height) ~width:label_w ~height:Layout.text_line_height ;
  Widget.simple_select momentum_options Simulation.momentum ~x:(x + label_w) ~y:(y + height - 8 * Layout.text_line_height) ~width:(width - label_w) ~height:Layout.text_line_height ]

let render_results ~x ~y ~width ~height =
  fun_of Layout.control_column_width (fun control_width -> [
    group (render_result_controls ~x ~y ~width:control_width ~height) ;
    let x = x + control_width and width = width - control_width in
    let render_tot_err () =
      fun_of Simulation.tot_err_graph (fun graph ->
        [ Graph.render graph ~x ~y ~width ~height ])
    and render_predictions n =
      fun_of Neuron.io_maps_update (fun () -> [
        let output = find_io outputs.value n.Neuron.io_key in
        fun_of output.col (fun col -> [
          fun_of output.avg (fun avg -> [
            if avg <> 0 then Widget.todo ~x ~y ~width ~height else
            fun_of Simulation.test_set_size (fun test_set_sz -> [
              CSV.render ~col csv test_set_sz !max_lag ~x ~y ~width ~height ]) ]) ]) ])
    in
    fun_of Neuron.hovered (function
      | Some n when n.layer = Output -> [ render_predictions n ]
      | _ -> [
        fun_of Neuron.selected (function
          | Some n when n.layer = Output -> [ render_predictions n ]
          | _ -> [ render_tot_err () ]) ]) ])

let is_in_box_selection start stop pos =
  let in_between a b c = a <= c && c <= b in
  let in_between a b c = in_between a b c || in_between b a c in
  in_between start.(0) stop.(0) pos.(0) &&
  in_between start.(1) stop.(1) pos.(1)

let background screen_w screen_h =
  let on_drag_start shifted pos =
    Param.set box_selection (Some (pos, pos, shifted)) in
  let on_drag stop =
    match box_selection.value with
    | None -> ()
    | Some (start, _, shifted) ->
      if Neuron.fold_all (fun chg neuron ->
          let orig = pi Layout.control_column_width.value
                        Layout.(screen_height.value - inputs_height - neural_net_height.value) in
          let pos = Point.Infix.(neuron.position.value +~ orig) in
          let in_selbox = is_in_box_selection start stop pos in
          if in_selbox = neuron.selected ||
             shifted && neuron.selected
          then chg else (
            neuron.selected <- in_selbox ;
            true)) false
      then (
        Param.change Neuron.neurons ;
        Param.incr Neuron.selection_generation) ;
      Param.set box_selection (Some (start, stop, shifted)) in
  let on_drag_stop stop =
    on_drag stop ;
    Param.none box_selection in
  let on_click _ _pos =
    Param.none box_selection in
  let on_hover _ =
    if Neuron.hovered.value <> None then
      Param.set Neuron.hovered None
  in
  Widget.rect ~on_drag_start ~on_drag_stop ~on_drag ~on_click ~on_sub_click:on_click ~on_hover
              C.white ~x:0 ~y:0 ~width:screen_w ~height:screen_h

let background_selection =
  fun_of box_selection (function
    | None -> []
    | Some (start, stop, _shifted) ->
      let x0 = K.min start.(0) stop.(0) |> K.to_int
      and y0 = K.min start.(1) stop.(1) |> K.to_int
      and x1 = K.max start.(0) stop.(0) |> K.to_int
      and y1 = K.max start.(1) stop.(1) |> K.to_int
      and color = c 0.6 0.6 0.9 in
      let width = 1 + (x1 - x0)
      and height = 1 + (y1 - y0)
      and w = 2 in
      let hw = w/2 in
      [ Widget.rect color ~x:(x0-hw) ~y:(y0-hw) ~width:(width+w) ~height:w ;
        Widget.rect color ~x:(x1-hw) ~y:(y0-hw) ~width:w ~height:(height+w) ;
        Widget.rect color ~x:(x0-hw) ~y:(y1-hw) ~width:(width+w) ~height:w ;
        Widget.rect color ~x:(x0-hw) ~y:(y0-hw) ~width:w ~height:(height+w) ])

(* Main view *)

let dataviz_layout =
  fun_of Layout.screen_width (fun screen_w -> [
    fun_of Layout.screen_height (fun screen_h -> [
      background screen_w screen_h ;
      background_selection ;
      fun_of Layout.input_width (fun input_width -> [
        IO.render_all "Input" inputs input_width ~x:0 ~y:(screen_h - Layout.inputs_height) ~width:screen_w ~height:Layout.inputs_height ]) ;
      fun_of Layout.neural_net_height (fun neural_net_height -> [
        Neuron.render_all ~x:0 ~y:(screen_h - (Layout.inputs_height + neural_net_height)) ~width:screen_w ~height:neural_net_height ;
        fun_of Layout.outputs_height (fun outputs_height -> [
          fun_of Layout.output_width (fun output_width -> [
            IO.render_all "Output" outputs output_width ~x:0 ~y:(screen_h - (Layout.inputs_height + neural_net_height + outputs_height)) ~width:screen_w ~height:outputs_height ;
            fun_of Layout.result_height (fun result_height -> [
              render_results ~x:0 ~y:0 ~width:screen_w ~height:result_height ])])])])])])

let () =
  let width = 800 and height = 600 in
  Param.set Layout.screen_width width ;
  Param.set Layout.screen_height height ;
  let init double_buffer msaa =
    let dir = ONeuNeuConfig.lib_dir in
    let dir =
      let len = String.length dir in
      if len > 0 && dir.[len - 1] <> '/' then dir ^"/" else dir in
    let title = "NeuralNet - "^
        (if !test_name <> "" then !test_name else csv.name) in
    Ogli_render.init ~title ~font:(dir ^"vera.ttf") ~double_buffer ~msaa width height ;
    Ogli_view.make ~double_buffer ~width:Layout.screen_width ~height:Layout.screen_height dataviz_layout in
  let view =
    try init !double_buffer !msaa with Failure _ ->
      (* Silently ignore and look harder: *)
      try init false true with Failure _ ->
        Format.eprintf "Cannot get a single-buffer visual with MSAA :-(@." ;
        try init true true with Failure _ ->
          Format.eprintf "Cannot get a double-buffer visual with MSAA :'(@." ;
          try init false false with Failure _ ->
            Format.eprintf "Cannot get a double-buffer visual without MSAA! >8(@." ;
            exit 1
  in
  while true do
    Ogli_view.render view ;
    Ogli_render.display () ;
    Ogli_view.next_event view (Ogli_render.next_event ~wait:(not Simulation.is_running.value)) Ogli_render.resize ;
    Simulation.step ()
  done
