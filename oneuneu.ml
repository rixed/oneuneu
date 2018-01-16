open Batteries
open Ogli
open Ogli_view

let debug = true
let f2i = int_of_float
let i2f = float_of_int

let f2s v = Printf.sprintf "%+.3f" v

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
    avg : int Param.t }

let sort_io =
  List.fast_sort (fun io1 io2 -> compare io1.id io2.id)

let find_io ios id =
  List.find (fun io -> io.id = id) ios

let need_save = Param.make "need save" false
let set_need_save () = Param.set need_save true

let inputs = Param.make ~on_update:set_need_save "inputs" []
let outputs = Param.make ~on_update:set_need_save "outputs" []

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
  let result_height = Param.make "result height" 100

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

  let button ?on_click label ~x ~y ~width ~height =
    group [
      rect ?on_click (c 0.8 0.8 0.8) ~x ~y ~width ~height ;
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
        (if wrap || cur_idx < max_idx - 1 then
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
      let label = "  "^ string_of_int selected
      and arrow_w = 10 in
      [ button label ~on_click ~x ~y ~width ~height ;
        (if wrap || selected > min then
          text "‹" ~x ~y ~width:arrow_w ~height
        else group []) ;
        (if wrap || selected < max then
          text "›" ~x:(x + width - arrow_w) ~y ~width:arrow_w ~height
        else group []) ])
end

(* Reading the CSV file providing the training data *)

module CSV =
struct
  type t =
    { columns : string array ;
      extremums : (float * float) array ;
      lines : float array array ;
      predictions : float option array array ;
      naive_err : float array ;
      shuffling : int array ;
      mutable shuffling_idx : int ;
      mutable min_tot_err : float ;
      mutable idx : int (* current line to read values from *) ;
      name : string }

  let random_columns nb_cols =
    Array.init nb_cols (fun c -> "column "^ string_of_int c)

  let make columns lines name =
    let nb_lines = Array.length lines
    and nb_cols = Array.length columns  in
    let extremums = Array.map (fun v -> v, v) lines.(0) in
    let naive_err = Array.create nb_cols 0. in
    for l = 1 to nb_lines - 1 do
      for c = 0 to Array.length lines.(l) - 1 do
        extremums.(c) <- min (fst extremums.(c)) lines.(l).(c),
                         max (snd extremums.(c)) lines.(l).(c) ;
        if l >= 1 then
          naive_err.(c) <- naive_err.(c) +.
                           abs_float (lines.(l).(c) -. lines.(l-1).(c))
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
    { columns ; lines ; predictions ; min_tot_err = max_float ; naive_err ;
      name ; extremums ; shuffling ; shuffling_idx = 0 ; idx = shuffling.(0) }

  let reset csv =
    for l = 0 to Array.length csv.predictions - 1 do
      for c = 0 to Array.length csv.predictions.(l) - 1 do
        csv.predictions.(l).(c) <- None
      done
    done ;
    csv.min_tot_err <- max_float

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

  let offset_idx csv off =
    Lr44.pos_mod (csv.idx + off) (Array.length csv.lines)

  let next csv =
    (* Iter over each line before looping *)
    csv.shuffling_idx <- csv.shuffling_idx + 1 ;
    if csv.shuffling_idx >= Array.length csv.shuffling then (
      Array.shuffle csv.shuffling ;
      csv.shuffling_idx <- 0 ;
    ) ;
    csv.idx <- csv.shuffling.(csv.shuffling_idx)

  let get_extremum csv io = csv.extremums.(io.col.value)

  let get_value csv io =
    match io.lag.value, io.avg.value with
    | 0, 0 -> (* fast path *)
      csv.lines.(csv.idx).(io.col.value), (csv.idx, io.col.value)
    | lag, 0 ->
      let idx = offset_idx csv ~-(lag) in
      csv.lines.(idx).(io.col.value), (idx, io.col.value)
    | lag, avg ->
      let rec sum i s =
        if i < 0 then s else
          let idx = offset_idx csv ~-(lag + i) in
          sum (i - 1) (s +. csv.lines.(idx).(io.col.value)) in
      sum avg 0. /. i2f avg, (~-1, ~-1 (* TODO *))

  let predict csv (line, col) v =
    if line < 0 then () (* TODO *) else
      csv.predictions.(line).(col) <- Some v

  let render ~col csv ~x ~y ~width ~height =
    (* Draw a simple bar for each prediction/value. When we have several
     * values per pixel draw the bounding box. *)
    let vmi, vma = csv.extremums.(col) in
    (* Enlarge to account for bad predictions: *)
    let vmi, vma =
      let l = (vma -. vmi) *. 0.1 in
      vmi -. l, vma +. l in
    let y_ratio = i2f (height - 1) /. (vma -. vmi) in
    let x_of_line l =
      let max_l = Array.length csv.lines - 1 in
      x + ((i2f l *. i2f (width - 1) /. i2f max_l) |> f2i)
    and y_of_v v =
      y + ((v -. vmi) *. y_ratio |> f2i |> max 0 |> min (height - 1))
    in
    let rec loop tot_err skipped polys last_x y_lims line =
      if line >= Array.length csv.lines then tot_err, skipped, polys
      else match csv.predictions.(line).(col) with
      | None ->
          (* Just skip *)
          loop tot_err (skipped + 1) polys last_x y_lims (line + 1)
      | Some v0 ->
        let v1 = csv.lines.(line).(col) in
        let v0, v1 =
          if v0 <= v1 then v0, v1 else v1, v0 in
        let tot_err = tot_err +. v1 -. v0 in
        let x = x_of_line line in
        (match y_lims with
        | None ->
            (* Start a new box *)
            loop tot_err skipped polys x (Some (v0, v1)) (line + 1)
        | Some (pv0, pv1) ->
            if x = last_x then
              (* If we are on the same x then grow the box: *)
              loop tot_err skipped polys last_x (Some (min v0 pv0, max v1 pv1)) (line + 1)
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
              loop tot_err skipped polys x (Some (v0, v1)) (line + 1)))
    in
    let tot_err, skipped, polys = loop 0. 0 [] ~-1 None 0 in
    let title = "naive err: "^ f2s csv.naive_err.(col) in
    let title =
      (* Wait before we have visited at least half the data before recording min error: *)
      if skipped > Array.length csv.lines / 2 then title else
        let avg = tot_err /. i2f (Array.length csv.lines - skipped) in
        let e = tot_err +. avg *. i2f skipped in
        if e < csv.min_tot_err then csv.min_tot_err <- e ;
        title ^" - ∑err: "^ f2s e ^" (min: "^ f2s csv.min_tot_err ^")"
    and color = c 0.5 0.5 0.5 in
    group [
      Ogli_render.shape_of_polys (List.map (fun p -> color, [ p ]) polys) Point.origin [] ;
      Widget.text title ~x ~y:(y + height - Layout.text_line_height) ~width ~height:Layout.text_line_height ]
end

let msaa = ref true
let double_buffer = ref false

let csv =
  let csv_file = ref ""
  and seconds_field = ref ~-1
  and minutes_field = ref ~-1
  in
  Arg.(parse [
    "--csv", Set_string csv_file, "CSV file to use" ;
    "--timestamp", Set_int seconds_field, "Use this field number (starting at 0) as source of EPOCH seconds" ;
    "--minutes", Set_int minutes_field, "Use this field number (starting at 0) as source of EPOCH minutes" ;
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
      accum : float array ;
      title : string ;
      mutable max : float ;
      mutable min : float }

  let scroll g =
    let len = Array.length g.values in
    let speed i = (i2f i /. i2f len) ** 4. in
    for i = 0 to len - 2 do
      g.accum.(i) <- g.accum.(i) +. speed i ;
      if g.accum.(i) >= 1. then (
        g.values.(i) <- max g.values.(i) g.values.(i + 1) ;
        g.accum.(i) <- g.accum.(i) -. 1. ;
        for j = i + 1 to len - 2 do
          g.accum.(j) <- g.accum.(j) -. 1. ;
          g.values.(j) <- g.values.(j + 1)
        done
      )
    done

  let push g v =
    scroll g ;
    g.min <- min g.min v ;
    g.max <- max g.max v ;
    g.values.(Array.length g.values - 1) <- v

  let make title len =
    { values = Array.make len 0. ;
      accum = Array.make len 0. ;
      title ; max = 0. ; min = 0. }

  let reset g =
    for i = 0 to Array.length g.values - 1 do
      g.values.(i) <- 0. ;
      g.accum.(i) <- 0.
    done ;
    g.max <- 0. ;
    g.min <- 0.

  let reset_param p =
    reset p.Param.value ;
    Param.change p

  let render g ~x ~y ~width ~height =
    ignore g ; ignore x ; ignore y ; ignore height ; ignore width ;
    let len = Array.length g.values in
    let title =
      let last_val = g.values.(len - 1) in
      let txt = g.title ^":"^ f2s last_val in
      Widget.text txt ~x ~y:(y + height - Layout.text_line_height) ~width ~height:Layout.text_line_height in
    let bg = Widget.rect (c 0.9 0.9 0.9) ~x ~y ~width ~height in
    let y_scale = if g.max = g.min then 0.
                  else i2f height /. (g.max -. g.min) in
    let v2y v = (v -. g.min) *. y_scale in
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
      let poly = append 0 1 len Poly.empty |>
                 append (len-1) ~-1 ~-1 in
      try
        let poly = Algo.inflate (K.of_int 1) poly in
        Ogli_render.shape_of_polys [ c 0.6 0.6 0.6, [ poly ] ] (pi x y) []
      with e ->
        Format.printf "Cannot render graph as poly@ %a@ because of %s at %s@."
          Poly.print poly
          (Printexc.to_string e)
          (Printexc.get_backtrace ()) ;
        group []
    in
    group [ bg ; plot ; axis ; tick ; title ]
end

(* Neurons & Neural Net (UI only, see neuron.ml for actual neurons simulation *)

module Neuron =
struct
  (* Input and output neurons position and parameters depend on inputs/outputs controls (for instance, an input for recent values can have
   * from 1 to N input neurons, changing when the input config is changed. so we recompute the list of input/outputs neurons whenever those
   * controllers are updated, conservatively. Neurons position is allowed to change while the neuron id and internal states do not change,
   * though. *)

  type layer = Input | Hidden | Output [@@ppp PPP_JSON]

  type dendrit =
    { source : t ;
      mutable weight : float ;
      mutable gradient : float (* accumulated during mini-batches *) ;
      mutable prev_weight_adj : float (* for momentum *) }
  and axon =
    { dest : t ;
      dendrit : dendrit }
  and t =
    { id : int ;
      layer : layer ;
      mutable output : float ;
      mutable dE_dOutput : float ;
      mutable func : transfer ;
      mutable io_key : int ; (* either input number of layer number *)
      mutable dendrits : dendrit list ;
      mutable axons : axon list ;
      mutable selected : bool ;
      position : Point.t Param.t (* relative to the net window (right column) so it eventually depends on screen size *) }
  let make_position id pos =
    Param.make ("position of neuron#"^ string_of_int id) pos

  type ser_dendrit =
    { ser_source : int ; ser_weight : float ;
      ser_prev_weight_adj : float [@ppp_default 0.] } [@@ppp PPP_JSON]
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
      ser_prev_weight_adj = d.prev_weight_adj }
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
      gradient = 0. (* restart from start of mini-batch *) ;
      source = Array.find (fun n -> n.id = s.ser_source) neurons }
  let unserialize_axon neurons s =
    let dest = Array.find (fun n -> n.id = s.ser_dest) neurons in
    { dest ;
      dendrit = List.nth dest.dendrits s.ser_dendrit }
  let unserialize0 s =
    { id = s.ser_id ; layer = s.ser_layer ; output = s.ser_output ;
      dE_dOutput = 0. ; func = s.ser_func ; io_key = s.ser_io_key ;
      dendrits = [] ; axons = [] ; selected = false ;
      position = make_position s.ser_id Point.origin }
  let unserialize_dendrits neurons n s =
    n.dendrits <- List.map (unserialize_dendrit neurons) s.ser_dendrits
  let unserialize_axons neurons n s =
    n.axons <- List.map (unserialize_axon neurons) s.ser_axons

  let random_weight () =
    let init_weight_amplitude = 1. in
    (Random.float 1. -. 0.5) *. init_weight_amplitude

  let dendrit source =
    { source ; weight = random_weight () ; gradient = 0. ; prev_weight_adj = 0. }

  let id_seq = ref 0
  let make io_key layer position =
    incr id_seq ;
    { id = !id_seq ; dendrits = [] ; axons = [] ; io_key ; layer ;
      selected = false ; position = make_position !id_seq position ;
      func = Sigmoid ; output = 0. ; dE_dOutput = 0. }

  (* List of neurons, that we recompute (conservatively) whenever
   * the net is changed. *)
  let neurons = Param.make ~on_update:set_need_save "neurons" [||]
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
    to_.dendrits <-
      List.filter (fun d -> d.source.id <> from.id) to_.dendrits

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

  let rearrange_input_neurons () = rearrange_io_neurons Input inputs.value Layout.input_width.value
  let rearrange_output_neurons () = rearrange_io_neurons Output outputs.value Layout.output_width.value
  let () =
    Param.on_update Layout.neural_net_height (fun () ->
      rearrange_input_neurons () ;
      rearrange_output_neurons ()) ;
    Param.on_update inputs rearrange_input_neurons ;
    Param.on_update Layout.input_width rearrange_input_neurons ;
    Param.on_update outputs rearrange_output_neurons ;
    Param.on_update Layout.output_width rearrange_output_neurons

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
        let dx = i2f Layout.(screen_width.value - control_column_width.value) /.
                 i2f (nb_neurons + 1) in
        List.fast_sort (fun n1 n2 -> Int.compare n1.id n2.id) neurons |>
        List.iteri (fun i n ->
          n.io_key <- new_depth ;
          let disp = 7. and a = i2f n.id in
          let position = pf (i2f (i + 1) *. dx +. disp *. cos a)
                            (y +. disp *. sin a) in
          if not (Point.eq position n.position.value) then
            Param.set n.position position) ;
        loop (new_depth + 1) rem_layers
    in
    loop 0 layers

  let () =
    Param.on_update Layout.neural_net_height rearrange_hidden_neurons ;
    Param.on_update Layout.screen_width rearrange_hidden_neurons ;
    Param.on_update Layout.control_column_width rearrange_hidden_neurons

  let unselect_all () =
    iter_all (fun n -> n.selected <- false) ;
    Param.incr selection_generation ;
    Param.change neurons

  let hovered = Param.make "hovered neuron" None

  let touch_hovered () = (* Used to redraw the neuron details *)
    Option.may (fun _ -> Param.change hovered) hovered.value

  let io_history = 15
  (* TODO: instead of output have one graph per incoming weight *)
  let last_outputs = Param.make "last output into hovered neuron" ~on_update:touch_hovered (Graph.make "Out" io_history)

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
          if not is_hovered then (
            Param.set hovered (Some t) ;
            Graph.reset_param last_outputs)
        and on_click shifted _ =
          if shifted then (
            Format.printf "shift clicked neuron %d@." t.id;
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
        let dendrit_width = min 10. (2.55 +. abs_float weight) in
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
              Param.none box_selection ;
              unselect_all ()
            and connect_fully sel _ _ =
              List.iter (fun from ->
                List.iter (fun to_ ->
                  if from.io_key < to_.io_key then
                    connect_from_to [ from ] [ to_ ]
                ) sel
              ) sel ;
              Param.none box_selection ;
              unselect_all ()
            and disconnect_all sels _ _ =
              List.iter (fun n -> disconnect_from sels n) sels ;
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

  let render_neuron_details ~x ~y ~width ~height =
    (* TODO: also a map of one input to another displaying the output of this neuron according to those inputs.
     * Requires that we stop the simulation every N steps, then run a `recording` simulation phase during which
     * we run all possible inputs for the two globally selected, and for each neuron record their output (without
     * back propagating of course!). Then we restart the sim as normal.
     * Those maps can also be used to display the neuron instead of the big dot. *)
    let render_details_of_neuron n =
      let last_output = "Last Output:"^ f2s n.output in
      let render_csv_value =
        match n.layer with
        | Input ->
          fun_of inputs (fun ios -> [
            let io = find_io ios n.io_key in
            let extr = CSV.get_extremum csv io in
            let i, _curs = CSV.get_value csv io in
            let i' = scale_input_rev n.func extr n.output in
            fun_of io.col (fun col ->
              let txt = csv.columns.(col) ^": "^ f2s i' ^" for "^ f2s i in
              [ Widget.text txt ~x ~y:(y + height - 4 * Layout.text_line_height) ~width ~height:Layout.text_line_height ]) ])
        | Output ->
          fun_of outputs (fun ios -> [
            let io = find_io ios n.io_key in
            let extr = CSV.get_extremum csv io in
            let t, _curs = CSV.get_value csv io in
            let t' = scale_output_rev n.func extr n.output in
            fun_of io.col (fun col ->
              let txt = csv.columns.(col) ^": "^ f2s t' ^" for "^ f2s t in
              [ Widget.text txt ~x ~y:(y + height - 4 * Layout.text_line_height) ~width ~height:Layout.text_line_height ]) ])
        | Hidden -> group []
      in [
        Widget.text ("Neuron "^ string_of_int n.id ^(if n.layer = Hidden then "("^ string_of_int n.io_key ^")" else "")) ~x ~y:(y + height - 1 * Layout.text_line_height) ~width:(width/2) ~height:Layout.text_line_height ;
        Widget.text (string_of_int (List.length n.dendrits) ^"/"^ string_of_int (List.length n.axons) ^"cnx") ~x:(x + width/2) ~y:(y + height - 1 * Layout.text_line_height) ~width:(width/2) ~height:Layout.text_line_height ;
        Widget.text (string_of_transfer n.func) ~x ~y:(y + height - 2 * Layout.text_line_height) ~width:(width/3) ~height:Layout.text_line_height ;
        Widget.text ("dE/dO="^ f2s n.dE_dOutput) ~x:(x + width/3) ~y:(y + height - 2 * Layout.text_line_height) ~width:(width/2) ~height:Layout.text_line_height ;
        Widget.text last_output ~x ~y:(y + height - 3 * Layout.text_line_height) ~width ~height:Layout.text_line_height ;
        render_csv_value ;
        fun_of last_outputs (fun last_out -> [
          Graph.render last_out ~x ~y ~width ~height:(height - 4 * Layout.text_line_height) ]) ]
    in
    fun_of hovered (function
      | None -> [
        fun_of selected (function
          | None -> []
          | Some n -> render_details_of_neuron n) ]
      | Some n -> render_details_of_neuron n)

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

  let field_selector col ~x ~y ~width ~height =
    let text_width = 40 in
    group [
      Widget.text ~x ~y ~width:text_width ~height "Field:" ;
      Widget.simple_select csv_fields col ~x:(x + text_width) ~y ~width:(width - text_width) ~height ]

  let lag_selector lag ~x ~y ~width ~height =
    let text_width = 40 in
    group [
      Widget.text ~x ~y ~width ~height "Lag:" ;
      Widget.int_select lag ~max:(Array.length csv.lines) ~x:(x + text_width) ~y ~width:(width - text_width) ~height ]

  let avg_selector avg ~x ~y ~width ~height =
    let text_width = 40 in
    group [
      Widget.text ~x ~y ~width ~height "Avg:" ;
      Widget.int_select avg ~max:(Array.length csv.lines) ~x:(x + text_width) ~y ~width:(width - text_width) ~height ]

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
      field_selector t.col ~x ~y:(y + height - 2 * Layout.text_line_height) ~width ~height:Layout.text_line_height ;
      lag_selector t.lag ~x ~y:(y + height - 3 * Layout.text_line_height) ~width:(width/2) ~height:Layout.text_line_height ;
      avg_selector t.avg ~x:(x + width/2) ~y:(y + height - 3 * Layout.text_line_height) ~width:(width/2) ~height:Layout.text_line_height ]

  let id_seq = ref 0

  let make_ id col lag avg =
    { id ;
      col = Param.make ~on_update:set_need_save "some IO CSV column" col ;
      lag = Param.make ~on_update:set_need_save "some IO CSV lag" lag ;
      avg = Param.make ~on_update:set_need_save "some IO CSV avg" avg }

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
    Widget.button ("Add a new "^ what) ~on_click:(fun _ _ -> Param.cons ios_param (make ())) ~x ~y:(y + height - Layout.text_line_height) ~width ~height:Layout.text_line_height

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
  let minibatch_steps = ref 0
  let nb_steps_update = Param.make "nb_steps should be refreshed" () (* TODO: Param.make ~update_every:16 ? *)
  let tot_err_history = 150
  let tot_err_graph = Param.make "total error graph" (Graph.make "Error" tot_err_history)

  (* inputs must be an array of input values, order by io_key. *)
  let forward_propagation () =
    let open Neuron in
    iter_only Input (fun n ->
      let io = find_io inputs.value n.io_key in
      let x, _curs = CSV.get_value csv io
      and extr = CSV.get_extremum csv io in
      let o = scale_input n.func extr x in
      (* Note that we do *not* go through the transfer function itself.
       * We just use its type to scale. If we did, than for sigmoids the
       * output would be 0..1 which would be biased right from the start.
       * So better not.
       * Therefore, set the transfer function of the input neurons just
       * to control the scaling. If you mix different transfer function
       * in your network you are on your own. *)
      n.output <- o
      ) ;
    iter_all (fun n ->
      (* If there is no dendrit keep current output (for biases and input neurons) *)
      if n.dendrits <> [] then
        n.output <-
          List.fold_left (fun s d ->
            s +. d.source.output *. d.weight
          ) 0. n.dendrits |>
          transfer n.func ;
      match hovered.value with
      | Some hn when hn == n ->
          Graph.push last_outputs.value n.output ;
          Param.change last_outputs
      | _ -> ())

  let accum_gradient n =
    let open Neuron in
    List.iter (fun d ->
        let de_dw = d.source.output *. n.dE_dOutput in
        d.gradient <- d.gradient +. de_dw ;
      ) n.dendrits

  let adjust_dendrits learn_speed momentum n =
    let open Neuron in
    List.iter (fun d ->
        let adj = momentum *. d.prev_weight_adj -. learn_speed *. d.gradient in
        d.weight <- d.weight +. adj ;
        d.prev_weight_adj <- adj ;
        d.gradient <- 0.
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

  (* [outputs] must be a float array with the output targets for
   * each output neuron, in io_key order. *)
  (* TODO: choose various methods (momentum, resilient back prop...
   * see https://en.wikipedia.org/wiki/Rprop *)
  let back_propagation () =
    let open Neuron in
    (* Begin with setting the dE_dOutput of the output nodes.
     * While at it, also save the prediction in the CSV. *)
    let tot_err, _ =
      fold_only Output (fun (e, i) n ->
        let io = find_io outputs.value n.io_key in
        let t, cursor = CSV.get_value csv io
        and extr = CSV.get_extremum csv io in
        (* Same remark as in forward_propagation regarding the
         * transfer function of output neurons. *)
        let r = scale_output_rev n.func extr n.output in
        n.dE_dOutput <- (r -. t) /. (snd extr -. fst extr) ;
        assert (Float.compare nan n.dE_dOutput <> 0) ;
        CSV.predict csv cursor r ;
        e +. 0.5 *. sq n.dE_dOutput,
        i + 1
      ) (0., 0) in
    (* Now we can back-propagate to the hidden layer *)
    iter_back_but Output propagate_err_backward ;
    (* Accumulate the gradient *)
    iter_all accum_gradient ;
    (* Returns the total error *)
    tot_err

  let adjust_weights learn_speed momentum =
      let open Neuron in
      iter_all (adjust_dendrits learn_speed momentum)

  let learn_speed = Param.make "learning speed" 0.03
  let momentum = Param.make "momentum" 0.
  let minibatch_size = Param.make "minibatch size" 1

  let step () =
    if is_running.value then (
      CSV.next csv ;
      forward_propagation () ;
      let tot_err = back_propagation () in

      incr minibatch_steps ;
      if !minibatch_steps >= minibatch_size.value then (
        adjust_weights learn_speed.value momentum.value ;
        minibatch_steps := 0) ;
      Graph.push tot_err_graph.value tot_err ;

      Param.change tot_err_graph ;
      Neuron.touch_hovered () ;
      set_need_save () ;
      decr rem_steps ;
      incr nb_steps ;
      if !rem_steps = 0 then Param.set is_running false ;
      (* Refresh the dendrits width from time to time: *)
      if !rem_steps land 15 = 0 then (
        Param.change Neuron.neurons ;
        Param.change nb_steps_update))
end

module LoadSave =
struct
  type t =
    { io_id_seq : int [@ppp_default 0] ;
      neuron_id_seq : int [@ppp_default 0] ;
      nb_steps : int [@ppp_default 0] ;
      minibatch_size : int [@ppp_default 1] ;
      momentum : float [@ppp_default 0.] ;
      learn_speed : float [@ppp_default 0.03] ;
      inputs : IO.serialized_io list ;
      outputs : IO.serialized_io list ;
      neurons : Neuron.serialized_neuron array } [@@ppp PPP_JSON]

  let make () =
    { io_id_seq = !IO.id_seq ;
      neuron_id_seq = !Neuron.id_seq ;
      nb_steps = !Simulation.nb_steps ;
      minibatch_size = Simulation.minibatch_size.value ;
      momentum = Simulation.momentum.value ;
      learn_speed = Simulation.learn_speed.value ;
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
    Param.set Simulation.minibatch_size t.minibatch_size ;
    Param.set Simulation.learn_speed t.learn_speed ;
    Param.set Simulation.momentum t.momentum ;
    Param.set inputs (List.map IO.unserialize t.inputs) ;
    Param.set outputs (List.map IO.unserialize t.outputs) ;
    let neurons = Array.map Neuron.unserialize0 t.neurons in
    Array.iter2 (Neuron.unserialize_dendrits neurons) neurons t.neurons ;
    Array.iter2 (Neuron.unserialize_axons neurons) neurons t.neurons ;
    Param.set Neuron.neurons neurons

  let file_prefix = csv.name ^".save."

  let save _ _ =
    let fname n = file_prefix ^ string_of_int n
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
        Param.change Layout.neural_net_height (* trigger the rearrangement of neurons *)))
end

let render_result_controls ~x ~y ~width ~height =
  let learn_speed_options =
    [| 0.00001, "0.00001" ; 0.0001, "0.0001" ; 0.001, "0.001" ;
       0.003, "0.003" ; 0.01, "0.01" ; 0.03, "0.03" ; 0.1, "0.1" ;
       0.3, "0.3" ; 1., "1" ; 3., "3" ; 10., "10" |]
  and momentum_options =
    [| 0., "0" ; 0.1, "0.1" ; 0.5, "0.5" ;
       0.9, "0.9" ; 0.95, "0.95" ; 0.99, "0.99" |]
  and minibatch_options =
    [| 1, "1" ; 10, "10" ; 100, "100" ; 1_000, "1,000" ;
       10_000, "10,000" ; 100_000_000, "100,000,000" |]
  and label_w = 90 in [
  fun_of need_save (fun need_save ->
    if need_save then
      [ Widget.button "Save" ~on_click:LoadSave.save ~x ~y:(y + height - 1 * Layout.text_line_height) ~width:(width / 2) ~height:Layout.text_line_height ]
    else []) ;
  Widget.button "Load" ~on_click:LoadSave.load ~x:(width / 2) ~y:(y + height - 1 * Layout.text_line_height) ~width:(width / 2) ~height:Layout.text_line_height ;
  fun_of Simulation.is_running (fun is_running ->
    let run_for n _ _ =
      Param.set Simulation.is_running (n > 0) ;
      Simulation.rem_steps := n
    and reset shifted _ =
      if shifted then (
        Neuron.iter_all (fun n ->
          List.iter (fun d ->
            d.Neuron.weight <- Neuron.random_weight () ;
            d.Neuron.prev_weight_adj <- 0. ;
            d.Neuron.gradient <- 0.
          ) n.dendrits) ;
        Graph.reset_param Neuron.last_outputs ;
        Graph.reset_param Simulation.tot_err_graph ;
        Param.change Neuron.neurons ;
        CSV.reset csv ;
        Simulation.nb_steps := 0 ;
        Param.change Simulation.nb_steps_update)
    and y = y + height - 2 * Layout.text_line_height and height = Layout.text_line_height in
    if is_running then [
      Widget.button "Pause" ~on_click:(run_for 0) ~x ~y ~width ~height
    ] else [
        Widget.button "Run" ~on_click:(run_for max_int) ~x ~y ~width:(width/3) ~height ;
        Widget.button "Step" ~on_click:(run_for 1) ~x:(width/3) ~y ~width:(width/3) ~height ;
        Widget.button "Reset!" ~on_click:reset ~x:(2 * width/3) ~y ~width:(width/3) ~height ]) ;
  fun_of Simulation.nb_steps_update (fun _ -> [
    Widget.text ("Steps: "^ string_of_int !Simulation.nb_steps) ~x ~y:(y + height - 3 * Layout.text_line_height) ~width ~height:Layout.text_line_height ]) ;
  Widget.text "Minibatches:" ~x ~y:(y + height - 4 * Layout.text_line_height) ~width:label_w ~height:Layout.text_line_height ;
  Widget.simple_select minibatch_options Simulation.minibatch_size ~x:(x + label_w) ~y:(y + height - 4 * Layout.text_line_height) ~width:(width - label_w) ~height:Layout.text_line_height ;
  Widget.text "Learn.Rate:" ~x ~y:(y + height - 5 * Layout.text_line_height) ~width:label_w ~height:Layout.text_line_height ;
  Widget.simple_select learn_speed_options Simulation.learn_speed ~x:(x + label_w) ~y:(y + height - 5 * Layout.text_line_height) ~width:(width - label_w) ~height:Layout.text_line_height ;
  Widget.text "Momentum:" ~x ~y:(y + height - 6 * Layout.text_line_height) ~width:label_w ~height:Layout.text_line_height ;
  Widget.simple_select momentum_options Simulation.momentum ~x:(x + label_w) ~y:(y + height - 6 * Layout.text_line_height) ~width:(width - label_w) ~height:Layout.text_line_height ]

let render_results ~x ~y ~width ~height =
  fun_of Layout.control_column_width (fun control_width -> [
    group (render_result_controls ~x ~y ~width:control_width ~height) ;
    let x = x + control_width and width = width - control_width in
    let render_tot_err () =
      fun_of Simulation.tot_err_graph (fun graph ->
        [ Graph.render graph ~x ~y ~width ~height ])
    and render_predictions n =
      fun_of Simulation.nb_steps_update (fun () -> [
        let output = find_io outputs.value n.Neuron.io_key in
        fun_of output.col (fun col -> [
          fun_of output.avg (fun avg -> [
            if avg <> 0 then Widget.todo ~x ~y ~width ~height
            else CSV.render ~col csv ~x ~y ~width ~height ]) ]) ])
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
    Ogli_render.init ~title:"NeuralNet Test" ~font:(dir ^"vera.ttf") ~double_buffer ~msaa width height ;
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
