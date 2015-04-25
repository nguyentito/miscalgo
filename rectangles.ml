(* Solution to http://prologin.org/training/challenge/demi2011/rectangles *)

let (@@) f x = f x
let (|>) x f = f x

type rect = { x1 : int ;
              y1 : int ;
              x2 : int ;
              y2 : int }

type quadtree = EmptyLeaf
              | FullLeaf
              | Node of quadtree * quadtree * quadtree * quadtree

let rec insert height rect = function
  | FullLeaf -> FullLeaf
  | x when rect.x1 >= rect.x2 || rect.y1 >= rect.y2 -> x
  | _ when rect = { x1 = 0 ; y1 = 0 ; x2 = 1 lsl height ; y2 = 1 lsl height } -> FullLeaf
  | EmptyLeaf -> insert height rect @@ Node (EmptyLeaf, EmptyLeaf, EmptyLeaf, EmptyLeaf)
  | Node (tl, tr, bl, br) ->
    let h = height - 1 in
    let mid = 1 lsl h in
    match (insert h { rect with x2 = min rect.x2 mid ; y2 = min rect.y2 mid } tl,
           insert h { rect with x1 = max 0 (rect.x1-mid) ; x2 = rect.x2 - mid ; y2 = min rect.y2 mid } tr,
           insert h { rect with x2 = min rect.x2 mid ; y1 = max 0 (rect.y1-mid) ; y2 = rect.y2 - mid } bl,
           insert h { x1 = max 0 (rect.x1-mid) ; y1 = max 0 (rect.y1-mid)
                    ; x2 = rect.x2 - mid ; y2 = rect.y2 - mid} br) with 
    | FullLeaf, FullLeaf, FullLeaf, FullLeaf -> FullLeaf
    | tl', tr', bl', br' -> Node (tl', tr', bl', br')

let rec area height = function
  | EmptyLeaf -> 0
  | FullLeaf -> 1 lsl (2 * height)
  | Node (tl, tr, bl, br) ->
    [tl ; tr ; bl ; br]
    |> List.map (area (height-1))
    |> List.fold_left (+) 0

let solution (n : int) (rects : rect list) = 
  let max_height = 14 in
  let qt = List.fold_left (fun qt r -> insert max_height r qt) EmptyLeaf rects in
  Printf.printf "%d\n" (area max_height qt)

let () =
  let n = int_of_string (read_line ()) in
  let rec _f = function
    |  (_1::_2::_3::_4::_q) ->
       { x1 = _1 ; y1 = _2; x2 = _3 ; y2 = _4; } :: _f _q
    | _ -> []
  in
  let coords = Array.init (4*n) (fun _ -> Scanf.scanf "%d " (fun x -> x)) in
  let rects = coords |> Array.to_list |> _f in
(*
  let rects = 
    read_line ()
    |> Str.split (Str.regexp "[ ]")
    |> List.map int_of_string
    |> _f
  in
*)
  solution n rects

