(* Implémentation en OCaml du codage de Huffman *)

type code_tree = CTLeaf of char * int
               | CTNode of char list * code_tree * code_tree * int

let ct_symbols = function
  | CTLeaf (c,_)    -> [c]
  | CTNode (cs,_,_,_) -> cs

let freq = function
  | CTLeaf (_,f) -> f
  | CTNode (_,_,_,f) ->f

let make_ct left right =
  CTNode ((ct_symbols left @ ct_symbols right), left, right, (freq left + freq right))

let make_leaf c f = CTLeaf (c, f)

let decode tree msg =
  let rec loop node = function
    | [] -> []
    | cur_bit :: next_bits ->
      let next_node = match (cur_bit, node) with
                        | (0, CTNode (_,left,_,_)) -> left
                        | (1, CTNode (_,_,right,_)) -> right
                        | _ -> failwith "invalid code"
      in
      match next_node with
        | CTLeaf (c,_) -> (c :: loop tree next_bits)
        | _ -> loop next_node next_bits
  in
  loop tree msg

let rec list_mem elt = function
  | x::xs -> x = elt || list_mem elt xs
  | [] -> false

let encode_symbol tree c =
  let rec loop = function
    | CTLeaf _ -> []
    | CTNode (_,left,right,_) ->
      if      list_mem c (ct_symbols left)  then 0 :: loop left
      else if list_mem c (ct_symbols right) then 1 :: loop right
      else failwith "the symbol to encode is not in the tree"
  in
  loop tree;;

let rec encode tree = function
  | [] -> []
  | c::cs -> encode_symbol tree c @ encode tree cs

type 'a minset = Minset of 'a list * ('a -> int)

let make_empty_minset weight = Minset ([], weight)

let adjoin_set elt (Minset (lst, weight)) =
  let rec add_elt_to_list = function
    | [] -> [elt]
    | x::xs -> if weight elt <= weight x
               then elt::x::xs
               else x :: add_elt_to_list xs
  in
  Minset (add_elt_to_list lst, weight)

let rec foldl f z = function
  | [] -> z
  | x::xs -> foldl f (f z x) xs

let flip f x y = f y x

let make_minset weight list =
  let set = make_empty_minset weight in
  foldl (flip adjoin_set) set list

let remove_min_weight (Minset (lst, weight)) =
  match lst with
    | [] -> failwith "empty minset"
    | x::xs -> (x, Minset (xs, weight))

let is_empty_set (Minset (lst,_)) = lst = []

let generate_huffman_tree sym_list =
  let leaf_set = make_minset freq (List.map (fun (x,y) -> CTLeaf (x,y)) sym_list) in
  let rec loop tree_set =
    let (min1, set1) = remove_min_weight tree_set in
    if is_empty_set set1 then min1
    else (let (min2, set2) = remove_min_weight set1 in
          let new_set = adjoin_set (make_ct min1 min2) set2 in
          loop new_set)
  in
  loop leaf_set
