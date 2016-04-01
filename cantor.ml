type 'a sequence = int -> 'a

let cons : 'a -> 'a sequence -> 'a sequence = fun x u ->
  function
  | 0 -> x
  | i -> u (i-1)

type cantor = bool sequence
           
let rec find : (cantor -> bool) -> cantor = fun p ->
  let u0 = try_first_bit false p in
  if p u0 then u0
  else try_first_bit true p
and try_first_bit : bool -> (cantor -> bool) -> cantor = fun b p ->
  cons b (fun i -> find (fun u -> p (cons b u)) i)

let forall : (cantor -> bool) -> bool = fun p ->
  p (find (fun u -> not (p u)))
       
let least p =
  let rec aux n = if p n then n else aux (n+1) in
  aux 0

let rec eq_prefix n u v = match n with
  | 0 -> true
  | n -> u 0 = v 0 && eq_prefix (n-1) (fun i -> u (i+1)) (fun j -> v (j+1))

let modulus : (cantor -> 'a) -> int = fun f ->
  least (fun n ->
         forall (fun u ->
                 forall (fun v ->
                         not (eq_prefix n u v) || f u = f v)))
                                  
      
let forty_two : cantor -> bool = fun u ->
  (u 0, u 1, u 2, u 3, u 4, u 5) >= (true, false, true, false, true, false)

let simple : cantor -> bool = fun u ->
  (u 0, u 1, u 2) >= (true, false, true)

let ultra_simple : cantor -> bool = fun u ->
  u 0 = false
                  

                                      
