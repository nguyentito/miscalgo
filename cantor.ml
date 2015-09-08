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

let forty_two : cantor -> bool = fun u ->
  (u 0, u 1, u 2, u 3, u 4, u 5) >= (true, false, true, false, true, false)
