open Seq 

let is_digit digit =
  match digit with
   '0' .. '9' -> true
  | _ -> false 

(* Should be in Seq, but can't find it, re doing as exercise*)
let rec find f lst  = 
  match lst () with
  | Nil -> None
  | Cons (hd, tl) -> if f hd then Some hd
            else find f tl
let rec reverse_rec lst acc  = 
  match lst () with
  | Nil -> acc
  | Cons (hd, tl) -> reverse_rec tl (Cons (hd, fun () -> acc )) 

  let reverse lst = fun () -> reverse_rec lst Nil  

let find_first_digit str = find is_digit str 

let calibration_value str = 
    let first_digit = String.make 1 (Option.fold ~none:'0' ~some:Fun.id (find_first_digit str)) 
    in let last_digit = String.make 1 (Option.fold ~none:'0' ~some:Fun.id (find_first_digit (reverse str)) )
  in let digits = String.cat first_digit last_digit in int_of_string digits