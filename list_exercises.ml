(* My implementation for the 99 Ocaml exercises, list part *)

let rec sum lst =
  match lst with 
  | [] -> 0
  | h::t -> h + (sum t)

let rec last lst =
  match lst with
  | [] -> None
  | h::[] -> Some h
  | h::r -> last r

let rec last_two lst =
  match lst with
  | [] -> None
  | [_] -> None
  | [a; b] -> Some (a, b)
  | a::b::r -> last_two (b::r)

let findAt index lst =
  if List.length lst <= index then None
  else
    List.nth lst index

let rec length lst =
  let rec findLen lst len =
    match lst with
    | [] -> len
    | h::r -> findLen r len + 1
  in
  findLen lst 0

let rev lst =
  let rec reverse lst result =
    match lst with
    | [] -> result
    | h::r -> reverse r (h::result)
  in
  reverse lst []

let is_palindrome lst =
  lst = List.rev lst

type 'a node = 
  | One of 'a
  | Many of 'a node list

let flatten lst =
  let rec f lst result =
    match lst with
    | One o -> o::result
    | Many n -> (
      match n with
      | [] -> result
      | h::r -> (f h []) @ (f (Many r) result)
    )
  in
  f lst []

(* #8 compress *)
let compress lst =
  let rec get lst prev result =
    match lst with
    | [] -> result
    | h::r -> (
      if h = prev then get r prev result
      else get r h (h::result)
    )
  in
  match lst with
  | [] -> []
  | h::r -> List.rev (get r h [h])

(* #9 pack *)
let pack lst =
  let rec f lst sublst result =
    match lst with
    | a::((b::r) as k) -> (
      if a = b then f k (a::sublst) result
      else (
        let temp = a::sublst in
        f k [] (temp::result)
      )
    )
    | [k] -> let temp = k::sublst in temp::result
    | [] -> sublst::result
  in
  List.rev (f lst [] [])

(* #10, #11 encode *)
let encode lst = 
  let rec f lst subcount result =
    match lst with
    | a::((b::r) as k) -> (
      if a = b then f k (subcount + 1) result
      else (
        let temp = (subcount, a) in
        f k 1 (temp::result)
      )
    )
    | [k] -> let temp = (subcount, k) in temp::result
    | [] -> result
  in
  List.rev (f lst 1 [])

type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let encode_rle lst =
  let rec f lst subcount result =
    match lst with
    | a::((b::r) as k) -> (
      if a = b then f k (subcount + 1) result
      else (
        if subcount = 1 then f k 1 ((One a)::result)
        else (
          let temp = Many (subcount, a) in
          f k 1 (temp::result)
        )
      )
    )
    | [k] -> (
      if subcount = 1 then (One k)::result
      else (
        let temp = Many (subcount, k) in
        temp::result
      )
    )
    | [] -> result
  in
  List.rev (f lst 1 [])

(* #12 decode *)
let decode_rle lst =
  let rec n_elem elem n r =
    if n > 0 then n_elem elem (n - 1) (elem::r)
    else r
  in
  let rec f lst result =
    match lst with
    | [] -> result
    | (One o)::r -> f r (o::result)
    | (Many (count, e))::r -> f r (n_elem e count result)
  in
  List.rev (f lst [])

let duplicate lst = 
  let rec f lst result =
    match lst with
    | h::r -> f r (h::h::result)
    | [] -> result
  in 
  List.rev (f lst [])



let l = [ 1; 2; 3; 10 ]

let ll = List.map (fun x -> Some x) l;;

flatten (Many [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ]);;