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

let l = [ 1; 2; 3; 10 ]

let ll = List.map (fun x -> Some x) l;;

flatten (Many [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ]);;