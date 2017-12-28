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


    
let l = [ 1; 2; 3; 10 ];;

let ll = List.map (fun x -> Some x) l;;