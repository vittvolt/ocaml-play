let rec sum lst =
  match lst with 
  | [] -> 0
  | h::t -> h + (sum t)

let rec last lst =
  match lst with
  | [] -> None
  | h::[] -> Some h
  | h::r -> last r

let l = [ 1; 2; 3; 10 ]

let r = last l

let () = 
  match r with
  | None -> Printf.printf "None...\n"
  | Some k -> Printf.printf "%d\n" k