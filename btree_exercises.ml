type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

(* #55 *)
let rec cbal_tree num =
  let helper trees1 trees2 result =
    let f acc elem =
      List.fold_left (fun a e -> (Node('x', e, elem))::a) acc trees2
    in
    List.fold_left f result trees1
  in
  if num = 0 then [Empty]
  else if num mod 2 = 1 then
    let trees = cbal_tree (num / 2) in
    helper trees trees []
  else
    let trees1 = cbal_tree (num / 2) in
    let trees2 = cbal_tree (num / 2 - 1) in
    helper trees1 trees2 (helper trees2 trees1 [])
