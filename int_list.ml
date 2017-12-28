type 'a pointer = NULL | Pointer of 'a ref

let (!^) = function
  | NULL -> invalid_arg "deref failed"
  | Pointer r -> !r

let (^:=) l r =
  match l with
  | NULL -> invalid_arg "assign ref failed"
  | Pointer l -> l := r

let get_pointer v = Pointer(ref v)

type nodeptr = cell pointer
and cell = {mutable v : int; mutable next : nodeptr}

let new_cell () = {v = 0; next = NULL}
let concat v lst =
  let c = new_cell () in
  c.v <- v;
  c.next <- lst;
  get_pointer c

let getValue (lst : nodeptr) = (!^lst).v
let getNext (lst : nodeptr) = (!^lst).next

let listConcat (l1 : nodeptr) (l2 : nodeptr) =
  let t = ref l1 in
  while getNext !t <> NULL do
    t := getNext !t
  done;
  (!^(!t)).next <- l2

let l1 = concat 1 (concat 2 NULL)
let l2 = concat 3 (concat 4 (concat 5 NULL));;

