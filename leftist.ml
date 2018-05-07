(*  Zadanie 2: Leftist          *)
(*  autor: Kamil Dubil, 370826  *)
(*  reviewer: Agata Lonc        *)


(* Typ złączalnej kolejki priorytetowej                *)
(* implementacja za pomocą drzewa lewicowego           *)
(* Null - puste drzewo                                 *)
(* Node (lewe poddrzewo, wartość (priorytet) korzenia, *)
(* prawe poddrzewo, prawa wysokość)                    *)
type 'a queue =
    | Node of 'a queue * 'a * 'a queue * int
    | Null

(* Wyjątek podnoszony przez [delete_min] gdy kolejka jest pusta *)
exception Empty

(* Pusta kolejka priorytetowa *)
let empty = Null

(* zwraca prawą wysokość drzewa *)
let right_path_length q =
    match q with
        | Null -> -1
        | Node(_, _, _, h) -> h

(* [join q1 q2] zwraca złączenie kolejek [q1] i [q2] *)
let rec join q1 q2 =
    match (q1, q2) with
        | (Null, _) -> q2
        | (_, Null) -> q1
        | (Node(_, e1, _, _), Node(_, e2, _, _)) when e1 > e2 -> join q2 q1
        | (Node(l1, e1, r1, _), _) ->
            let r = join r1 q2
            in if right_path_length r <= right_path_length l1
                then Node(l1, e1, r, (right_path_length r) + 1)
                else Node(r, e1, l1, (right_path_length l1) + 1)

(* [add e q] zwraca kolejkę powstałą z dołączenia elementu [e] do kolejki [q] *)
let add elem q =
    let q2 = Node(Null, elem, Null, 0)
    in join q q2

(* Zwraca [true] jeśli dana kolejka jest pusta. W przeciwnym razie [false] *)
let is_empty q =
    q=Null

(* Dla niepustej kolejki [q], [delete_min q] zwraca parę [(e,q')] gdzie [e]
jest elementem minimalnym kolejki [q] a [q'] to [q] bez elementu [e].
Jeśli [q] jest puste podnosi wyjątek [Empty]. *)
let delete_min q =
    match q with
        | Null -> raise Empty
        | Node(l, e, r, _) -> (e, join l r)

(*****************************************************************************)

(* testy *)

(*
open Leftist;;

(* simple tests *)
let a = empty;;
let b = add 1 empty;;

assert (is_empty a = true);;
assert (try let _=delete_min a in false with Empty -> true);;
assert (is_empty b <> true);;

let b = join a b ;;
assert (is_empty b <> true);;

let (x,y) = delete_min b;;

assert (x = 1);;
assert (is_empty y = true);;
assert (try let _=delete_min y in false with Empty -> true);;

(* delete_min integer tests *)
let b = add 1 empty;;
let b = add 3 b;;
let b = add (-1) b;;
let b = add 2 b;;
let b = add 1 b;;

let (a,b) = delete_min b;;
assert (a = -1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 2);;

let (a,b) = delete_min b;;
assert (a = 3);;

assert(is_empty b = true);;

(* delete_min string tests *)
let b = add "a" empty;;
let b = add "aca" b;;
let b = add "nzbzad" b;;
let b = add "nzbza" b;;
let b = add "bxbxc" b;;

let (a,b) = delete_min b;;
assert (a = "a");;

let (a,b) = delete_min b;;
assert (a = "aca");;

let (a,b) = delete_min b;;
assert (a = "bxbxc");;

let (a,b) = delete_min b;;
assert (a = "nzbza");;

let (a,b) = delete_min b;;
assert (a = "nzbzad");;

assert(is_empty b = true);;
assert (try let _=delete_min b in false with Empty -> true);;

(* join tests *)

let b = add 1 empty;;
let b = add 3 b;;
let b = add (-1) b;;
let b = add 2 b;;
let b = add 1 b;;

let c = add 10 empty;;
let c = add (-5) c;;
let c = add 1 c;;
let c = add 4 c;;
let c = add 0 c;;

let b = join b c;;

let (a,b) = delete_min b;;
assert (a = (-5));;

let (a,b) = delete_min b;;
assert (a = (-1));;

let (a,b) = delete_min b;;
assert (a = 0);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 2);;

let (a,b) = delete_min b;;
assert (a = 3);;

let (a,b) = delete_min b;;
assert (a = 4);;

let (a,b) = delete_min b;;
assert (a = 10);;

assert (try let _=delete_min b in false with Empty -> true);;

let b = add 1 empty;;
let b = add 3 b;;
let b = add (-1) b;;
let b = add 2 b;;
let b = add 1 b;;

let c = add 10 empty;;
let c = add (-5) c;;
let c = add 1 c;;
let c = add 4 c;;
let c = add 0 c;;

let b = join c b;;

let (a,b) = delete_min b;;
assert (a = (-5));;

let (a,b) = delete_min b;;
assert (a = (-1));;

let (a,b) = delete_min b;;
assert (a = 0);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 2);;

let (a,b) = delete_min b;;
assert (a = 3);;

let (a,b) = delete_min b;;
assert (a = 4);;

let (a,b) = delete_min b;;
assert (a = 10);;

assert (try let _=delete_min b in false with Empty -> true);;

let test a b num msg =
  if a = b then print_endline "ok"
  else (print_int num; print_endline msg);; 

let rec zwin l q num msg =
  try
    match l with
    | [] -> test q empty num msg
    | h::t -> let (mn,r) = delete_min q in test mn h num msg; zwin t r (num+1) msg
  with Empty -> (print_int num; print_string "Empty"; print_endline msg);;

let a = add 0. empty;;        (* 0.*)
let b = add 1. empty;;        (* 1. *)
let c = add (-0.1) empty;;    (* -0.1 *)
let d = add 7. a;;            (* 0., 7. *)
let e = add (-3.) d;;         (* -3., 0., 7. *)
let f = add (-0.5) c;;        (* -0.5, -0.1 *)
let g = join b c;;            (* -0.1, 1.*)
let h = join d e;;            (* -3., 0., 0., 7., 7. *)
let i = join f e;;            (* -3., -0.5, -0.1, 0., 7. *)
let j = join h i;;            (* -3., -3., -0.5, -0.1, 0., 0., 0., 7., 7., 7. *)

let la = [0.];;
let lb = [1.];;
let lc = [-0.1];;
let ld = la @ [7.];;
let le = -3.::ld;;
let lf = -0.5::lc;;
let lg = lc @ lb;;
let lh = [-3.; 0.; 0.; 7.; 7.];;
let li = [-3.; -0.5; -0.1; 0.; 7.];;
let lj = [-3.; -3.; -0.5; -0.1; 0.; 0.; 0.; 7.; 7.; 7.];;

test (join empty empty) empty (-1) ": empty + empty";;
zwin la a 0 ": a";;
zwin lb b 0 ": b";;
zwin lc c 0 ": c";;
zwin ld d 0 ": d";;
zwin le e 0 ": e";;
zwin lf f 0 ": f";;
zwin lg g 0 ": g";;
zwin lh h 0 ": h";;
zwin li i 0 ": i";;
zwin lj j 0 ": j";;


exception WA;;

(* Returns true if ALL values from q are taken out in the order given in l *)
let test q l =
    try
      let (b, nq) = List.fold_left (fun a x -> 
        let (e, nq) = delete_min (snd a)
        in 
        if(compare x e != 0) then raise WA 
        else (true, nq)) 
                                   (true, q) l
      in
      b && (is_empty nq)
    with WA -> false
;;

(* Adds iter times all elements from list l to a using add function f *)
let rec add_lots f a l iter =
  if iter = 0 then a
  else
    add_lots f (List.fold_left (fun q x -> f x q) a l) l (iter - 1)

(* Size of a queue *)
let size q =
  let rec aux q a =
    if is_empty q then a
    else aux (snd (delete_min q)) (a + 1)
  in
  aux q 0

(* Wrapper for (::) *)
let app h t = h::t
		   
(* Integer correctness tests *)

let q1 = empty |> add 3 |> add 5 |> add 10 |> add 2 |> add 2 |> add 7 |> add 22
let q2 = empty |> add 1 |> add 2 |> add 3 |> add 4 |> add 5

let q3 = join q1 q2
let q4 = join q3 q3

let l1 = List.sort compare [3; 5; 10; 2; 2; 7; 22]
let l2 = List.sort compare [1; 2; 3; 4; 5]
let l3 = List.sort compare (l1 @ l2)
let l4 = List.sort compare (l3 @ l3);;

assert(is_empty empty);;

assert(test q1 l1);;
assert(test q2 l2);;
assert(test q3 l3);;
assert(test q4 l4);;
assert(not(test q4 l3));;
assert(not(test q3 l4));;
assert(test empty []);;
assert(try test empty l1 with Empty -> true);;

(* Integer performance tests *)

let q5 = add_lots add empty [1] 50000
let q6 = add_lots add q5 [2] 50000
let q7 = add_lots add empty [1; 2; 4; 5; 8] 20000
let q8 = join (join q5 q5) (join q6 q6)
let q9 = join (join q5 q6) (join q7 q8)

let l5a = add_lots app [] [1] 50000
let l5b = 1::l5a
let l5c = List.tl(l5a)
let l6 = l5a @ (add_lots app [] [2] 50000)
let l7 = List.sort compare (add_lots app [] [1; 2; 4; 5; 8] 20000)
let l8 = List.sort compare (l5a @ (l5a @ (l6 @ l6)))
let l9 = List.sort compare (l5a @ l6 @ l7 @ l8);;

assert(test q5 l5a);;
assert(try test q5 l5b with Empty -> true);;
assert(not(test q5 l5c));;
assert(test q6 l6);;
assert(test q7 l7);;
assert(test q8 l8);;
assert(test q9 l9);;

(* Float correctness tests *)

let q1 = empty |> add 2.3 |> add 1.1 |> add 3.2 |> add 0.01 |> add 222.1 |> add 42.42 |> add 1.03
let q2 = empty |> add 1.5 |> add 2.4 |> add 3.3 |> add 4.2 |> add 5.1

let q3 = join q1 q2
let q4 = join q3 q3

let l1 = List.sort compare [2.3; 1.1; 3.2; 0.01; 222.1; 42.42; 1.03]
let l2 = List.sort compare [1.5; 2.4; 3.3; 4.2; 5.1]
let l3 = List.sort compare (l1 @ l2)
let l4 = List.sort compare (l3 @ l3);;

assert(is_empty empty);;

assert(test q1 l1);;
assert(test q2 l2);;
assert(test q3 l3);;
assert(test q4 l4);;
assert(not(test q4 l3));;
assert(not(test q3 l4));;
assert(test empty []);;
assert(try test empty l1 with Empty -> true);;

(* Float performance tests *)

let q5 = add_lots add empty [-0.123] 50000
let q6 = add_lots add q5 [21.1] 50000
let q7 = add_lots add empty [-3.1; 22.1; -43.2; 115.0; 8.8] 20000
let q8 = join (join q5 q5) (join q6 q6)
let q9 = join (join q5 q6) (join q7 q8);;

let app h t = h::t;;

let l5a = add_lots app [] [-0.123] 50000;;
let l5b = (-0.123)::l5a;;
let l5c = List.tl(l5a);;
let l6 = l5a @ (add_lots app [] [21.1] 50000);;
let l7 = List.sort compare (add_lots app [] [-3.1; 22.1; -43.2; 115.0; 8.8] 20000);;
let l8 = List.sort compare (l5a @ (l5a @ (l6 @ l6)));;
let l9 = List.sort compare (l5a @ l6 @ l7 @ l8);;

assert(test q5 l5a);;
assert(try test q5 l5b with Empty -> true);;
assert(not(test q5 l5c));;
assert(test q6 l6);;
assert(test q7 l7);;
assert(test q8 l8);;
assert(test q9 l9);;

(* String correctness tests *)

let q1 = empty |> add "abba" |> add "acca" |> add "baba" |> add "abbabb" |>
add "aabbab" |> add "aaabbaa" |> add "1.23"
let q2 = empty |> add "aab" |> add "aba" |> add "abb" |> add "baa" |> add "bab"

let q3 = join q1 q2
let q4 = join q3 q3

let l1 = List.sort compare ["abba"; "acca"; "baba"; "abbabb"; "aabbab"; "aaabbaa"; "1.23"]
let l2 = List.sort compare ["aab"; "aba"; "abb"; "baa"; "bab"]
let l3 = List.sort compare (l1 @ l2)
let l4 = List.sort compare (l3 @ l3);;

assert(is_empty empty);;

assert(test q1 l1);;
assert(test q2 l2);;
assert(test q3 l3);;
assert(test q4 l4);;
assert(not(test q4 l3));;
assert(not(test q3 l4));;
assert(test empty []);;
assert(try test empty l1 with Empty -> true);;

(* String performance tests *)

let q5 = add_lots add empty [".24dz455"] 50000
let q6 = add_lots add q5 ["forty-two"] 50000
let q7 = add_lots add empty ["lorem"; "ipsum"; "dolor"; "sit"; "amet"] 20000
let q8 = join (join q5 q5) (join q6 q6)
let q9 = join (join q5 q6) (join q7 q8);;

let app h t = h::t;;

let l5a = add_lots app [] [".24dz455"] 50000;;
let l5b = (".24dz455")::l5a;;
let l5c = List.tl(l5a);;
let l6 = l5a @ (add_lots app [] ["forty-two"] 50000);;
let l7 = List.sort compare (add_lots app [] ["lorem"; "ipsum"; "dolor"; "sit"; "amet"] 20000);;
let l8 = List.sort compare (l5a @ (l5a @ (l6 @ l6)));;
let l9 = List.sort compare (l5a @ l6 @ l7 @ l8);;

assert(test q5 l5a);;
assert(try test q5 l5b with Empty -> true);;
assert(not(test q5 l5c));;
assert(test q6 l6);;
assert(test q7 l7);;
assert(test q8 l8);;
assert(test q9 l9);;

(* Other *)

let qe : int queue = empty;; 
let q1 = add qe empty;;
let q2 = add q1 empty;;
let q3 = add ([ []; [ [2] ]; [ [2; 5; 1]; [5; 11; 3] ]; [ [22; 22; 22] ] ]) empty;;
let q4 = add q3 empty;;
let q5 = qe |> join empty |> join empty |> join empty |> join empty;;
let q6 = add_lots add empty [q3] 500000;;

let l6 = add_lots app [] [q3] 500000;;

assert(is_empty qe);;
assert(not(is_empty q1));;
assert(not(is_empty q2));;
assert(not(is_empty q3));;
assert(not(is_empty q4));;
assert(is_empty q5);;
assert(test q6 l6);;

*)
