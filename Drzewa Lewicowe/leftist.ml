(* Autor: Philip Smolenski-Jensen *)
(* Code rewiev: Krzysztof Małysa *)

(* Typ kolejki *)
type 'a queue = Null| Node of ('a queue * 'a * 'a queue * int)

(* Stworzenie pustej kolejki *)
let empty = Null

(* Procedura wyznaczająca prawą wysokość drzewa *)
let rh q =
    match q with
    | Null -> -1
    | Node (_, _, _, h) -> h

(* Łączenie dwóch kolejek tak, jak opisano w treści zadania *)
let rec join q1 q2 =
    match q1, q2 with
    | _, Null -> q1
    | Null, _ -> q2
    | Node (l1, x1, r1, h1), Node (l2, x2, r2, h2) ->
            if x2 <= x1 then 
                let t1 = join (Node (l1, x1, r1, h1)) r2
                in 
                    if (rh t1) > (rh l2) 
                        then Node (t1, x2, l2, (rh l2) + 1)
                    else Node (l2, x2, t1, (rh t1) + 1)
            else join q2 q1

(* Dodanie elementu e do kolejki q *)
let add e q =
    join (Node (Null, e, Null, 0)) q

(* Wyjątek podnoszony gdy kolejka jest pusta *)
exception Empty

(* Usunięcie najmniejszego elementu z kolejki *)
let delete_min q =
    match q with
    | Null -> raise Empty
    | Node (l, x, r, h) -> (x, join l r)

(* Sprawdzenie, czy kolejka jest pusta *)
let is_empty q = q = Null;;