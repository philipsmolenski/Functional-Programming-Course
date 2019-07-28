(* Autor: Philip Smolenski-Jensen *)
(* Code review: Konrad Owsikowski *)
(* Sortowanie topologiczne *)

open List

(* typ mapy z treści zadania *)
type ('k, 'v) map =
  | Empty
  | Node of ('k, 'v) map * 'k * 'v * ('k, 'v) map * int

type ('k, 'v) t =
  {
    cmp : 'k -> 'k -> int;
    map : ('k, 'v) map;
  }

let height = function
  | Node (_, _, _, _, h) -> h
  | Empty -> 0

let make l k v r = Node (l, k, v, r, max (height l) (height r) + 1)

let bal l k v r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lv, lr, _) ->
        if height ll >= height lr then make ll lk lv (make lr k v r)
        else
          (match lr with
          | Node (lrl, lrk, lrv, lrr, _) ->
              make (make ll lk lv lrl) lrk lrv (make lrr k v r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rv, rr, _) ->
        if height rr >= height rl then make (make l k v rl) rk rv rr
        else
          (match rl with
          | Node (rll, rlk, rlv, rlr, _) ->
              make (make l k v rll) rlk rlv (make rlr rk rv rr)
          | Empty -> assert false)
    | Empty -> assert false
  else Node (l, k, v, r, max hl hr + 1)

let rec min_binding = function
  | Node (Empty, k, v, _, _) -> k, v
  | Node (l, _, _, _, _) -> min_binding l
  | Empty -> raise Not_found

let rec remove_min_binding = function
  | Node (Empty, _, _, r, _) -> r
  | Node (l, k, v, r, _) -> bal (remove_min_binding l) k v r
  | Empty -> invalid_arg "PMap.remove_min_binding"

let merge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let k, v = min_binding t2 in
      bal t1 k v (remove_min_binding t2)

let create cmp = { cmp = cmp; map = Empty }
let empty = { cmp = compare; map = Empty }

let is_empty x = 
	x.map = Empty

let add x d { cmp = cmp; map = map } =
  let rec loop = function
    | Node (l, k, v, r, h) ->
        let c = cmp x k in
        if c = 0 then Node (l, x, d, r, h)
        else if c < 0 then
          let nl = loop l in
          bal nl k v r
        else
          let nr = loop r in
          bal l k v nr
    | Empty -> Node (Empty, x, d, Empty, 1) in
  { cmp = cmp; map = loop map }

let find x { cmp = cmp; map = map } =
  let rec loop = function
    | Node (l, k, v, r, _) ->
        let c = cmp x k in
        if c < 0 then loop l
        else if c > 0 then loop r
        else v
    | Empty -> raise Not_found in
  loop map

let remove x { cmp = cmp; map = map } =
  let rec loop = function
    | Node (l, k, v, r, _) ->
        let c = cmp x k in
        if c = 0 then merge l r else
        if c < 0 then bal (loop l) k v r else bal l k v (loop r)
    | Empty -> Empty in
  { cmp = cmp; map = loop map }

let mem x { cmp = cmp; map = map } =
  let rec loop = function
    | Node (l, k, v, r, _) ->
        let c = cmp x k in
        c = 0 || loop (if c < 0 then l else r)
    | Empty -> false in
  loop map

let exists = mem

let iter f { map = map } =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, v, r, _) -> loop l; f k v; loop r in
  loop map

let map f { cmp = cmp; map = map } =
  let rec loop = function
    | Empty -> Empty
    | Node (l, k, v, r, h) -> 
	  let l = loop l in
	  let r = loop r in
	  Node (l, k, f v, r, h) in
  { cmp = cmp; map = loop map }

let mapi f { cmp = cmp; map = map } =
  let rec loop = function
    | Empty -> Empty
    | Node (l, k, v, r, h) ->
	  let l = loop l in
	  let r = loop r in
	  Node (l, k, f k v, r, h) in
  { cmp = cmp; map = loop map }

let fold f { cmp = cmp; map = map } acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, v, r, _) ->
	  loop (f v (loop acc l)) r in
  loop acc map

let foldi f { cmp = cmp; map = map } acc =
  let rec loop acc = function
    | Empty -> acc
	| Node (l, k, v, r, _) ->
       loop (f k v (loop acc l)) r in
  loop acc map;;

(* Wyjątek podnoszony, gdy lista jest cykliczna *)
exception Cykliczne

(* Sortowanie topologiczne *)
let topol l =
  (* Pusta mapa to mapa,
   w której każdy wierzchołek połączony jest z pustą listą *) 
  let pusta_mapa = fold_left 
  (fun a x -> (fold_left (fun a y -> add y [] a) a (snd x))) empty l in
  (* Do otrzymanej mapy wrzucamy krawędzie, łącząc każdy wierzchołek 
     z listą  sąsiadów (wierzchołki, z których nic nie wychodzi dalej połaczone
     będą z pustą listą) *)
  let krawedzie = fold_left (fun a (x, y) -> add x y a) pusta_mapa l
  (* wyn jest tablicą, w której akumulujemy wynik *)
  in let wyn = ref [] 
  (* Przetworzone to mapa, która dla każdego wierzchołka informuje,
  czy jeszcze do niego nie dotarliśmy (0), czy właśnie go przetwarzamy (1),
  czy skonczylismy go przetwarzac (2) *)
  and przetworzone = 
  ref (foldi (fun x _ a -> add x (0) a)  krawedzie empty)
  in
  begin
    let rec dfs x = 
    if (find x !przetworzone) = 0 then
    begin
      przetworzone := add x 1 (!przetworzone);
      List.iter (fun y -> begin
        if ((find y (!przetworzone)) = 1) then
          raise Cykliczne;
	      if ((find y (!przetworzone)) = 0) then
	         dfs y;
    end) (find x krawedzie);
      przetworzone := (add x 2 (!przetworzone));
      wyn := x::(!wyn);
  end
  in 
  List.iter ( fun x -> dfs (fst x) ) l;
  !wyn
end
