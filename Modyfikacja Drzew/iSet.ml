(* Autor: Philip Smolenski - Jensen *)
(* Code rewiev: Michał Niciejewski *)

(* Typ seta to drzewo, w każdym wierzchołku trzymamy synów, przedział
    wysokość oraz rozmiar *)
type t = Empty | Node of t * (int * int) * t * int * int 

let height = function
  | Node (_, _, _, h, _) -> h
  | Empty -> 0

(* Oblicza rozmiar drzewa (łączną liczbę elementów w przedziałach) *)
let size = function
  | Node (_, _, _, _, q) -> q
  | Empty -> 0

let make l (k, n) r = Node (l, (k, n), r, max (height l) (height r) + 1,
                            (size l) + (size r) + n + 1 - k) 

let bal l (k, n) r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
        if height ll >= height lr then make ll lk (make lr (k, n) r)
        else
          (match lr with
          | Node (lrl, lrk, lrr, _, _) ->
              make (make ll lk lrl) lrk (make lrr (k, n) r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
        if height rr >= height rl then make (make l (k, n) rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _, _) ->
              make (make l (k, n) rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else Node (l, (k, n), r, max hl hr + 1, (size l) + (size r) + n + 1 - k)

let rec min_elt = function
  | Node (Empty, k, _, _, _) -> k
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> raise Not_found

let rec remove_min_elt = function
  | Node (Empty, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "iSet.remove_min_elt"

let rec max_elt = function
  | Node (_ , k, Empty, _, _) -> k
  | Node (_, _, r, _, _) -> max_elt r
  | Empty -> raise Not_found

let rec remove_max_elt = function
  | Node (l , _, Empty, _, _) -> l
  | Node (l, k, r, _, _) -> bal l k (remove_max_elt r)
  | Empty -> invalid_arg "iSet.remove_max_elt"

let merge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let k = min_elt t2 in
      bal t1 k (remove_min_elt t2)

let empty = Empty

let is_empty t = t = Empty

let rec add_pom (x, y) = function
  | Node (l, (k, n), r, h, q) ->
      let c = compare x k in
      if c = 0 then Node (l, (x, y), r, h, q)
      else if c < 0 then
        let nl = add_pom (x, y) l in
        bal nl (k, n) r
      else
        let nr = add_pom (x, y) r in
        bal l (k, n) nr
  | Empty -> Node (Empty, (x, y), Empty, 1, y - x + 1)

let rec join l v r =
    match (l, r) with
    (Empty, _) -> add_pom v r
    | (_, Empty) -> add_pom v l
    | (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
        if lh > rh + 2 then bal ll lv (join lr v r) else
        if rh > lh + 2 then bal (join l v rl) rv rr else
        make l v r

(* zwraca drzewo przedziałów mniejszych od x, 
drzewo przedziałów większych od x, 
sprawdza czy x jest w jakimś przedziale,
 a jeśli tak to podaje jego konce w liście *)
let split_pom x t =
  let rec loop x = function
      Empty ->
        (Empty, false, Empty, [])
    | Node (l, (k, n), r, _, _) ->
        let c = compare x k
        and d = compare x n in
        if c >= 0 && d <= 0 then (l, true, r, [k; n])
        else if c < 0 then
          let (ll, pres, rl, lst) = loop x l 
          in (ll, pres, join rl (k, n) r, lst)
        else
          let (lr, pres, rr, lst) = loop x r 
          in (join l (k, n) lr, pres, rr, lst)
  in loop x t

(* Naprawia drzewo w którym przedział w korzeniu nachodzi na 
przedział mniejszy *)
let fix_right t =
    match t with
    | Empty -> Empty
    | Node(l, v, Empty, h, q) -> Node(l, v, Empty, h, q)
    | Node(l, (k, n), r, _, _) -> 
        let (x, y) = min_elt r in
            if x <= n+1 then make l (k, y) (remove_min_elt r)
            else t

(* Naprawia drzewo w którym przedział w korzeniu nachodzi na 
przedział większy *)
let fix_left t =
    match t with
    | Empty -> Empty
    | Node(Empty, v, r, h, q) -> Node(Empty, v, r, h, q)
    | Node(l, (k, n), r, _, _) -> 
        let (x, y) = max_elt l in
            if k <= y+1 then join (remove_max_elt l) (x, n) r
            else join l (k, n) r 

(* Naprawia drzewo w którym przedział w korzeniu nachodzi na 
sąsiednie przedziały *)
let fix t =
    fix_left (fix_right t)

(* Wyjątek podnoszony w sytuacjach, które nie występują *)
exception Not_possible

(* Aby dodać przedział wypisujemy drzewo mniejszych przedziałów,
drzewo więkrzych przedziałów, a następnie łączymy i naprawiamy *)
let add (x, y) t =
    let (lx, _, _, lstx) = split_pom x t
    and (_, _, ry, lsty) = split_pom y t in 
    let (a, b) = match lstx , lsty with
    | [], [] -> (x, y) 
    | [], [_; b] -> (x, b)
    | [a; _], [] -> (a, y)
    | [a; _], [_; d] -> (a, d)
    | _, _ -> raise Not_possible in
    match fix (make lx (a, b) ry) with
        | Empty -> raise Not_possible
        | Node(l1, v1, r1, _, _) -> join l1 v1 r1

(* Aby usunąć przedział dodajemy go,
 a nasstępnie usuwamy z powstałego drzewa *)
let remove (x, y) t =
    let t1 = add (x, y) t in
    match split_pom x t1 with
        | (lx, _, _, [a; b]) -> 
           let (_, _, ry, _) = split_pom y t1 in 
               if a < x && b <= y then join lx (a, x-1) ry
               else if a >= x && b > y then join lx (y+1, b) ry
               else if a < x && b > y then add (a, x-1) (join lx (y+1, b) ry)  
               else (match (merge lx ry) with
                  | Empty -> Empty
                  | Node(l1, v1, r1, _, _) -> join l1 v1 r1)
        | _ -> raise Not_possible

let mem x t =
  let rec loop = function
    | Node (l, (k, n), r, _, _) ->
        let c = compare x k 
        and d = compare x n in
        c >=0 && d <=0 || loop (if c < 0 && d < 0 then l else r)
    | Empty -> false in
  loop t

let iter f t =
  let rec loop = function
    | Empty -> ()
    | Node (l, (k, n), r, _, _) -> loop l; f (k, n); loop r in
  loop t

let fold f t acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, (k, n), r, _, _) ->
          loop (f (k, n) (loop acc l)) r in
  loop acc t

let elements t = 
    let rec loop acc = function
          Empty -> acc
        | Node(l, (k, n), r, _, _) -> loop ((k, n) :: loop acc r) l in
    loop [] t

(* Dzieli drzewo na drzewo elementów mniejszych oraz większych
    i sprawdza czy emelent znajduje się w jakimś pprzedziale *)
let split x t =
    let (l, pres, r, lst) = split_pom x t
    in match lst with
    | [] -> (l, pres, r)
    | [a; b] -> if a = b then (l, pres, r)
                else if a = x then (l , pres, add (x+1, b) r)
                else if b = x then (add (a, x-1) l, pres, r )
                else (add (a, x-1) l, pres, add (x+1, b) r)
    | _ -> raise Not_possible


(* Sprawdza ile jest elemetnów t mniejszych od x. 
   Jako, że liczba elementów w drzewie nie przekracza
    2*max_int + 1 to liczba ta przekracza max_int wtedy
    i tylko wtedy gdy rozmiar drzewa jest niedodatni*)
let below x t = 
    let (l, pres, _) = split x t in
    if l = Empty then if pres then 1 else 0 
    else let n = if pres = true then (size l) +1 else size l in
    if n <= 0 then max_int else n


