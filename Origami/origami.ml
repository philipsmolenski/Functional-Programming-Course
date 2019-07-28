(* Autor: Philip Smolenski-Jensen *)
(* Code review: Bartosz Dzięcioł *)

open List;;

type point = float * float

type kartka = point -> int 

(* Tworzy prostokąt *)
let prostokat (x1, y1) (x2, y2) = fun (a, b) -> 
    if a >= x1 && a <= x2 && b >= y1 && b <= y2 then 1
    else 0

(* Tworzy kółko *)
let kolko (x, y) r = fun (a, b) ->
    if r *.r >= (a -. x) *. (a -. x) +. (y -. b)*.(y -. b) then 1
    else 0

(* Bierze punkt p i prostą l i zrwaca odbicie p względem l *)
let odbij (s, t) (x1, y1) (x2, y2) =
    let a = y2 -. y1
    and b = x1 -. x2 in
    let c = a *. x1 +. b *. y1 in
    (* ax + by = c to równanie prostej h przechodzącej przez (x1, y1) (x2, y2)*)
    let a1 = -.b
    and b1 = a in
    let c1 = a1 *. s +. b1 *. t in
    (* a1*x + b*y = c1 to równanie prostej h1 prostopadłej do l *)
    let det = a *. b1 -. a1 *. b in 
    let k = (b1 *. c -. b *. c1) /. det
    and l = (a *. c1 -. c *. a1) /. det in 
    (* współrzędne punktu przecięcia h i h1*)
    (2. *. k -. s, 2. *. l -. t)

(* Składa kartkę wzdłóż prostej przechodzącej przez (x1, y1) (x2, y2) *)
let zloz (x1, y1) (x2, y2) f = fun (a1, b1) ->
    let x = x2 -. x1
    and y = y2 -. y1
    and a = a1 -. x1
    and b = b1 -. y1 in
    let det = (x *. b -. a *. y) in
    if det = 0. then f (a1, b1) 
    else if det < 0. then 0
    else f (a1, b1) + f (odbij (a1, b1) (x1, y1) (x2, y2))

(* Przerabia funkcje zloz tak, aby mogła być użyta w fold_lefcie *)
let zloz2 f ((x1, y1), (x2, y2)) =
    zloz (x1, y1) (x2, y2) f

let skladaj l f = 
    fold_left zloz2 f l 