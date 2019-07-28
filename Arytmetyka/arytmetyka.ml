(* Autor: Philip Smolenski-Jensen *)
(* Reviewer: Wojciech Mitros *)

(* przedzaiał jest pusty lub reprezentowany przez parę liczb x, y*)
(* jeśli x<=y to przedzaiał jest [x, y] 
    gdy x>y to przedział jest (-inf; x]U[y; inf)*)
type wartosc = Interval of float*float | Empty

(* Tworzy przedział [x-xp/100, x+xp/100] dla x>=0
      lub przedział [x+xp/100, x-xp/100] dla x<0 *)
let wartosc_dokladnosc x p =
    Interval(min(x +. x *. p /. 100.) (x -. x *. p /. 100.),
             max(x +. x *. p /. 100.) (x -. x *. p /. 100.))

(* Tworzy przedział [x,y] *)
let wartosc_od_do x y =
    Interval(x, y)

(* Tworzy przedział [x,x] *)
let wartosc_dokladna x =
    Interval(x, x)

(* Sprawdzenie czy wartość jest w przedziale *)
let in_wartosc i war =
    match i with
    | Empty-> false
    | Interval(x, y)->
        if x<=y then x<=war && war<=y
        else war<=y || war>=x

(* Zwraca minimalną wartość przedziału *)
let min_wartosc i =
    match i with
    |Empty-> nan
    |Interval(x, y)-> 
        if x<=y then x 
        else neg_infinity

(* Zwraca maksymalną wartość przedziału*)
let max_wartosc i =
    match i with
    |Empty-> nan
    |Interval(x, y)->
        if x<=y then y
        else infinity

(* Zwraca średnią wartość przedziału *)
let sr_wartosc i = 
    match i with 
    |Empty-> nan
    |Interval(x, y)->
        if x=neg_infinity && y=infinity then nan
        else if x<=y then (x +. y) /. 2.
        else nan

(* Dodawanie dwóch przedziałów *)
let plus i j =
    match i, j with
    |_, Empty -> Empty
    |Empty,_  -> Empty
    |Interval(x, y), Interval(t, z) ->
        if x<=y && t<=z then Interval(x +. t, y +. z)
        else if x>y && t>z then Interval (neg_infinity, infinity)
        else  
             if(t +. x<=z +. y) then Interval(neg_infinity, infinity)
             else Interval(t +. x, z +. y)

(* Zamiana przedziału i na przedział przeciwny (-i) *)
let reverse i =
    match i with
    |Empty -> Empty
    |Interval(x, y)-> Interval(-.y, -.x)

(* Odejmowanie przedziałów, czyli dodawanie przedziału przeciwnego *)
let minus i j =
    plus i (reverse j)

(* Zamiana przedziału i na odwrotność przedziału (1/i) *)
let inverse i =
    if i = Interval(neg_infinity, infinity)
    then Interval(neg_infinity, infinity)
    else
        match i with
        |Interval(0., 0.) -> Empty
        |Empty->Empty
        |Interval(0., y) -> Interval(1. /. y, infinity)
        |Interval(x, 0.) -> Interval(neg_infinity, 1. /. x)
        |Interval(x, y)->Interval(1. /. y, 1. /. x)

exception Ten_przypadek_nie_wystepuje
exception Ten_przypadek_nie_wystepuje_1

(* Zamienia przedziały postaci [neg_inf, x]; [y, inf] na ich sumę mnogościową *)
let merge i j =
    match i, j with
        | Empty, _ -> raise Ten_przypadek_nie_wystepuje
        | _, Empty -> raise Ten_przypadek_nie_wystepuje
        | Interval(x, y), Interval(t, z) ->
            if x = neg_infinity && z = infinity then 
                if y>=t then Interval(neg_infinity, infinity)
                else Interval(t, y)
            else if t = neg_infinity && y = infinity then
                if z>=x then Interval(neg_infinity, infinity)
                else Interval(x, z) 
            else raise Ten_przypadek_nie_wystepuje

(* mnożenie które w działaniu 0*+-infinity zwraca 0 zamiast nan *)
let multiply x y =
    if x = 0. || y = 0. then 0.
    else x *. y

(* Mnożenie przedziałów ograniczonych z przynajmniej jednej strony *)
let pom_razy i j = 
    match i, j with
    | Empty, _ -> raise Ten_przypadek_nie_wystepuje_1
    | _, Empty -> raise Ten_przypadek_nie_wystepuje_1
    |Interval (x, y), Interval(t, z)->
       if x>=0. && t>=0. then Interval(multiply x t, multiply y z)
       else if y<=0. && z<=0.then Interval(multiply y z, multiply x t)
       else if y<=0. && t>=0. then Interval(multiply x z, multiply y t)
       else if z<=0. && x>=0. then Interval(multiply y t, multiply x z)
       else if x<=0. && y>=0. && t>=0. then Interval(multiply x z, multiply y z)
       else if t<=0. && z>=0. && x>=0. then Interval(multiply y t, multiply y z)
       else if y<=0. && t<=0. && z>=0. then Interval(multiply x z, multiply x t)
       else if z<=0. && x<=0. && y>=0. then Interval(multiply y t, multiply x t)
       else if t<=0. && x<=0. && y>=0. && z>=0. then
        Interval(min (multiply t y) (multiply x z),
                 max (multiply t x) (multiply y z))
       else raise Ten_przypadek_nie_wystepuje_1

(* Mnożenie dwóch przedziałów, korzysta z rozdzielności 
   sumy mnogościowej zgledem mnożenia *)
let razy i j =
    match i, j with 
    |Empty, _-> Empty  
    |_, Empty-> Empty
    |Interval (0., 0.), _ ->Interval(0., 0.)
    |_, Interval (0., 0.) -> Interval(0., 0.)
    |Interval (x, y), Interval(t, z)->
        if x<=y && t<=z then pom_razy i j
        else if y<x && t<=z then
            merge (pom_razy (Interval(neg_infinity, y)) (Interval(t, z)))
                  (pom_razy (Interval(x,     infinity)) (Interval(t, z)))
        else if x<=y && t>z then   
            merge (pom_razy (Interval(neg_infinity, z)) (Interval(x, y)))
                  (pom_razy (Interval(t,     infinity)) (Interval(x, y))) 
        else if x<=0. || y>=0. || t<=0. || z>=0. then 
            Interval(neg_infinity, infinity)
        else Interval(min (multiply x t) (multiply y z), 
                      max (multiply x z) (multiply y t))

(* Dzielenie, czyli mnożenie przez odwrotność *)
let podzielic i j =
    razy i (inverse j)
