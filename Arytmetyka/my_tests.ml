(* Philip Smolenski-Jensen *)
(* open Arytmetyka;; *)

let is x y = compare x y = 0;;

let check i x ans =
    assert ((List.map (in_wartosc i) x) = ans);;

let min_avg_max i (x, y, z) = 
    assert ((is (min_wartosc i)  x , is (sr_wartosc i)  y, 
        is (max_wartosc i)  z) = (true, true, true))


let a = wartosc_dokladnosc (-100.) 25. ;; (* [-125; -75] *)  
check a [-125.1; -125.; -75.; -74.9]
        [false; true; true; false];; 

min_avg_max a (-125., -100., -75.);;

let b = wartosc_dokladnosc 20. 300. ;; (* [-40.; 80.] *)
check b [-41.; -40. ; 80. ; 81.]
        [false; true; true; false];;

min_avg_max b (-40., 20., 80.);;

let c = wartosc_od_do 0. 842. ;; (* [0.; 842.] *)
check c [-100.; 30.; 900.]
        [false; true; false];;

min_avg_max c (0., 421., 842.);; 


let d = wartosc_od_do 100. 100. ;; (* [100; 100] *)
check d [-100.; 100.]
        [false; true];;

min_avg_max d (100., 100., 100.);; 

let e = wartosc_dokladna 0. ;; (* [0.; 0.] *)
check e [0.; 1.]
         [true; false];;

min_avg_max e (0., 0., 0.)

let f = plus a b ;; (* [-165; 5] *)
check f [-166.; -164.; 4.; 6.]
        [false; true; true; false];;

min_avg_max f (-165., -80., 5.);;

let g = minus c a ;; (* [75.; 967; *)
check g [70.; 80.; 965.; 970.]
        [false; true; true; false];;

min_avg_max g (75., 521., 967.);;

let ab = razy a b;;
let ac = razy a c;;
let bf = razy b f;;
let gf = razy g f;;
let a_b = podzielic a b;;
let a_c = podzielic a c;;
let c_a = podzielic c a;;
let b_f = podzielic b f;;

check a_c [-10000000.; -0.09; -0.08]
          [true; true; false];;

min_avg_max b_f (neg_infinity, nan, infinity);;

min_avg_max ab (-10000., -2500., 5000.);;

check a_b [-2.; 1.; 2.]
          [true; false; true];;

let h = podzielic c_a e;;
let i = razy bf gf;;
let j = podzielic a_c g;;

check h [-10000000.; 0.; 1000000.]
          [false; false; false];;

min_avg_max h (nan, nan, nan);;

check i [-1000000000.; 2000000000.]
        [true; true];;

check j [-10000000000000.; -0.001; -0.000000001]
        [true; true; false];;

let b_h = podzielic b h;;
let hb  = razy h b;;

min_avg_max b_h (nan, nan, nan);;
min_avg_max hb  (nan, nan, nan);;

let ei = razy e i;;

check ei [-0.001; 0.; 0.0001]
        [false; true; false];;

let k = podzielic a_b a_c;;
let l = podzielic a_b c_a;;
let m = podzielic a_c c_a;;
let n = podzielic c_a b_f;;

min_avg_max k (neg_infinity, nan, infinity);;
min_avg_max n (neg_infinity, nan, infinity);;

check l [-1.; 0.; 1.]
        [true; false; true];;

check m [0.; 1.; 1000000.]
        [false; true; true];;
