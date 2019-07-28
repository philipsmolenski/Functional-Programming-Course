(* Autor: Philip Smolenski - Jensen *)
(* Code rewiev: Paweł Kopaczyk *)

(* NWD pary liczb *)
let nwd a b =
    let rec pom a b =
        if a = 0 then b
        else if b = 0 then a
        else if a > b then pom b (a mod b)
        else pom a (b mod a) in
    pom a b

(* Sprawdza czy nwd liczb w tablicy a dzieli każdą liczbę w b *)
let ok1 a b =
    let d = Array.fold_left nwd 0 a in
    Array.fold_left (fun h x -> h && x mod d = 0) true b

(* Sprawdza czy tablica nie zawiera samych zer *)
let zera a =
    Array.fold_left (fun h x -> h && x = 0) true a

(* Sprawdza czy na koniec procedury któraś szklanka będzie pełna lub pusta *)
let pusty a b =
    let ok = ref false
    and n = Array.length a in
    for i = 0 to n - 1 do
        if b.(i) = a.(i) || b.(i) = 0 then ok := true
    done;
    !ok

let przelewanka arr =
    let n = Array.length arr in
    let pocz = Array.init n (fun x -> fst arr.(x))
    and kon = Array.init n (fun x -> snd arr.(x)) in
    (* Sprawdzenie wstępnych warunków *)
    if arr = [||] || zera pocz then 0
    else if not(pusty pocz kon  && ok1 pocz kon) then -1
    (* Dla każdego stanu określa do jakich stanów można dojść w jednym ruchu *)
    else let sasiedzi a =
        let wyn = ref [] in
        (* Stany po wylaniu wody z któregoś kubka *)
        for i = 0 to n - 1 do
            if a.(i) <> 0 then
                let b = Array.copy a in begin
                    b.(i) <- 0;
                    wyn := b::!wyn
                end;
        done;
        (* Stany po napełnieniu któregoś kubka *)
        for i = 0 to n - 1 do
            if a.(i) <> pocz.(i) then
                let b = Array.copy a in begin
                    b.(i) <- pocz.(i);
                    wyn := b::!wyn
                end;
        done;
        (* Stany po przelaniu wody z jednego kubka do innego *)
        for i = 0 to n - 1 do
            for j = 0 to n - 1 do
                if i <> j && a.(i) <> 0 && a.(j) <> pocz.(j) then begin
                    if a.(i) < pocz.(j) - a.(j) then
                        let b = Array.copy a in begin
                            b.(i) <- 0;
                            b.(j) <- a.(j) + a.(i);
                            wyn := b::!wyn
                        end;
                    else if a.(i) >= pocz.(j) - a.(j) then
                        let b = Array.copy a in begin
                            b.(i) <- a.(i) + a.(j) - pocz.(j);
                            b.(j) <- pocz.(j);
                            wyn := b::!wyn
                        end;
                end;
            done;
        done;
        !wyn
    in
    (* Backtrack - BFS po grafie stanów,
        zaczynamy od stanu opisanego tablicą a *)
    let bfs a =
        let q = Queue.create ()
        and odl = Hashtbl.create 1000037 in
        Queue.add a q;
        Hashtbl.add odl a 0;
        let stop = ref false in
        while Queue.is_empty q = false && !stop = false do
            let hdq = Queue.pop q in
            if hdq = kon then
                stop := true
            else begin
                let pom tab =
                    if Hashtbl.mem odl tab = false then begin
                        if  tab = kon then begin
                        stop := true
                        end;
                        Hashtbl.add odl tab (Hashtbl.find odl hdq + 1);
                        Queue.add tab q
                    end;
                in
                List.iter pom (sasiedzi hdq)
            end;
        done;
        if Hashtbl.mem odl kon = false then -1
        else Hashtbl.find odl kon in
        let t = Array.make n 0 in
        bfs t
