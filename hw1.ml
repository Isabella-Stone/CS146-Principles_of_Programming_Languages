(*I pledge my honor that I have abided by the Stevens Honor System. -Isabella Stone*)

type program = int list
let square : program = [0; 2; 2; 3; 3; 4; 4; 5; 5; 1]
let letter_e : program = [0;2;2;3;3;5;5;4;3;5;4;3;3;5;5;1]

(*1 - mirror image*)
let mirror : int -> int =
    (*helper for mirror_image, returns mirror int of ints*)
    fun x -> 
    match x with
    | 0 -> 0
    | 1 -> 1
    | 2 -> 4
    | 3 -> 5
    | 4 -> 2
    | 5 -> 3
    | _ -> failwith "invalid"

let mirror_image : int list -> int list = 
    fun l ->
    List.map (mirror) l

(*2 - rotate 90 letter*)
let rotate_90_l : int -> int =
    (*helper for rotate_90_letter, rotates a single int*)
    fun x -> 
    match x with
    | 0 -> 0
    | 1 -> 1
    | 2 -> 3
    | 3 -> 4
    | 4 -> 5
    | 5 -> 2
    | _ -> failwith "invalid"

let rotate_90_letter : int list -> int list =
    fun l ->
    List.map (rotate_90_l) l

(*3 - rotate 90 word*)
let rec rotate_90_w : int list -> int list = 
    (*helper for rotate_90_word, rotates a single list*)
    fun x ->
    match x with
    | [] -> []
    | h::t -> if h=0 then 0 :: rotate_90_w t
                else if h=1 then 1 :: rotate_90_w t
                else if h=2 then 3 :: rotate_90_w t
                else if h=3 then 4 :: rotate_90_w t
                else if h=4 then 5 :: rotate_90_w t
                else if h=5 then 2 :: rotate_90_w t
                else rotate_90_w t

let rotate_90_word : int list list -> int list list = 
    fun l ->
    List.map (rotate_90_w) l

(*4 - repeat*)
let rec repeat : int -> 'a -> 'a list =
    (*repeats x n-times and returns in a list*)
    fun n x ->
    match n with
    | 0 -> []
    | _ -> x :: repeat (n-1) x

(*5 - pantograph*)
(*5 - solution with map*)
let repeat2 : int -> 'a -> 'a list =
    fun n x ->
    (*repeats x n-times and returns in a list but doesn't repeat 0&1s*)
    if x=0 || x=1 then [x]
    else repeat n x

let pantograph : int -> int list -> int list = 
    fun n p ->
    List.flatten (List.map (repeat2 n) p)

(*5 - solution without map*)
let rec pantograph_nm : int -> int list -> int list = 
    fun n p ->
    match p with 
    | [] -> []
    | h::t -> if (h = 0 || h = 1) then [h] @ pantograph_nm n t
                else if (h > 1 && h < 6) then repeat n h @ pantograph_nm n t
                else failwith "invalid"


(*5 - solution with fold*)
let pantograph_f : int -> int list -> int list = 
    fun n p ->
    List.fold_right (fun x t -> (repeat2 n x) @ t) p []

(*6 - coverage*)
let new_point : int * int -> int -> int * int = 
    (*takes in a tuple and returns it edited to reflect the direction given by d*)
    fun (x,y) d ->
    match d with
    | 0 -> (x,y)
    | 1 -> (x,y)
    | 2 -> (x,y+1)
    | 3 -> (x+1,y)
    | 4 -> (x,y-1)
    | 5 -> (x-1,y)
    | _ -> failwith "invalid"

let rec coverage_helper : int*int -> int list -> (int*int) list =
    (*recursively creates a list of tuples with the help of new_point*)
    fun (x,y) l ->
    match l with
    | [] -> []
    | h::t -> new_point (x,y) h  :: coverage_helper (new_point (x,y) h) t

let coverage : int*int -> int list -> (int*int) list =
    fun (x,y) l ->
    (x,y) :: coverage_helper (x,y) l

(*7 - compress*)
let rec counter : 'a * int -> 'a list -> 'a * int = 
    (*creates a tuples to represent the amount of times(y) x is present*)
    fun (x,y) e ->
    match e with 
    | [] -> (x,y)
    | h::t -> if x=h then counter(x,y+1) t
                else (x,y)

let rec fix : 'a -> 'a list -> 'a list =
    (*takes away leading identical values that have already been accounted for*)
    fun i l ->
    match l with
    | [] -> []
    | h::t -> if h=i then fix i t
                else h::t

let rec compress : int list -> (int*int) list =
    fun l ->
    match l with 
    | [] -> []
    | x::y::t when x=y -> counter (x, 2) t :: compress (fix x t)
    | h::t -> (h, 1) :: compress t

(*8 - uncompress*)
let rec uncompress_helper : 'a * int -> 'a list =
    (*returns a list that contains x y-times*)
    fun (x,y) ->
    match (x,y) with
    | (x,0) -> []
    | (x,y) -> [x] @ uncompress_helper (x,y-1)

let rec uncompress : (int*int) list -> int list =
    fun l ->
    match l with
    | [] -> []
    | (x,y)::t -> uncompress_helper(x,y) @ uncompress t

let uncompress_m : (int*int) list -> int list =
    fun l ->
    List.flatten (List.map (uncompress_helper) l)

let uncompress_f : (int*int) list -> int list =
    fun l ->
    List.fold_right (fun (x,y) t -> (repeat y x) @ t) l []

(*9 - optimize*)  
let rec clean : int list -> int list =
    (*returns the list with any leading 1s taken away*)
    fun l ->
    match l with
    | [] -> []
    | h::t -> if h=1 then clean t
                else h::t

let rec optimize_helper : int list -> int list = 
    fun l ->
    match l with
    | [] -> [] 
    | [x] -> [x]
    | x::y::t when (x=y && (x=1 || x=0)) -> optimize_helper (y::t)
    | x::y::t -> x :: optimize_helper (y::t) 

let optimize : program -> program = 
    fun l ->
    clean (optimize_helper l)
