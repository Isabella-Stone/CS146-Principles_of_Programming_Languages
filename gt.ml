(* I pledge my honor that I have abided by the Stevens Honor System. -Isabella Stone *)
type 'a gt = Node of 'a *('a gt) list

let t : int gt =
Node (33,
    [Node (12,[]) ;
    Node (77,
        [Node (37,
            [Node (14, [])]) ;
    Node (48, []) ;
    Node (103, [])])
    ])

(*-----------------------------------------------------------------------------------------------------------*)

let rec height : 'a gt -> int = 
    fun t ->
    match t with
    | Node(_,[]) -> 1
    | Node(_, r) -> 1 + List.fold_left (fun h t -> max h t) 0 (List.map height r)

(*-----------------------------------------------------------------------------------------------------------*)

let rec size : 'a gt -> int = 
    fun t -> 
    match t with
    | Node(_,[]) -> 1
    | Node(_, r) -> 1 + List.fold_left (fun h t -> h + t) 0 (List.map size r)

(*-----------------------------------------------------------------------------------------------------------*)

let rec paths_to_leaves : 'a gt -> int list list =
    fun t ->
    match t with
    | Node(_, []) -> [[]]
    | Node(x, r) -> List.flatten (List.mapi (fun i cd -> (List.map (fun l -> i :: l) (paths_to_leaves cd))) r) 

(*-----------------------------------------------------------------------------------------------------------*)

let rec eqlists : 'a list list -> bool =
    (*returns true if all lists within the given list are of equal length, and false otherwise*)
    fun l ->
    match l with
    | [] -> true
    | [x] -> true
    | x::y::t -> if List.length x != List.length y then false
                else eqlists (y::t)

let is_leaf_perfect : 'a gt -> bool =
    fun t -> 
    eqlists (paths_to_leaves t)

(*-----------------------------------------------------------------------------------------------------------*)

let rec preorder : 'a gt -> 'a list =
    fun l ->
    match l with 
    | Node(x,[]) -> [x]
    | Node(x, r) -> List.fold_left (fun h t -> h @ t) [x] (List.map preorder r)

(*-----------------------------------------------------------------------------------------------------------*)

let rec mirror : 'a gt -> 'a gt =
    fun t ->
    match t with 
    | Node(x, []) -> Node(x, [])
    | Node(x, r) -> Node(x, List.map mirror (List.rev r))

(*-----------------------------------------------------------------------------------------------------------*)

let rec mapt : ('a -> 'b) -> 'a gt -> 'b gt = 
    fun f (Node(x, r)) -> 
    match r with 
    | [] -> Node(f x, [])
    | l -> Node(f x, List.map (fun i -> mapt f i) l)  
    
(*-----------------------------------------------------------------------------------------------------------*)

let rec foldt : ('a -> 'b list -> 'b) -> 'a gt -> 'b =
    fun f (Node(x, r)) ->
    f x @@ (List.map (foldt f) r)

let sumt : int gt -> int =
    fun t ->
    foldt (fun i rs -> i + List.fold_left (fun i j -> i+j ) 0 rs) t

let memt : 'a gt -> 'a -> bool = 
    fun t e ->
    foldt (fun i rs -> i=e || List.exists (fun i -> i) rs) t

(*-----------------------------------------------------------------------------------------------------------*)

let rec mirror' : 'a gt -> 'a gt = 
    fun t ->
    foldt (fun x r -> Node(x, (fun i -> List.rev i) r) ) t
