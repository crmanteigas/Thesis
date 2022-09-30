exception Exception of string


(* TYPES *)

type node_ty = 
    | Empty
    | Top 
    | Screen 
    | Table 
    | Column
    | Icon 
    | Expression
    | Input
    | Button
    | Pages 
    | CheckBox
    | Calendar

type label = string

type path = int list

type apply_op_ty =
    | Insert
    | Remove
    | Replace
    | Move
    | AddProp
    | ReplaceProp
    | RemoveProp

type ty = 
    (*| BaseT of basic_ty*)
    | UnitT
    | NumT
    | StringT
    | BoolT
    | LabelT of label
    | LabelAttrT of ty * ty
    | NameT of string
    | RecordT of (label * ty) list
    | RecordAttrT of ty
    | EntityT of ty * ty
    | AttributeT of ty * ty
    | NodeTP of node_ty list  * (int list * ( label * ty ) list) list
    | BoxT of ty
    | ListT of ty list
    | TemplateT of ty * ty
    | ForAllNameT of string * ty
    | VarNT of string 
    | ForAllTypeT of string * ty
    | VarT of string
    | ForAllRowsT of string * ty 
    | VarR of string
    | Top
    | PathT of ty list


(* TERMS *)

type 'a env = (string * 'a) list

type set_env = unit env

type prop = label * term

and term =
    | Unit 
    | Num of int
    | String of string
    | Bool of bool

    | Var of string
    | Label of label
     (*
    | Id of string
*)
    | Entity of string * term
    | Attribute of string * label * ty * term

    | Select of term * term
    | Concat of term * term

    (* boxed op *)

    | NameOf of term
    | LabelOf of term
    | PropsOf of term
    | AttributesOf of term

    | IsOfType of term * ty
    | Let of string * term * term
    (* template decl + template inst *)

    | Template of string * ty * term (* Compile-time Abstraction *)
    | Instantiate of term * term (* Compile-time Application *)

    | Node of node_ty * prop list * term 
    | ForNode of string * string * term * term
    | IfNode of term * term * term 

    | Box of term
    | LetBox of string * term * term
    | VarRT of string

    | Closure of term * term env

    | Record of (label * term) list

    | List of term list

    | ForAllName of string * term
    | CallName of term * string

    | ForAllType of string * term
    | CallType of term * ty

    | ForAllRows of string * term
    | CallRows of term * ty

    | Path of term list (* Path([Num(1)])*)
    
    | ApplyOp of term * apply_op_ty * term * term * term 
      (* Original tree * operation * Path * index of insert * (argument tree/record of attributes/label of attr) *)


(* PRINTS *)

let string_of_nodetype t =
    match t with
    | Empty -> "Empty"
    | Top -> "Top"
    | Screen -> "Screen"
    | Table -> "Table"
    | Column -> "Column"
    | Icon -> "Icon"
    | Expression -> "Expression"
    | Input -> "Input"
    | CheckBox -> "CheckBox"
    | Calendar -> "Calendar"
    | Button -> "Button"
    | Pages -> "Pages"


let rec string_of_type t =
    match t with
    | NumT -> "Num"
    | StringT -> "String"
    | BoolT -> "Bool"
    | LabelT label -> "Label("^label^")"
    | LabelAttrT (t,t') -> "{"^(string_of_type t)^":"^(string_of_type t')^"}"
    | NameT s -> "Name("^s^")"
    | RecordT l -> "{"^(String.concat ", " (List.map (fun (x,y) ->  x ^":"^ (string_of_type y)) l))^"}"
    | RecordAttrT t -> "Attr{"^string_of_type t^"}"
    | EntityT (t,t') -> "Entity("^string_of_type t^", "^string_of_type t'^")"
    | AttributeT (t,t') -> "Attribute("^string_of_type t^":"^string_of_type t'^")"
    | NodeTP (l,_) -> "NodeP("^(String.concat ", " (List.map (fun t -> string_of_nodetype t) l))^")"
    | BoxT t -> "Box("^string_of_type t^")"
    | ListT l -> "List("^(String.concat ", " (List.map (fun t -> string_of_type t) l))^")" (* Why is this a list? *)
    | TemplateT (t,t') -> "Template("^string_of_type t^" -> "^string_of_type t'^")"
    | ForAllNameT (s,t) -> "ForAllName("^s^" -> "^string_of_type t^")"
    | VarNT s  -> "VarNT("^s^")"
    | ForAllTypeT (s,t) -> "ForAllType("^s^" -> "^string_of_type t^")"
    | ForAllRowsT (s,t) -> "ForAllRows("^s^" -> "^string_of_type t^")"
    | VarT s  -> "VarT("^s^")"
    | VarR s  -> "VarR("^s^")"
    | Top -> "Top"

and string_of_alphas a =
    match a with
    | hd :: [] -> string_of_nodetype hd 
    | hd :: tl -> string_of_nodetype hd ^ " | " ^ string_of_alphas tl

and string_of_recordt r =
    match r with
    | (s,t)::[] -> s^"="^string_of_type t
    | (s,t)::rem -> s^"="^string_of_type t^" ; "^string_of_recordt rem
    | _ -> ""

and string_of_listt l =
    match l with
    | x::[] -> string_of_type x 
    | x::r -> string_of_type x^" , "^string_of_listt r
    | [] -> "[]"

let rec string_of_record_label r = 
    match r with
    | (s,t)::[] -> s^"="^string_of_term t
    | (s,t)::rem -> s^"="^string_of_term t^" ; "^string_of_record_label rem
    | _ -> ""

and string_of_list l =
    match l with
    | x::[] -> string_of_term x 
    | x::r -> string_of_term x^" , "^string_of_list r
    | [] -> ""

and string_of_record l =
    match l with
    | (s,t)::[] -> s^"="^string_of_term t
    | (s,t)::r -> s^"="^string_of_term t^" ; "^string_of_record r
    | _ -> ""

and string_of_term t =
    match t with
    | Num n -> string_of_int n 
    | String s -> "'"^s^"'"
    | Bool b -> string_of_bool b

    | Var x -> x
    | Label l -> l

    | Entity(s,l) -> "Entity<"^s^", {"^string_of_term l^"}>"
    | Attribute(n,l,t',v) -> "Attribute<"^n^", "^l^", "^string_of_type t'^", "^string_of_term v^">"
(*
    | Op(o,e,e') -> string_of_term e^string_of_op o^string_of_term e'
*)
    | Let(x,e1,e2) -> "let "^x^"="^string_of_term e1^" in "^string_of_term e2
    | Select(e,e') -> string_of_term e^"."^string_of_term e'

    | Node(a,p,List n) -> "Node<"^string_of_nodetype a^", {"^string_of_record p^"}, List["^string_of_list n^"]>"
    | ForNode(x,t,e,e') -> "forNode("^x^":"^t^" in "^string_of_term e^", "^string_of_term e'^")"
    | IfNode(c,e,e') -> "ifNode("^string_of_term c^", "^string_of_term e^", "^string_of_term e'^")"

    | NameOf e -> "(NameOf "^string_of_term e^")"
    | LabelOf a -> "(LabelOf "^string_of_term a^")"
    
    | Closure(e,env) -> "Closure("^string_of_term e^" , ["^string_of_record env^"])"
    | Box e -> "Box("^string_of_term e^")"
    | LetBox(u,e1,e2) -> "letbox "^u^"*="^string_of_term e1^" in "^string_of_term e2
    | VarRT u -> u^"*"

    | Record r -> "{"^string_of_record_label r^"}"

    | IsOfType(a,t) -> string_of_term a ^ " isOfType " ^ string_of_type t

    | AttributesOf e -> "AttributesOf " ^ string_of_term e

    | List l -> "List[" ^ string_of_list l ^ "]"

    | Template(x,t,e) -> "Template<" ^ x ^ ", " ^ string_of_type t ^ ", " ^ string_of_term e ^ ">"

    | Instantiate(t,e) -> string_of_term t ^ "(" ^ string_of_term e ^ ")"


(* MAKES *)

let mk_num n = Num n
let mk_string s = String s
let mk_bool b = Bool b

let mk_entity s l = Entity(s,l)
let mk_attribute n l t v = Attribute(n,l,t,v)

let mk_node a p n = Node(a,p,n)
let mk_if c e e' = IfNode(c,e,e')
let mk_for x t e e' = ForNode(x,t,e,e')

let mk_prop p e = (p, e)


let mk_select e e' = Select(e, e')
let mk_concat e e' = Concat(e, e')


(* build select from list. tail-recursive *)
let mk_select_list l =
    let rec select_aux acc l =
        match l with 
        | hd::tl -> select_aux (Select(acc, hd)) tl
        | [] -> acc
    in 
    match l with
    | hd::tl -> select_aux hd tl
    | [] -> assert false



let find x env = List.assoc x env (* 'a -> ('a * 'b) list -> 'b *)
let define x v env = (x,v)::env


(* tail-recursive append *)
let append l1 l2 =
    let rec append_aux acc l1 l2 =
        match l1, l2 with
        | [], [] -> List.rev acc
        | [], h :: t -> append_aux (h :: acc) [] t
        | h :: t, l -> append_aux (h :: acc) t l
    in
    append_aux [] l1 l2

(* tail-recursive map *)
let map f l = 
    let rec map_aux acc f l =
        match l with
        | [] -> List.rev acc
        | h::t -> map_aux ((f h)::acc) f t 
    in
    map_aux [] f l 

(* Tree operations *)

let insert_at l e (Num(i)) =
    let rec insert_at_aux a l c=
        match l with
        | [] -> List.rev_append a [e]
        | x :: xs -> 
            if c = i then
                List.rev_append a (e::x::xs)
            else insert_at_aux (x::a) xs (c+1)

    in
    insert_at_aux [] l 0
;;

let replace_prop_aux l1 l2 =
    let rec replace_aux (l,t) l2 =
        match l2 with
        | [] -> (l,t)
        | (l',t') :: l2' -> if l' = l then (l,t') else replace_aux (l,t) l2'
    in
    map (fun (l,t) -> replace_aux (l,t) l2) l1

let rec apply_on_path ts p fn =
    match p,ts with
    | Path([]), _ -> raise (Exception "Path cannot be empty.")
    | Path(Num(x) :: p'), [] -> raise (Exception "There is no tree for the path left.")
    | Path([Num(0)]), t :: ts' -> fn t ts'
    | Path(Num(0) :: p'), Node(n,a,List ts') :: ts'' -> 
        let r, o = apply_on_path ts' (Path(p')) fn in
            ((Node(n,a, List r) :: ts''),o)
    | Path(Num(x) :: p'), t :: ts' -> 
        let r, o = apply_on_path ts' (Path(Num(x-1)::p')) fn in
            (t :: r, o)
    | _ -> raise (Exception "Apply on path needs to be applied to a path and tree.") (*TODO*)

let insert ts p i t = 
    apply_on_path ts p (fun (Node(n,a,List ts'')) ts' -> (Node(n,a, List (insert_at ts'' t i))::ts'),Node(n,a,List ts''))

let replace ts p t =
    apply_on_path ts p (fun t' ts' -> (t :: ts', t'))

let remove ts p =
    apply_on_path ts p (fun t ts' -> (ts',t))

let move ts p i = (* ni - new index*)
    let ts',t = remove ts p in
    let rec remove_last l acc =
        match l with
        | Path([]) -> Path(List.rev acc)
        | Path(x :: []) -> Path(List.rev acc)
        | Path(x :: xs) -> remove_last (Path(xs)) (x::acc)
        | _ -> raise (Exception "Move: term needs to be Path")
    in
    insert ts' (remove_last p []) i t

let add_prop ts p (Record(a)) =
    apply_on_path ts p (fun (Node(n, a', ts'')) ts' -> (Node(n,append a a',ts'')::ts'),Node(n,a',ts''))

let replace_prop ts p (Record(a)) =
    apply_on_path ts p (fun (Node(n, a', ts'')) ts' -> (Node(n,replace_prop_aux a' a,ts'')::ts'),Node(n,a',ts''))

let remove_prop ts p (List ids) =
    apply_on_path ts p (fun (Node(n,a',ts'')) ts' -> 
        (Node(n,List.filter (fun (l,_) -> not (List.exists (fun (String id) -> id = l) ids)) a',ts'')::ts'),
        Node(n,a',ts'')
    )

let rec remove_last acc l = 
    begin match l with
    | [] -> assert false (* Not possible *)
    | x :: [] -> (List.rev acc, x)
    | x :: xs -> remove_last (x :: acc) xs
    end


let build_entity_record r =
    Record( 
        ("list" , Record( 
            ("current" , Record r)::[]
        ))::[] 
    )

let rec eval_record r env =
    map (fun (l, e) -> (l, eval e env)) r

and eval e env =
    match e with
    | Unit -> Unit
    | Num n -> Num n       
    | String s -> String s    
    | Bool b -> Bool b     

    | Var x -> find x env
    | Label l -> Label l

    | Entity(n,al) -> Entity(n, eval al env)

    | Attribute(n,l,t,p) -> Attribute(n, l, t, eval p env)

    | NameOf e1 -> 
        begin 
        match eval e1 env with
        | Entity(n, Record ar) -> eval ( Box( build_entity_record ar ) ) env 
        | _ -> assert false
        end
    | LabelOf a ->
        begin
        match eval a env with
        | Attribute(n,l,t,p) -> eval ( Box( Label l ) ) env 
        | _ -> assert false
        end

    | Let(x,e1,e2) -> 
        let v1 = eval e1 env in
        let env' = define x v1 env in
        eval e2 env' 
    
    | Node(a,p,n) -> 
        let ps = eval_record p env in
        begin 
        match eval n env with 
        | List l -> 
            let ns = map (fun x -> eval x env) l in
            let ns' = map (fun x -> match x with
                    | List l -> l
                    | _ -> [x]) ns in
            Node(a, ps, List (List.flatten ns'))
        | _ -> assert false
        end

    | Box e1 -> Closure(e, env)
    | LetBox(u,e1,e2) ->
        let v1 = eval e1 env in
        begin
        match v1 with
        | Closure(Box(e3), env') ->
            let env'' = define u (Closure(e3, env')) env in 
            eval e2 env''
        | _ -> assert false
        end
    | VarRT v -> eval (find v env) env
    | Closure(e1, env') -> eval e1 env'

    | Record r -> Record( eval_record r env )

    | Select(e1, e2) ->
        let v1 = eval e1 env in
        let v2 = eval e2 env in 
        begin
        match v1, v2 with
        | Record r , Label l -> find l r 
        | Entity(n, Record ar) , Label l -> find l ar 
        | Attribute(n, l', t, Record p) , Label l -> find l p 
        | _ -> assert false
        end

    | IsOfType(e,t) ->
        begin
        match eval e env with 
        | Attribute(n,l,t',p) -> Bool (t'=t)
        | _ -> assert false
        (* | Variable t' -> Bool (t'==t) *)
        end

    | IfNode(c,t,f) ->
        begin
        match eval c env with
        | Bool b -> if b then 
                        eval t env
                    else 
                        eval f env
        | _ -> assert false
        end

    | AttributesOf e1 -> 
        begin
        match eval e1 env with
        | Entity(n, Record ar) -> List (map (fun (l, a) -> a) ar) 
        | _ -> assert false
        end

    | PropsOf a ->          (* not tested yet *)
        begin
        match eval a env with
        | Attribute(n, l, t, p) -> p
        | _ -> assert false (* node???  *)
        end

    | ForNode(s, t, e1, e2) -> 
        let v1 = eval e1 env in
        begin
        match v1 with
        | Record r -> 
            let f = fun (l, e3) -> (
                let env' = define s e3 env in
                eval e2 env'
            ) in 
            List (map f r)      (* List ?? *)
        | List l ->
            let f = fun e3 -> (
                let env' = define s e3 env in
                eval e2 env'
            ) in
            List (map f l)
        | _ -> assert false
        end

    | List l -> List (map (fun x -> eval x env) l)  
    | Path p -> Path p
    | Template(x,t,e1) -> Closure(e, env)
    | Instantiate(e1,e2) -> 
        let v1 = eval e1 env in
        begin
        match v1 with
        | Closure(Template(x,t,e3), env') -> 
            let v2 = eval e2 env in
            let env'' = define x v2 env' in
            eval e3 env''
        | _ -> assert false
        end
    
    | ForAllName(n, e1) ->
        Closure(e, env)
    | CallName(e1, n) ->
        let v1 = eval e1 env in
        begin
        match v1 with
        | Closure(ForAllName(n', e2), env') ->
            eval e2 env'            (* ASSIM ??? *)
        | _ -> assert false
        end

    | ForAllType(t, e1) ->
        Closure(e, env)
    | CallType(e1, t) ->
        let v1 = eval e1 env in
        begin
        match v1 with
        | Closure(ForAllType(t', e2), env') ->
            eval e2 env'
        | _ -> assert false
        end

    | ForAllRows(x, e1) ->
        Closure(e, env)
    | CallRows(e1, e2) ->
        let v1 = eval e1 env in
        begin
        match v1 with
        | Closure(ForAllRows(x, e3), env') ->
            eval e3 env'
        | _ -> assert false
        end
    | _ -> print_endline "Not yet implemented"; assert false
and eval_tree  e env =
    match e with
    | ApplyOp(t1, op, p, i, t2) ->
        let t1' = match t1 with
            | ApplyOp _ -> let r, _ = eval_tree t1 [] in (List r)
            | _ -> List [eval t1 []]
        in
        let t1'' = match t1' with
            | List xs -> map (fun n -> eval n env) xs
            (*| Node _ -> [eval t1' env]*)
            | _ -> raise (Exception "First argument of ApplyOp needs to eval to a Node or a list of Nodes.")
        in
        let rec check_path_validity ts p =
            match p,ts with
            | Path([]), _ -> (false,Unit)
            | Path(Num(x) :: p'), [] -> (false,Unit)
            | Path([Num(0)]), t :: ts' -> (true,t)
            | Path(Num(0) :: p'), Node(n,a,List ts') :: ts'' -> 
                check_path_validity ts' (Path(p'))
            | Path(Num(x) :: p'), t :: ts' -> 
                check_path_validity ts' (Path(Num(x-1)::p')) 
            | _ -> (false,Unit)
        in
        let is_path_valid,node = check_path_validity t1'' p in 
        let check_index_validity op i p = 
            match op,i,p with
                | Insert, Num(n), Path(ns) -> 
                    if n = 0 then
                        is_path_valid
                    else 
                        let p,_ = check_path_validity t1'' (Path(append ns [Num(n-1)])) in p
                | Move, Num(n), Path(ns) -> 
                    let ns',_ = remove_last [] ns in
                    let p,_ = check_path_validity t1'' (Path(append ns' [i])) in p
                | _ -> true
        in
        let is_index_valid = check_index_validity op i p in
        if not (is_path_valid && is_index_valid) then 
            (t1'',Node(Empty,[], List []))
        else begin
            let t2' = 
                match t2 with
                | ApplyOp _ -> 
                    let r, _ = eval_tree t2 [] in (* Ã‰ sempre vÃ¡lido *)
                    if List.length r <> 1 then 
                        raise (Exception "eval_tree: Should evaluate to a tree")
                    else (List.hd r)
                | _ -> eval t2 []
            in
            let check_t2_validity op t n =
                match op, t, n with
                | AddProp, Record r,Node(n,a,_) -> 
                    let r' = List.filter (fun (x,_) -> not (List.exists (fun (l,_) -> l = x) a)) r in 
                    let r'' = map (fun (l,m) -> (l,eval m env)) r' in
                    if r'= [] then (false, Unit) else (true,Record r'')
                | ReplaceProp, Record r, Node(n,a,_) -> 
                    let r' = List.filter (fun (l, _) -> (List.exists (fun (l', _) -> l' = l) a)) r in 
                    let r'' = map (fun (l,m) -> (l,eval m env)) r' in
                    if r' = [] then (false, Unit) else (true, Record r'')
                | RemoveProp, List l, Node(n,a,_) -> 
                    let l' = List.filter(fun (String x) -> List.exists (fun (y,_) -> x = y) a) l in
                    if l' = [] then (false,Unit) else (true, List l')
                | _ -> (true,t)
            in
            let is_t2_valid,t2'' = check_t2_validity op t2' node in
            if not (is_t2_valid) then
                (t1'',Node(Empty,[], List []))
            else begin match op with
                | Insert -> insert t1'' p i t2''
                | Remove -> remove t1'' p
                | Replace -> replace t1'' p t2''
                | Move -> move t1'' p i 
                | AddProp -> add_prop t1'' p t2''
                | ReplaceProp -> replace_prop t1'' p t2''
                | RemoveProp -> remove_prop t1'' p t2''
            end
        end
    | _ -> raise (Exception "eval_tree can only be applied to Trees.")





let rec unify t t' = (* NÃ£o precisa de ser unificaÃ§Ã£o *)
    if t = t' then Some []
    else match t,t' with 
    | EntityT(n,a), EntityT(n',t') -> if n = n' then Some [] else None (* TODO Add rows *)
    | AttributeT(n,t), AttributeT(n',t') -> if n = n' then unify t t' else None
    | _, Top -> Some [] (* TODO Tirar quando houver foralltype *)
    | Top, _ -> Some []
    | _ -> None (* TODO *)
    
let rec apply unification t = t (* TODO: Just in case we need to apply substitutions *)


(* SUBST FUNCTIONS *)

let rec subst_name o n t =
    match t with
    | VarNT x -> 
        if x = o then 
            NameT n 
        else t 
    | EntityT(e, ar) ->
        EntityT(subst_name o n e, ar)
    | AttributeT(e, t) ->
        AttributeT(subst_name o n e, t)
    | TemplateT(t1, t2) ->
        TemplateT(subst_name o n t1, subst_name o n t2)
    | ForAllNameT(s, t1) ->
        if s = o then
            subst_name o n t1
        else
            ForAllNameT(s, subst_name o n t1)
    | ForAllTypeT(t', t1) ->
        ForAllTypeT(t', subst_name o n t1)
    | ForAllRowsT(x, t1) ->
        ForAllRowsT(x, subst_name o n t1)
    | RecordT r -> 
        RecordT( map (fun (l,e) -> (l, subst_name o n e)) r )
    | RecordAttrT n' ->
        RecordAttrT(subst_name o n n')
    | LabelAttrT(n', t') ->
        LabelAttrT(subst_name o n n', t')
    | _ -> t



let rec subst_type o n e =
    match e with
    | VarT t -> if t=o then n else e
    | LabelAttrT(l, t) -> LabelAttrT(l, subst_type o n t)
    | AttributeT(n', t) -> AttributeT(n', subst_type o n t)
    | BoxT b -> BoxT(subst_type o n b)
    | ListT l -> ListT( map (fun e' -> subst_type o n e') l)
    | TemplateT(t1, t2) -> TemplateT(subst_type o n t1, subst_type o n t2)
    | ForAllNameT(n', e1) -> ForAllNameT(n', subst_type o n e1)
    | ForAllTypeT(t, e1) -> 
        if t = o then
            subst_type o n e1
        else
            ForAllTypeT(t, subst_type o n e1)
    | ForAllRowsT(x, e1) -> ForAllRowsT(x, subst_type o n e1)
    | _ -> e


let rec subst_rec o n e =
    match e with
    | VarR x -> if x = o then n else e 
    | ForAllRowsT(x, t1) ->
        if x = o then
            subst_rec o n t1
        else
            ForAllRowsT(x, subst_rec o n t1)
    | ForAllTypeT(t, e1) -> ForAllTypeT(t, subst_rec o n e1)
    | ForAllNameT(n', e1) -> ForAllNameT(n', subst_rec o n e1)
    | TemplateT(t1, t2) -> TemplateT(subst_rec o n t1, subst_rec o n t2)
    | ListT l -> ListT( map (fun e' -> subst_rec o n e') l )
    | RecordT r -> RecordT( map (fun (l,e') -> (l, subst_rec o n e')) r )
    | BoxT b -> BoxT(subst_rec o n b)
    | EntityT(n', r) -> EntityT(n', subst_rec o n r)
    | _ -> e

(* Builds the outsystems builtin way of reaching the iterator of a list *)
let mk_type_entity_rec n =
    RecordT(
        ("list", RecordT(
            ("current", RecordAttrT n)::[]
        ))::[]
    )

let rec typecheck_t t env mod_env tenv nenv renv = 
    match t with
    | VarT st -> find st tenv
    | VarNT nt ->
        let _ = find nt nenv in
        t 
    | VarR rt -> find rt renv
    | AttributeT(n, t') ->
        AttributeT(typecheck_t n env mod_env tenv nenv renv, typecheck_t t' env mod_env tenv nenv renv)
    | EntityT(n, r) ->
        EntityT(typecheck_t n env mod_env tenv nenv renv, typecheck_t r env mod_env tenv nenv renv)
    | LabelAttrT(n, t') -> 
        LabelAttrT(typecheck_t n env mod_env tenv nenv renv, typecheck_t t' env mod_env tenv nenv renv)
    | _ -> t 


let intersect1 l1 l2 = 
    let intersect_aux acc l1 l2 = List.map (
    fun x -> 
            let r = (List.find_opt (fun y -> y = x) l2) in
            match r with
            Some(n) -> [n]
            | None -> []
    ) l1
    in
    List.flatten (intersect_aux [] l1 l2)

let intersect2 l1 l2 = 
    let intersect_aux acc l1 l2 = List.map (
    fun (p1,pt1) -> 
        let r = (List.find_opt (fun (p2,pt2) -> p2 = p1) l2) in
        match r with
            Some((p2',pt2')) -> [(p1,intersect1 pt1 pt2')]
        | None -> []
    ) l1
    in
    List.flatten (intersect_aux [] l1 l2)

let rec typecheck_record r env mod_env tenv nenv renv =
    map (fun (l, e) -> (l, typecheck e env mod_env tenv nenv renv)) r

and typecheck e env mod_env tenv nenv renv =
    match e with
    | Unit -> UnitT
    | Num n -> NumT
    | String s -> StringT
    | Bool b -> BoolT

    | Var x -> find x env
    | Label l -> LabelT l

    | Entity(n, ar) ->
        let r = typecheck ar env mod_env tenv nenv renv in
        begin match r with
        | RecordT rt -> EntityT(NameT n, r)
        | _ -> assert false
        end

    | Attribute(n,l,t,p) ->
        let pt = typecheck p env mod_env tenv nenv renv in
        begin match pt with
        | RecordT r -> AttributeT(NameT n, t)
        | _ -> assert false
        end

    | NameOf e1 -> (* The access to the runtime record of the entity *)
        let et = typecheck e1 env mod_env tenv nenv renv in
        begin match et with
        | EntityT(n, ar) -> BoxT (mk_type_entity_rec n)
        | _ -> assert false
        end

    | LabelOf a -> 
        let at = typecheck a env mod_env tenv nenv renv in 
        begin match at with
        | AttributeT(n,t) -> BoxT (LabelAttrT(n, t))
        | _ -> assert false
        end

    | Let(x,e1,e2) -> 
        let t1 = typecheck e1 env mod_env tenv nenv renv in
        let env' = define x t1 env in
        typecheck e2 env' mod_env tenv nenv renv

    | Node(a,p,n) -> 
        let pts = typecheck_record p env mod_env tenv nenv renv in    (* TODO ?? What is to be done? *)
        let nts = typecheck n env mod_env tenv nenv renv in
        begin match nts with
        | ListT l -> 
            let extractpath (i,l) t = 
                begin match t with 
                  NodeTP (_,ps) -> 
                    let get_max = List.fold_left (fun max (p,_) -> let hd = List.hd p in if hd > max then hd else max) 0 ps in
                    (i+1+get_max, List.append ( 
                                            map (fun (p,t) -> 
                                                let fst = List.hd p in
                                                (0::(i+fst)::List.tl p,t)) 
                                                ps) 
                                            l)
                | _ -> assert false
                end
            in let paths = snd (List.fold_left extractpath (0,[]) l)
            in NodeTP( [a], ([0],pts)::paths )
        | _ -> assert false
        end

    | Box e1 -> BoxT (typecheck e1 [] mod_env tenv nenv renv) (* Clear the compile time env *)

    | LetBox(u,e1,e2) -> 
        let t1 = typecheck e1 env mod_env tenv nenv renv in
        begin
        match t1 with
        | BoxT t1' -> 
            let mod_env' = define u t1' mod_env in
            typecheck e2 env mod_env' tenv nenv renv 
        | _ -> assert false
        end

    | VarRT u -> find u mod_env

    | Closure(e1, env') -> typecheck e1 env mod_env tenv nenv renv (* TODO env' not used ???? *)

    | Record r -> RecordT (typecheck_record r env mod_env tenv nenv renv)    (* TODO *)
    
    | Select(e1, e2) ->
        let t1 = typecheck e1 env mod_env tenv nenv renv in
        let t2 = typecheck e2 env mod_env tenv nenv renv in
        begin match t1, t2 with
        | RecordT r , LabelT l -> 
            (* print_endline (string_of_type (find l r)); *) 
            find l r  (* TODO please insert type errors, use Option to handle unknown situations *)

        | RecordAttrT n , LabelAttrT(n', t) ->
            if n = n' then t
            else assert false

        (* | EntityT(n, RecordT at) , LabelT l -> find l at *)
        (* | AttributeT(n, t, RecordT pt) , LabelT l -> find l pt  *)

        | _ -> 
            print_string (string_of_type t1 ); 
            print_string ("\n"^(string_of_type t2 ));
            assert false (* TODO JCS: SHOULD BE AN ERROR *)
        end
    
    | IsOfType(e', t) ->  (* TODO JCS: THIS CANNOT BE OF ANY TYPE? *)
        let te = typecheck e' env mod_env tenv nenv renv in
        begin match te with
        | AttributeT(n, t') -> BoolT
        | _ -> assert false
        end
    
    | IfNode(c, t, f) -> 
        let tc = typecheck c env mod_env tenv nenv renv in
        begin match tc with
          BoolT -> 
            let tt = typecheck t env mod_env tenv nenv renv in
            let tf = typecheck f env mod_env tenv nenv renv in
            begin match tt,tf with
              ListT l1, ListT [] ->raise (Exception "IfNode: List of Nodes cannot be empty.")
            | ListT [], ListT l2 ->raise (Exception "IfNode: List of Nodes cannot be empty.")
            | ListT l1, ListT l2 -> 
                (* Para cada nÃ³, modificar cada path *)
                
                let modify_path l i = map (fun (x,pt) -> (i :: (List.tl x),pt)) l in 
                let modify_list l = List.mapi (fun i (NodeTP(ct,pt)) -> (NodeTP(ct, modify_path pt i))) l in 
                let l1' = modify_list l1 in 
                let l2' = modify_list l2 in 
                
                let fn = (fun (ct,pt) (NodeTP(x,y)) -> (append x ct, append y pt)) in
                let p1, pt1 = List.fold_left fn ([],[]) l1' in
                let p2, pt2 = List.fold_left fn ([],[]) l2' in
                NodeTP(append (List.rev p1) (List.rev p2), intersect2 pt1 pt2)
            | _ -> raise (Exception "Second and third term of IfNode should be a List of Nodes.")
            end
        | _ -> raise (Exception "First term of IfNode needs to be of type BoolT.")
        end 
    | AttributesOf e1 ->
        begin 
        match typecheck e1 env mod_env tenv nenv renv with
        | EntityT(n, RecordT at) -> ListT (map (fun (l, t) -> t) at)
        | _ -> assert false
        end 

    | ForNode(s, t, e1, e2) ->
        let t1 = typecheck e1 env mod_env tenv nenv renv in
        begin
        match t1 with
        | RecordT r ->
            let loop acc r = List.fold_left (fun (acc,ct,i) (l,e') ->
                let env' = define s e' env in 
                let t2 = typecheck e2 env' mod_env tenv nenv renv in
                begin 
                match t2 with
                | NodeTP (a, pt) -> 
                    let pt' = map (fun (p,t) -> (i :: List.tl p,t)) pt in
                    (append acc a, append ct pt', i+1)
                | _ -> assert false
                end
            ) acc r
            in 
            let p,pt,_ = loop ([],[],0) r in
            NodeTP (p,pt)
        | ListT l ->
            let loop acc l = List.fold_left (fun (acc,ct,i) hd ->
                let env' = define s hd env in
                let hd' = (
                    match hd with
                    | AttributeT(n, t') -> t'
                    | _ -> hd 
                ) in 
                let tenv' = define t hd' env in
                let t2 = typecheck e2 env' mod_env tenv' nenv renv in
                begin
                match t2 with
                | NodeTP (a, pt) -> 
                    let pt' = map (fun (p,t) -> (i :: List.tl p,t)) pt in
                    (append acc a, append ct pt', i+1)
                | _ -> assert false
                end
            ) acc l
            in 
            let p, pt, _ = loop ([],[],0) l in
            NodeTP (p,pt)
        | _ -> assert false
        end 

    
    | List l -> 
        let lt = map (fun x -> typecheck x env mod_env tenv nenv renv) l in ListT lt (* TODO Not OK *)

    | Path p ->
        let typecheck_path_element x = 
            let r = typecheck x env mod_env tenv nenv renv in
            begin match r with 
                NumT -> r 
            | _ -> raise (Exception "Path is only made up of Num.") 
            end in
        let p' = map typecheck_path_element p in
        PathT p'

    | Template(x, t, e1) -> (* Compile time abstraction *)
        let _ = typecheck_t t env mod_env tenv nenv renv in (* TODO typecheck_t -> well formed type *)
        let env' = define x t env in
        let t1 = typecheck e1 env' mod_env tenv nenv renv in
        TemplateT(t,t1) (* TemplateT == Fun type *)

    | Instantiate(e1, e2) -> (* Compile time application *)
        let t1 = typecheck e1 env mod_env tenv nenv renv in
        begin
        print_endline (string_of_type t1);
        match t1 with
        | TemplateT(t3,t4) -> 
            let t2 = typecheck e2 env mod_env tenv nenv renv in 
            print_endline "Unifying ";
            print_endline (string_of_type t2);
            print_endline (string_of_type t3);
            let unification = unify t2 t3 in
            if unification <> None then (* Isto nÃ£o pode ser um = tem que unificar os tipos e obter uma substituiÃ§Ã£o *)
                apply unification t4 
            else assert false (* JCS: Tens que retornar erros decentes *)
        | _ -> assert false
        end

    | ForAllName(n, e1) ->
        let nenv' = define n (VarNT n) nenv in
        let t1 = typecheck e1 env mod_env tenv nenv' renv in
        ForAllNameT(n, t1)

    | CallName(e1, n) ->
        print_endline ("CallName "^n);
        let t1 = typecheck e1 env mod_env tenv nenv renv in
        begin
        match t1 with
        | ForAllNameT(n1, t2) -> let t = subst_name n1 n t2 in print_endline (string_of_type t); t
        | _ -> assert false
        end

    | ForAllType(t, e1) ->
        let tenv' = define t (VarT t) tenv in
        let t1 = typecheck e1 env mod_env tenv' nenv renv in
        ForAllTypeT(t, t1)

    | CallType(e1, t) ->
        let t1 = typecheck e1 env mod_env tenv nenv renv in
        let t' = typecheck_t t env mod_env tenv nenv renv in
        begin
        match t1 with
        | ForAllTypeT(t'', t2) -> subst_type t'' t' t2 
        | _ -> assert false
        end

    | ForAllRows(x, e1) ->
        let renv' = define x (VarR x) renv in
        let t1 = typecheck e1 env mod_env tenv nenv renv' in
        ForAllRowsT(x, t1) 
        
    | CallRows(e1, r) ->
        let t1 = typecheck e1 env mod_env tenv nenv renv in
        let r' = typecheck_t r env mod_env tenv nenv renv in
        begin
        match t1 , r' with
        | ForAllRowsT(x, t2) , RecordT l -> subst_rec x r t2 
        | _ -> assert false
        end

    | ApplyOp(t1,op,p,i,t) ->
        let t1' = typecheck t1 env mod_env tenv nenv renv in
        let p' = typecheck p env mod_env tenv nenv renv in
        let extract_path (Path(p)) = List.rev (List.fold_left (fun a (Num(n)) -> n :: a) [] p) in
        let p'' = begin match p' with
        | PathT _ -> extract_path p
        | _-> raise (Exception "ApplyOp: Third term needs to be of type Path.")
        end in
        let i' = typecheck i env mod_env tenv nenv renv in
        let t' = typecheck t env mod_env tenv nenv renv in
        let rec firstel k xs = 
            match xs with
            | [] -> xs
            | x::xs -> if k=1 then [x] else x :: firstel (k-1) xs
        in
        let update p idx l fn =
            let s = List.length p in
            map (fun (x,y) -> 
                if firstel s x = p then
                    (List.mapi (fun i x' -> if i = s && x' >= idx then fn x' else x') x,y)
                else
                    (x,y)) l in
        begin match t1',op,i',t' with
        | NodeTP(n,l), Insert, NumT, NodeTP(_,l2) -> 
            let idx = match i with 
                | Num(n) -> n
                | _ -> assert false
            in 
            if idx < 0 then
                raise (Exception "ApplyOp/Insert: Index needs to be bigger than 0.")
            else
                let l' = update p'' idx l (fun x -> x+ 1) in 
                let get_path_with_index (Num(i)) p = append p [i] in
                let pni = get_path_with_index i p'' in
                let r = map (fun (x,y) -> (append pni (List.tl x),y)) l2 in
                NodeTP(n, append l' r )
        | NodeTP(n,l), Remove, UnitT, UnitT ->
            let s = List.length p'' in
            let l' = List.filter(fun (x,_) -> (firstel s x) <> p'') l in
            let p''', idx = remove_last [] p'' in
            let l'' = update p''' idx l' (fun x -> x-1) in 
            if l'' = [] then
                UnitT
            else
                NodeTP(n, l'')
        | NodeTP(n,l), Replace, UnitT, NodeTP(n1,l1) ->
            let n' = 
                if List.length p'' = 1 then
                    let replace l pos a  = List.mapi (fun i x -> if i = pos then a else x) l in
                    replace n (List.hd p'') (List.hd n1)
                else n
            in  (* Se for para substituir na raÃ­z de n, entÃ£o colocar n1 em n *)
            let r' = List.filter (fun (x,a) -> (firstel (List.length p'') x) <> p'') l in 
            let r = (map (fun (x,a) -> (p'' @ (List.tl x),a) ) l1) @ r' in
            NodeTP(n', r)
        | NodeTP(n,l), Move, NumT, UnitT -> 
                let pn', last = remove_last [] p'' in
                let idx = match i with 
                    | Num(n) -> n
                    | _ -> assert false
                in 
                if idx < 0 then
                    raise (Exception "ApplyOp/Move: Index needs to be bigger than 0.")
                else
                    let r = List.filter (fun (x,a) -> (firstel (List.length p'') x) = p'') l in
                    let r' = update pn' last r (fun x -> idx) in
                    let l = List.filter (fun (x,a) -> (firstel (List.length p'') x) <> p'') l in 
                    let l' = update pn' last l (fun x -> x - 1) in
                    let l'' = update pn' idx l' (fun x -> x + 1) in
                    NodeTP(n, append r' l'') 
        | NodeTP(n,l), AddProp, UnitT, RecordT r -> 
            let el = List.filter (fun(p,pt) -> p = p'') l in
            if el = [] then
                NodeTP(n,(p'',r)::l)
            else
                let _,a = List.hd el in
                let r' = List.filter (fun(l,_) -> not (List.exists (fun(l',_) -> l = l') a)) r in
                let l' = map (fun (x,a) -> if x = p'' then (x,append a r') else (x,a)) l in
                NodeTP(n,l')
        | NodeTP(n,l), RemoveProp, UnitT, ListT lt -> 
            let rec check_list l = match l with
            | [] -> ()
            | StringT :: xs -> check_list xs
            | _ :: xs -> raise (Exception ("ApplyOp/RemoveProp: List needs to be of Strings."))
            in
            check_list lt;
            let el = List.filter (fun(p,pt) -> p = p'') l in
            if el = [] then
                NodeTP(n,(p'',[])::l)
            else 
                let lt' = match t with
                    | List l -> l
                    | _ -> assert false
                in
                let _,a = List.hd el in
                let r = List.filter (fun (l,_) -> not (List.exists (fun (String x) -> x = l) lt')) a in
                let l' = map (fun (x,pt) -> if x = p'' then (p'',r) else (x,pt)) l in
                NodeTP(n,l') 
        | NodeTP(n,l), ReplaceProp, UnitT, RecordT r -> 
            let el = List.filter (fun(p,pt) -> p = p'') l in
            if el = [] then
                NodeTP(n,(p'',r)::l)
            else 
                let _,a = List.hd el in
                let r'= List.filter (fun(l,_) -> not (List.exists (fun(l',_) -> l = l') a)) r in
                let l' = map (fun (x,a) -> if x = p'' then (x,append a r') else (x,a)) l in
                NodeTP(n,l')
        | _ -> raise (Exception "ApplyOp: Incorrect types.")
        end
    | _ -> print_endline "Not yet implemented"; assert false


let a1 = Attribute("Product", "IsInStock", BoolT, Record(("DisplayName", String "Is In Stock")::[]));;
let a2 = Attribute("Product", "Description", StringT, Record(("DisplayName", String "Description")::[]));;
let e = Entity("Product", Record(("IsInStock",a1)::("Description",a2)::[]) );;

let exp1 = Node(Expression, [], List []);;
let c1 = Node(Column, [], List [exp1])
let i1 = Node(Icon, [], List []);;
let cb = Node(CheckBox, [], List []);;
let exp2 = Node(Expression, [], List []);;
let ifn = IfNode(IsOfType(Var "attr", BoolT), List[i1;cb], List [exp2])
let lt = Let("attr", a1, ifn)
let c2 = Node(Column, [], List[lt])
let t = Node(Table, [], List [c1;c2])
let inp = Node(Input, [], List [])
let pg = Node(Pages, [], List [])
let btn = Node(Button, [], List [])
let screen = Node(Screen, [], List [inp; btn; t; pg])

let r = typecheck screen [] [] [] [] []

let aop1 = ApplyOp(screen, Insert, Path(Num(0)::Num(2)::Num(1)::[]), Num(2),Node(Input, [], List []))
let _ = assert (typecheck aop1 [] [] [] [] [] =  
    NodeTP ([Screen],
        [([0], []); ([0; 3], []); ([0; 2], []); ([0; 2; 1], []); ([0; 2; 1; 0], []);
        ([0; 2; 0], []); ([0; 2; 0; 0], []); ([0; 1], []); ([0; 0], []);
        ([0; 2; 1; 2], [])]))
let _ = let r,_ = eval_tree aop1 [] in assert (r = [Node (Screen, [],
    List
    [Node (Input, [], List []); Node (Button, [], List []);
    Node (Table, [],
    List
        [Node (Column, [], List [Node (Expression, [], List [])]);
        Node (Column, [],
        List
        [Node (Icon, [], List []); Node (CheckBox, [], List []);
            Node (Input, [], List [])])]);
    Node (Pages, [], List [])])])

let aop2 = ApplyOp(screen, Remove, Path(Num(0)::Num(2)::Num(1)::Num(1)::[]), Unit,Unit)
let t = assert (typecheck aop2 [] [] [] [] [] =  
    NodeTP ([Screen],
        [([0], []); ([0; 3], []); ([0; 2], []); ([0; 2; 1], []); ([0; 2; 1; 0], []);
        ([0; 2; 0], []); ([0; 2; 0; 0], []); ([0; 1], []); ([0; 0], [])]))
let _ = let r,_= eval_tree aop2 [] in assert (r = [Node (Screen, [],
        List
        [Node (Input, [], List []); Node (Button, [], List []);
        Node (Table, [],
        List
            [Node (Column, [], List [Node (Expression, [], List [])]);
            Node (Column, [], List [Node (Icon, [], List [])])]);
        Node (Pages, [], List [])])])

let aop3 = ApplyOp(screen, Replace, Path(Num(0)::Num(2)::Num(1)::Num(1)::[]), Unit,Node(Input, [], List []))
let _ = assert (typecheck aop3 [] [] [] [] [] = NodeTP ([Screen],
    [([0; 2; 1; 1], []); ([0], []); ([0; 3], []); ([0; 2], []); ([0; 2; 1], []);
    ([0; 2; 1; 0], []); ([0; 2; 0], []); ([0; 2; 0; 0], []); ([0; 1], []);
    ([0; 0], [])]))
let _ = let r,_ = eval_tree aop3 [] in assert (r =
    [Node (Screen, [],
  List
   [Node (Input, [], List []); Node (Button, [], List []);
    Node (Table, [],
     List
      [Node (Column, [], List [Node (Expression, [], List [])]);
       Node (Column, [],
        List [Node (Icon, [], List []); Node (Input, [], List [])])]);
    Node (Pages, [], List [])])])

let aop4 = ApplyOp(screen, Move, Path(Num(0)::Num(2)::Num(1)::Num(1)::[]), Num(0),Unit)
let _ = assert (typecheck aop4 [] [] [] [] [] = NodeTP ([Screen],
    [([0], []); ([0; 3], []); ([0; 2], []); ([0; 2; 1], []); ([0; 2; 1; 1], []);
    ([0; 2; 0], []); ([0; 2; 0; 0], []); ([0; 1], []); ([0; 0], [])]))
let _ = let r,_ = eval_tree aop4 [] in assert (r = [Node (Screen, [],
    List
    [Node (Input, [], List []); Node (Button, [], List []);
    Node (Table, [],
    List
        [Node (Column, [], List [Node (Expression, [], List [])]);
        Node (Column, [],
        List [Node (CheckBox, [], List []); Node (Icon, [], List [])])]);
    Node (Pages, [], List [])])])

let aop5 = ApplyOp(screen, AddProp, Path(Num(0)::Num(2)::Num(1)::Num(0)::[]), Unit,Record[("Title", String "Title")])
let _ = assert (typecheck aop5 [] [] [] [] [] = NodeTP ([Screen],
    [([0], []); ([0; 3], []); ([0; 2], []); ([0; 2; 1], []);
    ([0; 2; 1; 0], [("Title", StringT)]); ([0; 2; 0], []); ([0; 2; 0; 0], []);
    ([0; 1], []); ([0; 0], [])]))
let _ = let r,_ = eval_tree aop5 [] in assert (r = [Node (Screen, [],
    List
    [Node (Input, [], List []); Node (Button, [], List []);
    Node (Table, [],
    List
        [Node (Column, [], List [Node (Expression, [], List [])]);
        Node (Column, [],
        List
        [Node (Icon, [("Title", String "Title")], List []);
            Node (CheckBox, [], List [])])]);
    Node (Pages, [], List [])])])

let aop6 = ApplyOp(screen, AddProp, Path(Num(0)::Num(2)::Num(1)::Num(1)::[]), Unit,Record[("Title", String "Title")])
let _ = assert (typecheck aop6 [] [] [] [] [] = NodeTP ([Screen],
    [([0; 2; 1; 1], [("Title", StringT)]); ([0], []); ([0; 3], []); ([0; 2], []);
    ([0; 2; 1], []); ([0; 2; 1; 0], []); ([0; 2; 0], []); ([0; 2; 0; 0], []);
    ([0; 1], []); ([0; 0], [])]))
let _ = let r,_ = eval_tree aop6 [] in assert (r =
    [Node (Screen, [],
    List
    [Node (Input, [], List []); Node (Button, [], List []);
        Node (Table, [],
        List
        [Node (Column, [], List [Node (Expression, [], List [])]);
        Node (Column, [],
            List
            [Node (Icon, [], List []);
            Node (CheckBox, [("Title", String "Title")], List [])])]);
        Node (Pages, [], List [])])])

let aop7 = ApplyOp(aop6, RemoveProp, Path(Num(0)::Num(2)::Num(1)::Num(1)::[]), Unit, List [String "Title"; String "A"])
let _ = assert (typecheck aop7 [] [] [] [] []  = NodeTP ([Screen],
    [([0; 2; 1; 1], []); ([0], []); ([0; 3], []); ([0; 2], []); ([0; 2; 1], []);
    ([0; 2; 1; 0], []); ([0; 2; 0], []); ([0; 2; 0; 0], []); ([0; 1], []);
    ([0; 0], [])]))
let _ = let r,_ = eval_tree aop7 [] in assert (r = [Node (Screen, [],
        List
        [Node (Input, [], List []); Node (Button, [], List []);
        Node (Table, [],
        List
            [Node (Column, [], List [Node (Expression, [], List [])]);
            Node (Column, [],
            List [Node (Icon, [], List []); Node (CheckBox, [], List [])])]);
        Node (Pages, [], List [])])])

let aop8 = ApplyOp(aop6, ReplaceProp, Path(Num(0)::Num(2)::Num(1)::Num(1)::[]), Unit,Record[("Title", String "New Title");("Display Name", String "S")])
let _ = assert (typecheck aop8 [] [] [] [] [] = NodeTP ([Screen],
    [([0; 2; 1; 1], [("Title", StringT); ("Display Name", StringT)]); ([0], []);
    ([0; 3], []); ([0; 2], []); ([0; 2; 1], []); ([0; 2; 1; 0], []);
    ([0; 2; 0], []); ([0; 2; 0; 0], []); ([0; 1], []); ([0; 0], [])]))
let _ = let r,_ = eval_tree aop8 [] in assert (r = [Node (Screen, [],
        List
        [Node (Input, [], List []); Node (Button, [], List []);
        Node (Table, [],
        List
            [Node (Column, [], List [Node (Expression, [], List [])]);
            Node (Column, [],
            List
            [Node (Icon, [], List []);
                Node (CheckBox, [("Title", String "New Title")], List [])])]);
        Node (Pages, [], List [])])])


(**)

let exp1 = Node(Expression, [], List []);;
let c1 = Node(Column, [], List [exp1])
let i1 = Node(Icon, [], List []);;
let c2 = Node(Column, [], List[i1])
let t = Node(Table, [], List [c1;c2])
let inp = Node(Input, [], List [])
let pg = Node(Pages, [], List [])
let btn = Node(Button, [], List [])
let screen = Node(Screen, [], List [inp; btn; t; pg])

let aop1 = ApplyOp(screen, Move, Path(Num(0)::Num(2)::[]), Num(0),Unit)
let _ = assert (typecheck aop1 [] [] [] [] [] = NodeTP ([Screen],
    [([0; 0], []); ([0; 0; 1], []); ([0; 0; 1; 0], []); ([0; 0; 0], []);
    ([0; 0; 0; 0], []); ([0], []); ([0; 3], []); ([0; 2], []); ([0; 1], [])]))
let _ = let r,_ = eval_tree aop1 [] in assert (r = [Node (Screen, [],
    List
    [Node (Table, [],
    List
        [Node (Column, [], List [Node (Expression, [], List [])]);
        Node (Column, [], List [Node (Icon, [], List [])])]);
    Node (Input, [], List []); Node (Button, [], List []);
    Node (Pages, [], List [])])])

let aop2 = ApplyOp(screen, AddProp, Path(Num(0)::Num(0)::[]), Unit,Record[("A", String "a")])
let _ = assert (typecheck aop2 [] [] [] [] [] =
    NodeTP ([Screen],
 [([0], []); ([0; 3], []); ([0; 2], []); ([0; 2; 1], []); ([0; 2; 1; 0], []);
  ([0; 2; 0], []); ([0; 2; 0; 0], []); ([0; 1], []);
  ([0; 0], [("A", StringT)])]))
let _ = let r,_ = eval_tree aop2 [] in assert (r = [Node (Screen, [],
    List
    [Node (Input, [("A", String "a")], List []); Node (Button, [], List []);
    Node (Table, [],
    List
        [Node (Column, [], List [Node (Expression, [], List [])]);
        Node (Column, [], List [Node (Icon, [], List [])])]);
    Node (Pages, [], List [])])])

let aop3 = ApplyOp(aop2, AddProp, Path(Num(0)::Num(0)::[]), Unit,Record[("A", String "ab");("B",String "b")])
let _ = assert (typecheck aop3 [] [] [] [] [] = NodeTP ([Screen],
    [([0], []); ([0; 3], []); ([0; 2], []); ([0; 2; 1], []); ([0; 2; 1; 0], []);
    ([0; 2; 0], []); ([0; 2; 0; 0], []); ([0; 1], []);
    ([0; 0], [("A", StringT); ("B", StringT)])]))
let _ = let r,_ = eval_tree aop3 [] in assert (r = [Node (Screen, [],
    List
    [Node (Input, [("B", String "b"); ("A", String "a")], List []);
    Node (Button, [], List []);
    Node (Table, [],
    List
        [Node (Column, [], List [Node (Expression, [], List [])]);
        Node (Column, [], List [Node (Icon, [], List [])])]);
    Node (Pages, [], List [])])])

let aop4 = ApplyOp(aop3, RemoveProp, Path(Num(0)::Num(0)::[]), Unit,List [String "A";String "C"])
let _ = assert (typecheck aop4 [] [] [] [] [] = NodeTP ([Screen],
    [([0], []); ([0; 3], []); ([0; 2], []); ([0; 2; 1], []); ([0; 2; 1; 0], []);
    ([0; 2; 0], []); ([0; 2; 0; 0], []); ([0; 1], []);
    ([0; 0], [("B", StringT)])]))
let _ = let r,_ = eval_tree aop4 [] in assert (r = [Node (Screen, [],
    List
    [Node (Input, [("B", String "b")], List []); Node (Button, [], List []);
    Node (Table, [],
    List
        [Node (Column, [], List [Node (Expression, [], List [])]);
        Node (Column, [], List [Node (Icon, [], List [])])]);
    Node (Pages, [], List [])])])

let aop5 = ApplyOp(aop4, ReplaceProp, Path(Num(0)::Num(0)::[]), Unit,Record[("B",Bool false);("C", String "c")])
let _ = assert (typecheck aop5 [] [] [] [] [] = NodeTP ([Screen],
    [([0], []); ([0; 3], []); ([0; 2], []); ([0; 2; 1], []); ([0; 2; 1; 0], []);
    ([0; 2; 0], []); ([0; 2; 0; 0], []); ([0; 1], []);
    ([0; 0], [("B", StringT); ("C", StringT)])]))
let _ = let r,_ = eval_tree aop5 [] in assert (r = [Node (Screen, [],
    List
    [Node (Input, [("B", Bool false)], List []); Node (Button, [], List []);
    Node (Table, [],
    List
        [Node (Column, [], List [Node (Expression, [], List [])]);
        Node (Column, [], List [Node (Icon, [], List [])])]);
    Node (Pages, [], List [])])])


(**)

(* 
let e = Entity<...> in
    let attr = Attribute<...> in
        letbox name = NameOf e in
            letbox label = LabelOf attr in
                Node<Column, 
                    {Title=attr.DisplayName}, 
                    [ 
                        Node<Expression, {Description=box(name.list.current.label)}}, []>
                    ]
                >
                
*)

let a1 = Attribute("Product", "Description",StringT, Record(("DisplayName", String "Description")::[]));;
let a2 = Attribute("Product", "IsInStock",BoolT, Record(("DisplayName", String "Is In Stock")::[]));;
let e = Entity("Product",Record(("Description",a1)::("IsInStock",a2)::[]));;

let sel = (VarRT "name")::(Label "list")::(Label "current")::(VarRT "label")::[]
let exp = mk_select_list sel
let ps = ("Description", (Box exp)) :: []
let ex = Node(Expression, [], List [])
let ps' = ("Title", Select(Var "attr", Label "DisplayName")) :: []
let n' = Node(Column, ps', List (ex :: []))
let label = LetBox("label", LabelOf(Var "attr"), n')
let name = LetBox("name", NameOf(Var "e"), label)
let e'' = Let("attr", a2, name)
let e' = Let("e", e, e'')

let _ = string_of_term (eval e' [])

(**)

let i1 = Node(Icon, [], List []);;
let cb = Node(CheckBox, [], List []);;
let exp1 = Node(Expression, [], List []);;
let inp = Node(Input, [], List []);;
let c = Node(Calendar, [], List []);;
let ifn = IfNode(IsOfType(Var "attr", BoolT), List[i1;cb], List [exp1;c;inp])
let lt = Let("attr", a1, ifn)

let a = typecheck lt [] [] [] [] []
