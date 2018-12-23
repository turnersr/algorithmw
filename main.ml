open Base

let printf = Core.printf

type lit =
    | LInt of int
    | LBool of bool

type exp =
    | EVar of string
    | ELint of lit
    | EApp of exp * exp
    | EAbs of string * exp 
    | ELet of string * exp * exp 


type typevalue =
    | TVar of string
    | TInt
    | TBool
    | TFun of typevalue * typevalue


type scheme = Scheme of string list * typevalue

type subst  = Subst of (string, typevalue, String.comparator_witness) Map.t


module type Transforms = sig
    type t

    val ftv :  t -> (string, String.comparator_witness)Set.t
    val apply : subst -> t -> t
end

module Operations (T:Transforms) = struct 
    let ftv t = T.ftv t
    let apply s t = T.apply s t
end 

module TypeTransforms = struct
    type t = typevalue

    let ftv (TVar n) = Set.singleton (module String) n
    let ftv (TInt) = Set.empty (module String)
    let ftv (TBool) = Set.empty (module String)
    let ftv (TFun(t1,t2)) = Set.union (ftv t1) (ftv t2)

    let apply (Subst s) (TVar n) = match Map.find s n with
                                        | None -> (TVar n)
                                        | Some t -> t
    let apply (Subst s) (TFun(t1, t2)) = TFun((apply (Subst s) t1),(apply (Subst s) t1))
    let apply (Subst s) t = t   
end


module Type =  Operations(TypeTransforms)


module SchemeTransforms = struct
    type t = scheme
    let ftv (Scheme(vars, t)) =  Set.diff (Type.ftv t) (Set.of_list (module String) vars)

    let apply (Subst s) (Scheme(vars, t))  =  
    let x = List.map ~f:(Map.remove s) vars in 
    Scheme(vars, (Type.apply (Subst s) t))
end 


module Scheme = Operations(SchemeTransforms)

let nullSubst = Map.empty (module String)
let composeSubst s1 s2   = Map.map ~f:(Type.apply s1) s2


let  () = 
    printf "%s\n" "Hello World";
    let testexp = [LInt 1; LBool true] in 
    let e0 = ELet ("id", EAbs ("x" , EVar "x"), EVar "id") in ()