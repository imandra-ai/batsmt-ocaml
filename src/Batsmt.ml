
module Ctx = struct
  type t

  external create_ : unit -> t = "ml_batsmt_ctx_new"

  let create() : t =
    let c = create_ () in
    c
end

module Lit = struct
  type t = int

  let neg n = -n
  let abs = abs
  let sign n = n > 0
  let to_int n = n
  let to_string x = (if sign x then "" else "-") ^ string_of_int (abs @@ to_int x)
  let pp out x = Format.pp_print_string out (to_string x)
  let equal : t -> t -> bool = Pervasives.(=)
  let compare : t -> t -> int = Pervasives.compare
  let hash : t -> int = Hashtbl.hash
end

module Lbool = struct
  type t = True | False | Undefined

  let neg = function Undefined -> Undefined | True -> False | False -> True
  let of_bool b = if b then True else False
  let equal : t -> t -> bool = (=)
  let to_string = function
    | True -> "true"
    | False -> "false"
    | Undefined -> "undefined"

  let pp out l = Format.pp_print_string out (to_string l)
end

module Ty = struct
  type t = int

  let id (t:t) : t = t
  let equal (a:t) b = a=b
  let hash (a:t) = Hashtbl.hash a
  let compare (a:t) b = Pervasives.compare a b

  external bool_ : Ctx.t -> t = "ml_batsmt_ty_bool" [@@noalloc]
  external const_ : Ctx.t -> string -> t = "ml_batsmt_ty_const" [@@noalloc]

  let mk_bool = bool_
  let mk_str = const_

  type view =
    | Bool
    | Const of string
end

module Term = struct
  type t = int

  let id (t:t) : t = t
  let equal (a:t) b = a=b
  let hash (a:t) = Hashtbl.hash a
  let compare (a:t) b = Pervasives.compare a b

  external const_ : Ctx.t -> string -> Ty.t array -> Ty.t -> t = "ml_batsmt_term_const" [@@noalloc]
  external app_fun_ : Ctx.t -> t -> unit = "ml_batsmt_term_app_fun" [@@noalloc]
  external app_arg_ : Ctx.t -> t -> unit = "ml_batsmt_term_app_arg" [@@noalloc]
  external app_finalize_ : Ctx.t -> t = "ml_batsmt_term_app_finalize" [@@noalloc]
  external select_ : Ctx.t -> t -> int -> t -> t = "ml_batsmt_term_select" [@@noalloc]
  external set_cstor_: Ctx.t -> t -> unit = "ml_batsmt_term_set_cstor" [@@noalloc]
  external bool_ : Ctx.t -> bool -> t = "ml_batsmt_term_bool" [@@noalloc]
  external not_ : Ctx.t -> t -> t = "ml_batsmt_term_not" [@@noalloc]
  external eq_ : Ctx.t -> t -> t -> t = "ml_batsmt_term_eq" [@@noalloc]

  external kind : Ctx.t -> t -> int = "ml_batsmt_term_kind" [@@noalloc]
  external get_cst_name_ : Ctx.t -> t -> string = "ml_batsmt_term_get_const_name"
  external get_bool_ : Ctx.t -> t -> bool = "ml_batsmt_term_get_bool" [@@noalloc]
  external get_app_fun_ : Ctx.t -> t -> t = "ml_batsmt_term_get_app_fun" [@@noalloc]
  external get_app_n_args_ : Ctx.t -> t -> int = "ml_batsmt_term_get_app_n_args" [@@noalloc]
  external get_app_nth_arg_ : Ctx.t -> t -> int -> t = "ml_batsmt_term_get_app_nth_arg" [@@noalloc]
  external get_select_ : Ctx.t -> t -> (t * int * t) = "ml_batsmt_term_get_select"

  let mk_bool = bool_
  let mk_eq = eq_
  let mk_not = not_
  let mk_const ctx s args ret : t = const_ ctx s (Array.of_list args) ret

  let mk_cstor ctx s args ret : t =
    let c = mk_const ctx s args ret in
    set_cstor_ ctx c;
    c

  let[@inline] mk_select ctx ~cstor idx u : t =
    select_ ctx cstor idx u

  let app_l ctx f l =
    match l with
    | [] -> f
    | _ ->
      app_fun_ ctx f;
      List.iter (app_arg_ ctx) l;
      app_finalize_ ctx

  let app_a ctx f a =
    if Array.length a = 0 then f
    else (
      app_fun_ ctx f;
      Array.iter (app_arg_ ctx) a;
      app_finalize_ ctx
    )

  type view =
    | Bool of bool
    | App of t * t list
    | Cst_unin of string
    | Cst_cstor of string
    | Select of {
        c: t;
        idx: int;
        sub: t;
      }
    | Not of t

  let list_init n f =
    let rec aux i n f =
      if i=n then []
      else f i :: aux (i+1) n f
    in aux 0 n f

  let view (ctx:Ctx.t) (t:t) : view =
    (* NOTE: keep in sync with `ctx.rs: AstKind` *)
    match kind ctx t with
    | 0 -> (* bool *)
      let b = get_bool_ ctx t in
      Bool b
    | 1 -> (* app *)
      let f = get_app_fun_ ctx t in
      let n = get_app_n_args_ ctx t in
      let args = list_init n (get_app_nth_arg_ ctx t) in
      App (f, args)
    | 2 -> (* const *)
      let s = get_cst_name_ ctx t in
      Cst_unin s
    | 3 -> (* cstor *)
      let s = get_cst_name_ ctx t in
      Cst_cstor s
    | 4 -> (* select *)
      let c, idx, sub = get_select_ ctx t in
      Select {c; idx; sub}
    | 5 -> (* not *)
      let u = get_app_nth_arg_ ctx t 0 in
      Not u
    | n -> failwith ("invalid term kind "^ string_of_int n)

  let pp ctx (out) t =
    (* TODO: print id *)
    let pplist ppx out l =
      List.iteri (fun i x -> if i>0 then Format.fprintf out "@ "; ppx out x) l
    in
    let rec pp out t =
      match view ctx t with
      | Bool b -> Format.pp_print_bool out b
      | Cst_unin s -> Format.fprintf out "%s/%d" s t
      | Cst_cstor s -> Format.fprintf out "%s/%d" s t
      | App (f, []) -> pp out f
      | App (f, l) ->
        Format.fprintf out "(@[%a@ %a@])/%d" pp f (pplist pp) l t
      | Select {c; idx; sub} ->
        Format.fprintf out "(@[select-%d-%a@ %a@])/%d" idx pp c pp sub t
      | Not u -> Format.fprintf out "(@[not@ %a@])" pp u
    in
    pp out t

  let __undef = max_int
end

type res =
  | Sat
  | Unsat

exception E_unsat

module Solver = struct
  type t

  external create_ : Ctx.t -> t = "ml_batsmt_solver_new"
  external mk_lit_ : t -> Lit.t = "ml_batsmt_solver_new_lit" [@@noalloc]
  external mk_term_lit_ : t -> Ctx.t -> Term.t -> Lit.t = "ml_batsmt_solver_new_term_lit" [@@noalloc]
  external push_assumption_ : t -> Lit.t -> unit = "ml_batsmt_solver_add_assumption" [@@noalloc]
  external push_clause_lit_ : t -> Lit.t -> unit = "ml_batsmt_solver_add_clause_lit" [@@noalloc]
  external add_clause_ : t -> unit = "ml_batsmt_solver_add_clause" [@@noalloc]
  external solve_ : t -> Ctx.t -> bool = "ml_batsmt_solver_solve" [@@noalloc]
  external unsat_core_ : t -> Lit.t array = "ml_batsmt_solver_unsat_core"
  external unsat_core_contains_ : t -> Lit.t -> bool = "ml_batsmt_solver_unsat_core_contains" [@@noalloc]
  external value_lvl_0_ : t -> Lit.t -> int = "ml_batsmt_solver_value_lvl_0" [@@noalloc]
  external value_ : t -> Lit.t -> int = "ml_batsmt_solver_value" [@@noalloc]

  let create (ctx:Ctx.t) : t =
    let s = create_ ctx in
    s

  let add_clause_l (s:t) (c: Lit.t list) : unit =
    List.iter (push_clause_lit_ s) c;
    add_clause_ s

  let add_clause_a (s:t) (c: Lit.t array) : unit =
    Array.iter (push_clause_lit_ s) c;
    add_clause_ s

  let make_lit = mk_lit_
  let make_term_lit = mk_term_lit_

  external simplify_ : t -> bool = "ml_batsmt_solver_simplify"

  let simplify (s:t) : res =
    if simplify_ s then Sat else Unsat

  let simplify_exn (s:t) : unit =
    if simplify_ s then () else raise E_unsat

  let solve_a ?(assumptions=[||]) (s:t) (ctx:Ctx.t) : res =
    Array.iter (push_assumption_ s) assumptions;
    let is_sat = solve_ s ctx in
    if is_sat then Sat else Unsat

  let solve_exn_a ?(assumptions=[||]) (s:t) (ctx:Ctx.t) : unit =
    Array.iter (push_assumption_ s) assumptions;
    let is_sat = solve_ s ctx in
    if is_sat then () else raise E_unsat

  let solve ?(assumptions=[]) (s:t) (ctx:Ctx.t) : res =
    List.iter (push_assumption_ s) assumptions;
    let is_sat = solve_ s ctx in
    if is_sat then Sat else Unsat

  let solve_exn ?(assumptions=[]) (s:t) (ctx:Ctx.t) : unit =
    List.iter (push_assumption_ s) assumptions;
    let is_sat = solve_ s ctx in
    if is_sat then () else raise E_unsat

  let unsat_core = unsat_core_
  let unsat_core_contains = unsat_core_contains_

  let lbool_of_int_ = function
    | 0 -> Lbool.True
    | 1 -> Lbool.False
    | 2 -> Lbool.Undefined
    | _ -> assert false

  let value_lvl_0 (s:t) (lit: Lit.t) : Lbool.t = value_lvl_0_ s lit |> lbool_of_int_
  let value (s:t) (lit: Lit.t) : Lbool.t = value_ s lit |> lbool_of_int_

  external n_lits: t -> int = "ml_batsmt_nlits" [@@noalloc]
  external n_clauses : t -> int = "ml_batsmt_nclauses" [@@noalloc]
  external n_conflicts : t -> int = "ml_batsmt_nconflicts" [@@noalloc]
  external n_decisions : t -> int = "ml_batsmt_ndecisions" [@@noalloc]
  external n_props : t -> int = "ml_batsmt_nprops" [@@noalloc]

  external n_proved_lvl_0 : t -> int = "ml_batsmt_solver_n_proved_lvl_0" [@@noalloc]
  external proved_lvl_0 : t -> int -> Lit.t = "ml_batsmt_solver_proved_lvl_0" [@@noalloc]
end

external log_ : string -> unit = "ml_batsmt_set_log_lvl"
let set_log_lvl = log_
