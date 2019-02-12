
module Ctx = struct
  type t

  external create_ : unit -> t = "ml_batsmt_ctx_new"
  external delete_ : t -> unit = "ml_batsmt_ctx_delete"

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
end

module Term = struct
  type t = int

  (* TODO: constructors + view? *)

end

type res =
  | Sat
  | Unsat

module Solver = struct
  type t

  external create_ : Ctx.t -> t = "ml_batsmt_solver_new"
  external delete_ : t -> unit = "ml_batsmt_solver_delete"
  external mk_lit_ : t -> Lit.t = "ml_batsmt_solver_new_lit"
  external push_assumption_ : t -> Lit.t -> unit = "ml_batsmt_solver_add_assumption" [@@noalloc]
  external push_clause_lit_ : t -> Lit.t -> unit = "ml_batsmt_solver_add_clause_lit" [@@noalloc]
  external add_clause_ : t -> unit = "ml_batsmt_solver_add_clause" [@@noalloc]
  external solve_ : t -> Ctx.t -> bool = "ml_batsmt_solver_solve" [@@noalloc]
  external unsat_core_ : t -> Lit.t array = "ml_batsmt_solver_unsat_core"
  external unsat_core_contains_ : t -> Lit.t -> bool = "ml_batsmt_solver_unsat_core_contains" [@@noalloc]
  external value_lvl_0_ : t -> Lit.t -> int = "ml_batsmt_solver_value_lvl_0" [@@noalloc]

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

  let solve ?(assumptions=[]) (s:t) (ctx:Ctx.t) : res =
    List.iter (push_assumption_ s) assumptions;
    let is_sat = solve_ s ctx in
    if is_sat then Sat else Unsat

  let unsat_core = unsat_core_
  let unsat_core_contains = unsat_core_contains_

  let value_lvl_0 (s:t) (lit: Lit.t) : Lbool.t =
    let i = value_lvl_0_ s lit in
    if i=0 then Lbool.True
    else if i=1 then Lbool.False
    else (
      assert (i=2);
      Lbool.Undefined
    )
end
