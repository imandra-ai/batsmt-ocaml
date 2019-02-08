
module Ctx = struct
  type t

  external create_ : unit -> t = "ml_batsmt_ctx_new"
  external delete_ : t -> unit = "ml_batsmt_ctx_delete"

  let create() : t =
    let c = create_ () in
    Gc.finalise delete_ c;
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

  external make : Ctx.t -> t = "mk_batsmt_lit_mk" [@@noalloc]
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
  external push_assumption_ : t -> Lit.t -> unit = "ml_batsmt_solver_add_assumption" [@@noalloc]
  external solve_ : t -> bool = "ml_batsmt_solver_solve" [@@noalloc]

  let create (ctx:Ctx.t) : t =
    let s = create_ ctx in
    Gc.finalise delete_ s;
    s

  let solve ?(assumptions=[]) (s:t) : res =
    List.iter (push_assumption_ s) assumptions;
    let is_sat = solve_ s in
    if is_sat then Sat else Unsat
end
