
module Ctx : sig
  type t

  val create : unit -> t
end

(* TODO: expose BLit, with conversions from integers, etc. *)
module Lit : sig
  type t

  val abs : t -> t
  val to_int : t -> int
  val sign : t -> bool
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int

  val make : Ctx.t -> t
end

module Term : sig
  type t

  (* TODO
  val mk_unin : Ctx.t -> string -> arity:int -> t
  val mk_cstor : Ctx.t -> string -> arity:int -> t
  val mk_select: Ctx.t -> cstor:t -> int -> t
  val mk_bool : Ctx.t -> bool -> t
  val mk_eq : Ctx.t -> t -> t -> t
  val apply : Ctx.t -> t -> t list -> t

  type view =
    | Bool of bool
    | App of t * t list
    | Cst_unin of string
    | Cst_cstor of string
    | Select of { cstor: t; idx: int }

  val view : Ctx.t -> t -> view

  (** Printing, based on {!view} *)
  val pp : Ctx.t -> Format.formatter -> t -> unit
  *)

  (* TODO:
     - constructors
     - selectors
     - unin-functions (from arity)
   *)
end

type res =
  | Sat
  | Unsat

module Solver : sig
  type t

  val create : Ctx.t -> t

  val add_clause_l : t -> Lit.t list -> unit
  val add_clause_a : t -> Lit.t array -> unit

  val solve : ?assumptions:Lit.t list -> t -> res

  (* TODO:
    - unsat core
    - model
    - add-term-lit (bidir mapping)
  *)
end

(* TODO: insert [blit -> term] *)
(* TODO: get [blit -> (term*bool) option] *)
(* TODO: get [term -> blit option] *)
