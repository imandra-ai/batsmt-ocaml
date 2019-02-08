
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

  val solve : ?assumptions:Lit.t list -> t -> res
end

(* TODO: insert [blit -> term] *)
(* TODO: get [blit -> (term*bool) option] *)
(* TODO: get [term -> blit option] *)
