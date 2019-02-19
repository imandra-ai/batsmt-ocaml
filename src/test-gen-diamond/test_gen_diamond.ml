
module S = Batsmt

let debug_ = ref false
let logf_ msg =
  if !debug_ then (
    Format.eprintf msg
  ) else
    Format.ifprintf Format.err_formatter msg

module Solve = struct
  module Ctx = S.Ctx
  module T = S.Term
  module Solver = S.Solver
  module Lit = S.Lit

  type t = {
    ctx: Ctx.t;
    solver: Solver.t;
  }

  let mk_lit_eq (self:t) t1 t2 : Lit.t =
    let t = T.mk_eq self.ctx t1 t2 in
    logf_ "eqn term: %a@." (T.pp self.ctx) t;
    Solver.make_term_lit self.solver self.ctx t

  (* generate the problem and return the lit [x0 != xn] *)
  let gen_problem ({ctx; solver} as self) n : Lit.t =
    let x0 = T.mk_const ctx "x0" in
    let prev_x = ref x0 in
    for i = 1 to n do
      let x = T.mk_const ctx (Printf.sprintf "x%d" i) in
      let y = T.mk_const ctx (Printf.sprintf "y%d" @@ i-1) in
      let z = T.mk_const ctx (Printf.sprintf "z%d" @@ i-1) in
      let branch = Solver.make_lit solver in

      logf_ "lit: %a@." Lit.pp branch;

      Solver.add_clause_l solver [Lit.neg branch; mk_lit_eq self !prev_x y];
      Solver.add_clause_l solver [Lit.neg branch; mk_lit_eq self y x];
      Solver.add_clause_l solver [branch; mk_lit_eq self !prev_x z];
      Solver.add_clause_l solver [branch; mk_lit_eq self z x];

      prev_x := x;
    done;
    (* add [xn != x0] *)
    Lit.neg @@ mk_lit_eq self x0 !prev_x

  let pp_res = function
    | S.Sat -> Format.printf "SAT@."
    | S.Unsat -> Format.printf "UNSAT@."

  let top n : unit =
    logf_  "create ctx and solver@.";
    let ctx = S.Ctx.create() in
    let solver = S.Solver.create ctx in
    let st = { ctx; solver } in
    let lit = gen_problem st n in
    logf_ "solve with no assumption:@.";
    pp_res @@ S.Solver.solve st.solver st.ctx;
    logf_ "solve with assumption x0!=xn@.";
    pp_res @@ S.Solver.solve ~assumptions:[lit] st.solver st.ctx;
    let ucore = S.Solver.unsat_core solver in
    logf_ "unsat-core: [@[%a@]]@." (Format.pp_print_list Lit.pp) (Array.to_list ucore);
    ()
end

let solve_for ~n () : unit = Solve.top n

let () =
  let n = ref 5 in
  let opts = [
    "-n", Arg.Set_int n, " size of diamond problem";
    "-d", Arg.Set debug_, " enable debug";
  ] |> Arg.align
  in
  let usage = "test_gen_diamond [opts]" in
  Arg.parse opts (fun _ -> raise (Arg.Help usage)) usage;
  logf_ "solve for %d@." !n;
  solve_for ~n:!n ()
