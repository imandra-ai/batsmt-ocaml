
use {
    batsmt_core::ast_u32::AST,
    batsmt_solver as solver,
    batsmt_cc::{self as cc, theories as ccth},
    crate::ctx::Ctx,
};

type MTheories = (ccth::Selector<AST>, ccth::Disjointness<AST>);
type Th = cc::CCTheory<Ctx, MTheories>;

/// A boolean literal.
pub type Lit = solver::BLit;

/// The SMT solver.
pub struct Solver {
    s: solver::Solver<Ctx, Th>,
    cur_clause: Vec<Lit>, // TODO: actually use sat::Lit instead, convert upon push
    assumptions: Vec<Lit>,
}

impl Solver {
    /// Create a new solver with the given context.
    pub fn new(c: &mut Ctx) -> Self {
        let th: Th = cc::CCTheory::new(&mut c);
        let s = solver::Solver::new(c.builtins(), th);
        Solver{s, cur_clause: vec![], assumptions: vec![], }
    }

    /// Create a new boolean literal.
    pub fn make_lit(&mut self) -> Lit {
        // FIXME
        let v = self.s.s0.sat.new_var_default();
        Lit::new(v, true)
    }

    /// Add a new assumption for the next call to `solve`
    pub fn api_add_assumption(&mut self, lit: Lit) {
        self.assumptions.push(lit);
    }

    /// Add a literal to the next call to `add_clause`
    pub fn api_add_clause_lit(&mut self, lit: Lit) {
        self.cur_clause.push(lit);
    }

    pub fn api_solve(&mut self, c: &mut Ctx) -> bool {
        let r = self.s.solve_with(c, &self.assumptions);
        self.assumptions.clear();
        r
    }

    pub fn api_add_clause(&mut self) {
        self.s.add_bool_clause_reuse(&mut self.cur_clause);
        self.cur_clause.clear();
    }
}