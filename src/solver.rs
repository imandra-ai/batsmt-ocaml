
use {
    batsmt_solver as solver,
    batsmt_cc::{self as cc, theories as ccth},
    crate::ctx::Ctx,
};

type MTheories = (ccth::Ite, ccth::Selector, ccth::Disjoint);
type Th = cc::CCTheory<Ctx, MTheories>;

/// A boolean literal.
pub type Lit = solver::Lit;

/// The SMT solver.
pub struct Solver {
    s: solver::Solver<Th>,
}

impl Solver {
    /// Create a new solver with the given context.
    pub fn new(c: &mut Ctx) -> Self {
        let th: Th = cc::CCTheory::new(&mut c);
        let s = solver::Solver::new(c.builtins(), th);
        Solver{s}
    }

    /// Solve the current set of clauses.
    pub fn solve(&mut self) -> bool {
        self.solve_with(&[])
    }

    /// Solve the current set of clauses under assumptions.
    pub fn solve_with(&mut self, assms: &[Lit]) -> bool {
        unimplemented!()
    }

    pub fn add_clause(&mut self, c: &[Lit]) {
        self.s.add_clause(c);
    }
}
