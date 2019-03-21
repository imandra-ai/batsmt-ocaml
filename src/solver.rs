
use {
    batsmt_core::{ast, ast_u32::AST},
    batsmt_solver::{self as solver, blit::SatLit},
    batsmt_cc::{self as cc, theories as ccth, Ctx as CCCtx, CCView},
    crate::ctx::Ctx,
};

type MTheories = (ccth::Constructor<AST>, );
type Th = cc::CCTheory<Ctx, MTheories>;

/// A boolean literal.
pub type Lit = solver::BLit;

/// An optional truth value.
pub type Lbool = solver::solver::lbool;

/// The SMT solver.
pub struct Solver {
    s: solver::Solver<Ctx, Th>,
    cur_clause: Vec<SatLit>,
    assumptions: Vec<SatLit>,
    iter: ast::iter_dag::State<AST, ast::HashSet<AST>>,
}

#[inline]
fn bool_of_res(r: solver::solver::Res) -> bool {
    match r {
        solver::solver::Res::SAT => true,
        solver::solver::Res::UNSAT => false,
    }
}

impl Solver {
    /// Create a new solver with the given context.
    pub fn new(c: &mut Ctx) -> Self {
        let th: Th = cc::CCTheory::new(c);
        let s = solver::Solver::new(c.builtins(), th);
        Solver{
            s, cur_clause: vec![], assumptions: vec![],
            iter: ast::iter_dag::new(),
        }
    }

    /// Create a new boolean literal.
    #[inline]
    pub fn api_make_lit(&mut self) -> Lit {
        let lit = Lit::from(self.s.new_bool_lit());
        //println!("make-lit {:?}", lit);
        lit
    }

    /// Create or get the boolean literal for this term.
    #[inline]
    pub fn api_make_term_lit(&mut self, ctx: &mut Ctx, t: AST) -> Lit {
        let Solver{s, iter, ..} = self;
        let lit = s.new_term_lit(ctx, t);
        //println!("make-term-lit for {:?}: {:?}", batsmt_pretty::pp1(ctx, &t), lit);
        // add boolean subterms
        iter.iter_mut(ctx, &t, |ctx, u| {
            if ctx.is_boolean_term(u) {
                if let CCView::Not(_) = ctx.view_as_cc_term(u) {
                } else {
                    // map to literal
                    s.new_term_lit(ctx, *u);
                }
            }
        });
        lit
    }

    /// Add a new assumption for the next call to `solve`
    pub fn api_add_assumption(&mut self, lit: Lit) {
        self.assumptions.push(lit.0);
    }

    /// Add a literal to the next call to `add_clause`
    pub fn api_add_clause_lit(&mut self, lit: Lit) {
        self.cur_clause.push(lit.0);
    }

    /// Solve the current set of clauses using the current assumptions.
    /// 
    /// Returns `true` if satisfiable, `false` otherwise.
    pub fn api_solve(&mut self, c: &mut Ctx) -> bool {
        let r = self.s.solve_with(c, &self.assumptions[..]);
        self.assumptions.clear();
        bool_of_res(r)
    }

    pub fn api_simplify(&mut self) -> bool {
        let r = self.s.sat_simplify();
        bool_of_res(r)
    }

    /// Obtain unsat-core (subset of assumptions).
    /// 
    /// precondition: last call to `api_solve` returned `false.
    #[inline]
    pub fn api_unsat_core(&mut self) -> &[SatLit] {
        self.s.get_unsat_core()
    }

    /// Check if this literal belongs in the unsat-core (subset of assumptions).
    /// 
    /// precondition: last call to `api_solve` returned `false.
    #[inline]
    pub fn api_unsat_core_contains(&mut self, lit: Lit) -> bool {
        self.s.unsat_core_contains_lit(lit.0)
    }

    /// Obtain literals proved at level 0.
    #[inline]
    pub fn api_proved_at_lvl_0(&self) -> &[SatLit] {
        self.s.proved_at_lvl_0()
    }

    /// Obtain value of this literal at level 0.
    #[inline]
    pub fn api_value_lvl_0(&self, lit: Lit) -> Lbool {
        self.s.value_at_lvl_0(lit)
    }

    /// Obtain value of this literal in the model.
    #[inline]
    pub fn api_value(&self, lit: Lit) -> Lbool {
        self.s.value_lit(lit)
    }

    /// Add the current clause to the SAT solver.
    pub fn api_add_clause(&mut self) {
        //println!("add clause {:?}", &self.cur_clause);
        self.s.add_bool_clause_reuse(&mut self.cur_clause);
        self.cur_clause.clear();
    }

    pub fn api_n_lits(&self) -> usize { self.s.n_lits() }
    pub fn api_n_props(&self) -> usize { self.s.n_props() }
    pub fn api_n_clauses(&self) -> usize { self.s.n_clauses() }
    pub fn api_n_conflicts(&self) -> usize { self.s.n_conflicts() }
    pub fn api_n_decisions(&self) -> usize { self.s.n_decisions() }
}
