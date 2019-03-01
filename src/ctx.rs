
use {
    fxhash::FxHashMap,
    bit_set::BitSet,
    batsmt_core::{ast, ast_u32::AST, AstView},
    batsmt_hast::{HManager, StrSymbolManager},
    batsmt_theory::{self as theory, LitMapBuiltins},
    batsmt_cc::{self as cc, CCView},
    batsmt_solver as solver,
    batsmt_pretty as pp,
};

/// The Manager we use.
pub type M = HManager<StrSymbolManager>;

/// The builtin symbols.
#[derive(Clone,Debug)]
pub struct Builtins {
    pub bool_: AST,
    pub true_: AST,
    pub false_: AST,
    pub not_: AST,
    pub eq: AST,
    pub distinct: AST,
    pub select: AST, // pseudo-term
}

/// The main context.
pub struct Ctx {
    pub m: M,
    pub lmb: LitMapBuiltins,
    pub b: Builtins,
    syms: FxHashMap<String, AST>, // caching of symbols
    flags: Flags,
    f: Option<AST>,
    args: Vec<AST>,
}

#[derive(Default,Clone)]
struct Flags {
    cstor: BitSet,
    selector: BitSet,
}

/// An enum for the various kinds of terms we have.
#[repr(u8)]
#[derive(Eq,PartialEq,Copy,Clone)]
pub enum AstKind {
    Bool,
    App,
    Const,
    Cstor,
    Selector,
    Not,
}

pub mod ctx {
    use {super::*, batsmt_core::Manager};
    use cc::intf::{
        HasConstructor, ConstructorView as CView,
    };

    impl Ctx {
        /// New context.
        pub fn new() -> Self {
            let mut m = HManager::new();
            let b = Builtins::new(&mut m);
            let lmb = b.clone().into();
            Ctx {
                m, b, lmb, f: None, args: vec!(),
                flags: Default::default(), syms: FxHashMap::default(),
            }
        }

        /// Copy of builtins
        pub fn builtins<U>(&self) -> U
            where Builtins: Into<U>
        { self.b.clone().into() }

        pub fn lmb(&self) -> LitMapBuiltins { self.lmb.clone() }

        pub fn is_cstor(&self, t: &AST) -> bool { self.flags.cstor.contains(t.idx() as usize) }

        pub fn set_cstor(&mut self, t: &AST) { self.flags.cstor.insert(t.idx() as usize); }

        pub fn api_const(&mut self, s: &str) -> AST {
            match self.syms.get(s) {
                Some(t) => *t,
                None => {
                    let t = self.m.mk_const(s);
                    self.syms.insert(s.to_string(), t);
                    t
                }
            }
        }

        pub fn api_not(&mut self, t: AST) -> AST {
            if t == self.b.true_ { self.b.false_ }
            else if t == self.b.false_ { self.b.true_ }
            else { self.m.mk_app(self.b.not_, &[t]) }
        }

        pub fn api_kind(&self, t: AST) -> AstKind {
            if t == self.b.true_ || t == self.b.false_ {
                AstKind::Bool
            } else if self.is_cstor(&t) {
                AstKind::Cstor
            } else if self.m.is_const(&t) {
                AstKind::Const
            } else {
                match self.view(&t) {
                    AstView::App{f, ..} if *f == self.b.select => AstKind::Selector,
                    AstView::App{f, ..} if *f == self.b.not_ => AstKind::Not,
                    AstView::App{..} => AstKind::App,
                    _ => unreachable!()
                }
            }
        }

        pub fn api_get_bool(&self, t: AST) -> bool {
            if t == self.b.true_ { true }
            else if t == self.b.false_ { false }
            else { panic!("term is not a boolean") }
        }

        pub fn api_const_get_name(&self, t: AST) -> &str {
            match self.m.view(&t) {
                AstView::Const(s) => s,
                _ => panic!("term is not a constant")
            }
        }

        pub fn api_app_get_fun(&self, t: AST) -> AST {
            match self.m.view(&t) {
                AstView::App{f, ..} => *f,
                _ => panic!("term is not an app")
            }
        }

        pub fn api_app_get_args(&self, t: AST) -> &[AST] {
            match self.m.view(&t) {
                AstView::App{args, ..} => args,
                _ => panic!("term is not an app")
            }
        }

        pub fn api_bool(&mut self, b: bool) -> AST {
            if b { self.b.true_ } else { self.b.false_ }
        }

        pub fn api_app_fun(&mut self, f: AST) {
            self.f = Some(f);
            self.args.clear();
        }

        pub fn api_app_arg(&mut self, t: AST) {
            debug_assert!(self.f.is_some());
            self.args.push(t)
        }

        pub fn api_app_finalize(&mut self) -> AST {
            let f = self.f.unwrap();
            let t = self.m.mk_app(f, &self.args);
            self.f = None;
            self.args.clear();
            t
        }

        pub fn api_eq(&mut self, t1: AST, t2: AST) -> AST {
            let f = self.b.eq;
            self.m.mk_app(f, &[t1, t2])
        }

        pub fn api_set_is_cstor(&mut self, t: AST) {
            debug_assert!({let k = self.api_kind(t); k == AstKind::Const|| k == AstKind::Cstor});
            self.set_cstor(&t)
        }

        pub fn api_select(&mut self, c: AST, i: u32, sub: AST) -> AST {
            debug_assert!(self.api_kind(c) == AstKind::Cstor);
            let args = [c, self.m.mk_idx(i), sub];
            self.m.mk_app(self.b.select, &args)
        }
    }

    impl theory::BoolLitCtx for Ctx {
        type B = solver::BLit;
    }

    impl ast::HasManager for Ctx {
        type M = M;
        fn m(&self) -> &Self::M { &self.m }
        fn m_mut(&mut self) -> &mut Self::M { &mut self.m }
    }

    impl pp::Pretty1<AST> for Ctx {
        fn pp1_into(&self, t: &AST, ctx: &mut pp::Ctx) {
            ast::pp_ast(self, t, &mut |s,ctx| { ctx.display(s); }, ctx);
        }
    }

    // a valid context!
    impl theory::Ctx for Ctx {
        fn pp_ast(&self, t: &AST, ctx: &mut pp::Ctx) {
            ctx.pp1(&self.m, t);
        }
    }

    impl cc::Ctx for Ctx {
        type Fun = cc::intf::Void;

        fn get_bool_term(&self, b: bool) -> AST {
            if b { self.b.true_ } else { self.b.false_ }
        }

        fn view_as_cc_term<'a>(&'a self, t: &'a AST) -> CCView<'a,Self::Fun,AST> {
            if *t == self.b.true_ {
                CCView::Bool(true)
            } else if *t == self.b.false_ {
                CCView::Bool(false)
            } else if self.m.is_const(t) {
                CCView::Opaque(t) // shortcut
            } else {
                match self.m.view(t) {
                    // TODO: separate between `const` (no subterms, but has node+parent list)
                    // and `opaque-syntactic` (no subterms, no node, pure equality — use for index
                    // and special symbols and types as in `undefined ty`)

                    // TODO: special case for select? use `opaque-syntactic` for select
                    // and its constructor and index, but not `sub`

                    AstView::Const(_) | AstView::Index(..) => CCView::Opaque(t),
                    AstView::App{f, args} if *f == self.b.eq => {
                        debug_assert_eq!(args.len(), 2);
                        CCView::Eq(&args[0], &args[1])
                    },
                    AstView::App{f, args} if *f == self.b.distinct => {
                        CCView::Distinct(args)
                    },
                    AstView::App{f,args} => CCView::ApplyHO(f,args),
                }
            }
        }
    }

    impl HasConstructor<AST> for Ctx {
        type F = AST;

        fn view_as_constructor<'a>(
            &'a self, t: &'a AST
        ) -> CView<'a, Self::F, AST>
        {
            match self.view(t) {
                AstView::Const(_) if self.is_cstor(t) => {
                    CView::AppConstructor(t, &[])
                },
                AstView::App {f, args} if self.is_cstor(f) => {
                    CView::AppConstructor(f,args)
                },
                /* TODO: remove select?
                AstView::App {f, args} if *f == self.b.select => {
                    debug_assert_eq!(3, args.len());
                    let c = &args[0];
                    let idx = match self.view(&args[1]) {
                        AstView::Index(i) => i,
                        _ => panic!("invalid selector term"),
                    };
                    let sub = &args[2];
                    CView::Select{f: c, idx, sub}
                },
                */
                _ => {
                    CView::Other(t)
                },
            }
        }
    }
}

mod builtins {
    use super::*;

    impl Builtins {
        /// New builtins structure.
        pub(super) fn new(m: &mut M) -> Self {
            Builtins {
                bool_: m.mk_str("Bool"),
                true_: m.mk_str("true"),
                false_: m.mk_str("false"),
                eq: m.mk_str("="),
                not_: m.mk_str("not"),
                distinct: m.mk_str("distinct"),
                select: m.mk_str("select-"),
            }
        }
    }

    impl Into<LitMapBuiltins> for Builtins {
        fn into(self) -> LitMapBuiltins {
            let Builtins {true_, false_, not_, bool_, ..} = self;
            LitMapBuiltins {true_,false_,not_,bool_}
        }
    }
}
