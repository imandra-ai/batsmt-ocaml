
use {
    fxhash::FxHashMap,
    bit_set::BitSet,
    batsmt_core::{ast, ast_u32::{self,AST}, AstView},
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
    injective: BitSet,
    cstor: BitSet,
    selector: BitSet,
}

#[repr(u8)]
pub enum AstKind {
    Bool,
    App,
    Const,
    // Cstor,
    // Select,
}

pub mod ctx {
    use {super::*, batsmt_core::Manager};
    use cc::intf::{
        HasSelector,SelectorView,HasInjectivity,InjectiveView,HasDisjointness,
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

        pub fn is_injective(&self, t: &AST) -> bool { self.flags.injective.contains(t.idx() as usize) }
        pub fn is_cstor(&self, t: &AST) -> bool { self.flags.cstor.contains(t.idx() as usize) }
        pub fn is_selector(&self, t: &AST) -> bool { self.flags.selector.contains(t.idx() as usize) }

        pub fn set_injective(&mut self, t: &AST) { self.flags.injective.insert(t.idx() as usize); }
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

        pub fn api_kind(&self, t: AST) -> AstKind {
            if t == self.b.true_ || t == self.b.false_ {
                AstKind::Bool
            } else if self.m.is_const(&t) {
                // TODO: const-or-cstor
                AstKind::Const
            } else {
                // TODO: select-or-app
                assert!(self.m.is_app(&t));
                AstKind::App
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
                AstView::App{..} => panic!("term is not a constant")
            }
        }

        pub fn api_app_get_fun(&self, t: AST) -> AST {
            match self.m.view(&t) {
                AstView::App{f, ..} => *f,
                AstView::Const(..) => panic!("term is not an app")
            }
        }

        pub fn api_app_get_args(&self, t: AST) -> &[AST] {
            match self.m.view(&t) {
                AstView::App{args, ..} => args,
                AstView::Const(..) => panic!("term is not an app")
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
                    AstView::Const(_) => CCView::Opaque(t),
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

    impl HasInjectivity<AST> for Ctx {
        type F = AST;

        fn view_as_injective<'a>(
            &'a self, t: &'a AST
        ) -> InjectiveView<'a, Self::F, AST>
        {
            if let AstView::App {f, args} = self.view(t) {
                if self.is_injective(f) {
                    return InjectiveView::AppInjective(f,args);
                }
            }
            InjectiveView::Other(t)
        }
    }

    impl HasDisjointness<AST> for Ctx {
        type F = AST;

        fn get_disjoint_label(&self, t: &AST) -> Option<Self::F> {
            match self.view(t) {
                AstView::App{f, args: _} if self.is_cstor(f) => {
                    Some(*f)
                },
                AstView::Const(_) if self.is_cstor(t) => Some(*t),
                _ => None,
            }
        }
    }

    impl HasSelector<AST> for Ctx {
        fn view_as_selector<'a>(
            &'a self, t: &'a AST
        ) -> SelectorView<'a, Self::F, AST>
        {
            if let AstView::App {f, args} = self.view(t) {
                if self.is_selector(f) {
                    debug_assert_eq!(3, args.len());
                    unimplemented!(); // FIXME: how to put the integer into the AST?
                }
            }
            SelectorView::Other(t)
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
