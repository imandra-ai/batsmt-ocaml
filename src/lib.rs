
#[macro_use]
extern crate ocaml;

extern crate batsmt_core;
extern crate batsmt_hast;
extern crate batsmt_solver;
extern crate batsmt_theory;
extern crate batsmt_cc;

#[link(name="batsmt-core")]
#[link(name="batsmt-hast")]
#[link(name="batsmt-solver")]
#[link(name="batsmt-theory")]
#[link(name="batsmt-cc")]

use {
    std::{ptr, mem},
    batsmt_core::ast_u32::{self,AST},
    batsmt_cc::{HasConstructor, ConstructorView as CView},
    ocaml::{ToValue,Value,value,Str,Tuple}
};

mod ctx;
mod solver;

pub type Lit = solver::Lit;
pub type Ctx = ctx::Ctx;
pub type Solver = solver::Solver;
pub type Lbool = solver::Lbool;

#[inline]
fn lit_of_int(lit: i32) -> Lit {
    Lit::unsafe_from_int(lit)
}

#[inline]
fn int_of_lit(lit: Lit) -> i32 {
    Lit::to_int(&lit)
}

#[inline]
fn ast_of_int(t: u32) -> AST {
    ast_u32::manager_util::ast_from_u32(t)
}

#[inline]
fn int_of_ast(t: AST) -> u32 {
    t.idx()
}

#[inline]
fn int_of_lbool(r: Lbool) -> isize {
    if r == Lbool::TRUE { 0 }
    else if r == Lbool::FALSE { 1 }
    else { 2 }
}

// NOTE on storage:
// we use an OCaml custom block to store the pointer to the Solver (not the
// solver itself). Similarly for the context.


// macro to locally borrow context. `with_ctx!(s, v, block)`
// runs `block` in a context where `ctx` binds to a `&mut ctx` from `v`
macro_rules! with_ctx {
    ($s: ident, $v:expr, $code:block) => {
        {
            assert!(! $v.custom_ptr_val::<* const Ctx>().is_null());
            let $s : &mut Ctx = &mut (**$v.custom_ptr_val_mut::<*mut Ctx>());
            $code;
        };
    }
}

// macro to locally borrow solver. `with_solver!(s, v, block)`
// runs `block` in a context where `s` binds to a `&mut solver` from `v`
macro_rules! with_solver {
    ($s: ident, $v:expr, $code:block) => {
        {
            assert!(! $v.custom_ptr_val::<* const Solver>().is_null());
            let $s : &mut Solver = &mut (**$v.custom_ptr_val_mut::<*mut Solver>());
            $code;
        };
    }
}

fn delete_value<T>(v: Value) {
    if unsafe{ *v.custom_ptr_val::<*const T>() } != ptr::null() {
        //println!("delete value");
        let s = unsafe { Box::from_raw(*v.custom_ptr_val_mut::<*mut T>()) };
        mem::drop(s); // delete!
    }
    // be sure not to delete twice
    unsafe { * v.custom_ptr_val_mut::<*const Solver>() = ptr::null() };
}

extern "C" fn batsmt_ctx_finalizer(v: ocaml::core::Value) {
    delete_value::<Ctx>(Value::new(v));
}

// finalizer for values
extern "C" fn batsmt_solver_finalizer(v: ocaml::core::Value) {
    delete_value::<Solver>(Value::new(v));
}

caml!(ml_batsmt_ctx_new, |_params|, <res>, {
    let ctx = Box::new(Ctx::new());
    let ptr = Box::into_raw(ctx) as *mut Ctx;
    res = Value::alloc_custom(ptr, batsmt_ctx_finalizer);
} -> res);

caml!(ml_batsmt_ctx_delete, |param|, <res>, {
    delete_value::<Ctx>(param);
    res = value::UNIT;
} -> res);

caml!(ml_batsmt_solver_new, |ptr|, <res>, {
    with_ctx!(ctx, ptr, {
        let solver = Box::new(Solver::new(ctx));
        let ptr = Box::into_raw(solver) as *mut Solver;
        res = Value::alloc_custom(ptr, batsmt_solver_finalizer);
    })
} -> res);

caml!(ml_batsmt_solver_delete, |param|, <res>, {
    delete_value::<Solver>(param);
    res = value::UNIT;
} -> res);

caml!(ml_batsmt_solver_new_lit, |ptr|, <res>, {
    with_solver!(solver, ptr, {
        let lit = solver.api_make_lit();
        res = Value::isize(int_of_lit(lit) as isize);
    })
} -> res);

caml!(ml_batsmt_solver_new_term_lit, |ptr, ptr_c, t|, <res>, {
    with_solver!(solver, ptr, {
        with_ctx!(ctx, ptr_c, {
            let t = ast_of_int(t.isize_val() as u32);
            let lit = solver.api_make_term_lit(ctx, t);
            res = Value::isize(int_of_lit(lit) as isize);
        })
    })
} -> res);

/// Add literal
caml!(ml_batsmt_solver_add_clause_lit, |ptr, lit|, <res>, {
    with_solver!(solver, ptr, {
        let lit = lit_of_int(lit.isize_val() as i32);
        solver.api_add_clause_lit(lit);
        res = value::UNIT;
    })
} -> res);

/// Add clause
caml!(ml_batsmt_solver_add_clause, |ptr|, <res>, {
    with_solver!(solver, ptr, {
        solver.api_add_clause();
        res = value::UNIT;
    })
} -> res);

/// Add assumption
caml!(ml_batsmt_solver_add_assumption, |ptr, lit|, <res>, {
    with_solver!(solver, ptr, {
        let lit = lit_of_int(lit.isize_val() as i32);
        solver.api_add_assumption(lit);
        res = value::UNIT;
    })
} -> res);

caml!(ml_batsmt_solver_solve, |ptr_s, ptr_ctx|, <res>, {
    with_ctx!(ctx, ptr_ctx, {
        with_solver!(solver, ptr_s, {
            let r = solver.api_solve(ctx);
            //println!("res: {:?}, model: {:?}", r, solver.get_model());
            res = Value::bool(r);
        });
    })
} -> res);

caml!(ml_batsmt_solver_simplify, |ptr_s|, <res>, {
    with_solver!(solver, ptr_s, {
        let r = solver.api_simplify();
        res = Value::bool(r);
    });
} -> res);

caml!(ml_batsmt_solver_unsat_core, |ptr|, <res>, {
    with_solver!(solver, ptr, {
        let core =
            solver.api_unsat_core()
            .iter()
            .map(|&lit| int_of_lit(Lit::new(lit)))
            .collect::<Vec<_>>();
        res = core.to_value();
    })
} -> res);

caml!(ml_batsmt_solver_unsat_core_contains, |ptr, lit|, <res>, {
    with_solver!(solver, ptr, {
        let lit = lit_of_int(lit.isize_val() as i32);
        res = Value::bool(solver.api_unsat_core_contains(lit));
    })
} -> res);

caml!(ml_batsmt_nclauses, |ptr, lit|, <res>, {
    with_solver!(solver, ptr, {
        res = Value::isize( solver.api_n_clauses() as isize );
    })
} -> res);

caml!(ml_batsmt_nlits, |ptr, lit|, <res>, {
    with_solver!(solver, ptr, {
        res = Value::isize( solver.api_n_lits() as isize );
    })
} -> res);

caml!(ml_batsmt_ndecisions, |ptr, lit|, <res>, {
    with_solver!(solver, ptr, {
        res = Value::isize( solver.api_n_decisions() as isize );
    })
} -> res);

caml!(ml_batsmt_nprops, |ptr, lit|, <res>, {
    with_solver!(solver, ptr, {
        res = Value::isize( solver.api_n_props() as isize );
    })
} -> res);

caml!(ml_batsmt_nconflicts, |ptr, lit|, <res>, {
    with_solver!(solver, ptr, {
        res = Value::isize( solver.api_n_conflicts() as isize );
    })
} -> res);

caml!(ml_batsmt_solver_value_lvl_0, |ptr, lit|, <res>, {
    with_solver!(solver, ptr, {
        let lit = lit_of_int(lit.isize_val() as i32);
        let r = solver.api_value_lvl_0(lit);
        res = Value::isize(int_of_lbool(r));
    })
} -> res);

caml!(ml_batsmt_solver_value, |ptr, lit|, <res>, {
    with_solver!(solver, ptr, {
        let lit = lit_of_int(lit.isize_val() as i32);
        let r = solver.api_value(lit);
        res = Value::isize(int_of_lbool(r));
    })
} -> res);

caml!(ml_batsmt_solver_n_proved_lvl_0, |ptr|, <res>, {
    with_solver!(solver, ptr, {
        let r = solver.api_proved_at_lvl_0();
        res = Value::isize(r.len() as isize);
    })
} -> res);

caml!(ml_batsmt_solver_proved_lvl_0, |ptr, idx|, <res>, {
    with_solver!(solver, ptr, {
        let idx = idx.isize_val() as usize;
        let r = solver.api_proved_at_lvl_0();
        let lit = Lit::new(r[idx]);
        res = Value::isize(int_of_lit(lit) as isize);
    })
} -> res);

caml!(ml_batsmt_term_bool, |ptr, b|, <res>, {
    with_ctx!(ctx, ptr, {
        let b = b.isize_val() != 0;
        let t = ctx.api_bool(b);
        res = Value::isize(int_of_ast(t) as isize);
    })
} -> res);

caml!(ml_batsmt_term_const, |ptr, s|, <res>, {
    with_ctx!(ctx, ptr, {
        let s: Str = s.into();
        let t = ctx.api_const(s.as_str());
        res = Value::isize(int_of_ast(t) as isize);
    })
} -> res);

caml!(ml_batsmt_term_set_cstor, |ptr, t|, <res>, {
    with_ctx!(ctx, ptr, {
        let t = ast_of_int(t.isize_val() as u32);
        ctx.api_set_is_cstor(t);
        res = value::UNIT;
    })
} -> res);

caml!(ml_batsmt_term_eq, |ptr, t1, t2|, <res>, {
    with_ctx!(ctx, ptr, {
        let t1 = ast_of_int(t1.isize_val() as u32);
        let t2 = ast_of_int(t2.isize_val() as u32);
        let t = ctx.api_eq(t1, t2);
        res = Value::isize(int_of_ast(t) as isize);
    })
} -> res);

caml!(ml_batsmt_term_select, |ptr, c, idx, u|, <res>, {
    with_ctx!(ctx, ptr, {
        let c = ast_of_int(c.isize_val() as u32);
        let u = ast_of_int(u.isize_val() as u32);
        let idx = idx.isize_val() as u32;
        let r = ctx.api_select(c, idx, u);
        res = Value::isize(int_of_ast(r) as isize);
    })
} -> res);

caml!(ml_batsmt_term_app_fun, |ptr, f|, <res>, {
    with_ctx!(ctx, ptr, {
        let f = ast_of_int(f.isize_val() as u32);
        ctx.api_app_fun(f);
        res = value::UNIT;
    })
} -> res);

caml!(ml_batsmt_term_app_arg, |ptr, t|, <res>, {
    with_ctx!(ctx, ptr, {
        let t = ast_of_int(t.isize_val() as u32);
        ctx.api_app_arg(t);
        res = value::UNIT;
    })
} -> res);

caml!(ml_batsmt_term_app_finalize, |ptr|, <res>, {
    with_ctx!(ctx, ptr, {
        let t = ctx.api_app_finalize();
        res = Value::isize(int_of_ast(t) as isize);
    })
} -> res);

caml!(ml_batsmt_term_kind, |ptr, t|, <res>, {
    with_ctx!(ctx, ptr, {
        let t = ast_of_int(t.isize_val() as u32);
        res = Value::isize(ctx.api_kind(t) as u8 as isize);
    })
} -> res);

caml!(ml_batsmt_term_get_bool, |ptr, t|, <res>, {
    with_ctx!(ctx, ptr, {
        let t = ast_of_int(t.isize_val() as u32);
        let b = ctx.api_get_bool(t);
        res = Value::bool(b);
    })
} -> res);

caml!(ml_batsmt_term_get_const_name, |ptr, t|, <res>, {
    with_ctx!(ctx, ptr, {
        let t = ast_of_int(t.isize_val() as u32);
        let s = ctx.api_const_get_name(t);
        res = Value::from(Str::from(s));
    })
} -> res);

caml!(ml_batsmt_term_get_app_fun, |ptr, t|, <res>, {
    with_ctx!(ctx, ptr, {
        let t = ast_of_int(t.isize_val() as u32);
        let f = ctx.api_app_get_fun(t);
        res = Value::isize(int_of_ast(f) as isize);
    })
} -> res);

caml!(ml_batsmt_term_get_app_n_args, |ptr, t|, <res>, {
    with_ctx!(ctx, ptr, {
        let t = ast_of_int(t.isize_val() as u32);
        let args = ctx.api_app_get_args(t);
        res = Value::isize(args.len() as isize);
    })
} -> res);

caml!(ml_batsmt_term_get_app_nth_arg, |ptr, t, i|, <res>, {
    with_ctx!(ctx, ptr, {
        let t = ast_of_int(t.isize_val() as u32);
        let args = ctx.api_app_get_args(t);
        let a = args[i.isize_val() as usize];
        res = Value::isize(int_of_ast(a) as isize);
    })
} -> res);

caml!(ml_batsmt_term_get_select, |ptr, t|, <res>, {
    with_ctx!(ctx, ptr, {
        let t = ast_of_int(t.isize_val() as u32);
        let mut tup = Tuple::new(3);
        match ctx.view_as_constructor(&t) {
            CView::Select{f, idx, sub} => {
                let _ = tup.set(0, Value::isize(int_of_ast(*f) as isize));
                let _ = tup.set(1, Value::isize(idx as isize));
                let _ = tup.set(2, Value::isize(int_of_ast(*sub) as isize));
            },
            _ => panic!("not a select term"),
        };
        res = tup.into();
    })
} -> res);
