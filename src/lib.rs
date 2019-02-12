
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
    std::{ptr, mem, ops},
    ocaml::{ToValue,Value,value}
};

mod ctx;
mod solver;

pub type Lit = solver::Lit;
pub type Ctx = ctx::Ctx;
pub type Solver = solver::Solver;
pub type lbool = solver::lbool;

#[inline]
fn lit_of_int(lit: i32) -> Lit {
    Lit::unsafe_from_int(lit)
}

#[inline]
fn int_of_lit(lit: Lit) -> i32 {
    Lit::to_int(&lit)
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

caml!(ml_batsmt_solver_value_lvl_0, |ptr, lit|, <res>, {
    with_solver!(solver, ptr, {
        let lit = lit_of_int(lit.isize_val() as i32);
        let r = solver.api_value_lvl_0(lit);
        let r =
            if r == lbool::TRUE { 0 }
            else if r == lbool::FALSE { 1 }
            else { 2 };
        res = Value::isize(r);
    })
} -> res);

/* TODO

caml!(ml_batsmt_check_assumption, |ptr, lit|, <res>, {
    with_solver!(solver, ptr, {
        let lit = lit.isize_val() as i32;

        // check unsat-core
        let lit = solver.get_lit(lit);
        let r = solver.s.unsat_core_contains_var(lit.var());

        res = Value::bool(r);
    })
} -> res);

/// Convert a literal into a signed integer for the OCaml frontend
#[inline]
fn int_of_lit(lit: Lit) -> isize {
    lit.var().idx() as isize * if lit.sign() { 1 } else { -1 }
}

caml!(ml_batsmt_unsat_core, |ptr|, <res>, {
    with_solver!(solver, ptr, {
        let core =
            solver.s.unsat_core()
            .iter()
            .map(|&lit| int_of_lit(lit))
            .collect::<Vec<_>>();
        res = core.to_value();
    })
} -> res);

caml!(ml_batsmt_nvars, |ptr|, <res>, {
    with_solver!(solver, ptr, {
        let r = solver.s.num_vars() as isize;
        res = Value::isize(r);
    });
} -> res);

caml!(ml_batsmt_nclauses, |ptr|, <res>, {
    with_solver!(solver, ptr, {
        let r = solver.s.num_clauses();
        res = Value::isize(r as isize);
    })
} -> res);

caml!(ml_batsmt_nconflicts, |ptr|, <res>, {
    with_solver!(solver, ptr, {
        let r = solver.s.num_conflicts();
        res = Value::isize(r as isize);
    })
} -> res);

caml!(ml_batsmt_nprops, |ptr|, <res>, {
    with_solver!(solver, ptr, {
        let r = solver.s.num_propagations();
        res = Value::isize(r as isize);
    })
} -> res);

caml!(ml_batsmt_ndecisions, |ptr|, <res>, {
    with_solver!(solver, ptr, {
        let r = solver.s.num_decisions();
        res = Value::isize(r as isize);
    })
} -> res);

/*
caml!(ml_batsmt_nrestarts, |ptr|, <res>, {
    with_solver!(solver, ptr, {
        let r = solver.s.num_restarts();
        res = Value::isize(r as isize);
    })
} -> res);
*/

caml!(ml_batsmt_n_proved, |ptr|, <res>, {
    with_solver!(solver, ptr, {
        let r = solver.s.proved_at_lvl_0().len();
        res = Value::isize(r as isize);
    })
} -> res);

caml!(ml_batsmt_get_proved, |ptr, idx|, <res>, {
    let i = idx.isize_val() as usize;
    with_solver!(solver, ptr, {
        let lit = solver.s.proved_at_lvl_0()[i];
        let lit = lit.var().idx() as isize * if lit.sign() { 1 } else { -1 };
        res = Value::isize(lit);
    })
} -> res);

*/

