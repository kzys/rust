// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Code for the 'normalization' query. This consists of a wrapper
//! which folds deeply, invoking the underlying
//! `normalize_projection_ty` query when it encounters projections.

use infer::at::At;
use infer::{InferCtxt, InferOk};
use mir::interpret::{ConstValue, GlobalId};
use rustc_data_structures::small_vec::SmallVec;
use traits::project::Normalized;
use traits::{Obligation, ObligationCause, PredicateObligation, Reveal};
use ty::fold::{TypeFoldable, TypeFolder};
use ty::subst::{Subst, Substs};
use ty::{self, Ty, TyCtxt};

use super::NoSolution;

impl<'cx, 'gcx, 'tcx> At<'cx, 'gcx, 'tcx> {
    /// Normalize `value` in the context of the inference context,
    /// yielding a resulting type, or an error if `value` cannot be
    /// normalized. If you don't care about regions, you should prefer
    /// `normalize_erasing_regions`, which is more efficient.
    ///
    /// If the normalization succeeds and is unambiguous, returns back
    /// the normalized value along with various outlives relations (in
    /// the form of obligations that must be discharged).
    ///
    /// NB. This will *eventually* be the main means of
    /// normalizing, but for now should be used only when we actually
    /// know that normalization will succeed, since error reporting
    /// and other details are still "under development".
    pub fn normalize<T>(&self, value: &T) -> Result<Normalized<'tcx, T>, NoSolution>
    where
        T: TypeFoldable<'tcx>,
    {
        debug!(
            "normalize::<{}>(value={:?}, param_env={:?})",
            unsafe { ::std::intrinsics::type_name::<T>() },
            value,
            self.param_env,
        );
        let mut normalizer = QueryNormalizer {
            infcx: self.infcx,
            cause: self.cause,
            param_env: self.param_env,
            obligations: vec![],
            error: false,
            anon_depth: 0,
        };
        if !value.has_projections() {
            return Ok(Normalized {
                value: value.clone(),
                obligations: vec![],
            });
        }

        let value1 = value.fold_with(&mut normalizer);
        if normalizer.error {
            Err(NoSolution)
        } else {
            Ok(Normalized {
                value: value1,
                obligations: normalizer.obligations,
            })
        }
    }
}

/// Result from the `normalize_projection_ty` query.
#[derive(Clone, Debug)]
pub struct NormalizationResult<'tcx> {
    /// Result of normalization.
    pub normalized_ty: Ty<'tcx>,
}

struct QueryNormalizer<'cx, 'gcx: 'tcx, 'tcx: 'cx> {
    infcx: &'cx InferCtxt<'cx, 'gcx, 'tcx>,
    cause: &'cx ObligationCause<'tcx>,
    param_env: ty::ParamEnv<'tcx>,
    obligations: Vec<PredicateObligation<'tcx>>,
    error: bool,
    anon_depth: usize,
}

impl<'cx, 'gcx, 'tcx> TypeFolder<'gcx, 'tcx> for QueryNormalizer<'cx, 'gcx, 'tcx> {
    fn tcx<'c>(&'c self) -> TyCtxt<'c, 'gcx, 'tcx> {
        self.infcx.tcx
    }

    fn fold_ty(&mut self, ty: Ty<'tcx>) -> Ty<'tcx> {
        let ty = ty.super_fold_with(self);
        match ty.sty {
            ty::Anon(def_id, substs) if !substs.has_escaping_regions() => {
                // (*)
                // Only normalize `impl Trait` after type-checking, usually in codegen.
                match self.param_env.reveal {
                    Reveal::UserFacing => ty,

                    Reveal::All => {
                        let recursion_limit = *self.tcx().sess.recursion_limit.get();
                        if self.anon_depth >= recursion_limit {
                            let obligation = Obligation::with_depth(
                                self.cause.clone(),
                                recursion_limit,
                                self.param_env,
                                ty,
                            );
                            self.infcx.report_overflow_error(&obligation, true);
                        }

                        let generic_ty = self.tcx().type_of(def_id);
                        let concrete_ty = generic_ty.subst(self.tcx(), substs);
                        self.anon_depth += 1;
                        if concrete_ty == ty {
                            // The type in question can only be inferred in terms of itself. This
                            // is likely a user code issue, not a compiler issue. Thus, we will
                            // induce a cycle error by calling the parent query again on the type.
                            //
                            // FIXME: Perhaps a better solution would be to have fold_ty()
                            // itself be a query. Then, a type fold cycle would be detected
                            // and reported more naturally as part of the query system, rather
                            // than forcing it here.
                            //
                            // FIXME: Need a better span than just one pointing to the type def.
                            // Should point to a defining use of the type that results in this
                            // un-normalizable state.
                            if let Some(param_env_lifted) =
                                self.tcx().lift_to_global(&self.param_env)
                            {
                                if let Some(ty_lifted) = self.tcx().lift_to_global(&concrete_ty) {
                                    let span = self.tcx().def_span(def_id);
                                    self.tcx()
                                        .global_tcx()
                                        .at(span)
                                        .normalize_ty_after_erasing_regions(
                                            param_env_lifted.and(ty_lifted),
                                        );
                                    self.tcx().sess.abort_if_errors();
                                }
                            }
                            // If a cycle error can't be emitted, indicate a NoSolution error
                            // and let the caller handle it.
                            self.error = true;
                            return concrete_ty;
                        }
                        let folded_ty = self.fold_ty(concrete_ty);
                        self.anon_depth -= 1;
                        folded_ty
                    }
                }
            }

            ty::Projection(ref data) if !data.has_escaping_regions() => {
                // (*)
                // (*) This is kind of hacky -- we need to be able to
                // handle normalization within binders because
                // otherwise we wind up a need to normalize when doing
                // trait matching (since you can have a trait
                // obligation like `for<'a> T::B : Fn(&'a int)`), but
                // we can't normalize with bound regions in scope. So
                // far now we just ignore binders but only normalize
                // if all bound regions are gone (and then we still
                // have to renormalize whenever we instantiate a
                // binder). It would be better to normalize in a
                // binding-aware fashion.

                let gcx = self.infcx.tcx.global_tcx();

                let mut orig_values = SmallVec::new();
                let c_data = self.infcx
                    .canonicalize_query(&self.param_env.and(*data), &mut orig_values);
                debug!("QueryNormalizer: c_data = {:#?}", c_data);
                debug!("QueryNormalizer: orig_values = {:#?}", orig_values);
                match gcx.normalize_projection_ty(c_data) {
                    Ok(result) => {
                        // We don't expect ambiguity.
                        if result.is_ambiguous() {
                            self.error = true;
                            return ty;
                        }

                        match self.infcx.instantiate_query_result_and_region_obligations(
                            self.cause,
                            self.param_env,
                            &orig_values,
                            &result,
                        ) {
                            Ok(InferOk {
                                value: result,
                                obligations,
                            }) => {
                                debug!("QueryNormalizer: result = {:#?}", result);
                                debug!("QueryNormalizer: obligations = {:#?}", obligations);
                                self.obligations.extend(obligations);
                                return result.normalized_ty;
                            }

                            Err(_) => {
                                self.error = true;
                                return ty;
                            }
                        }
                    }

                    Err(NoSolution) => {
                        self.error = true;
                        ty
                    }
                }
            }

            _ => ty,
        }
    }

    fn fold_const(&mut self, constant: &'tcx ty::Const<'tcx>) -> &'tcx ty::Const<'tcx> {
        if let ConstValue::Unevaluated(def_id, substs) = constant.val {
            let tcx = self.infcx.tcx.global_tcx();
            if let Some(param_env) = self.tcx().lift_to_global(&self.param_env) {
                if substs.needs_infer() || substs.has_skol() {
                    let identity_substs = Substs::identity_for_item(tcx, def_id);
                    let instance = ty::Instance::resolve(tcx, param_env, def_id, identity_substs);
                    if let Some(instance) = instance {
                        let cid = GlobalId {
                            instance,
                            promoted: None,
                        };
                        match tcx.const_eval(param_env.and(cid)) {
                            Ok(evaluated) => {
                                let evaluated = evaluated.subst(self.tcx(), substs);
                                return self.fold_const(evaluated);
                            }
                            Err(_) => {}
                        }
                    }
                } else {
                    if let Some(substs) = self.tcx().lift_to_global(&substs) {
                        let instance = ty::Instance::resolve(tcx, param_env, def_id, substs);
                        if let Some(instance) = instance {
                            let cid = GlobalId {
                                instance,
                                promoted: None,
                            };
                            match tcx.const_eval(param_env.and(cid)) {
                                Ok(evaluated) => return self.fold_const(evaluated),
                                Err(_) => {}
                            }
                        }
                    }
                }
            }
        }
        constant
    }
}

BraceStructTypeFoldableImpl! {
    impl<'tcx> TypeFoldable<'tcx> for NormalizationResult<'tcx> {
        normalized_ty
    }
}

BraceStructLiftImpl! {
    impl<'a, 'tcx> Lift<'tcx> for NormalizationResult<'a> {
        type Lifted = NormalizationResult<'tcx>;
        normalized_ty
    }
}

impl_stable_hash_for!(struct NormalizationResult<'tcx> {
    normalized_ty
});
