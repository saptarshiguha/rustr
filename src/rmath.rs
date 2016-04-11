/* automatically generated by rust-bindgen */
#![allow(dead_code)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

extern "C" {
    pub fn R_pow(x: f64, y: f64)                  
     -> f64;
    pub fn R_pow_di(arg1: f64,
                    arg2: i32) -> f64;
    pub fn norm_rand() -> f64;
    pub fn unif_rand() -> f64;
    pub fn exp_rand() -> f64;
    pub fn Rf_dnorm4(arg1: f64,
                     arg2: f64,
                     arg3: f64,
                     arg4: i32) -> f64;
    pub fn Rf_pnorm5(arg1: f64,
                     arg2: f64,
                     arg3: f64,
                     arg4: i32, arg5: i32)
     -> f64;
    pub fn Rf_qnorm5(arg1: f64,
                     arg2: f64,
                     arg3: f64,
                     arg4: i32, arg5: i32)
     -> f64;
    pub fn Rf_rnorm(arg1: f64,
                    arg2: f64)
     -> f64;
    pub fn Rf_pnorm_both(arg1: f64,
                         arg2: *mut f64,
                         arg3: *mut f64,
                         arg4: i32,
                         arg5: i32);
    pub fn Rf_dunif(arg1: f64,
                    arg2: f64,
                    arg3: f64,
                    arg4: i32) -> f64;
    pub fn Rf_punif(arg1: f64,
                    arg2: f64,
                    arg3: f64,
                    arg4: i32, arg5: i32)
     -> f64;
    pub fn Rf_qunif(arg1: f64,
                    arg2: f64,
                    arg3: f64,
                    arg4: i32, arg5: i32)
     -> f64;
    pub fn Rf_runif(arg1: f64,
                    arg2: f64)
     -> f64;
    pub fn Rf_dgamma(arg1: f64,
                     arg2: f64,
                     arg3: f64,
                     arg4: i32) -> f64;
    pub fn Rf_pgamma(arg1: f64,
                     arg2: f64,
                     arg3: f64,
                     arg4: i32, arg5: i32)
     -> f64;
    pub fn Rf_qgamma(arg1: f64,
                     arg2: f64,
                     arg3: f64,
                     arg4: i32, arg5: i32)
     -> f64;
    pub fn Rf_rgamma(arg1: f64,
                     arg2: f64)
     -> f64;
    pub fn Rf_log1pmx(arg1: f64)
     -> f64;
    pub fn log1pexp(arg1: f64)
     -> f64;
    pub fn Rf_lgamma1p(arg1: f64)
     -> f64;
    pub fn Rf_logspace_add(logx: f64,
                           logy: f64)
     -> f64;
    pub fn Rf_logspace_sub(logx: f64,
                           logy: f64)
     -> f64;
    pub fn logspace_sum(arg1: *mut f64,
                        arg2: i32)
     -> f64;
    pub fn Rf_dbeta(arg1: f64,
                    arg2: f64,
                    arg3: f64,
                    arg4: i32) -> f64;
    pub fn Rf_pbeta(arg1: f64,
                    arg2: f64,
                    arg3: f64,
                    arg4: i32, arg5: i32)
     -> f64;
    pub fn Rf_qbeta(arg1: f64,
                    arg2: f64,
                    arg3: f64,
                    arg4: i32, arg5: i32)
     -> f64;
    pub fn Rf_rbeta(arg1: f64,
                    arg2: f64)
     -> f64;
    pub fn Rf_dlnorm(arg1: f64,
                     arg2: f64,
                     arg3: f64,
                     arg4: i32) -> f64;
    pub fn Rf_plnorm(arg1: f64,
                     arg2: f64,
                     arg3: f64,
                     arg4: i32, arg5: i32)
     -> f64;
    pub fn Rf_qlnorm(arg1: f64,
                     arg2: f64,
                     arg3: f64,
                     arg4: i32, arg5: i32)
     -> f64;
    pub fn Rf_rlnorm(arg1: f64,
                     arg2: f64)
     -> f64;
    pub fn Rf_dchisq(arg1: f64,
                     arg2: f64,
                     arg3: i32) -> f64;
    pub fn Rf_pchisq(arg1: f64,
                     arg2: f64,
                     arg3: i32, arg4: i32)
     -> f64;
    pub fn Rf_qchisq(arg1: f64,
                     arg2: f64,
                     arg3: i32, arg4: i32)
     -> f64;
    pub fn Rf_rchisq(arg1: f64)
     -> f64;
    pub fn Rf_dnchisq(arg1: f64,
                      arg2: f64,
                      arg3: f64,
                      arg4: i32)
     -> f64;
    pub fn Rf_pnchisq(arg1: f64,
                      arg2: f64,
                      arg3: f64,
                      arg4: i32,
                      arg5: i32)
     -> f64;
    pub fn Rf_qnchisq(arg1: f64,
                      arg2: f64,
                      arg3: f64,
                      arg4: i32,
                      arg5: i32)
     -> f64;
    pub fn Rf_rnchisq(arg1: f64,
                      arg2: f64)
     -> f64;
    pub fn Rf_df(arg1: f64,
                 arg2: f64,
                 arg3: f64, arg4: i32)
     -> f64;
    pub fn Rf_pf(arg1: f64,
                 arg2: f64,
                 arg3: f64, arg4: i32,
                 arg5: i32) -> f64;
    pub fn Rf_qf(arg1: f64,
                 arg2: f64,
                 arg3: f64, arg4: i32,
                 arg5: i32) -> f64;
    pub fn Rf_rf(arg1: f64,
                 arg2: f64) -> f64;
    pub fn Rf_dt(arg1: f64,
                 arg2: f64, arg3: i32)
     -> f64;
    pub fn Rf_pt(arg1: f64,
                 arg2: f64, arg3: i32,
                 arg4: i32) -> f64;
    pub fn Rf_qt(arg1: f64,
                 arg2: f64, arg3: i32,
                 arg4: i32) -> f64;
    pub fn Rf_rt(arg1: f64) -> f64;
    pub fn Rf_dbinom_raw(x: f64,
                         n: f64,
                         p: f64,
                         q: f64,
                         give_log: i32)
     -> f64;
    pub fn Rf_dbinom(arg1: f64,
                     arg2: f64,
                     arg3: f64,
                     arg4: i32) -> f64;
    pub fn Rf_pbinom(arg1: f64,
                     arg2: f64,
                     arg3: f64,
                     arg4: i32, arg5: i32)
     -> f64;
    pub fn Rf_qbinom(arg1: f64,
                     arg2: f64,
                     arg3: f64,
                     arg4: i32, arg5: i32)
     -> f64;
    pub fn Rf_rbinom(arg1: f64,
                     arg2: f64)
     -> f64;
    pub fn Rf_rmultinom(arg1: i32,
                        arg2: *mut f64,
                        arg3: i32,
                        arg4: *mut i32);
    pub fn Rf_dcauchy(arg1: f64,
                      arg2: f64,
                      arg3: f64,
                      arg4: i32)
     -> f64;
    pub fn Rf_pcauchy(arg1: f64,
                      arg2: f64,
                      arg3: f64,
                      arg4: i32,
                      arg5: i32)
     -> f64;
    pub fn Rf_qcauchy(arg1: f64,
                      arg2: f64,
                      arg3: f64,
                      arg4: i32,
                      arg5: i32)
     -> f64;
    pub fn Rf_rcauchy(arg1: f64,
                      arg2: f64)
     -> f64;
    pub fn Rf_dexp(arg1: f64,
                   arg2: f64,
                   arg3: i32) -> f64;
    pub fn Rf_pexp(arg1: f64,
                   arg2: f64,
                   arg3: i32, arg4: i32)
     -> f64;
    pub fn Rf_qexp(arg1: f64,
                   arg2: f64,
                   arg3: i32, arg4: i32)
     -> f64;
    pub fn Rf_rexp(arg1: f64)
     -> f64;
    pub fn Rf_dgeom(arg1: f64,
                    arg2: f64,
                    arg3: i32) -> f64;
    pub fn Rf_pgeom(arg1: f64,
                    arg2: f64,
                    arg3: i32, arg4: i32)
     -> f64;
    pub fn Rf_qgeom(arg1: f64,
                    arg2: f64,
                    arg3: i32, arg4: i32)
     -> f64;
    pub fn Rf_rgeom(arg1: f64)
     -> f64;
    pub fn Rf_dhyper(arg1: f64,
                     arg2: f64,
                     arg3: f64,
                     arg4: f64,
                     arg5: i32) -> f64;
    pub fn Rf_phyper(arg1: f64,
                     arg2: f64,
                     arg3: f64,
                     arg4: f64,
                     arg5: i32, arg6: i32)
     -> f64;
    pub fn Rf_qhyper(arg1: f64,
                     arg2: f64,
                     arg3: f64,
                     arg4: f64,
                     arg5: i32, arg6: i32)
     -> f64;
    pub fn Rf_rhyper(arg1: f64,
                     arg2: f64,
                     arg3: f64)
     -> f64;
    pub fn Rf_dnbinom(arg1: f64,
                      arg2: f64,
                      arg3: f64,
                      arg4: i32)
     -> f64;
    pub fn Rf_pnbinom(arg1: f64,
                      arg2: f64,
                      arg3: f64,
                      arg4: i32,
                      arg5: i32)
     -> f64;
    pub fn Rf_qnbinom(arg1: f64,
                      arg2: f64,
                      arg3: f64,
                      arg4: i32,
                      arg5: i32)
     -> f64;
    pub fn Rf_rnbinom(arg1: f64,
                      arg2: f64)
     -> f64;
    pub fn Rf_dnbinom_mu(arg1: f64,
                         arg2: f64,
                         arg3: f64,
                         arg4: i32)
     -> f64;
    pub fn Rf_pnbinom_mu(arg1: f64,
                         arg2: f64,
                         arg3: f64,
                         arg4: i32,
                         arg5: i32)
     -> f64;
    pub fn Rf_qnbinom_mu(arg1: f64,
                         arg2: f64,
                         arg3: f64,
                         arg4: i32,
                         arg5: i32)
     -> f64;
    pub fn Rf_rnbinom_mu(arg1: f64,
                         arg2: f64)
     -> f64;
    pub fn Rf_dpois_raw(arg1: f64,
                        arg2: f64,
                        arg3: i32)
     -> f64;
    pub fn Rf_dpois(arg1: f64,
                    arg2: f64,
                    arg3: i32) -> f64;
    pub fn Rf_ppois(arg1: f64,
                    arg2: f64,
                    arg3: i32, arg4: i32)
     -> f64;
    pub fn Rf_qpois(arg1: f64,
                    arg2: f64,
                    arg3: i32, arg4: i32)
     -> f64;
    pub fn Rf_rpois(arg1: f64)
     -> f64;
    pub fn Rf_dweibull(arg1: f64,
                       arg2: f64,
                       arg3: f64,
                       arg4: i32)
     -> f64;
    pub fn Rf_pweibull(arg1: f64,
                       arg2: f64,
                       arg3: f64,
                       arg4: i32,
                       arg5: i32)
     -> f64;
    pub fn Rf_qweibull(arg1: f64,
                       arg2: f64,
                       arg3: f64,
                       arg4: i32,
                       arg5: i32)
     -> f64;
    pub fn Rf_rweibull(arg1: f64,
                       arg2: f64)
     -> f64;
    pub fn Rf_dlogis(arg1: f64,
                     arg2: f64,
                     arg3: f64,
                     arg4: i32) -> f64;
    pub fn Rf_plogis(arg1: f64,
                     arg2: f64,
                     arg3: f64,
                     arg4: i32, arg5: i32)
     -> f64;
    pub fn Rf_qlogis(arg1: f64,
                     arg2: f64,
                     arg3: f64,
                     arg4: i32, arg5: i32)
     -> f64;
    pub fn Rf_rlogis(arg1: f64,
                     arg2: f64)
     -> f64;
    pub fn Rf_dnbeta(arg1: f64,
                     arg2: f64,
                     arg3: f64,
                     arg4: f64,
                     arg5: i32) -> f64;
    pub fn Rf_pnbeta(arg1: f64,
                     arg2: f64,
                     arg3: f64,
                     arg4: f64,
                     arg5: i32, arg6: i32)
     -> f64;
    pub fn Rf_qnbeta(arg1: f64,
                     arg2: f64,
                     arg3: f64,
                     arg4: f64,
                     arg5: i32, arg6: i32)
     -> f64;
    pub fn Rf_rnbeta(arg1: f64,
                     arg2: f64,
                     arg3: f64)
     -> f64;
    pub fn Rf_dnf(arg1: f64,
                  arg2: f64,
                  arg3: f64,
                  arg4: f64, arg5: i32)
     -> f64;
    pub fn Rf_pnf(arg1: f64,
                  arg2: f64,
                  arg3: f64,
                  arg4: f64, arg5: i32,
                  arg6: i32) -> f64;
    pub fn Rf_qnf(arg1: f64,
                  arg2: f64,
                  arg3: f64,
                  arg4: f64, arg5: i32,
                  arg6: i32) -> f64;
    pub fn Rf_dnt(arg1: f64,
                  arg2: f64,
                  arg3: f64, arg4: i32)
     -> f64;
    pub fn Rf_pnt(arg1: f64,
                  arg2: f64,
                  arg3: f64, arg4: i32,
                  arg5: i32) -> f64;
    pub fn Rf_qnt(arg1: f64,
                  arg2: f64,
                  arg3: f64, arg4: i32,
                  arg5: i32) -> f64;
    pub fn Rf_ptukey(arg1: f64,
                     arg2: f64,
                     arg3: f64,
                     arg4: f64,
                     arg5: i32, arg6: i32)
     -> f64;
    pub fn Rf_qtukey(arg1: f64,
                     arg2: f64,
                     arg3: f64,
                     arg4: f64,
                     arg5: i32, arg6: i32)
     -> f64;
    pub fn Rf_dwilcox(arg1: f64,
                      arg2: f64,
                      arg3: f64,
                      arg4: i32)
     -> f64;
    pub fn Rf_pwilcox(arg1: f64,
                      arg2: f64,
                      arg3: f64,
                      arg4: i32,
                      arg5: i32)
     -> f64;
    pub fn Rf_qwilcox(arg1: f64,
                      arg2: f64,
                      arg3: f64,
                      arg4: i32,
                      arg5: i32)
     -> f64;
    pub fn Rf_rwilcox(arg1: f64,
                      arg2: f64)
     -> f64;
    pub fn Rf_dsignrank(arg1: f64,
                        arg2: f64,
                        arg3: i32)
     -> f64;
    pub fn Rf_psignrank(arg1: f64,
                        arg2: f64,
                        arg3: i32,
                        arg4: i32)
     -> f64;
    pub fn Rf_qsignrank(arg1: f64,
                        arg2: f64,
                        arg3: i32,
                        arg4: i32)
     -> f64;
    pub fn Rf_rsignrank(arg1: f64)
     -> f64;
    pub fn Rf_gammafn(arg1: f64)
     -> f64;
    pub fn Rf_lgammafn(arg1: f64)
     -> f64;
    pub fn Rf_lgammafn_sign(arg1: f64,
                            arg2: *mut i32)
     -> f64;
    pub fn Rf_dpsifn(arg1: f64,
                     arg2: i32, arg3: i32,
                     arg4: i32,
                     arg5: *mut f64,
                     arg6: *mut i32,
                     arg7: *mut i32);
    pub fn Rf_psigamma(arg1: f64,
                       arg2: f64)
     -> f64;
    pub fn Rf_digamma(arg1: f64)
     -> f64;
    pub fn Rf_trigamma(arg1: f64)
     -> f64;
    pub fn Rf_tetragamma(arg1: f64)
     -> f64;
    pub fn Rf_pentagamma(arg1: f64)
     -> f64;
    pub fn Rf_beta(arg1: f64,
                   arg2: f64)
     -> f64;
    pub fn Rf_lbeta(arg1: f64,
                    arg2: f64)
     -> f64;
    pub fn Rf_choose(arg1: f64,
                     arg2: f64)
     -> f64;
    pub fn Rf_lchoose(arg1: f64,
                      arg2: f64)
     -> f64;
    pub fn Rf_bessel_i(arg1: f64,
                       arg2: f64,
                       arg3: f64)
     -> f64;
    pub fn Rf_bessel_j(arg1: f64,
                       arg2: f64)
     -> f64;
    pub fn Rf_bessel_k(arg1: f64,
                       arg2: f64,
                       arg3: f64)
     -> f64;
    pub fn Rf_bessel_y(arg1: f64,
                       arg2: f64)
     -> f64;
    pub fn Rf_bessel_i_ex(arg1: f64,
                          arg2: f64,
                          arg3: f64,
                          arg4: *mut f64)
     -> f64;
    pub fn Rf_bessel_j_ex(arg1: f64,
                          arg2: f64,
                          arg3: *mut f64)
     -> f64;
    pub fn Rf_bessel_k_ex(arg1: f64,
                          arg2: f64,
                          arg3: f64,
                          arg4: *mut f64)
     -> f64;
    pub fn Rf_bessel_y_ex(arg1: f64,
                          arg2: f64,
                          arg3: *mut f64)
     -> f64;
    pub fn Rf_pythag(arg1: f64,
                     arg2: f64)
     -> f64;
    pub fn Rf_imax2(arg1: i32, arg2: i32)
     -> i32;
    pub fn Rf_imin2(arg1: i32, arg2: i32)
     -> i32;
    pub fn Rf_fmax2(arg1: f64,
                    arg2: f64)
     -> f64;
    pub fn Rf_fmin2(arg1: f64,
                    arg2: f64)
     -> f64;
    pub fn Rf_sign(arg1: f64)
     -> f64;
    pub fn Rf_fprec(arg1: f64,
                    arg2: f64)
     -> f64;
    pub fn Rf_fround(arg1: f64,
                     arg2: f64)
     -> f64;
    pub fn Rf_fsign(arg1: f64,
                    arg2: f64)
     -> f64;
    pub fn Rf_ftrunc(arg1: f64)
     -> f64;
    pub fn cospi(arg1: f64) -> f64;
    pub fn sinpi(arg1: f64) -> f64;
    pub fn tanpi(arg1: f64) -> f64;
}
