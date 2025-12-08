# -*- coding: utf-8 -*-
"""
Python translation of the provided Rust functions.
Requires: numpy
"""

from __future__ import annotations
from typing import Tuple
import numpy as np


# ----------------------------
# Public API (parity with Rust)
# ----------------------------

def micro_full_smooth_lam_py(
    t: float,                     # kept for signature parity; unused
    state: np.ndarray,            # shape (3,) -> [ph, th, xt]
    j: float,
    h: float,
    g: float,
    bes: float,
    ep: float,
    smooth: int,
) -> np.ndarray:
    """
    Equivalent to the Rust #[pyfunction] micro_full_smooth_lam_py.
    """
    state = np.asarray(state, dtype=float)
    lams = state_to_lambda_sigmoid(state, ep, smooth)
    lam_eta, lam_phi, lam_the, lam_xtd = lams
    return micro_full(t, state, j, h, g, bes, lam_eta, lam_phi, lam_the, lam_xtd)


def micro_full_py(
    t: float,                     # kept for signature parity; unused
    state: np.ndarray,            # shape (3,)
    j: float,
    h: float,
    g: float,
    bes: float,
    lam_eta: float,
    lam_phi: float,
    lam_the: float,
    lam_xtd: float,
) -> np.ndarray:
    """
    Equivalent to the Rust #[pyfunction] micro_full_py.
    """
    state = np.asarray(state, dtype=float)
    return micro_full(t, state, j, h, g, bes, lam_eta, lam_phi, lam_the, lam_xtd)


def micro_full(
    t: float,                     # kept for signature parity; unused
    state: np.ndarray,            # shape (3,) -> [ph, th, xt]
    j: float,
    h: float,
    g: float,
    bes: float,
    lam_eta: float,
    lam_phi: float,
    lam_the: float,
    lam_xtd: float,
) -> np.ndarray:
    """
    Direct translation of the Rust micro_full body.
    Returns a vector (3,) corresponding to [pḣ, tḣ, ẋt].
    """
    state = np.asarray(state, dtype=float)
    assert state.shape[0] == 3, "state must be length 3: [ph, th, xt]"
    ph, th, xt = state

    # uf(x) = (1 - tanh(x)) / 2
    uf = lambda x: (1.0 - np.tanh(x)) / 2.0

    jtt, jtd, jdd, h, gb, gc = expand_params(j, h, g)
    rho_tg, rho_dg, rho_tc, rho_dc = lam_to_rhos(lam_eta, lam_phi, lam_the, lam_xtd)

    ntttr = ((ph - xt / 2.0) ** 2 / ph) * uf(bes / 2.0 * (2.0 * jtd - 2.0 * jtt - h - gb))
    ndtdr = ((xt / 2.0) ** 2 / ph) * uf(bes / 2.0 * (2.0 * jdd - 2.0 * jtd - h - gb))
    ndddr = ((th - xt / 2.0) ** 2 / th) * uf(bes / 2.0 * (2.0 * jtd - 2.0 * jdd + h + gb))
    ntdtr = ((xt / 2.0) ** 2 / th) * uf(bes / 2.0 * (2.0 * jtt - 2.0 * jtd + h + gb))

    ndttr = ((ph - xt / 2.0) * xt / 2.0 / ph) * uf(bes / 2.0 * (1.0 * jdd - 1.0 * jtt - h - gb))
    nttdr = ((ph - xt / 2.0) * xt / 2.0 / ph) * uf(bes / 2.0 * (1.0 * jdd - 1.0 * jtt - h - gb))
    nddtr = ((th - xt / 2.0) * xt / 2.0 / th) * uf(bes / 2.0 * (1.0 * jtt - 1.0 * jdd + h + gb))
    ntddr = ((th - xt / 2.0) * xt / 2.0 / th) * uf(bes / 2.0 * (1.0 * jtt - 1.0 * jdd + h + gb))

    ntterm = rho_tc * (ph - xt / 2.0) / ph * uf(-bes / 2.0 * (jtt + h))
    ndterm = rho_tc * (xt / 2.0) / ph * uf(-bes / 2.0 * (jtd + h))
    ndderm = rho_dc * (th - xt / 2.0) / th * uf(-bes / 2.0 * (jdd))
    ntderm = rho_dc * (xt / 2.0) / th * uf(-bes / 2.0 * (jtd))

    ntterc = rho_tc * (ph - xt / 2.0) / ph * uf(bes / 2.0 * (jtd - jtt - h - gc))
    ndterc = rho_tc * (xt / 2.0) / ph * uf(bes / 2.0 * (jdd - jtd - h - gc))
    ndderc = rho_dc * (th - xt / 2.0) / th * uf(bes / 2.0 * (jtd - jdd + h + gc))
    ntderc = rho_dc * (xt / 2.0) / th * uf(bes / 2.0 * (jtt - jtd + h + gc))

    wmttts = ntttr
    wpttts = ntdtr
    wmddds = ndddr
    wpddds = ndtdr
    wpttds = nddtr + ntddr
    wmttds = nttdr + ndttr

    wptts = rho_tg * uf(bes / 2.0 * (jtt + h))
    wmtts = ntterm

    wpdds = rho_dg * uf(bes / 2.0 * (jdd))
    wmdds = ndderm

    wptds = rho_tg * uf(bes / 2.0 * (jtd))
    wmtds = ntderm

    wpdts = rho_dg * uf(bes / 2.0 * (jtd + h))
    wmdts = ndterm

    wmttes = ntterc
    wpttes = ntderc
    wmddes = ndderc
    wpddes = ndterc

    phm_ds = 0.0
    phm_ds += wmddds + wpttts + wpttds + wpttes + wmddes + wpdts + wptts
    phm_ds -= wpddds + wmttts + wmttds + wmttes + wpddes + wmdts + wmtts

    thm_ds = 0.0
    thm_ds += wmttts + wpddds + wmttds + wmttes + wpddes + wptds + wpdds
    thm_ds -= wpttts + wmddds + wpttds + wpttes + wmddes + wmtds + wmdds

    xhm_ds = 0.0
    xhm_ds += 2.0 * wmttts + 2.0 * wmddds + wmttes + wmddes + wptds + wpdts
    xhm_ds -= 2.0 * wpttts + 2.0 * wpddds + wpttes + wpddes + wmtds + wmdts

    return np.array([phm_ds, thm_ds, xhm_ds], dtype=float)


def micro_full_drift_and_diff_smooth_lam(
    t: float,                     # kept for signature parity; unused
    state: np.ndarray,            # shape (3,)
    j: float,
    h: float,
    g: float,
    bes: float,
    ep: float,
    smooth: int,
) -> Tuple[np.ndarray, np.ndarray]:
    """
    Compute (drift, diffusion-like matrix) using smoothed lambdas.
    Returns:
        drift_vec: (3,)
        diff_mat:  (3, 18)
    """
    state = np.asarray(state, dtype=float)
    lams = state_to_lambda_sigmoid(state, ep, smooth)
    lam_eta, lam_phi, lam_the, lam_xtd = lams
    return micro_full_drift_and_diff(t, state, j, h, g, bes, lam_eta, lam_phi, lam_the, lam_xtd)


def micro_full_drift_and_diff(
    t: float,                     # kept for signature parity; unused
    state: np.ndarray,            # shape (3,)
    j: float,
    h: float,
    g: float,
    bes: float,
    lam_eta: float,
    lam_phi: float,
    lam_the: float,
    lam_xtd: float,
) -> Tuple[np.ndarray, np.ndarray]:
    """
    Direct translation of the Rust micro_full_drift_and_diff.
    Returns:
        drift_vec: (3,)
        diff_mat:  (3, 18)  == delta_mat @ w_mat
    """
    state = np.asarray(state, dtype=float)
    assert state.shape[0] == 3, "state must be length 3: [ph, th, xt]"
    ph, th, xt = state

    uf = lambda x: (1.0 - np.tanh(x)) / 2.0

    jtt, jtd, jdd, h, gb, gc = expand_params(j, h, g)
    rho_tg, rho_dg, rho_tc, rho_dc = lam_to_rhos(lam_eta, lam_phi, lam_the, lam_xtd)

    ntttr = ((ph - xt / 2.0) ** 2 / ph) * uf(bes / 2.0 * (2.0 * jtd - 2.0 * jtt - h - gb))
    ndtdr = ((xt / 2.0) ** 2 / ph) * uf(bes / 2.0 * (2.0 * jdd - 2.0 * jtd - h - gb))
    ndddr = ((th - xt / 2.0) ** 2 / th) * uf(bes / 2.0 * (2.0 * jtd - 2.0 * jdd + h + gb))
    ntdtr = ((xt / 2.0) ** 2 / th) * uf(bes / 2.0 * (2.0 * jtt - 2.0 * jtd + h + gb))

    ndttr = ((ph - xt / 2.0) * xt / 2.0 / ph) * uf(bes / 2.0 * (1.0 * jdd - 1.0 * jtt - h - gb))
    nttdr = ((ph - xt / 2.0) * xt / 2.0 / ph) * uf(bes / 2.0 * (1.0 * jdd - 1.0 * jtt - h - gb))
    nddtr = ((th - xt / 2.0) * xt / 2.0 / th) * uf(bes / 2.0 * (1.0 * jtt - 1.0 * jdd + h + gb))
    ntddr = ((th - xt / 2.0) * xt / 2.0 / th) * uf(bes / 2.0 * (1.0 * jtt - 1.0 * jdd + h + gb))

    ntterm = rho_tc * (ph - xt / 2.0) / ph * uf(-bes / 2.0 * (jtt + h))
    ndterm = rho_tc * (xt / 2.0) / ph * uf(-bes / 2.0 * (jtd + h))
    ndderm = rho_dc * (th - xt / 2.0) / th * uf(-bes / 2.0 * (jdd))
    ntderm = rho_dc * (xt / 2.0) / th * uf(-bes / 2.0 * (jtd))

    ntterc = rho_tc * (ph - xt / 2.0) / ph * uf(bes / 2.0 * (jtd - jtt - h - gc))
    ndterc = rho_tc * (xt / 2.0) / ph * uf(bes / 2.0 * (jdd - jtd - h - gc))
    ndderc = rho_dc * (th - xt / 2.0) / th * uf(bes / 2.0 * (jtd - jdd + h + gc))
    ntderc = rho_dc * (xt / 2.0) / th * uf(bes / 2.0 * (jtt - jtd + h + gc))

    wmttts = ntttr
    wpttts = ntdtr
    wmddds = ndddr
    wpddds = ndtdr
    wpttds = nddtr + ntddr
    wmttds = nttdr + ndttr

    wptts = rho_tg * uf(bes / 2.0 * (jtt + h))
    wmtts = ntterm

    wpdds = rho_dg * uf(bes / 2.0 * (jdd))
    wmdds = ndderm

    wptds = rho_tg * uf(bes / 2.0 * (jtd))
    wmtds = ntderm

    wpdts = rho_dg * uf(bes / 2.0 * (jtd + h))
    wmdts = ndterm

    wmttes = ntterc
    wpttes = ntderc
    wmddes = ndderc
    wpddes = ndterc

    phm_ds = 0.0
    phm_ds += wmddds + wpttts + wpttds + wpttes + wmddes + wpdts + wptts
    phm_ds -= wpddds + wmttts + wmttds + wmttes + wpddes + wmdts + wmtts

    thm_ds = 0.0
    thm_ds += wmttts + wpddds + wmttds + wmttes + wpddes + wptds + wpdds
    thm_ds -= wpttts + wmddds + wpttds + wpttes + wmddes + wmtds + wmdds

    xhm_ds = 0.0
    xhm_ds += 2.0 * wmttts + 2.0 * wmddds + wmttes + wmddes + wptds + wpdts
    xhm_ds -= 2.0 * wpttts + 2.0 * wpddds + wpttes + wpddes + wmtds + wmdts

    drift_vec = np.array([phm_ds, thm_ds, xhm_ds], dtype=float)

    delta_mat = np.zeros((3, 18), dtype=float)
    w_mat = np.zeros((18, 18), dtype=float)

    # TTT -> TDT
    delta_mat[:, 0] = [-1.0,  1.0,  2.0]
    w_mat[0, 0] = np.sqrt(abs(wmttts))
    # TTD -> TDD
    delta_mat[:, 1] = [-1.0,  1.0,  0.0]
    w_mat[1, 1] = np.sqrt(abs(wmttds))
    # DTD -> DDD
    delta_mat[:, 2] = [-1.0,  1.0, -2.0]
    w_mat[2, 2] = np.sqrt(abs(wpddds))

    # DDD -> DTD
    delta_mat[:, 3] = [ 1.0, -1.0,  2.0]
    w_mat[3, 3] = np.sqrt(abs(wmddds))
    # TTD -> DTT
    delta_mat[:, 4] = [ 1.0, -1.0,  0.0]
    w_mat[4, 4] = np.sqrt(abs(wpttds))
    # DTD -> TTT
    delta_mat[:, 5] = [ 1.0, -1.0, -2.0]
    w_mat[5, 5] = np.sqrt(abs(wpttts))

    # TEE -> TTE
    delta_mat[:, 6] = [ 1.0,  0.0,  0.0]
    w_mat[6, 6] = np.sqrt(abs(wptts))
    # DEE -> DTE
    delta_mat[:, 7] = [ 1.0,  0.0,  1.0]
    w_mat[7, 7] = np.sqrt(abs(wpdts))
    # DEE -> DDE
    delta_mat[:, 8] = [ 0.0,  1.0,  0.0]
    w_mat[8, 8] = np.sqrt(abs(wpdds))
    # TEE -> TDE
    delta_mat[:, 9] = [ 0.0,  1.0,  1.0]
    w_mat[9, 9] = np.sqrt(abs(wptds))

    # TTE -> TEE
    delta_mat[:, 10] = [-1.0,  0.0,  0.0]
    w_mat[10, 10] = np.sqrt(abs(wmtts))
    # DTE -> TEE
    delta_mat[:, 11] = [-1.0,  0.0, -1.0]
    w_mat[11, 11] = np.sqrt(abs(wmdts))
    # DDE -> DEE
    delta_mat[:, 12] = [ 0.0, -1.0,  0.0]
    w_mat[12, 12] = np.sqrt(abs(wmdds))
    # TDE -> TEE
    delta_mat[:, 13] = [ 0.0, -1.0, -1.0]
    w_mat[13, 13] = np.sqrt(abs(wmtds))

    # TTE -> TDE
    delta_mat[:, 14] = [-1.0,  1.0,  1.0]
    w_mat[14, 14] = np.sqrt(abs(wmttes))
    # DTE -> DDE
    delta_mat[:, 15] = [-1.0,  1.0, -1.0]
    w_mat[15, 15] = np.sqrt(abs(wpddes))
    # DDE -> DTE
    delta_mat[:, 16] = [ 1.0, -1.0,  1.0]
    w_mat[16, 16] = np.sqrt(abs(wmddes))
    # TDE -> TTE
    delta_mat[:, 17] = [ 1.0, -1.0, -1.0]
    w_mat[17, 17] = np.sqrt(abs(wpttes))

    diff_mat = delta_mat @ w_mat  # shape (3, 18)

    return drift_vec, diff_mat


# --------------------------------
# Helpers mirroring your Rust code
# --------------------------------

def lam_to_rhos(l_eta: float, l_phi: float, l_the: float, l_xtd: float) -> Tuple[float, float, float, float]:
    """Rust-equivalent of MATLAB lam_to_rhos (scalar in/scalar out)."""
    lep = (1.0 + l_eta) / 2.0
    lpp = (1.0 + l_phi) / 2.0
    ltp = (1.0 + l_the) / 2.0
    lxp = (1.0 + l_xtd) / 2.0

    lem = (1.0 - l_eta) / 2.0
    lpm = (1.0 - l_phi) / 2.0
    ltm = (1.0 - l_the) / 2.0
    lxm = (1.0 - l_xtd) / 2.0

    pp = lpp * lpm
    tt = ltp * ltm
    xx = lxp * lxm

    rho_tg = 0.5
    rho_tg += tt * 2.0
    rho_tg -= pp * 2.0
    rho_tg -= xx * 2.0
    rho_tg += xx * pp * 8.0
    rho_tg += pp * tt * xx * 32.0

    rho_dg = 0.5
    rho_dg -= tt * 2.0
    rho_dg += pp * 2.0
    rho_dg += xx * 2.0
    rho_dg -= xx * pp * 8.0
    rho_dg -= pp * tt * xx * 32.0

    rho_tc = rho_tg * ltp * lpp * lxp
    rho_dc = rho_dg * ltp * lpp * lxp

    rho_tg *= lep
    rho_dg *= lep

    return rho_tg, rho_dg, rho_tc, rho_dc


def expand_params(j: float, h: float, g: float) -> Tuple[float, float, float, float, float, float]:
    """Rust-equivalent of MATLAB expand_params (all scalars)."""
    jtt = -j
    jtd = j
    jdd = j
    gb = g * 1.0
    gc = g * 1.0
    return jtt, jtd, jdd, h, gb, gc


def state_to_lambda_sigmoid(state: np.ndarray, ep: float, smooth: int) -> np.ndarray:
    """
    Build [lam_eta, lam_phi, lam_the, lam_xtd] via chosen smooth sigmoid.
    """
    state = np.asarray(state, dtype=float)
    assert state.shape[0] >= 3, "state must be at least [x1, x2, x3]"
    assert ep != 0.0, "ep must be non-zero to avoid division by zero"

    return np.array([
        smooth_sigmoid(sigma_eta(state), ep, smooth),
        smooth_sigmoid(sigma_phi(state), ep, smooth),
        smooth_sigmoid(sigma_the(state), ep, smooth),
        smooth_sigmoid(sigma_xtd(state), ep, smooth),
    ], dtype=float)


def smooth_sigmoid(sigma: float, ep: float, smooth: int) -> float:
    if smooth == 0:
        return smooth_sigmoid_c0_interp(sigma, ep)
    elif smooth == 1:
        return smooth_sigmoid_c1_interp(sigma, ep)
    else:
        return smooth_sigmoid_tanh(sigma, ep)


def smooth_sigmoid_tanh(sigma: float, ep: float) -> float:
    return np.tanh(sigma / ep)


def smooth_sigmoid_c0_interp(sigma: float, ep: float) -> float:
    if sigma <= -ep:
        return -1.0
    elif sigma >= ep:
        return 1.0
    else:
        return sigma / ep


def smooth_sigmoid_c1_interp(sigma: float, ep: float) -> float:
    if sigma <= -ep:
        return -1.0
    elif sigma >= ep:
        return 1.0
    else:
        x = sigma / ep
        return x + 0.5 * x**3 - 0.5 * x**5


def sigma_eta(x: np.ndarray) -> float:
    return 1.0 - x[0] - x[1]


def sigma_phi(x: np.ndarray) -> float:
    return 2.0 * x[0] - x[2]


def sigma_the(x: np.ndarray) -> float:
    return 2.0 * x[1] - x[2]


def sigma_xtd(x: np.ndarray) -> float:
    return x[2]


# ------------------------
# Linear-algebra utility
# ------------------------

def sqrt_ada_t(a: np.ndarray, d_diag: np.ndarray) -> np.ndarray:
    """
    Principal symmetric square root of A D A^T.
    a: (d, m)
    d_diag: (m,), entries must be >= 0
    Returns: (d, d) array equal to (A D A^T)^{1/2} = U Σ U^T
             where A sqrt(D) = U Σ V^T (thin SVD).
    """
    a = np.asarray(a, dtype=float)
    d_diag = np.asarray(d_diag, dtype=float)
    d, m = a.shape
    if d_diag.shape[0] != m:
        raise ValueError(f"D diagonal has length {d_diag.shape[0]}, but A has {m} columns")
    if np.any(d_diag < 0.0):
        raise ValueError("D must be PSD (all diagonal entries >= 0) for a real square root.")

    # B = A * sqrt(D): scale columns of A by sqrt(diag(D))
    d_sqrt = np.sqrt(d_diag)
    b = a * d_sqrt  # NumPy broadcasts columnwise with (m,) across axis=1

    # Thin SVD: B = U Σ V^T (U: d×r, Σ: r×r, V^T: r×m), r = rank
    U, S, _Vt = np.linalg.svd(b, full_matrices=False)
    # (A D A^T)^(1/2) = U Σ U^T
    sroot = U @ np.diag(S) @ U.T
    assert sroot.shape == (d, d)
    return sroot



def jac_micro_full_smooth_lam(
    state: np.ndarray,           # [ph, th, xt]
    j: float,
    h: float,
    g: float,
    bes: float,
    ep: float,
    smooth: int,
    *, denom_eps: float = 0.0) -> np.ndarray:
    lams = state_to_lambda_sigmoid(state, ep, smooth)
    lam_eta, lam_phi, lam_the, lam_xtd = lams
    return jac_micro_full(state, j, h, g, bes, lam_eta, lam_phi, lam_the, lam_xtd)




def jac_micro_full(
    state: np.ndarray,           # [ph, th, xt]
    j: float,
    h: float,
    g: float,
    bes: float,
    lam_eta: float,
    lam_phi: float,
    lam_the: float,
    lam_xtd: float,
    *, denom_eps: float = 0.0    # set e.g. 1e-12 if you want safety clamps
) -> np.ndarray:
    """
    Jacobian J = d/d[ph,th,xt] micro_full(·) at 'state'.
    Notes:
      - Assumes lambdas are provided explicitly, so rhos and uf-arguments are constants w.r.t. state.
      - If ph or th can get very small, consider a tiny denom_eps > 0 to avoid division issues.
    """
    state = np.asarray(state, dtype=float)
    assert state.shape[0] == 3, "state must be length 3: [ph, th, xt]"
    ph, th, xt = state

    # tiny stabiliser if desired
    a = ph if denom_eps == 0.0 else max(ph, denom_eps)
    b = th if denom_eps == 0.0 else max(th, denom_eps)
    x  = xt

    # aliases
    x2  = 0.5 * x
    A0  = a - x2          # ph - xt/2
    B0  = b - x2          # th - xt/2

    # uf(x) = (1 - tanh(x)) / 2  (constant wrt state here)
    uf = lambda z: (1.0 - np.tanh(z)) / 2.0

    # params & rhos
    jtt, jtd, jdd, h_eff, gb, gc = expand_params(j, h, g)
    rho_tg, rho_dg, rho_tc, rho_dc = lam_to_rhos(lam_eta, lam_phi, lam_the, lam_xtd)

    # ----- constant multipliers (w.r.t. state) -----
    # “plain” rates used with f1..f6
    A1 = uf(bes/2.0 * (2.0*jtd - 2.0*jtt - h_eff - gb))  # matches ntttr's uf
    B1 = uf(bes/2.0 * (2.0*jdd - 2.0*jtd - h_eff - gb))  # ndtdr's uf
    C1 = uf(bes/2.0 * (2.0*jtd - 2.0*jdd + h_eff + gb))  # ndddr's uf
    D1 = uf(bes/2.0 * (2.0*jtt - 2.0*jtd + h_eff + gb))  # ntdtr's uf

    E1 = uf(bes/2.0 * (jdd - jtt - h_eff - gb))          # ndttr/nttdr uf
    F1 = E1                                              # identical by code
    G1 = uf(bes/2.0 * (jtt - jdd + h_eff + gb))          # nddtr/ntddr uf
    H1 = G1

    # contact/cross multipliers (paired with g1,g2,h1,h2)
    T1 = rho_tc * uf(-bes/2.0 * (jtt + h_eff))           # ntterm coefficient of g1
    T2 = rho_tc * uf(-bes/2.0 * (jtd + h_eff))           # ndterm coefficient of g2
    Dc1 = rho_dc * uf(-bes/2.0 * (jdd))                  # ndderm coeff of h1
    Dc2 = rho_dc * uf(-bes/2.0 * (jtd))                  # ntderm coeff of h2

    Ec1 = rho_tc * uf(bes/2.0 * (jtd - jtt - h_eff - gc))  # ntterc coeff of g1
    Ec2 = rho_tc * uf(bes/2.0 * (jdd - jtd - h_eff - gc))  # ndterc coeff of g2
    Ec3 = rho_dc * uf(bes/2.0 * (jtd - jdd + h_eff + gc))  # ndderc coeff of h1
    Ec4 = rho_dc * uf(bes/2.0 * (jtt - jtd + h_eff + gc))  # ntderc coeff of h2

    # ----- helper partials for base rational factors -----
    # f1 = (A0)^2 / a
    df1_da = (2.0*A0*a - A0*A0) / (a*a)
    df1_dx = -A0 / a

    # f2 = x^2 / (4a)
    df2_da = -(x*x) / (4.0*a*a)
    df2_dx =  x / (2.0*a)

    # f3 = (B0)^2 / b
    df3_db = (2.0*B0*b - B0*B0) / (b*b)
    df3_dx = -B0 / b

    # f4 = x^2 / (4b)
    df4_db = -(x*x) / (4.0*b*b)
    df4_dx =  x / (2.0*b)

    # f5 = A0 * x2 / a
    df5_da = (x*x) / (4.0*a*a)
    df5_dx = (A0/(2.0*a)) - (x/(4.0*a))

    # f6 = B0 * x2 / b
    df6_db = (x*x) / (4.0*b*b)
    df6_dx = (b - x) / (2.0*b)

    # g1 = A0/a, g2 = x2/a
    dg1_da = x2 / (a*a)
    dg1_dx = -1.0 / (2.0*a)
    dg2_da = -x / (2.0*a*a)
    dg2_dx =  1.0 / (2.0*a)

    # h1 = B0/b, h2 = x2/b
    dh1_db = x2 / (b*b)
    dh1_dx = -1.0 / (2.0*b)
    dh2_db = -x / (2.0*b*b)
    dh2_dx =  1.0 / (2.0*b)

    # ------------------------------------------------------------------
    # Jacobian entries (rows: dph/d·, dth/d·, dxt/d· ; cols: ph, th, xt)
    # ------------------------------------------------------------------

    # Row 1: d(dot{ph})/d(ph,th,xt)
    J11 = -(
        B1*df2_da +
        A1*df1_da +
        (E1+F1)*df5_da +
        (Ec1+T1)*dg1_da +
        (Ec2+T2)*dg2_da
    )
    J12 = (
        C1*df3_db +
        D1*df4_db +
        (G1+H1)*df6_db +
        Ec4*dh2_db +
        Ec3*dh1_db
    )
    J13 = (
        C1*df3_dx +
        D1*df4_dx +
        (G1+H1)*df6_dx +
        Ec4*dh2_dx +
        Ec3*dh1_dx
        - B1*df2_dx
        - A1*df1_dx
        - (E1+F1)*df5_dx
        - Ec1*dg1_dx
        - (Ec2+T2)*dg2_dx
        - T1*dg1_dx
    )

    # Row 2: d(dot{th})/d(ph,th,xt)
    J21 = (
        A1*df1_da +
        B1*df2_da +
        (E1+F1)*df5_da +
        Ec1*dg1_da +
        Ec2*dg2_da
    )
    J22 = (
        C1*df3_db
        - D1*df4_db
        - (G1+H1)*df6_db
        - Ec4*dh2_db
        - Ec3*dh1_db
        - Dc2*dh2_db
        - Dc1*dh1_db
    )
    J23 = (
        A1*df1_dx +
        B1*df2_dx +
        (E1+F1)*df5_dx +
        Ec1*dg1_dx +
        Ec2*dg2_dx
        - D1*df4_dx
        - C1*df3_dx
        - (G1+H1)*df6_dx
        - Ec4*dh2_dx
        - Ec3*dh1_dx
        - Dc2*dh2_dx
        - Dc1*dh1_dx
    )

    # Row 3: d(dot{xt})/d(ph,th,xt)
    J31 = 2.0*A1*df1_da - 2.0*B1*df2_da
    J32 = 2.0*C1*df3_db - 2.0*D1*df4_db
    J33 = (
        2.0*A1*df1_dx +
        2.0*C1*df3_dx -
        2.0*D1*df4_dx -
        2.0*B1*df2_dx +
        Ec1*dg1_dx +
        Ec3*dh1_dx -
        Ec4*dh2_dx -
        Ec2*dg2_dx -
        Dc2*dh2_dx -
        T2*dg2_dx
    )

    return np.array([[J11, J12, J13],
                     [J21, J22, J23],
                     [J31, J32, J33]], dtype=float)
