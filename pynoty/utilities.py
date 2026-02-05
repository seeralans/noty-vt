from __future__ import annotations
from typing import Callable, Tuple, Optional, Sequence
import numpy as np

Array = np.ndarray

def euler_maruyama(
    drift: Callable[[float, Array, Sequence], Array],
    diffusion: Callable[[float, Array, Sequence], Array],
    x0: Array,
    t0: float,
    t1: float,
    dt: float,
    args: Sequence = (),
    trajectories: int = 1,
    rng: Optional[np.random.Generator] = None,
) -> Tuple[Array, Array]:
    """
    Euler–Maruyama for dX = f(t,x) dt + B(t,x) dW, with fixed step size.
    
    Parameters
    ----------
    drift : (t, x, args) -> (d,)
        Drift vector f(t, x).
    diffusion : (t, x, args) -> (d, m) or (d,)
        Diffusion matrix B(t, x). If shape is (d,), it's treated as diagonal diag(B).
    x0 : (d,)
        Initial state.
    t0, t1 : float
        Start and end time (t1 > t0).
    dt : float
        Step size (constant).
    args : extra positional args passed to drift/diffusion
    trajectories : int
        Number of independent paths to simulate.
    rng : np.random.Generator, optional
        Random number generator (for reproducibility).

    Returns
    -------
    t : (N+1,)
        Time grid.
    X : (trajectories, N+1, d)
        Simulated paths.
    """
    if rng is None:
        rng = np.random.default_rng()

    x0 = np.asarray(x0, dtype=float)
    d = x0.size
    assert t1 > t0 and dt > 0.0
    N = int(np.ceil((t1 - t0) / dt))
    t = np.linspace(t0, t0 + N*dt, N + 1)

    # Allocate output
    X = np.empty((trajectories, N + 1, d), dtype=float)
    X[:, 0, :] = x0

    # Determine noise dimension m from first diffusion evaluation
    B0 = diffusion(t0, x0, *args)
    B0 = np.asarray(B0, dtype=float)
    if B0.ndim == 1:             # diagonal diffusion
        m = d
    elif B0.ndim == 2:           # full matrix (d, m)
        assert B0.shape[0] == d
        m = B0.shape[1]
    else:
        raise ValueError("diffusion() must return shape (d,) or (d, m)")

    sqdt = np.sqrt(dt)

    for n in range(N):
        tn = t[n]
        for r in range(trajectories):
            x = X[r, n, :]

            f = np.asarray(drift(tn, x, *args), dtype=float)
            B = np.asarray(diffusion(tn, x, *args), dtype=float)

            if B.ndim == 1:
                # Diagonal: dW ~ N(0, dt I_d)
                dW = rng.standard_normal(size=d) * sqdt
                x_next = x + f * dt + B * dW
            else:
                # Full: B (d×m) @ dW (m,)
                dW = rng.standard_normal(size=m) * sqdt
                x_next = x + f * dt + B @ dW

            X[r, n + 1, :] = x_next

    return t, X


def running_time_average_and_empirical_density(t_vals, x_t, x_bins):
    """
    Compute running time average of x(t) and empirical density (time-weighted).

    Parameters
    ----------
    t_vals : array_like (N,)
        Strictly increasing times.
    x_t : array_like (N,)
        Trajectory values.
    x_bins : array_like
        Bin edges for histogram.

    Returns
    -------
    running_avg : ndarray (N,)
        Running time average up to each t_n.
    bin_centres : ndarray
        Centres of bins.
    density : ndarray
        Empirical density over x.
    """
    t_vals = np.asarray(t_vals)
    x_t = np.asarray(x_t)
    x_bins = np.asarray(x_bins)

    if t_vals.shape != x_t.shape:
        raise ValueError("t_vals and x_t must have the same shape")

    dt = np.diff(t_vals)
    if np.any(dt <= 0):
        raise ValueError("t_vals must be strictly increasing")

    # --- Running time average ---
    # Left-hand scheme: integral ≈ Σ x[i] * dt[i]
    partial_integral = np.zeros_like(x_t, dtype=float)
    partial_integral[1:] = np.cumsum(x_t[:-1] * dt)

    T_running = t_vals - t_vals[0]
    running_avg = np.zeros_like(x_t, dtype=float)
    running_avg[0] = x_t[0]  # convention
    running_avg[1:] = partial_integral[1:] / T_running[1:]

    # --- Empirical density with time weighting ---
    counts, edges = np.histogram(x_t[:-1], bins=x_bins, weights=dt)
    widths = np.diff(edges)
    T_total = T_running[-1]
    density = counts / (T_total * widths)
    bin_centres = 0.5 * (edges[:-1] + edges[1:])

    return running_avg, bin_centres, density


import numpy as np

def running_time_average_and_running_density(t_vals, x_t, x_bins):
    """
    Running time average and running empirical density (time–weighted).

    Parameters
    ----------
    t_vals : array_like (N,)
    x_t    : array_like (N,)
    x_bins : array_like (M+1,)

    Returns
    -------
    running_avg     : (N,)
    bin_centres     : (M,)
    running_density : (N, M)
    """

    t_vals = np.asarray(t_vals)
    x_t = np.asarray(x_t)
    x_bins = np.asarray(x_bins)

    if t_vals.shape != x_t.shape:
        raise ValueError("t_vals and x_t must be the same shape")

    dt = np.diff(t_vals)
    if np.any(dt <= 0):
        raise ValueError("t_vals must be strictly increasing")

    n = len(t_vals)
    m = len(x_bins) - 1
    widths = np.diff(x_bins)
    bin_centres = 0.5 * (x_bins[:-1] + x_bins[1:])

    # ---------------------------
    # running time average of x(t)
    # ---------------------------
    partial_int = np.zeros(n)
    partial_int[1:] = np.cumsum(x_t[:-1] * dt)

    t_running = t_vals - t_vals[0]

    running_avg = np.zeros(n)
    running_avg[0] = x_t[0]
    running_avg[1:] = partial_int[1:] / t_running[1:]

    # -------------------------------------
    # running time–averaged empirical density
    # -------------------------------------
    # bin index for each x_t[i], using left-rule times dt[i]
    bin_index = np.digitize(x_t[:-1], x_bins) - 1
    bin_index = np.clip(bin_index, 0, m - 1)

    time_in_bins = np.zeros((n, m))

    for i in range(n - 1):
        time_in_bins[i + 1] = time_in_bins[i]
        time_in_bins[i + 1, bin_index[i]] += dt[i]

    running_density = np.zeros_like(time_in_bins)
    for j in range(m):
        running_density[1:, j] = time_in_bins[1:, j] / (t_running[1:] * widths[j])

    return running_avg, bin_centres, running_density
