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
    *,
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
    B0 = diffusion(t0, x0, args)
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

            f = np.asarray(drift(tn, x, args), dtype=float)
            B = np.asarray(diffusion(tn, x, args), dtype=float)

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
