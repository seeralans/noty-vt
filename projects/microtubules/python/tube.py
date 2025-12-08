import numpy as np
import matplotlib.pyplot as pp
from mpl_toolkits.mplot3d.art3d import Line3DCollection

# ----------------- small maths helpers -----------------

def _unit(v, eps=1e-12):
    n = np.linalg.norm(v, axis=-1, keepdims=True)
    n = np.maximum(n, eps)
    return v / n

def estimate_tangents(P):
    """Finite-difference unit tangents for a polyline trajectory P (N,3)."""
    P = np.asarray(P)
    N = len(P)
    T = np.zeros_like(P)
    if N == 1:
        T[0] = np.array([1.0, 0.0, 0.0])
        return T
    T[1:-1] = P[2:] - P[:-2]
    T[0]    = P[1] - P[0]
    T[-1]   = P[-1] - P[-2]
    return _unit(T)

def normal_plane_basis(t):
    """Orthonormal basis (n1, n2) spanning the plane perpendicular to unit tangent t."""
    helper = np.array([1.0, 0.0, 0.0]) if abs(t[0]) < 0.9 else np.array([0.0, 1.0, 0.0])
    n1 = np.cross(t, helper)
    n1 = n1 / (np.linalg.norm(n1) + 1e-12)
    n2 = np.cross(t, n1)
    return n1, n2

def project_cov_to_plane(Sigma, basis2):
    """3×3 covariance -> 2×2 covariance in the plane spanned by basis2 (3×2)."""
    B = basis2
    return B.T @ Sigma @ B

def ellipse_points_from_cov(S2, k=1.0, num=100):
    """Return 2D ellipse points (2,num) for covariance S2 at Mahalanobis radius k."""
    vals, vecs = np.linalg.eigh(S2)
    vals = np.maximum(vals, 0.0)
    axes = k * np.sqrt(vals)
    theta = np.linspace(0, 2*np.pi, num, endpoint=True)
    circle = np.vstack([np.cos(theta), np.sin(theta)])
    return vecs @ np.diag(axes) @ circle  # (2,num)

# Optional: specify probability p (e.g. 0.95) instead of k; uses chi-square df=2.
def k_from_prob_2d(p):
    """Convert desired 2D coverage probability p to Mahalanobis radius k (df=2)."""
    # Try SciPy for exact quantile; otherwise Wilson–Hilferty approximation.
    try:
        from scipy.stats import chi2
        return np.sqrt(chi2.ppf(p, df=2))
    except Exception:
        # Wilson–Hilferty: if X~chi2(df), X ≈ df*(1 - 2/(9df) + z*sqrt(2/(9df)))^3
        from math import sqrt
        # Invert approx using a probit approximation for z
        def inv_norm(u):
            # Acklam's rational approximation (good enough for plotting)
            a = [ -3.969683028665376e+01,  2.209460984245205e+02,
                  -2.759285104469687e+02,  1.383577518672690e+02,
                  -3.066479806614716e+01,  2.506628277459239e+00 ]
            b = [ -5.447609879822406e+01,  1.615858368580409e+02,
                  -1.556989798598866e+02,  6.680131188771972e+01,
                  -1.328068155288572e+01 ]
            c = [ -7.784894002430293e-03, -3.223964580411365e-01,
                  -2.400758277161838e+00, -2.549732539343734e+00,
                   4.374664141464968e+00,  2.938163982698783e+00 ]
            d = [ 7.784695709041462e-03,  3.224671290700398e-01,
                  2.445134137142996e+00,  3.754408661907416e+00 ]
            plow  = 0.02425
            phigh = 1 - plow
            if u < plow:
                q = np.sqrt(-2*np.log(u))
                return (((((c[0]*q+c[1])*q+c[2])*q+c[3])*q+c[4])*q+c[5]) / \
                       ((((d[0]*q+d[1])*q+d[2])*q+d[3])*q+1)
            if u > phigh:
                q = np.sqrt(-2*np.log(1-u))
                return -(((((c[0]*q+c[1])*q+c[2])*q+c[3])*q+c[4])*q+c[5]) / \
                         ((((d[0]*q+d[1])*q+d[2])*q+d[3])*q+1)
            q = u - 0.5
            r = q*q
            return (((((a[0]*r+a[1])*r+a[2])*r+a[3])*r+a[4])*r+a[5])*q / \
                   (((((b[0]*r+b[1])*r+b[2])*r+b[3])*r+b[4])*r+1)
        z = inv_norm(p)
        df = 2.0
        x = df * (1 - 2/(9*df) + z*np.sqrt(2/(9*df)))**3
        return np.sqrt(max(x, 0.0))

# ----------------- 1) trajectory-only plot -----------------

def plot_trajectory_3d_pub(x, y, z, figax=None, title=None, outfile=None, **kwargs):
    """
    Plot just the 3D trajectory.
    Returns (fig, ax).
    """
    x, y, z = np.asarray(x), np.asarray(y), np.asarray(z)

    if figax is None:
        fig = pp.figure(figsize=(4, 4))
        ax = fig.add_subplot(111, projection="3d")
    else:
        fig, ax = figax

    ax.plot(x, y, z, **kwargs)

    ax.grid(False)
    ax.set_xlabel(r"$x$")
    ax.set_ylabel(r"$y$")
    ax.set_zlabel(r"$z$")
    # keep aspect sensible
    ax.set_box_aspect([np.ptp(x), np.ptp(y), np.ptp(z)] if all(np.ptp(a) > 0 for a in (x,y,z)) else [1,1,1])
    if title:
        ax.set_title(title)

    pp.tight_layout()
    if outfile:
        pp.savefig(outfile, dpi=900, bbox_inches="tight")
    return fig, ax

# ----------------- 2) add ellipses onto existing axes -----------------

def add_normalplane_cov_ellipses(ax, P, covariances, *,
                                 k=None, p=None, every=1,
                                 num_pts=120, ellipse_kwargs=None):
    """
    Draw covariance ellipses in planes perpendicular to the trajectory tangent.

    ax            : a 3D Axes
    P             : (N,3) trajectory points
    covariances   : (N,3,3) covariance at each point
    k             : Mahalanobis radius (e.g., 1.0, 2.0, 3.0)
    p             : desired 2D coverage probability (e.g., 0.95). If given, overrides k.
    every         : plot every Nth point
    num_pts       : points per ellipse
    ellipse_kwargs: styling dict passed to Line3DCollection
    """
    P = np.asarray(P)
    covs = np.asarray(covariances)
    assert P.shape[1] == 3 and covs.shape == (len(P), 3, 3), "Shapes must be (N,3) and (N,3,3)."

    if p is not None:
        k = k_from_prob_2d(p)
    if k is None:
        k = 1.0

    if ellipse_kwargs is None:
        ellipse_kwargs = dict(alpha=0.3, linewidths=0.8)

    T = estimate_tangents(P)

    segments = []
    for i in range(0, len(P), every):
        t = T[i]
        n1, n2 = normal_plane_basis(t)
        B = np.stack([n1, n2], axis=1)            # (3,2)
        S2 = project_cov_to_plane(covs[i], B)     # (2,2)
        E2 = ellipse_points_from_cov(S2, k=k, num=num_pts)  # (2,num_pts)
        E3 = (P[i].reshape(3,1) + B @ E2).T       # (num_pts,3)
        segments.append(E3)

    lc = Line3DCollection(segments, **ellipse_kwargs)
    ax.add_collection3d(lc)
    return lc  # in case you want to tweak/remove later

# ----------------- tiny demo -----------------
if __name__ == "__main__":
    t = np.linspace(0, 4*np.pi, 200)
    x, y, z = np.cos(t), np.sin(t), 0.2*t
    P = np.column_stack([x, y, z])

    # Make some varying 3×3 covariances
    covs = []
    for ti in t:
        lam = np.diag([0.02**2, 0.01**2, 0.03**2])
        c, s = np.cos(0.5*ti), np.sin(0.5*ti)
        Rz = np.array([[c,-s,0],[s,c,0],[0,0,1]])
        covs.append(Rz @ lam @ Rz.T)
    covs = np.stack(covs)

    fig, ax = plot_trajectory_3d_pub(x, y, z, color="k")
    add_normalplane_cov_ellipses(ax, P, covs, p=0.95, every=10)  # or k=2.0
    pp.show()
