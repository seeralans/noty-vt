import numpy as np
import matplotlib.pyplot as pp

def disc_sig(z):
  return (1 - z[0] - z[1])

def grad_sig(z):
  return np.array([-1, -1])

def lam(z, ep):
  sig = disc_sig(z)
  if abs(sig) >= ep:
    return np.sign(sig)
  else:
    return sig / ep

def lam_ss(z, grad_sig_func, ap_func, am_func, ep):
  sig = disc_sig(z)
  if abs(sig) >= ep:
    return np.sign(sig)
  grad_sig = grad_sig_func(z)
  ap = ap_func(_, z)
  am = am_func(_, z)

  val = np.dot(grad_sig, ap + am) / np.dot(grad_sig, am - ap)
  return val


def drift(t, z, ap_func, am_func, covp_func, covm_func, ep):
  ap = ap_func(t, z)
  am = am_func(t, z)
  lam_val = lam(z, ep)
  return (ap + am + lam_val * (ap - am)) * 0.5

def diff(t, z, ap_func, am_func, covp_func, covm_func, ep):
  covp = covp_func(t, z)
  covm = covm_func(t, z)
  lam_val = lam(z, ep)
  diff_mat = 0.5 * np.sqrt(ep) * (covp + covm + lam_val * (covp - covm))
  return diff_mat

