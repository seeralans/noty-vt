import numpy as np
import matplotlib.pyplot as pp

def lambda_func(x, sig_func, ep):
  sig = sig_func(x)
  if abs(sig) >= ep:
    return np.sign(sig)
  else:
    return sig / ep

def lam_ss(x, grad_sig_func, ap_func, am_func, ep):
  sig = disc_sig(x)
  if abs(sig) >= ep:
    return np.sign(sig)
  grad_sig = grad_sig_func(x)
  ap = ap_func(_, x)
  am = am_func(_, x)

  val = np.dot(grad_sig, ap + am) / np.dot(grad_sig, am - ap)
  return val

def drift_non_smooth(t, x, ap_func, am_func, bp_func, bm_func, cp_func, cm_func, sig_func, al, ep):
  sig = sig_func(x)
  if sig > 0:
    a = ap_func(t, x)
    c = cp_func(t, x)
  elif sig < 0:
    a = am_func(t, x)
    c = cm_func(t, x)
  else:
    if bool(np.random.randint(0, 2)):
      a = ap_func(t, x)
      c = cp_func(t, x)
    else:
      a = am_func(t, x)
      c = cm_func(t, x)
  return a + al * ep * c


def diff_non_smooth(t, x, ap_func, am_func, bp_func, bm_func, cp_func, cm_func, sig_func, al, ep):
  sig = sig_func(x)
  if sig > 0:
    b = bp_func(t, x)
  elif sig < 0:
    b = bm_func(t, x)
  else:
    if bool(np.random.randint(0, 2)):
      b = bp_func(t, x)
    else:
      b = bm_func(t, x)
  return np.sqrt(ep) * b



def drift_regular_lam(t, x, ap_func, am_func, bp_func, bm_func, cp_func, cm_func, cpm_func, sig_func, al, ep):
  lam = lambda_func(x, sig_func, ep)
  ap = ap_func(t, x)
  am = am_func(t, x)
  cp = cp_func(t, x)
  cm = cm_func(t, x)
  cpm = cpm_func(t, x)

  ae = al * ep

  app =  1/2*(1 + lam) * (ap +  ae * (1 + lam)/2 * cp)
  amm =  1/2*(1 - lam) * (am +  ae * (1 - lam)/2 * cm)
  apm = (1 - lam**2) * ae / 4 * cpm 
  return app + amm + apm

def diff_regular_lam(t, x, ap_func, am_func, bp_func, bm_func, cp_func, cm_func, cpm_func, sig_func, al, ep):
  lam = lambda_func(x, sig_func, ep)
  bp = bp_func(t, x)
  bm = bm_func(t, x)
  return np.sqrt(ep) * (1/2*(1 + lam) * bp + 1/2*(1 - lam) * bm)

