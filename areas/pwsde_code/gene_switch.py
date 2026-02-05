import numpy as np
import matplotlib.pyplot as pp
from pwsde_code.pyfuncs import lambda_func 


g1 = 4.5
g2 = 1.5

th11 = 4
th12 = 8

th21 = 4
th22 = 8

k1 = 40
k2 = 40

fac = 2
be1l = 0.3 * fac 
be1m = 0.6 * fac 
be1h = 1.2 * fac

be2l = 0.3 * fac
be2m = 0.6 * fac
be2h = 1.2 * fac  

xi1 = 1.0
xi2 = 1.0

ep = 0.01

sig11 = lambda x: x[0] - th11
sig12 = lambda x: x[0] - th12

sig21 = lambda x: x[1] - th21
sig22 = lambda x: x[1] - th22

def lambda_func_raw(x, sig_func):
  if sig_func(x) > 0:
    return 1
  elif sig_func(x) < 0:
    return -1
  else:
    print("Error: exactly 0!")
    return 0


def drift(x, lam_func):
  lam11 = lam_func(x, sig11)
  lam12 = lam_func(x, sig12)
  lam21 = lam_func(x, sig21)
  lam22 = lam_func(x, sig22)
  a1 = -g1*x[0]+(k1/4)*(1+lam21)*(1-lam12)
  a2 = -g2*x[1]+(k2/4)*(1+lam11)*(1-lam22)
  return np.array([a1, a2])



def diffu(x, lam_func):
  lam11 = lam_func(x, sig11)
  lam12 = lam_func(x, sig12)
  lam21 = lam_func(x, sig21)
  lam22 = lam_func(x, sig22)
  b1 = (np.sqrt(x[0])+xi1)*(be1h/2*(1-lam21)+be1m/4*(1+lam21)*(1-lam22)+be1l/2*(1+lam22))
  b2 = (np.sqrt(x[1])+xi2)*(be2h/2*(1-lam11)+be2m/4*(1+lam11)*(1-lam12)+be2l/2*(1+lam12))
  return np.array([[b1, 0], [0, b2]])


lam_func = lambda x, sig_func: lambda_func(x, sig_func, ep)
lam_func = lambda x, sig_func: lambda_func_raw(x, sig_func)

drift_ns = lambda t, x: drift(x, lam_func)
diffu_ns = lambda t, x: np.sqrt(ep)*diffu(x, lam_func)


x0 = np.array([4+3*ep, 7])
dt = 0.0005
t_max = 0.25
num_trajs = 500
ns_sol  = euler_maruyama(drift_ns, diffu_ns, x0, 0.0, t_max, dt, trajectories=num_trajs)
skel_sol  = euler_maruyama(drift_ns, lambda t, x: np.zeros((2, 2)), x0, 0.0, t_max, dt, trajectories=1)




fig_phase, ax_phase = pp.subplots(figsize=[9, 9])

for i in range(num_trajs):
  ax_phase.plot(ns_sol[1][i, :, 0], ns_sol[1][i, :, 1], c="tab:blue", alpha=0.3)

ax_phase.plot(skel_sol[1][0, :, 0], skel_sol[1][0, :, 1], c="tab:red")
ax_phase.plot(np.linspace(0, 12, 100), np.ones(100) * 4, c="k")
ax_phase.plot(np.linspace(0, 12, 100), np.ones(100) * 8, c="k")

ax_phase.plot(np.ones(100) * 4, np.linspace(0, 12, 100), c="k")
ax_phase.plot(np.ones(100) * 8, np.linspace(0, 12, 100), c="k")


# ax_phase.set_xlim([4, 8])
# ax_phase.set_ylim([6, 10])

ax_phase.set_xlim([4, 8])
ax_phase.set_ylim([7, 9])


ax_phase.set_title("Non-smooth", y=0.7, fontsize=16)
