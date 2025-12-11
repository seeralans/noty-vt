#import "@preview/theorion:0.4.1": *
#import "@preview/equate:0.3.2": equate
#import "@preview/gruvy:2.1.0": gruvbox, theme-colors, colors
#import "@preview/note-me:0.6.0": *
#import cosmos.simple: *

// choose your preferred theme color
#let theme-color = theme-colors.dark.hard

// apply colors to common typst components
#show: gruvbox.with(
  // use your preferred theme color as a default preset
  theme-color: theme-color,
  // customize `ref`, `link` and `footnote` colors
  accent: theme-color.strong.blue,
  // customize `highlight` color
  hl: theme-color.muted.yellow,
  // is the document printable?
  print: false,
)


#show: equate.with(breakable: true, sub-numbering: false)
#set math.equation(numbering: "(1.1)")
#set heading(numbering: "1.1")
//#set text(font: "Times new roman")
// #show raw: set text(font: "New Computer Modern Mono")
// #show math.equation: set text(font: "New Computer Modern")
// #show math.equation: set text(font: "Libertinus Serif")

#show: show-theorion
// #set page(height: auto)
#set page(footer: context [
  #h(1fr)
  #counter(page).display(
    // "1/1",
    // both: true,
  )
])

#show ref: it => {
  let bibfile = read("fixlib.bib")
  let entries = bibfile.split("@")
  entries.remove(0) // ""
  let keys = for entry in entries {
    (entry.slice(entry.position("{")+1, entry.position(",")),)
  }

  let el = it.element
  // Check if it's an equation reference
  if el != none and it.element.func() == math.equation {
    [Eq.#link(it.target)[ #numbering(el.numbering,
     ..counter(math.equation).at(el.location())
    )]]
    
  } else if str(it.target) not in keys and query(it.target).len() == 0 {
    "[? " + str(it.target) + "]"
  } else {
    it
  }
}

#let eqs(..labels) = {
  let refs = labels.pos()
  if refs.len() == 1 {
    ref(refs.at(0))  // Uses show rule: "Eq. (1)"
  } else if refs.len() == 2 {
    let el1 = query(refs.at(0)).first()
    let el2 = query(refs.at(1)).first()
    let num1 = numbering(el1.numbering, ..counter(math.equation).at(el1.location()))
    let num2 = numbering(el2.numbering, ..counter(math.equation).at(el2.location()))
    [Eqs. #link(refs.at(0))[#num1] and #link(refs.at(1))[#num2]]
  } else {
    let parts = refs.slice(0, -1).map(r => {
      let el = query(r).first()
      let num = numbering(el.numbering, ..counter(math.equation).at(el.location()))
      link(r)[#num]
    }).join([, ])
    let el_last = query(refs.at(-1)).first()
    let num_last = numbering(el_last.numbering, ..counter(math.equation).at(el_last.location()))
    [Eqs. #parts, and #link(refs.at(-1))[#num_last]]
  }
}

#let tns = math.op(text("T", font: "IBM Plex Sans"))
#let trc = math.op("Tr")
#let dom = math.op("Dom")
#let pm  = math.op($plus.minus$)
#let eqdef  = math.op($eq.def$)
#let ee  = math.op($upright(e)$)
#let epsilon = math.epsilon.alt
#let sign = math.op(text("sign"))
#let ss   = math.op(text("oo"))
#let ss   = math.op($oo$)
#let inprod(f, g, mu) = {
  [$chevron.l #f | #g chevron.r_(#mu)$]
}


#let l2norm(f, mu, p) = {
  [$|| #f ||^(#p)_(#mu)$]
}


#set text(lang: "en")

#title[On stochastic differential equations with piecewise smooth drift and
  noise amplitudes.]

= Introduction

== Physical Motivation

We consider stochastic differential equations arising from the large-$N$ limit of
chemical master equations, which themselves emerge from coarse-graining of Potts
models. The resulting SDEs have two key features:

1. *Piecewise-smooth drift*: The deterministic dynamics has a discontinuity
   across a codimension-one switching manifold

2. *Multiplicative noise*: The noise amplitude depends on the state inherited
   from the underlying CME

Our goal is to compute the Gaussian envelope of fluctuations around deterministic
orbits, particularly near and on the discontinuity surface.

== Literature and Gap

The Freidlin-Wentzell theory of large deviations @freidlinwentzell1998book provides
the foundational framework for weak-noise asymptotics of SDEs with smooth
coefficients. Extensions to piecewise-smooth systems include:

- *Chiang-Sheu* @chiangsheu2000, @chiangsheu2002: Large deviations for
  discontinuous drift with additive noise, using occupation times and local times

- *Chen-Baule-Touchette-Just* @chenetal2013, Mostly asmptotics, and the system
   is very simple it is piecewise constant with additive noise.
  - Useful to chaeck if my results match theirs in the limit.

- *Hill-Zanetell-Gemmer* @hilletal2022: Most probable paths via
  with mollification (smearing the shit out of), additive noise only

The gap: No existing work treats *multiplicative noise* with *discontinuous
diffusion coefficient* and derives the *Gaussian fluctuation structure* near the
switching manifold.

== Main Contributions

1. Derivation of the switching variable dynamics via Meyer-Itô calculus
2. Analysis of the fast Fokker-Planck equation with reflecting boundaries
3. Averaging principle for the slow dynamics
4. Explicit formula for the Gaussian envelope including contributions from
   switching variable fluctuations



== Literature Gap

The Freidlin-Wentzell theory of large deviations @freidlinwentzell1998book deals
with weak noise SDEs with smooth drift and noise amplitude.



== Background <sec-background>

#definition(title: [Piecewise-smooth ODE])[ 

  Let $sigma: bb(R)^d mapsto bb(R)$ be a smooth function, $epsilon>0$, and $x
  in bb(R)^d$ be a deterministic processes satisfying the ODE

  $
    (dif  x) / (dif t)  = cases(
    a^+(t, x) quad sigma(x) > 0,
    a^-(t, x) quad sigma(x) < 0) 
  $
  where $a^(pm): [0, oo) times RR^d mapsto RR^d$ are smooth vector fields. 
]
The switching paramter
$
  lambda  =  cases(
   &1 quad sigma(x) > 0 ,
  -&1 quad sigma(x) < 0 ,
)

$<eq-lam-def>


= Piecewise-Smooth Stochastic Differential Equations

We are interested in treating stochastic systems with weak noise whose behaviour
switches on either side of a discontinuity set. However, away from discontinuity
set we have sufficient smoothness in both the drift field and the noise
amplitude. The following definition formalises this setup.

#definition(title: [Weak-Noise Piecewise-Smooth SDE])[

  Let $T>0$, $t in [0, T]$, $epsilon>0$, $alpha in [0, 1]$, $sigma: bb(R)^d
  mapsto bb(R)$ be a smooth function, and $x_t in bb(R)^d$ be a stochastic
  processes satisfying the SDE

  $
    dif x_t = a(t, x_t) dif t + sqrt(epsilon) b(t, x_t) limits(*)^alpha dif W_t,
  $<eq-gen-sde>

  where 
  $  
    a(t, x) eqdef cases(
    a^+(t, x) quad sigma(x) > 0\,, 
    a^-(t, x) quad sigma(x) < 0\,, 
    )
    quad
    b(t, x) eqdef cases(
    b^+(t, x) quad sigma(x) > 0\,, 
    b^-(t, x) quad sigma(x) < 0\,, 
    )
  $<eq-ab-pw-def>
  are piecewise smooth drift and noise amplitudes respectively satisfying the
  following conditions:

  1. (A1 - Smoothnes)  $a^(pm): [0, T] times RR^d mapsto RR^d ,b^(pm) [0, T] times RR^d mapsto RR^(d times m) in C^2(RR times RR^d)$.

  2. (A2 - Linear Growth) $|a^(pm)(t, x)| + |b^(pm)(t, x)| <= C^pm (1 +
  |x|)$ for some $C>0$, where $|a^(pm)(dot, dot)|$ is the Euclidean norm and
  $
    |b^(pm) (dot, dot)| = sqrt(sum_(i j) |b_(i j) (dot, dot)|).
  $

  3. (A3 - Lipshitz Continuity)

  $
    |a^(pm)(t, x) - a^(pm)(t, y)| + |b^(pm)(t, x) - b^(pm)(t, y)| <= K^pm |x - y|,


  $

  for some $K^pm >0$. The $alpha$ is used to control evaluation point of the
  stochastic integral.
]<def-ns-gen-sde>

The conditions (A1-3) ensure that away from the discontinuity set, that is
$cal(D) = {x in RR^d | sigma(x) = 0}$, we have the existance and uniqueness of
solutions. In other words away from $cal(D)$, one can employ standard methods of
SDE theory to analyse the dynamics, while near the discontinuity one can The
stochastic integral in @eq-gen-sde is understood in the α–sense, i.e. with
evaluation point $(1 - alpha) x_t + alpha x_(t+ Delta t)$. While the recasting
of typical $alpha$-SDE into an Ito form is straight forward, see for example,
@oksendal2013book or @gardiner2009book, one cannot naively follow the procedure
here as the noise amplitude does not have a continuous derivative.

Instead, we must fisrt employ Filippov's convex construction @filippov2013book
for the drift and noise amplitude, with $lambda in [-1, 1]$ we define the convex
combinations
$
  a(t, x, lambda) eqdef 1/2(1 + lambda)a^+(t, x) + 1/2(1 - lambda)a^-(t, x),
$<eq-a-def>
and
$
  b(t, x, lambda)  eqdef 1/2(1 + lambda)b^+(t, x) + 1/2(1 - lambda)b^-(t, x),
$<eq-b-def>

which are smooth in $lambda$, as well as $x$ and $t$ as they inherit the
smoothness conditions given in @def-ns-gen-sde. The switching variable obviously
depends on the state and we will regularise the definition given in @eq-lam-def
as
$
  lambda = Lambda_(epsilon)[sigma(x)]
$

where 
$
  Lambda_epsilon (u) eqdef cases(
  u \/ epsilon  quad   &sigma(x) & <= epsilon,
  sign[sigma(x)]  quad &sigma(x) & > epsilon,
)
$<eq-big-lam-def>

is an auxiliary function used to control the regularisation. Notice that the regularisation
implicitly defines the layer

$
  cal(D)_(epsilon) eqdef {x in RR^d | |sigma(x)| <= epsilon},
$

and which affords a precise meaning to the term dynamics near the discontinuity,
i.e. when $x_t in cal(D)$. These definitions allow as to recast @eq-gen-sde into
the Ito analgoue

$
  dif x_t = [a(t, x_t, lambda_t) + alpha epsilon b(t, x_t, lambda_t)] dif t + sqrt(epsilon) b(t, x_t, lambda_t) dif W_t,
$<eq-ito-sde>

where the correction term is

$
  c(t, x, lambda)  = sum_j J_x [b_j (t, x, lambda)] b_j (t, x, lambda).
$ <eq-ito-al-cor-term>

with $b_j (t, x, lambda)$ denoting the $j^#text("th")$ column of the matrix
$b(t, x, lambda)$ and $J_x (dot)$ is the Jacobian matrix of the vector argument
with respect to $x$. Obiously $lambda$ is itself a stochastic variable since it
dependends on $x_t$ via $lambda_t = Lambda_(epsilon)[sigma(x_t)]$, and, like its
deterministic counterpart is dynamic on the the much faster timescale
$cal(O)(1\/epsilon)$. However, one cannot simply employ Ito's Lemma on $lambda_t
= Lambda_(epsilon)[sigma(x_t)]$ as the latter is not a smooth function of $x_t$.
Instead to study the dynamics of $lambda_t$ we must first introduce two new
concepts: local time of a semi-martingale and the Meyer-Ito Theorem.

Local time of a semi-martingale $x_t$, denoted with $L^x_t (z)$is a measure of
the "visits" of the proccess on a given value $z$ for times up to $t$. It is
given via Tanaka's formula which we summarise in the folowing defintion.

#definition(title: [Local time of a semi-martingale: Tanaka's Formula])[ 

  Let $x_t$ be a semi-martingale in $RR^d$, and let $L^(x)_t(z)$ be the local
  time of the process at level

  $
    |x_t - z| = |x_0 - z| + integral_0^t sign(x_t - a) dif x_s + L^(x)_t (z)
  $<eq-tanaka>
  where

  $
    sign(x) = cases(&1 quad &x & > 0\,, -&1 quad &x & <= 0.)
  $
]
// #proof(title: [Proof Sketch])[
// 
//   Start with a smooth approximation $psi_n$ for $|x - u|$, apply Ito's lemma and
//   one would find that
// 
//   $
//     L^x_t (z) =  integral_0^t delta(x_t - a) dif<X, X> 
//   $
// 
// ]

For derivation and discussion see See Chap. 3 of @karatzasshreve2014book, and
Chap. IV of @protter2012book. Secondly we require the Meyer-Ito theorem, also
called the genearlised Ito's formula. We restate it here without the proof which
can be found in Theorem 70, Chapter IV of @protter2012book.

#theorem(title: [Meyer-Ito])[ 

  Let $f: RR^d mapsto RR$ be the difference of two convex functions, $f^('-)$
  denote its left derivative, $mu_(f'')$ be signed measure of the second
  derivative of $f$ in the generalised function (distribution) sense, and let
  $x_t$ be a semi-martingale in $RR^d$ then evolution of $f(x_t)$ is given by 
  $
    f(x_t) = f(x_0) + integral_0^t f^('-)(x_t) dif x_t + 1/2 integral_(RR^d) L^(x)_t (z) dif mu_(f'')(z)
  $<eq-ito-meyer>

  where $L^x_t (z)$ is the local time of $x_t$ at $z$ and the final integral in
  @eq-ito-meyer is a Lebesque-Stieltjes integral.
]<thm-ito-meyer>

For $f in C^2 [RR^D, RR]$, @thm-ito-meyer redues to Ito's Lemma, 




#todo[
  - add definition for
  $ L_t^x (z)  = lim_(delta arrow.b 0) 1/(2delta) integral_RR bb(1)_((z - delta, z + delta))(z_s) dif chevron.l x_t chevron.r $<eq-local-time-def>
 - convex functions of martingales are themselves martingales
 -  generalises itos theorem
   - add example about how it redues to the standard itos lemma when $f$ has a second derivative
]


In order to apply @thm-ito-meyer, we must first write the our regulariser
function as a difference of convex function which is easily done as the following lemma shows.
#lemma(title: [The regulariser $Lambda_epsilon (u)$ is DC])[

  The function  $Lambda_epsilon (u)$ as defined in @eq-big-lam-def, can written as

  $
    Lambda_epsilon (u)  = phi_(epsilon)^+ (u) - phi_(epsilon)^- (u)
  $
  where $phi^(pm)(u)$ are convex functions. 
]<lem-big-lam-dif-conv>

#proof[

  Let 
  $
    psi_(+, epsilon)(u) eqdef cases(-1 quad &u &<= -epsilon\,, u \/ epsilon   quad  &u &> -epsilon\,)
    quad quad
    psi_(-, epsilon)(u) eqdef cases(0  quad &u &<= epsilon\,, u\/ epsilon - 1 quad &u &> epsilon,),
  $
  which are convex in $u$.
]

Notice that the functions $psi^pm$ chosen are left continuous, i.e. $lim_(u
arrow.b a)psi_(pm, epsilon) = psi_(pm, epsilon) (a)$, for all $a in RR$. This is intentional as we
must require the left derivative of $Lambda_epsilon(u)$ given by  
$
  Lambda^('-)_epsilon(u) &=  psi^('-)_(+, epsilon)(u ) - psi^('-)_(-, epsilon)(u )  = cases(
  0 quad  &u<= -epsilon\, ,
  1\/epsilon quad  &epsilon < u <= epsilon\, ,
  0 quad  &u  > epsilon\.
)
$<eq-big-lam-left-deriv>

Similarly we have second derivative as a signed measure
$
  mu_(Lambda_epsilon^'')(u) = 1/epsilon delta(u + epsilon) - 1/epsilon delta(u - epsilon),
$<eq-big-lam-sec-deriv>

where $delta(u)$ is the Dirac-delta distribution. In order to apply Meyer-Ito we
also need the dynamics of scalar observable $sigma(x_t)$ which we state in the following lemma.
#lemma(title: [SDE for $z_t = sigma(x_t)$])[

  Let $lambda in [-1, 1]$, $sigma in C^2(RR^d, RR)$, $x_t$ be an Ito process
  according to @eq-ito-sde, supplemented by the conditions in @def-ns-gen-sde,
  then the random variable $z_t = sigma(x_t)$ evolves according to the SDE

  $
    dif z_t = tilde(a)(t, x_t, lambda_t) dif t + sqrt(epsilon)tilde(b)(t, x_t, lambda_t) dif W_t,
  $<eq-z-sde>
  where
  $
    tilde(a)(t, x, lambda) eqdef partial_x sigma(x) dot a(t, x, lambda)
    + epsilon/2  trc[b(t, x, lambda) partial^2_(x x) sigma(x) b(t, x, lambda)],
  $<eq-a-tilde-def>
  and 
  $
    tilde(b)(t, x, lambda) eqdef partial_x sigma(x)^(tns) b(t, x, lambda)],
  $<eq-b-tilde-def>
]<lem-z-sde>
#proof[

  This is trivial application of Ito's lemma. Since $sigma(x_t)$ is smooth, apply Ito's lemma to obtain  
  $
    dif z_t = partial_x sigma(x_t)^(tns)  dif x_t + dif x_t^(tns)  partial^2_(x x) sigma(x) dif x^tns, 
  $<eq-dz-ito-lemma>

  then substritute for $dif x_t$ from @eq-ito-sde into @eq-dz-ito-lemma and
  apply Ito's product rule.

]


We are now in a position to consider the dynamics of the switching variable as
an SDE.

#theorem(title: [SDE for the switching variable $lambda$])[ 

  Let $epsilon>0$, $sigma in C^2[RR^d, RR]$ such that $cal(D)_(epsilon) = {x in
  RR^d | sigma(x) <= epsilon}$ is close set, let $x_t in cal(D)_epsilon$ evolve
  according to according to @eq-ito-sde, and let $Lambda_epsilon (u)$ be a be a
  family of regularisers of the sign function as dfined in @eq-big-lam-def. then
  the switching variable $lambda_t = Lambda_epsilon [sigma(x_t)]$ evolves in the
  interval interval [-1, 1] according to the SDE
  $
    dif lambda_t &= 1/epsilon bb(1)_((-epsilon, epsilon])[sigma(x_t)] tilde(a)(t, x_t, lambda_t) dif t
    + 1/sqrt(epsilon) bb(1)_((-epsilon, epsilon])[sigma(x_t)] tilde(b)(t, x_t, lambda_t) dif W_t \
      &+ 1/epsilon [dif L_t^(z)(-epsilon) - dif L_t^(z)(epsilon)]
    , \
  $<eq-lam-sde>

  where $tilde(a)(t, x, lambda)$ and $tilde(b)(t, x, lambda)$ are defined in
  @eq-a-tilde-def and @eq-b-tilde-def respectively, $dif L_t^(z)(pm epsilon)$ is the
  change in the local time of $z_t$ at $z = pm epsilon$ where the evolution of
  $z_t$ is given by @eq-z-sde. 

]

#proof[

  Since $Lambda_epsilon (u)$ is a difference of convex functions, whose left
derivative is given in @eq-big-lam-left-deriv and signed second derivative given
as a measure given in @eq-big-lam-sec-deriv, it then follows from
@thm-ito-meyer, that for a generic random variable $z_t$ we have
  $
    Lambda_(epsilon)(z_t) = Lambda_(epsilon)(z_0)
    + 1/epsilon integral_0^t bb(1)_((-epsilon, epsilon])(z_t)  dif z_t
    +   epsilon/2 [L^z_(t)(-epsilon) - L^z_(t)(epsilon)].
  $
  By letting $lambda_t = Lambda_epsilon (z_t = sigma(x_t))$, and using  @lem-z-sde we obtain 
  $
    lambda_t &= lambda_0 + integral_0^t 1/epsilon bb(1)_((-epsilon, epsilon])[sigma(x_s)] tilde(a)(s, x_s, lambda_s) dif t
    + 1/sqrt(epsilon) integral_0^t bb(1)_((-epsilon, epsilon])[sigma(x)] tilde(b)(s, x_s, lambda_s) dif W_s \
      &+ 1/epsilon [ L_t^(z)(-epsilon) -  L_t^(z)(epsilon)].
$<eq-lam-sde-meyer-ito-full>
]

The dynamics of the full system are then represented by the coupled SDE 

$
  dif x_t &= [a(t, x_t, lambda_t)
  + alpha epsilon b(t, x_t, lambda_t)] dif t
  + sqrt(epsilon) b(t, x_t, lambda_t) dif W_t, \
  dif lambda_t &= 1/epsilon bb(1)_((-epsilon, epsilon])[sigma(x_t)] tilde(a)(t, x_t, lambda_t) dif t
  + 1/sqrt(epsilon) bb(1)_((-epsilon, epsilon])[sigma(x_t)] tilde(b)(t, x_t, lambda_t) dif W_t \
    &+ 1/epsilon [dif L_t^(z)(-epsilon) - dif L_t^(z)(epsilon)]
    , 
$<eq-x-lam-sde-pair>

where $a(t, x, lambda)$ and $b(t, x, lambda)$ are defined, respectively, in
@eq-a-def and @eq-b-def, while $tilde(a)(t, x, lambda)$ and $tilde(b)(t, x,
lambda)$ are given defined in @eq-a-tilde-def and @eq-b-tilde-def respectively.
The coupled system is a slow-fast stochastic system, and our goal is to obtain
controlled approximation for the dynamics of the slow process by closing the
dynamics of the switching variable $lambda_t$.


= The intermediate timescale

== Necessity of an intermediate timescale. 

From @eq-lam-sde, it is evident that the switching variable $lambda_t$ evolves
on a faster timescale compared to the state variable $x_t$ when near the
discontinuity set. As we have discussed in @sec-background, in the deterministic
setting, the standard approach is to rescale time via $t = epsilon tau$, take
the limit $epsilon -> 0$, and solve the resulting algebraic condition to obtain
a $lambda^* in (-1, 1)$ which gives us our sliding mode. It is tempting to
follow the same procedure here, where instead of a single value for the
switching variable, we obtain the steady-state distribution $lim_(t ->
oo)P_(ss)(lambda, t, | x)$. However we shall see the stochastic nature
of the problem yeilds multiple objections concerning the mathematical subtleties
in the scaling, the physical interpretation, and the analysis of the original
problem given in @def-ns-gen-sde, that must be addressed individually.

=== Objection I: Incompatible scaling of the dynamics.

Before we attempt to rescale time must first clarify the $epsilon$-order of
local time terms in @eq-lam-sde which we do in the following lemma. 

#lemma(title: [Scaling of local time terms])[

  Let $x_t in RR^d$ be a stochastic process given the SDE
  $
    dif x_t =  a(t, x_t) dif t + b(t, x_t) dif W_t,
  $

  $
    EE[L^x_t (a)] <= C t
  $
  
]<lem-local-time-scaling>
#todo[
  Finish the lemma!!
]

Taking @eq-local-time-def and taking the
expectation of both sides we obtain

$
  EE[L^z_t (a)] &= EE[ lim_(delta arrow.b 0) 1/(2delta) integral_0^t bb(1)_((a - delta, a + delta)) (z_s) dif chevron.l  z chevron.r_s], \
    &= lim_(delta arrow.b 0) 1/(2delta) integral_0^t EE[bb(1)_((a - delta, a + delta))   dif chevron.l  z chevron.r_s] \ 
    &= lim_(delta arrow.b 0) 1/(2delta)  integral_0^t epsilon EE[bb(1)_((a - delta, a + delta))tilde(b)^2 (s, x_s, lambda_s) ]  dif s, \ 
    &=   epsilon  integral_0^t lim_(delta arrow.b 0) 1/(2delta)
    integral_(a - delta)^(a + delta) P^((z))(a, s) tilde(b)^2 (s, x_s,  lambda_s) dif s, \
    &=  epsilon integral_0^t P^((z))(a, s) tilde(b)^2 (s, x_s,  lambda_s) dif s,  #<eq-local-time-exp-z-int>\
$

or equivalently in differential form

$
  dif EE[L^z_t (a)]  = epsilon P^((z))(a, t) tilde(b)^2(t, x_t, lambda_t)   dif  t.
$<eq-diff-ee-lt>

From @eq-diff-ee-lt we conclude that $dif L^z_t(a) ~ epsilon P^((z))(a, t) tilde(b)^2(t,
x_t, lambda_t) dif t$. Now let us consider the rescaling $t = epsilon^beta tau$
for some $beta > 0$, then we have $dif t = epsilon^beta dif tau$ and $dif W_t =
epsilon^(beta \/2) dif W_tau$. Applying this to @eq-lam-sde obtain  

$
  O(epsilon^(beta - 1)) &: bb(1)_((-epsilon, epsilon])[sigma(x_(tau))]
  tilde(a)(tau, x_tau, lambda_tau) dif tau, \
  O(epsilon^((beta - 1)/2)) &: bb(1)_((-epsilon, epsilon])[sigma(x_(tau))]
  tilde(b)(tau, x_tau, lambda_tau) dif W_tau, \
  O(epsilon^beta) &:  dif L_(tau)^(z)(a).  \
$

The naive rescaling from the determinisitic dynamics would be to have $beta = 1$
which would put drift and martingale terms on $cal(O)(1)$, but the local time
contribution would be $cal(O)(epsilon)$ and vanishes in the limit. Clearly there
is no $beta$ that would bring all of these terms together on equal fotting such
that under $epsilon -> 0$ all of the features of the dynamics are maintained:
balancing drift and noise suppresses the local time; preserving the local time
would require a slower timescale on which drift or noise diverges. This presents
a fundemental technical hurdle. We have three contributions to the dynamics of
$lambda$ that operate on on incompatible scales. Any rescaling followed by
$epsilon->0$ necessarily discards at least one of these contributions.

=== Objection II: Loss of physical interpretation.

Even granting mathematical well-posedness, the $epsilon -> 0$ limit produces an
object whose physical meaning has degenerated. As $epsilon -> 0$:

+ The layer $cal(D)_epsilon = {x in RR^d : |sigma(x)| <= epsilon}$ shrinks to
  the codimension-1 surface $cal(D) = {x : sigma(x) = 0}$.

+ The switching variable $lambda in [-1, 1]$ parametrises a convex interpolation
  between the vector fields $a^pm$ and noise amplitudes $b^pm$. This
  interpolation only has meaning within the layer, where the dynamics
  transitions between the two regimes.

+ The stationary distribution $P_(ss)^epsilon (lambda | x)$ converges
  to some limiting distribution on $[-1, 1]$, but this limit lives on a domain
  whose connection to the original geometry has been severed.

In the deterministic case, the $epsilon -> 0$ limit yields a single value
$lambda^* (x)$ i.e. the Filippov sliding mode (see @sec-background). In that
case, interpretation is clear, $lambda^*$ selects the unique convex combination
that keeps trajectories on the discontinuity surface.

In the stochastic scenario,, even in the limit $epsilon -> 0$, one retains a
distribution over $lambda$ rather than a single value. But what is this
distribution the distribution _of_, when the layer on which $lambda$ was defined
has vanished? The switching variable was introduced to parametrise dynamics
within $cal(D)_epsilon$; without the layer, $lambda$ becomes an abstract
coordinate detached from the underlying phase space.

==== Objection III: Incompatibility with weak-noise analysis.

The most fundamental objection concerns the purpose of the analysis. The
weak-noise framework treats $epsilon$ as the small parameter governing the
asymptotic expansion. The objects of interest are beyon the typcal paths,
obtained by minimising the Freidlin-Wentzell action functional or analgously the
deterministic limit $epsilon -> 0$, instead we are interested in charertising
the Gaussian fluctuations around the most probable path, arising at order
$cal(O)(sqrt(epsilon))$. These phenomena are intrinsically $epsilon$-dependent.
The noise is not a nuisance to be eliminated but rather the object of study. The
stationary distribution $P_(ss)(lambda | x)$ at finite $epsilon$
encodes how noise selects among the continuum of Filippov solutions, how it
smooths the transition across $cal(D)$, and what the fluctuation structure looks
like near the discontinuity.

Taking $epsilon -> 0$ in the fast dynamics collapses this structure to a
deterministic Filippov vector field. Thus it eliminates the noise-induced
selection mechanism among sliding vectors returns us to the deterministic theory
discardign the stochastic phenomena and precluding the study of fluctaions which
we set out to analyse in the first place.

The weak-noise SDE is already the result of retaining terms to second order in
the noise amplitude. Consistency demands that $epsilon$ be preserved throughout
the analysis, including in the treatment of the fast variable.


=== The intermediate timescale resolution.
The intermediate timescale $delta$ satisfying

$
  epsilon << delta << 1
$<eq-delta-ordering>

resolves all three by avoiding the $epsilon -> 0$ limit in the layer dynamics
entirely. At fixed $epsilon>0$:

- *Well-defined quantities*: The layer $cal(D)_epsilon$ has finite width, the
  boundaries $pm epsilon$ are well-separated, and all probabilistic quantities
  --- including local times and their contributions to the stationary
  distribution --- are well-defined without singular limits.

- *Preserved interpretation*: The switching variable $lambda$ retains its
  meaning as parametrising the interpolation within a layer of finite width. The
  stationary distribution $P_(ss)(lambda | x)$ describes the
  equilibrium of $lambda$ within this layer, with clear geometric content.

- *Retained noise structure*: The parameter $epsilon$ appears throughout the
  effective slow dynamics, preserving the weak-noise structure required for
  Freidlin-Wentzell analysis.

The conditions on $delta$ ensure:

- $delta >> epsilon$: The fast variable $lambda$ equilibrates to
  $P_(ss)(lambda | x)$ within the $delta$-window (see @lem-exp-mixing).

- $delta << 1$: The slow variable $x$ remains approximately frozen over the
  $delta$-window (see @lem-slow-var-x).

The effective slow dynamics follows by averaging against the stationary
distribution:

$
  macron(a)(t, x) = integral_(-1)^1 a(t, x, lambda) P_(ss)(lambda | x) dif lambda,
  quad
  macron(b)(t, x) = integral_(-1)^1 b(t, x, lambda) P_(ss)(lambda | x) dif lambda.
$<eq-averaged-coeffs>

The resulting equation,

$
  dif x_t = macron(a)(t, x_t) dif t + sqrt(epsilon) macron(b)(t, x_t) dif W_t,
$<eq-averaged-sde>

remains a weak-noise SDE with $epsilon$-dependent coefficients, to which
standard Freidlin-Wentzell theory applies. This is the stochastic analogue of
Filippov's construction: where the deterministic theory yields a unique sliding
vector field via an algebraic condition, the stochastic theory yields an
averaged vector field weighted by the stationary distribution of the fast
variable.

#remark[
  The choice $delta = epsilon^alpha$ for some $alpha in (0, 1)$ provides a
  concrete realisation of the ordering @eq-delta-ordering. The value of $alpha$
  does not affect the limiting averaged dynamics, provided the bounds in
  @lem-slow-var-x and @lem-exp-mixing hold uniformly.
]

= Estimates for the dynamics on the intermediate time-scale  

We want to study the stochastic dynamics for a small but non zero $epsilon$, so
we will introduce an intermediate timescale $delta$ suchh that

$
  epsilon << delta << 1.
$<eq-delta-scale>

On this time scale, the dynamics of $x_t$ is frozen but the dynamics of
$lambda_t$ has equilibriated on to a steady state distribution. Typically one
defines $delta$ as a function of $epsilon$, e.g. $delta(epsilon) =
epsilon^alpha$, for some $alpha in (0, 1)$.


#lemma(title: [Slow variation of $x_t$  in the $delta$-window])[

  Let $x_t in cal(D)_epsilon$ and $delta>0$ satisfying @eq-delta-scale, then  
  $
    // EE[ sup_(0<=s<=delta) |x_(t+s) - x_t|^2 ] <= C (delta^2  + epsilon delta), 
    PP[sup_(0<=s<=delta) |x_(t+s) - x_t| > gamma ] <= C/gamma^2 (delta^2  + epsilon delta)
        
  $<eq-slow-var-x>
  for some $C,gamma>0$.
]<lem-slow-var-x>

#proof(title: [Proof of @lem-slow-var-x])[

  We start by bounding the squared deviation in the $delta$ time window,
  $
    EE[ |x_(t+s) - x_t|^2]
      &= EE[ lr(|integral_t^(t+s) a(tau, x_tau, lambda_(tau)) dif tau 
      + sqrt(epsilon) integral_t^(t+s) b(tau, x_tau, lambda_tau) dif W_(tau) |)^2],  \
      &<= 2EE[ lr(|integral_t^(t+s) a(tau, x_tau, lambda_tau) dif tau|) ^2 ]
      + 2 epsilon EE[ lr(|integral_t^(t+s) b(tau, x_tau, lambda_s) dif W_tau |)^2]. \
  $

  We will bound each integral term separately, for the drift part we have
  $
    EE[lr(|integral_t^(t+s) a(tau, x_tau, lambda_tau) dif tau|) ^2]
      &<= s integral_t^(t+s)  EE[lr(|a(tau, x_tau, lambda_tau)  |) ^2] dif tau, \
      &<= s integral_t^(t+s)  C (1 + EE[ |x_tau|^2]) dif tau, \
      &<= C' s^2,
  $
  and for the martingale part we have 
  $
    EE[ lr(|integral_t^(t+s) b(s, x_s, lambda_s) dif W_s |)^2] 
      &<=  integral_t^(t+s)EE[ lr(||b(s, x_s, lambda_s) ||)^2]dif s   \
      &<=  integral_t^(t+s) C (1 + EE[ |x_s|^2])dif s   \
      &<= C'' s.
  $
  Putting both bounds together we obtain @eq-slow-var-x, we obtain
  $
    EE[ |x_(t+s) - x_t|^2] &<= C(s^2 + epsilon s),
  $
  and taking the supremum over the interval we find
  $
    EE[ sup_(0<=s<=delta)|x_(t+s) - x_t|^2] &<= sup_(0<=s<=delta)C(s^2 + epsilon s)
    <= C(delta^2 + epsilon delta),
  $<eq-expec-ineq>

  Using Markov's inequality on @eq-expec-ineq yeilds
  $
    PP[sup_(0<=s<=delta)|x_(t+s) - x_t|^2 > gamma^2] <=1/gamma^2 EE[ sup_(0<=s<=delta)|x_(t+s) - x_t|^2] 
    <= 1/gamma^2 C(delta^2 + epsilon delta).
  $
]

The consequence of @lem-slow-var-x becomes more apparent when we choose any
mesoscopic scale $delta(epsilon)$ satisfying @eq-delta-scale, e.g.
$delta(epsilon) = epsilon^beta$, for some $beta>0$ and then letting $epsilon ->
0$. The bound in @eq-slow-var-x ensures that for any fixed $gamma$, 

$
  PP[sup_(0<=s<=delta(epsilon))|x_(t+s) - x_t| > gamma]
 <= C/gamma^2 (delta^2(epsilon) + epsilon delta(epsilon))


  -> 0,
$

and therefore the slow variable $x_t$, with probability tending to one, remains
constant on the entire interval $[t, t + delta(epsilon)]$. Simultaneously, we have

$
  delta(epsilon)/epsilon  = epsilon^(beta-1) limits(->) oo, quad #text[as] epsilon -> 0,
$

which shows that the $delta$-window is arbitrarily large on the fast
$lambda$–timescale. Thus, in $delta$-interval, the slow variable may be regarded as
fixed while the fast variable has sufficient time to equilibrate.


We now fix $x in cal(D)_epsilon$ and consider the dynamics of the $lambda$ on
the interval $[t, t + delta]$.

#lemma(title: [Backward generator of the switching variable])[

  Let $delta > 0$ satisfying @eq-delta-scale, let $t in [t', t' + delta] subset
  [0, T]$ for some $t' in [0, T-delta]$. Let $x_t = x in cal(D)_epsilon$ be
  fixed (see @lem-slow-var-x). Then the backward generator $cal(A)_x$ of
  $lambda_t in [-1, 1]$ evolving according to @eq-lam-sde, acts on sufficently
  smooth test function $f: [-1, -1] mapsto RR$ via

  $
    (cal(A)_x f)(lambda) &= 1/epsilon partial_lambda
    f(lambda){partial_x sigma(x_t)^tns a(t, x, lambda)
    + epsilon/2 trc[b(t, x, lambda)^(tns) partial^2_(x x) sigma(x) b(t, x, lambda)] } \ 
      &+ 1/(2 epsilon) partial^2_(lambda lambda)f(lambda)
      partial_x sigma(x_t)^tns  b(t, x, lambda) b(t, x, lambda)^tns partial_x sigma(x_t),
  $<eq-bwd-gen>

  with the domain

  $ 
  dom(cal(A)_x) = {f in C^2([-1, 1]) | partial_(lambda)(1) =
  partial_(lambda)(-1) = 0}.
  $
  Note that the generator is conditional on $x$.

]<lem-bwd-gen>


#proof[

  With $x_t = x$ fixed on the interval $t in [t' , t' + delta] subset [0, T]$
  for some $t' in [0, T]$ and $delta>0$, (see @lem-slow-var-x). Let $f in C^2([-1, 1])$ and set an initial conditoin
  $lambda_(t') = lambda in [-1, 1]$. Applying Ito's lemma to $f(lambda_t)$ to
  yield

  $
    f(lambda_t) - f(lambda)
    = integral_(t')^t partial_lambda f(lambda_s) dif lambda_s
    + 1/2 integral_(t')^t partial^2_(lambda lambda) f(lambda_s)
          dif chevron.l lambda chevron.r_s,
  $<eq-gen-ito-start>
  
  where $chevron.l lambda chevron.r$ is the quadratic variation of $lambda_t$.
  Substituting @eq-lam-sde into first the first integral @eq-gen-ito-start we
  obtain the decomposition

  $
    integral_(t')^t partial_lambda f(lambda_s) dif lambda_s = I^((1))_t + I^((2))_t + M_t , 
  $

  where

  $
    I^((1))_t
      &eqdef 1/epsilon integral_0^t partial_lambda f(lambda_s)
        tilde(a)(s, x, lambda_s) dif s, \
    I^((2))_t
      &eqdef 1/epsilon integral_0^t partial_lambda f(lambda_s)
        [dif L_s^(z)(-epsilon) - dif L_s^(z)(epsilon)], \
  $

  and $M_t$ is the martingale term arising from the stochastic integral with
  respect to $W_t$. For the quadratic variation, since we know from
  @lem-local-time-scaling that $dif L^(z)_t (pm epsilon) = O(dif t)$, the second
  integral in @eq-gen-ito-start becomes

  $
    1/2 integral_(t')^t partial^2_(lambda lambda) f(lambda_s)
    dif chevron.l lambda chevron.r_s = I^((3))_t eqdef 
      = 1/(2 epsilon) integral_0^t partial^2_(lambda lambda) f(lambda_s)
        tilde(b)(s, x, lambda_s) tilde(b)(s, x, lambda_s)^(tns) dif s, \
  $

  Taking expectations, the martingale term vanishes leaving

  $
    EE[f(lambda_t) - f(lambda)]
      = EE[I^((1))_t] + EE[I^((2))_t] + EE[I^((3))_t]. 
  $<eq-gen-decomp-expect>

  *Interior terms.* The terms $I^((1))_t$ and $I^((3))_t$ are the drift and
  diffusion on the interior of the [-1, 1], i.e. for $lambda in (-1, 1)$. Since
  the coefficients are continious in $t$, we have

  $
    lim_(t -> t') 1/t EE[I^((1))_t]
      = 1/epsilon partial_lambda f(lambda)
        tilde(a)(t', x, lambda), \
        lim_(t -> t') 1/t EE[II^((3))_t]
      = 1/(2 epsilon) partial^2_(lambda lambda) f(lambda)
        tilde(b)(t', x, lambda) tilde(b)(t', x, lambda)^(tns),
  $

  hence, the interior contribution to the generator is  the right-hand side
  of @eq-bwd-gen when we express $tilde(a)$ and $tilde(b)$ in terms of $a$ and
  $b$ using @eq-a-tilde-def and @eq-b-tilde-def.

  *Local time term.* For the local time contribution $I^((2))_t$, we know that
  when $z_t = pm epsilon$, we have $lambda_t = pm 1$, thus $partial_lambda
  f(lambda_s) = partial_lambda f(pm 1)$ and

  $
    EE[I^((2))_t] = 1/epsilon integral_0^t EE[partial_lambda f(-1) dif L^(z)_s (-epsilon) - partial_lambda f(1) dif L^(z)_s (epsilon)].
  $

  Using also @eq-diff-ee-lt (see @lem-local-time-scaling), we have 

  $
    dif EE[partial_lambda f(pm 1) L_t^(z)(pm epsilon)] &= partial_lambda f(pm 1) dif EE[ L_t^(z)(pm epsilon)], \
      &= epsilon partial_lambda f(pm 1) P^((z))(pm epsilon, t)
      tilde(b)(t, x, lambda_t) tilde(b)(t, x, lambda_t)^(tns) dif t,
  $
  from which we conclude
  $
    lim_(t -> t') 1/t EE[I^((2))_t] = epsilon C_(pm)(x, t') partial_lambda f(pm 1), 
  $

  where $|C_(pm)(x, t')| < oo$ are smooth $x, t'$ dependent coefficients.
  combining all of the expectation of the integrals together gives

  Combining all of the intergral expecations with @eq-gen-decomp-expect and
  dividing by $t$ gives

  $
    lim_(t -> t') 1/t EE[f(lambda_t) - f(lambda)]
      = (cal(A)_x f)(lambda)
        + C_+(x, t') partial_lambda f(1)
        - C_-(x, t') partial_lambda f(-1),
  $<eq-gen-limit-with-bdry>
  where $cal(A)_x$ is the interior differential operator defined in
  @eq-bwd-gen. By definition of the generator of a Markov process, and equivalently by
  Dynkin's formula, the infinitesimal generator must satisfy
  $
    lim_(t arrow.b 0) 1/t EE[f(lambda_t) - f(lambda)]
      = (cal(A)_x f)(lambda)
  $
  without any additional boundary terms. In view of
  @eq-gen-limit-with-bdry, this is possible if and only if
  $
    C^+(x, t') partial_lambda f(1)
    - C^-(x, t') partial_lambda f(-1) = 0,
  $

  Since $C^pm (x, t')$ are non-zero for $x in cal(D)_epsilon$ due to the
  accumulation of local time at the boundaries, the only way this can hold for
  all $x in cal(D)_(epsilon)$ is to impose the Neumann boundary conditions
  $
    partial_lambda f(1) = partial_lambda f(-1) = 0.
  $

  This characterises precisely the domain of $cal(A)_x$ claimed in the lemma.
]

From the backward generator it is then possible to obtain the forward generator
using the $L^2$-adjoint relation

$
  integral_-^1 P_t (lambda | x) (cal(A)_x f_t ) (lambda) dif lambda = integral_-^1 (A^(*)_x P_t)(lambda |x )  f_t (lambda) dif lambda , quad forall  f_t in dom(cal(A)_x)
$<eq-adjoint-def>

where $t = [t', t + delta] subset [0, T]$, and $P_t (lambda | x)$ is the
occupation probability density of the switching variable $lambda$. Going
forwards we will drop the $x$ notation in favour of $P(lambda, t)$. The forward
generator is sumarised in the following lemma.

#lemma(title: [Forward generator of the switching variable])[

  Let $delta > 0$ satisfying @eq-delta-scale, let $t in [t', t' + delta] subset
  [0, T]$ for some $t' in [0, T-delta]$. Let $x_t = x in cal(D)_epsilon$ be
  fixed (see @lem-slow-var-x). Then the forward generator $cal(A)^*_x$ of
  $lambda_t in [-1, 1]$ evolving according to @eq-lam-sde, acts on sufficently
  smooth probability density $P_t: [-1, -1] mapsto [0,  
 oo)$ via
  $
    (cal(A)^*_x f)(lambda) &= - 1/epsilon partial_lambda  ( 
    P_t (lambda) { partial_x sigma(x_t)^tns a(t, x, lambda)
    + epsilon/2 trc[b(t, x, lambda)^(tns) partial^2_(x x) sigma(x) b(t, x, lambda)] }) \ 
      &+ 1/(2 epsilon) partial^2_(lambda lambda)[P_t (lambda)
      partial_x sigma(x_t)^tns  b(t, x, lambda) b(t, x, lambda)^tns partial_x sigma(x_t)],
  $<eq-fwd-gen>
  with the domain 

  $
    dom(cal(A)_x^*) = {P_t in C^2([-1, 1]; [0, oo)) | J_t (pm 1) = 0},
  $<eq-dom-fwd-gen>
  where

  $
    J_t (lambda) &= 
P_t (lambda) { partial_x sigma(x_t)^tns a(t, x, lambda)
    + epsilon/2 trc[b(t, x, lambda)^(tns) partial^2_(x x) sigma(x) b(t, x, lambda)] } \ 
      &- 1/(2 ) partial_(lambda)[P_t (lambda)
      partial_x sigma(x_t)^tns  b(t, x, lambda) b(t, x, lambda)^tns partial_x sigma(x_t)],
  $

  is the scaled probability current, i.e. $J_t (lambda) \/ epsilon$ would be the
  probability current of the process.

]

#proof[

  Let $P_t (lambda)$ denote the occupation density of the switching variable
  $lambda_t in [-1, 1]$, conditioned on a frozen value of $x_t = x in
  cal(D)_(epsilon)$. Inserting the backward generator from @lem-bwd-gen into 
  @eq-adjoint-def we obtain 


  $
    1/epsilon   integral_(-1)^1  P_t (lambda) 
    partial_lambda
    f(lambda) tilde(a)(t, x, lambda) + 1/(2 epsilon) integral_(-1)^(1)
    P_t (lambda) partial^2_(lambda lambda)f(lambda)  tilde(b)(t, x, lambda) tilde(b)(t, x, lambda)^(tns) \
 =  integral_(-1)^(1) f_t (lambda) (cal(A)^*_x P_t)(lambda) 
  $

  $
    (cal(A)_x f)(lambda) &= 1/epsilon partial_lambda
    f(lambda){partial_x sigma(x_t)^tns a(t, x, lambda)
    + epsilon/2 trc[b(t, x, lambda)^(tns) partial^2_(x x) sigma(x) b(t, x, lambda)] } \ 
      &+ 1/(2 epsilon) partial^2_(lambda lambda)f(lambda)
      partial_x sigma(x_t)^tns  b(t, x, lambda) b(t, x, lambda)^tns partial_x sigma(x_t).
  $<eq-adj-deriv-setup>
  We treat the drift and diffusion contributions seperately.

  *Drift contribution.* Integration by parts gives
  $
    1/epsilon integral_(-1)^1 partial_lambda f(lambda)
      P_t (lambda) tilde(a)(t, x, lambda) dif lambda
      &= lr(1/epsilon f(lambda)
        P_t (lambda) tilde(a)(t, x, lambda)|)_(-1)^1 \
      &- 1/epsilon integral_(-1)^1
        f(lambda)
        partial_lambda [
          P_t (lambda) tilde(a)(t, x, lambda)
        ] dif lambda,
  $<eq-forward-drift>

  *Diffusion contribution.* Employing intrgration by parts twice yields
  $
    1/(2epsilon) integral_(-1)^1 partial^2_(lambda lambda) f(lambda)
      P_t (lambda) tilde(b)
      tilde(b)^(tns) dif lambda  &= 
       lr(1/(2epsilon)
        partial_lambda f(lambda)
        lr([
          P_t (lambda) tilde(b) tilde(b)^(tns)
           ])
      - 1/(2epsilon) f(lambda)
        partial_lambda [
          P_t (lambda) tilde(b) tilde(b)^(tns)
        ] |)_(-1)^1  \
      &+ 1/(2epsilon) integral_(-1)^1
        f(lambda) partial^2_(lambda lambda)
        [
         P_t (lambda) tilde(b) tilde(b)^(tns)
        ] dif lambda,
  $<eq-forward-diffusion>

  where the arguments $(t, x, lambda)$ are dropped in the notation of $tilde(a)$
  and $tilde(b)$ for clarity. Since $f in dom(cal(A)) = { f in C^2([-1, 1]) |
  partial_lambda f(pm 1)=0 }$, all boundary terms proportional to
  $partial_lambda f(pm 1)$ vanish. The remaining boundary terms must also vanish
  to respect conservation of probability (i.e. zero probability flux through
  $lambda = pm 1$), giving the boundary condtion

  $
    lr([
      P(lambda, t) tilde(a)(t, x_t, lambda)
      - 1/2 partial_lambda (
      P(lambda, t) tilde(b)(t,x, lambda) tilde(b)(t,x, lambda)^(tns)
      )
      ])_(-1)^1 = 0.
  $<eq-forward-zero-flux>

  Using @eq-forward-drift, @eq-forward-diffusion, and enforcing
  @eq-forward-zero-flux, we identify the forward operator as
  $
    (cal(A)^* P)(lambda, t)
      = - 1/epsilon partial_lambda [
          P(lambda, t) tilde(a)(t, x_t, lambda)
        ]
        + 1/(2epsilon) partial^2_(lambda lambda) [
          P(lambda, t) tilde(b)(t, x_t, lambda)
                       tilde(b)(t, x_t, lambda)^(tns)
        ].
  $<eq-forward-generator-final>

  This is also called the Fokker–Planck or Kolmogorov forward operator
  associated with the dynamics of the switching variable $lambda_t$ conditional
  on $x_t = x in cal(D)_(epsilon)$. Substituting the definitions @eq-a-tilde-def
  @eq-b-tilde-def yields the defnition give in the lemma.
]

#remark[

  Since $P_t (lambda)$ is the one dimensional occupation probability density
  with zero flux boundary conditions, and since $J_t (pm 1) = 0$ must always be
  satisfied, we have no current for all $lambda in [-1, 1]$ giving us the
  condtion

  $
    J_t (lambda) &= P_t (lambda) { partial_x sigma(x_t)^tns a(t, x, lambda)
    + epsilon/2 trc[b(t, x, lambda)^(tns) partial^2_(x x) sigma(x) b(t, x, lambda)] } \ 
      &- 1/(2 ) partial_(lambda)[P_t (lambda)
      partial_x sigma(x_t)^tns  b(t, x, lambda) b(t, x, lambda)^tns partial_x sigma(x_t)] = 0.
  $<eq-db-on-lam>

  This ofcourse also means detailed balance is satified,.
]<rem-db-lam>


Before we proceed it is usefull to introduce a defnition for the $L^2$ space that we will be working in, but first let us consider the general defintion of an $L^2$ inner product and norms on real functions.

#definition(title: [$L^2$-norm])[

  Let $(Omega, cal(F), mu)$ be a measurable space, where $Omega$ is the set,
  $cal(F)$ is the sigma algebra of $Omega$, and $mu$ the measure. Then for any
  $cal(F)$-measurable $f, g: Omega mapsto RR$, the $L^2$ inner product is defined as 

  $
    chevron.l f, g chevron.r_(L^2_(mu)) eqdef  integral_(Omega) f(omega) g(omega) dif mu(omega),
  $<eq-l2-inner-prod-def>

  which induces the $L^2$-norm
  
  $
    l2norm(f, L^2_(mu), ,) eqdef chevron.l f \, f chevron.r_(L^2_(mu))^(1/2)  =
    (integral_(Omega) f^2(omega) dif mu(omega))^(1/2).
  $<eq-l2-norm-def>
]

#corollary(title: [$L^2$-bound to supremum bound])[

  It is straight forward to bound an $L^2$-norm using a supremum bound via the
  argument

  $
    l2norm(f, L^2_(mu), ,)
    = (integral_(Omega) f^2(omega) dif mu(omega))^(1/2) 
      &= (integral_(Omega) |f(omega)|^2 dif mu(omega))^(1/2), \
      &<= sup_(x in Omega) |f(omega)| (integral_(Omega)  dif mu(omega))^(1/2), \
      &<= sqrt(mu(Omega))sup_(x in Omega) |f(omega)|.
  $
]

Since we have a probability density we will use the notation $inprod(., .,
L^2_(P_t))$ and $l2norm(., L^2_(P_t), ,)$ where $P_t in dom(cal(A)^*_x)$. 

#lemma(title: [Symmetry of $cal(A)_x$])[

  Let $x in cal(D)_epsilon$ be fixed, $cal(A)_x$ be the backward generator
  given in @lem-bwd-gen, and $P_(ss)(lambda | x)$ satisfy $cal(A)^*_x
  P_(ss) = 0$ with $J_(ss)(pm 1) = 0$. Then $cal(A)_x$ is
  symetric in $L^2_(P_(ss))$, that is satisfying the ralation 

  $
    chevron.l cal(A)_x f, g chevron.r_(L^2_(P_oo)) =
    chevron.l f, cal(A)_x g chevron.r_(L^2_(P_oo)) ,
  $<eq-self-adjoint-def>

  for all $f, g in dom(cal(A)_x)$.

]<lem-self-adjoint>

#proof[

  We proceed by substituting @eq-bwd-gen into the left hand side of @eq-self-adjoint-def gives,

  $
    integral_(-1)^(1)  (cal(A)_x f)(lambda) g(lambda) P_(ss)(lambda) = I_1 + I_2
  $<eq-adj-setup>
  where
  $
    I_1  &eqdef integral_(-1)^1 partial_lambda
    f(lambda) tilde(a)(t, x, lambda) g(lambda) P_(ss)(lambda) dif lambda ,\
    I_2 &eqdef 1/2 integral_(-1)^1 partial^2_(lambda lambda) f(lambda) tilde(d)(t, x, lambda) g(lambda) P_(ss), dif lambda
  $
  are, respectively, the drift and diffusion contributions which we treat separately.

  *Drift term.* Integration by parts gives

  $
    I_1     &= lr(f(lambda) tilde(a)(t, x, lambda) g P_(ss)(lambda)|)_(-1)^1 
    - integral_(-1)^1 f(lambda) partial_lambda [tilde(a)(t, x, lambda) g(lambda) P_(ss)(lambda)] dif lambda.
  $<eq-drift-adj-1>

  *Diffusion term.* Let $tilde(d)(t, x, lambda) eqdef tilde(b)(t, x, lambda)
   tilde(b)(t, x, lambda)^tns$. Integrating by parts twice yeilds

  $
    I_2 &= 1/2 lr(partial_lambda f(lambda) d(t, x, lambda) g(lambda) P_(ss)(lambda)|)_(-1)^1
    - 1/2 integral_(-1)^1 partial_lambda f(lambda) partial_lambda [d(t, x, lambda) g(lambda) P_(ss)(lambda)] dif lambda, \
      &= -1/2 lr(f(lambda) partial_lambda  [d(t, x, lambda) g(lambda) P_(ss)(lambda)]|)_(-1)^1
      + 1/2 integral_(-1)^1 f(lambda) partial^2_(lambda lambda) [d(t, x, lambda) g(lambda) P_(ss)(lambda)] dif lambda,
  $<eq-diff-adj-1>

  where the boundary term on the first line vanishes as $partial_lambda f(pm 1)
  = 0$. Considering only the boundary terms from @eq-drift-adj-1 and @eq-diff-adj-1, 

  $
    &lr(f(lambda)  {tilde(a)(t, x, lambda) g(lambda) P_(ss)(lambda) - 
    1/2 partial_lambda [tilde(d)(t, x, lambda) g(lambda) P_(ss)(lambda)]}|)_(-1)^1, \
      &= lr(f(lambda)  {tilde(a)(t, x, lambda) g(lambda) P_(ss)(lambda) - 
    1/2 partial_lambda g(lambda) tilde(d)(t, x, lambda) g(lambda) P_(ss)(lambda) - g(lambda) partial_lambda [tilde(d)(t, x, lambda)  P_(ss)(lambda)]}|)_(-1)^1, \
      &= lr(f(lambda) g(lambda) {tilde(a)(t, x, lambda)  P_(ss)(lambda) 
      -  partial_lambda [tilde(d)(t, x, lambda)  P_(ss)(lambda)]}|)_(-1)^1 = 0,
  $
  where we have used $partial_lambda g(pm 1) = 0$ and the condition in @eq-db-on-lam. Expanding the integrand of the first integral gives

  $
    f(lambda)  partial_lambda [g(lambda) tilde(a)(t, x, lambda) P_(ss)(lambda)] = 
    f(lambda) [partial_lambda g(lambda) tilde(a)(t, x, lambda)  P_(ss)(lambda) + g(lambda)partial_lambda [tilde(a)(t, x, lambda) P_(ss)(lambda)]],
  $
  similarlay for the second integrand
  $
    1/2 f(lambda) partial^2_(lambda lambda) [d(t, x, lambda) g(lambda) P_(ss)(lambda)]  &= 
    1/2 f(lambda) {
      partial^2_(lambda lambda) g(lambda) tilde(d)(t, x, lambda)  P_(ss)(lambda) \
        &+ 2 partial_lambda g(lambda) partial_lambda [tilde(d)(t, x, lambda)  P_(ss)(lambda)]  \
        &+ g(lambda) partial^2_(lambda lambda) [tilde(d)(t, x, lambda)  P_(ss)(lambda)] }.
  $

  The coeffcients of $g(lambda)$ vanish due to the steady-state condition on $P_(ss)(lambda)$, ie. $(cal(A)^(*)_x P_(ss))(lambda) = 0$, leaving
  $
    &1/2 f(lambda) partial^2_(lambda lambda) [d(t, x, lambda) g(lambda) P_(ss)(lambda)] - f(lambda)  partial_lambda [g(lambda) tilde(a)(t, x, lambda) P_(ss)(lambda)] \
      &= f(lambda) lr({
        partial_lambda g(lambda) [tilde(a)(t, x, lambda)
      + 1/2 partial^2_(lambda lambda) g(lambda) tilde(d)(t, x, lambda) ] P_(ss)(lambda)  \
            &quad  quad quad  - 2 partial_lambda g(lambda) [tilde(a)(t, x, lambda) P_(ss)(lambda) - 1/2 partial_lambda [tilde(d)(t, x, lambda)  P_(ss)(lambda)]]
      }).
  $

  The second term vanishes due to the zero current condition leaving only
  $(cal(A)_x g)(lambda)$ in the integrand, thus

  $
    integral_(-1)^1 (cal(A)_x f) g P_(ss) dif lambda 
    = integral_(-1)^1 f (cal(A)_x g) P_(ss) dif lambda.
  $

]

#todo[
  add text here to say we need to introduce this theorem without proof, for the proof see .... 
]


#theorem(title: [Poincaré inequality])[

  #todo[
    this proof relies on 
  ]

  Let $P_t: [-1, 1] -> (0, oo)$ be a probability density and $d: [-1, 1] -> (0, oo)$ satisfy $d(lambda) >= C_1 > 0$ and $P_t (lambda) >= C_2 > 0$ for all $lambda in [-1, 1]$. Then for all $f in C^1([-1, 1])$ with $integral_(-1)^(1) f(lambda) P_t (lambda) dif lambda = 0$ 

  $
    integral_(-1)^(1) f^2(lambda) P_t (lambda) dif lambda <= 1/kappa integral_(-1)^(1) [partial_lambda f(lambda)]^2 d(lambda) P_t (lambda) dif lambda,
  $<eq-poincare-bound-def>

where $kappa = (C_1 C_2) \/ 2$.
]<thm-poincare-ineq>

#proof[
  By the fudnemental thorem of calculus we have
  $
    f(a) - f(b) = integral_a^b partial_lambda f(lambda) dif lambda.
  $
  Integrating both sides by $P_t (b)$ and integrating over the support gives
  $
    integral_(-1)^(1) f(a)P_t (b) dif b - integral_(-1)^(1) P_t (b) f(b) dif b = integral_(-1)^(1) P_t (b) integral_a^b partial_lambda f(lambda) dif lambda  dif b, 
  $

  where the first integral on the left hand side simplifies due to the
  normalisastion of probability, while the second vanishes to zero due to
  $EE[f(lambda_t)] = 0$, giving the relation  
  $
    f(a) = integral_(-1)^(1) P_t (b) integral_a^b partial_lambda f(lambda) dif lambda  dif b.
  $

  Taking the absolute value, squaring and employing Caychy-Schwarz twice gives us the bound
  $
    | f(a)|^2 &= (integral_(-1)^(1) P_t (b) lr(|integral_a^b partial_lambda f(lambda) dif lambda |) dif b)^2 \
      &<= (integral_(-1)^(1) P_t (b) dif b)
      (integral_(-1)^(1)  P_t (b)
      lr(integral_a^b |partial_lambda f(lambda) |^2 dif lambda) dif b), \
      &<= 
      (integral_(-1)^(1)  P_t (b)
      integral_(-1)^(1) |partial_lambda f(lambda) |^2 dif lambda dif b), \
      &<=  
      2 lr(integral_(-1)^(1) lr([partial_lambda f(lambda) ])^2 dif lambda) .
  $<eq-mod-f-bound>

  To obtain the desired bound from @eq-mod-f-bound, we simply multiply both by
  $P_t (a)$ and integrate over the interval to get
  $
    integral_(-1)^(1) |f(a)|^2 P_t (a) dif a = integral_(-1)^(1) f^2(a) P_t (a) dif a &<=2 lr(integral_(-1)^(1) lr([partial_lambda f(lambda) ])^2 dif lambda), \
    &<= 2 lr(integral_(-1)^(1)
    lr([partial_lambda f(lambda) ])^2 / (d(lambda) P_t (lambda))  d(lambda) P_t (lambda)dif lambda), \
      &<=2/(C_1 C_2) lr(integral_(-1)^(1)
      lr([partial_lambda f(lambda) ])^2   d(lambda) P_t (lambda)dif lambda), \
  $

  and letting $kappa = (C_1 C_2 )\/ 2$ yields @eq-poincare-bound-def.
]




Since @thm-poincare-ineq relies on bounding the diffusion coefficient, in order
for us to to apply it we must bound

$
  tilde(d)(t, x, lambda) eqdef partial_x sigma(x)^tns tilde(b)(t, x, lambda)  tilde(b)(t, x, lambda)^tns partial_x sigma(x).
$<eq-d-tilde-def>

Indeed $tilde(d)$ as well as the coefficients $tilde(a)$, $tilde(b)$ can all b
can all be bounded from above as the inherit the conditions of $a$ and $b$ as
laid out in @def-ns-gen-sde. We summarise these in the next lemma as they will
then be used in the later results.


#lemma(title: [Linear growth of the interpolated drift and noise amplitude])[

  #todo[
    add all of the bounds
  ]

  Let $lambda in [-1, 1]$ and satisfies the bound

  $
    |a(t, x, lambda)| + |b(t, x, lambda)| <= C (1 - |x|)
  $
  for some $C$ 

  $

    
  $
]<lem-lin-grow-conv>
#proof[
  $
    |a(t, x, lambda)| + |b(t, x, lambda)| &<= |a^+(t, x)| + |a^-(t, x)| + |b^+(t, x)| + |b^-(t, x)|, \
      &<=  C^+_t (1 - |x|) + C^-_t (1 - |x|), \
      &<=  C_t (1 - |x|) 
  $
  #todo[complete the proof!]
]




#lemma(title: [Bounding zero-mean observables])[

  Let $cal(A)_x$ be the backward generator defined in @eq-bwd-gen in
  @lem-bwd-gen, $cal(A)^*_x$ be its adjoint defined in @eq-fwd-gen, let $P_ss
  (lambda)$ be the steady-state probability density such that $(cal(A)^*_x
  P_ss)(lambda) =0$, then for all $f in dom(cal(A))_x$ satisfying

  $
    integral_(-1)^(1)f(lambda) P_ss (lambda) dif lambda = 0,
  $ we have the inequality

  $
    l2norm(f, L^2_(P_ss), 2) <= 1/kappa inprod(-cal(A)_x f, f, L^(2)_(P_ss)) 
  $
]<lem-zero-mean-bound>

#proof[

  We need only show that 
  $
    inprod(-cal(A)_x f, f, L^(2)_(P_ss)) =  1/kappa
    integral_(-1)^1  [partial_lambda f(lambda)]^2 tilde(d)(t, x, lambda) P_ss (lambda) dif lambda,
  $

  as the @thm-poincare-ineq bounds the 
  
  $
    - integral_(-1)^(1) partial_lambda f(lambda)  tilde(a)(t, x, lambda) + 1/2 partial^2_(lambda lambda) f(lambda) tilde(d)(t, x, lambda) P_ss (lambda) dif lambda = integral_(-1)^1 d(t, x, lambda) [partial_lambda f(lambda)]^2 P_ss (lambda) dif lambda
  $

  then the rest follows via @thm-poincare-ineq
]


#lemma(title: [Dense sets in $L^2([-1, 1])$])[

  There exists a set $G subset dom(cal(A)_x)$ that are dense in $L^2$
  
]<lem-dense-l2>

#proof[
  
]

#lemma(title: [zero mean observables in $L^2$])[

  Let $cal(A)_x$ be the backward generator defined in @eq-bwd-gen in
  @lem-bwd-gen, $cal(A)^*_x$ be its adjoint defined in @eq-fwd-gen, let $P_ss
  (lambda)$ be the steady-state probability density such that $(cal(A)^*_x
  P_ss)(lambda) =0$, then for all $g in L^2([-1, 1]; RR)$ satisfying

  $
    integral_(-1)^(1)g(lambda) P_t (lambda) dif lambda = 0 quad  forall P_t in dom(cal(A)^*_x),
  $ we have the inequality

  $
    l2norm(g, L^2_(P_ss),2)<= 1/kappa inprod(-cal(A)_x g, g, L^(2)_(P_ss)) 
  $

]<lem-zero-mean-bound-l2>

#proof[

  By @lem-dense-l2, there exisits a sequence smooth function in $f_n in dom(cal(A)_x)$  such that 
  $
    lim_(n -> 0) ||f_n - g||  = 0,
  $

  We can approximte any $g$ in $L^2([-1, 1])$, by a sequance of smooth $f in
  dom(cal(A)_x)$, hence we can extend the  @lem-zero-mean-bound to generic $g$ 

  #todo[
    finish proof.
  ]
]


#lemma(title: [Exponential mixing of the switching variable])[

  Let $x in cal(D)_epsilon$ be fixed, let $P_t in dom(cal(A))^*_x$ represent the
  occupation probability density of $lambda$ conditioned on $x$, let $P_ss =
  dom(cal(A))^*_x$ such that $(cal(A)^* P_ss)(lambda) = 0$ be its steady-state
  conditioned on $x$, and let 


  then for any $t in [0, T]$, and for any measurable $A
  subset [-1, 1]$, there exisits $kappa>0$ and $C > 0$ such that

  $
    lr(| integral_A P_(t) (lambda | x) dif lambda - integral_A P_(ss)(lambda | x)dif lambda |) <= C ee^(-kappa t).
  $
]<lem-exp-mixing-prob>

#proof()[

  To aid in the proof we define $xi_t (lambda) eqdef P_t (lambda) - P_ss
  (lambda)$, and $zeta_t (lambda) eqdef xi_t (lambda) \/ P_ss (lambda)$, where
  we have dropped the conditional argument in the noation. Clearly $zeta_t
  (lambda)$ has

  $
    lr(|integral_A [P_t (lambda) - P (lambda)] dif lambda |)
      &= lr(|integral_A zeta_t (lambda) P_ss (lambda) dif lambda|), \
      &<= integral_(-1)^1 |zeta_t (lambda)| P_ss (lambda) dif lambda,  \
      &<= (integral_(-1)^1 zeta^2_t (lambda) P_ss (lambda) dif lambda )^(1\/2)
          (integral_(-1)^1  P_ss (lambda) dif lambda)^(1\/2), \ 
      &<= (integral_(-1)^1 zeta^2_t (lambda) P_ss (lambda) dif lambda )^(1\/2), \
      &= l2norm(zeta_t, L^2_(P_ss),  ,).
  $
  Then it only remains to bound $l2norm(zeta_t, L^2_(P_ss),  ,)$
  $
    dif /(dif t) l2norm(zeta_t, L^2_(P_ss), 2 ,) &= 2 integral_(-1)^1  zeta_t (lambda) partial_t zeta_t (lambda) P_ss (lambda) dif lambda, \ 
      &= -2 inprod(-cal(A)_x zeta_t,  zeta_t, L^2_(P_ss)), \
      &<= -2kappa l2norm(zeta_t, L^2_(P_ss),  2,), quad #text[(by @lem-zero-mean-bound-l2)].
  $ 

  Then by Gronwall's inequality, 

  $
    l2norm(zeta_t, L^2_(P_ss),  ,) <= l2norm(zeta_0, L^2_(P_ss),  ,) ee^(- kappa t),
  $

  defining $C = l2norm(zeta_0, L^2_(P_ss),  ,)$ yields the relation in the lemma.
]



#corollary(title: [Bound on expectations])[

  The bound on the differences between probability measures obtained in
  @lem-exp-mixing affords us a further bound, namely on the differences in
  expectations via

  $
    lr(|EE[f(lambda_t)] - integral^(1)_(-1) f(lambda)  P_ss (lambda) dif lambda |) 
      &= lr(|integral_(-1)^(1) f(lambda) zeta_t (lambda) P_ss (lambda) dif lambda|) ,\
      &= lr(|inprod(f,  zeta_t,L^2_(P_ss))|) ,\
      &<= l2norm(f, L^2_(P_ss),  ,) l2norm(zeta_t, L^2_(P_ss),  ,) ,\
      &<= 2 l2norm(f, L^2_(P_ss),  ,) ee^(-kappa t), \
      &<= 2 sup_(lambda in [-1, 1]) | f(lambda) | ee^(-kappa t).
  $

  The final supremum bound is of course only meaningful when we have bounded $f$
  on the interval $lambda in [-1, 1]$.
]


In summary we have estabilshed the existance of an intermediate timescale
$delta$ such that $epsilon << delta << 1$ where the following are satisfied:

- $EE[sup_s |x_(t+s) - x_t|^2] -> 0 $ as $epsilon -> 0$, that is the slow
  varible remains fixed.

- Exponentila mixing 



#definition(title: [The Reduced SDE])[

  $
  dif x_t = 
  $

]



#pagebreak()
#bibliography("fixlib.bib")   

// Local Variables:
// typst-preview--master-file: "/home/seeralan/work/noty-vt/areas/nssde.typ"
// End:
