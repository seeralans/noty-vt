#import "@preview/theorion:0.4.1": *
#import "@preview/equate:0.3.2": equate
#import "@preview/gruvy:2.1.0": gruvbox, theme-colors, colors
#import "@preview/note-me:0.6.0": *
#import "@preview/cetz:0.4.2"

#import emoji: face
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
//#set page(height: auto)
#set page(footer: context [
  #h(1fr)
  #counter(page).display(
    // "1/1",
    // both: true,
  )
])


// #show outline.entry.where(
//   level: 3
// ): set block(above: 1.2em)


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
#let ss   = math.op($oo$)
#let inprod(f, g, mu) = {
  [$chevron.l #f | #g chevron.r_(#mu)$]
}


#let l2norm(f, mu, p) = {
  [$|#f|^(#p)_(#mu)$]
}


#set text(lang: "en")

#align(center)[
  #text(18pt, weight: "bold")[
    On stochastic differential equations with piecewise smooth drift and
    noise coefficients.]
]


#align(center)[
  #text(14pt, weight: "bold")[
    Seeralan Sarvaharman $#emoji.lemon$ $dot$ Aljaz Godec $#emoji.lemon^2$]
]

// #figure(
//   caption: [A simple circle drawn with CeTZ],
//   cetz.canvas({
//     import cetz.draw: *
//     set-style(
//       stroke: 1.0pt,
//       grid: (
//         stroke: gray + 0.2pt,
//         step: 0.5
//       ),
//       mark: (
//         transform-shape: false,
//         fill: black
//       )
//     )
//     grid((-5, -5), (5, 5), step: 0.5, stroke: black + 0.2pt)
//     
// 
//     line((-1, 0), (1, 0), mark: (end: "stealth"))
// 
//     // circle((0, 0), radius: 1.5, fill: blue.lighten(50%))
//     // // content((0, 0), [Cunt])
// 
//   })
// )
// #pagebreak()

#outline(depth: 2)


= Introduction

== Physical Motivation

We consider stochastic differential equations arising from the large-$N$ limit of
chemical master equations, which themselves emerge from coarse-graining of Potts
models. The resulting SDEs have two key features:

1. *Piecewise-smooth drift*: The deterministic dynamics has a discontinuity
   across a codimension-one switching manifold

2. *Multiplicative noise*: The noise coefficient depends on the state inherited
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


The gap: No existing work treats multiplicative noise with discontinuous
noise coefficient and derives the Gaussian fluctuations near the
switching manifold. None have ever attempted to resolve the case when we have
hidden dynamics. 
- this requries one to obtain estimates for higher moments of $lambda$ not just the mean

== Main Contributions

1. Derivation of the switching variable dynamics via Meyer-Itō
2. Rigourous derivation of the fast Fokker-Planck equation with reflecting boundaries, via intermediate timescale
3. Averaging principle for the slow dynamics
4. Explicit formula for the Gaussian envelope including contributions from
   switching variable fluctuations



== Literature Gap

The Freidlin-Wentzell theory of large deviations @freidlinwentzell1998book deals
with weak noise SDEs with smooth drift and noise coefficient.



= Background <sec-background>

#definition(title: [Piecewise-smooth ODE])[ 

  Let $sigma: bb(R)^d mapsto bb(R)$ be a smooth function, $epsilon>0$, and $x
  in bb(R)^d$ be a deterministic processes satisfying the ODE

  $
    (dif  x) / (dif t)  = cases(
    a^+(x) quad sigma(x) > 0,
    a^-(x) quad sigma(x) < 0) 
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

$
  a(t, x, lambda)  =  1/2 (1 + lambda) a_+ (x) + 1/2 (1 - lambda) a_- (x) + (1 - lambda^2) h(x)
$



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
    dif x_t = a(x_t) dif t + sqrt(epsilon) b(x_t) limits(*)^alpha dif W_t,
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

 are, respectively, piecewise smooth drift and noise coefficients which switch
 on the disconiuty set $ cal(D) eqdef {x in RR^d | sigma(x) = 0} $ and satisfy
 following conditions:

  1. (A1 - Smoothnes) The constituent coefficients are sufficiently smooth
     $a^(pm) in C^2(RR^d; RR^d)$ and $b^(pm)in C^2(RR^d; RR^(d times m))$.

  2. (A2 - Linear Growth) We have $ ||a^(pm)(x)|| + ||b^(pm)(x)|| <= C^pm (1 +
  ||x||) $ for some $C>0$, where $||a^(pm)(dot, dot)||$ is the Euclidean norm and $
    ||b^(pm) (dot, dot)|| = sqrt(sum_(i j) |b_(i j) (dot, dot)|^2).
  $

  3. (A3 - Lipshitz Continuity) For any $x, y in RR^d$ and any $s, t in [0, T],$ we have $
    ||a^(pm)(t, x) - a^(pm)(s, y)|| + ||b^(pm)(t, x) - b^(pm)(s, y)|| <= K^pm_(x) |x - y| + K^pm_(t) |t - s|,
  $ for some $K^pm >0$.

  4. (A4 - Transversality )  For any $x in cal(D)$ $
    ||partial_(x) sigma(x)^tns b^pm (x)||  >= M^pm > 0. 
  $ 

  #todo[
    It may be that we need the condition A4 for $x in cal(D)_(epsilon)$
  ]


  The $alpha$ is used to control evaluation point of the stochastic integral.

]<def-ns-gen-sde>

The conditions (A1-3) ensure that away from the discontinuity set, that is $x
in.not cal(D)$, we have the existance and uniqueness of solutions. In other
words away from $cal(D)$, one can employ standard methods of SDE theory to
analyse the dynamics, while near the discontinuity one can The stochastic
integral in @eq-gen-sde is understood in the α–sense, i.e. with evaluation point
$(1 - alpha) x_t + alpha x_(t+ Delta t)$. While the recasting of typical
$alpha$-SDE into an Itō form is straight forward, see for example,
@oksendal2013book or @gardiner2009book, one cannot naively follow the procedure
here as the noise coefficient does not have a continuous derivative.

Instead, we must fisrt employ Filippov's convex construction @filippov2013book
for the drift and noise coefficient, with $lambda in [-1, 1]$ we define the convex
combinations
$
  a(t, x, lambda) eqdef 1/2(1 + lambda)a^+(x) + 1/2(1 - lambda)a^-(x),
$<eq-a-def>
and
$
  b(t, x, lambda)  eqdef 1/2(1 + lambda)b^+(x) + 1/2(1 - lambda)b^-(x),
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
the Itō analgoue

$
  dif x_t = [a(x_t, lambda_t) + alpha epsilon c(x_t, lambda_t)] dif t + sqrt(epsilon) b(x_t, lambda_t) dif W_t,
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
$cal(O)(1\/epsilon)$. However, one cannot simply employ Itō's Lemma on $lambda_t
= Lambda_(epsilon)[sigma(x_t)]$ as the latter is not a smooth function of $x_t$.
Instead to study the dynamics of $lambda_t$ we must first introduce two new
concepts: local time of a semi-martingale and the Meyer-Itō Theorem.

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
//   Start with a smooth approximation $psi_n$ for $|x - u|$, apply Itō's lemma and
//   one would find that
// 
//   $
//     L^x_t (z) =  integral_0^t delta(x_t - a) dif<X, X> 
//   $
// 
// ]

For derivation and discussion see See Chap. 3 of @karatzasshreve2014book, and
Chap. IV of @protter2012book. Secondly we require the Meyer-Itō theorem, also
called the genearlised Itō's formula. We restate it here without the proof which
can be found in Theorem 70, Chapter IV of @protter2012book.

#theorem(title: [Meyer-Itō])[ 

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

For $f in C^2 [RR^D, RR]$, @thm-ito-meyer redues to Itō's Lemma, 




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

where $delta(u)$ is the Dirac-delta distribution. In order to apply Meyer-Itō we
also need the dynamics of scalar observable $sigma(x_t)$ which we state in the following lemma.
#lemma(title: [SDE for $z_t = sigma(x_t)$])[

  Let $lambda in [-1, 1]$, $sigma in C^2(RR^d, RR)$, $x_t$ be an Itō process
  according to @eq-ito-sde, supplemented by the conditions in @def-ns-gen-sde,
  then the random variable $z_t = sigma(x_t)$ evolves according to the SDE

  $
    dif z_t = tilde(a)(x_t, lambda_t) dif t + sqrt(epsilon)tilde(b)(x_t, lambda_t) dif W_t,
  $<eq-z-sde>
  where
  $
    tilde(a)(t, x, lambda) eqdef partial_x sigma(x)^(tns) [a(t, x, lambda) + alpha epsilon c(t, x, lambda)]
    + epsilon/2  trc[b(t, x, lambda) partial^2_(x x) sigma(x) b(t, x, lambda)],
  $<eq-a-tilde-def>
  and 
  $
    tilde(b)(t, x, lambda) eqdef partial_x sigma(x)^(tns) b(t, x, lambda)],
  $<eq-b-tilde-def>
]<lem-z-sde>
#proof[

  This is trivial application of Itō's lemma. Since $sigma(x_t)$ is smooth, apply Itō's lemma to obtain  
  $
    dif z_t = partial_x sigma(x_t)^(tns)  dif x_t + dif x_t^(tns)  partial^2_(x x) sigma(x) dif x^tns, 
  $<eq-dz-ito-lemma>

  then substritute for $dif x_t$ from @eq-ito-sde into @eq-dz-ito-lemma and
  apply Itō's product rule.

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
    dif lambda_t &= 1/epsilon bb(1)_((-epsilon, epsilon])[sigma(x_t)] tilde(a)(x_t, lambda_t) dif t
    + 1/sqrt(epsilon) bb(1)_((-epsilon, epsilon])[sigma(x_t)] tilde(b)(x_t, lambda_t) dif W_t \
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
    lambda_t &= lambda_0 + integral_0^t 1/epsilon bb(1)_((-epsilon, epsilon])[sigma(x_s)] tilde(a)(x_s, lambda_s) dif t
    + 1/sqrt(epsilon) integral_0^t bb(1)_((-epsilon, epsilon])[sigma(x)] tilde(b)(x_s, lambda_s) dif W_s \
      &+ 1/epsilon [ L_t^(z)(-epsilon) -  L_t^(z)(epsilon)].
$<eq-lam-sde-meyer-ito-full>
]

The dynamics of the full system are then represented by the coupled SDE 

$
  dif x_t &= [a(x_t, lambda_t)
  + alpha epsilon b(x_t, lambda_t)] dif t
  + sqrt(epsilon) b(x_t, lambda_t) dif W_t, \
  dif lambda_t &= 1/epsilon bb(1)_((-epsilon, epsilon])[sigma(x_t)] tilde(a)(x_t, lambda_t) dif t
  + 1/sqrt(epsilon) bb(1)_((-epsilon, epsilon])[sigma(x_t)] tilde(b)(x_t, lambda_t) dif W_t \
    &+ 1/epsilon [dif L_t^(z)(-epsilon) - dif L_t^(z)(epsilon)]
    , 
$<eq-x-lam-sde-pair>

where $a(t, x, lambda)$ and $b(t, x, lambda)$ are defined, respectively, in
@eq-a-def and @eq-b-def, while $tilde(a)(t, x, lambda)$ and $tilde(b)(x,
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

Before we attempt to rescale time we must first clarify the $epsilon$-order of
local time terms in @eq-lam-sde.

#lemma(title: [Scaling of local time terms])[

  Let $z_t$ be a stochastic processes defined in @eq-z-sde with quadratic variation $dif chevron.l z
  chevron.r_t = epsilon tilde(b)(x_t, lambda_t) tilde(b)(x_t, lambda_t)^(tns)
  dif t$. Then

  $
    dif EE[L^z_t (a)] = epsilon P^((z))(a, t) tilde(b)^2(x_t, lambda_t) dif t,
  $
  where $P^((z))(a, t)$ denotes the density of $z_t$ at level $a$.
]<lem-local-time-scaling-x>

#proof[
  Taking the expectation of @eq-local-time-def,

$
  EE[L^z_t (a)] &= EE[ lim_(delta arrow.b 0) 1/(2delta) integral_0^t bb(1)_((a - delta, a + delta)) (z_s) dif chevron.l  z chevron.r_s], \
    &= lim_(delta arrow.b 0) 1/(2delta) integral_0^t EE[bb(1)_((a - delta, a + delta))   dif chevron.l  z chevron.r_s] \ 
    &= lim_(delta arrow.b 0) 1/(2delta)  integral_0^t epsilon EE[bb(1)_((a - delta, a + delta))tilde(b)(x_s, lambda_s) tilde(b)(x_s, lambda_s)^(tns) ]  dif s, \ 
    &=   epsilon  integral_0^t lim_(delta arrow.b 0) 1/(2delta)
    integral_(a - delta)^(a + delta) P^((z))(a, s) tilde(b) (x_s,  lambda_s) tilde(b)(x_s,  lambda_s)^(tns) dif s, \
    &=  epsilon integral_0^t P^((z))(a, s) tilde(b) (x_s,  lambda_s) tilde(b)(x_s,  lambda_s)^(tns) dif s,  #<eq-local-time-exp-z-int>\
$
  
  The result follows by differentiation.
]


From @eq-diff-ee-lt we conclude that $dif L^z_t(a) ~ epsilon P^((z))(a, t)
tilde(b)^2(t, x_t, lambda_t) dif t$. The following lemma establishes that no
time rescaling can balance all contributions to the $lambda$-dynamics.

#lemma(title: [Incompatibility of scaling])[

  Let $t = epsilon^beta tau$ for $beta > 0$. Under this rescaling, the terms in
  @eq-lam-sde scale as:

  $
    "Drift:" &bb(1)_((-epsilon, epsilon])[sigma(x_(tau))]
    tilde(a)(x_tau, lambda_tau) dif tau, quad &&cal(O)(epsilon^(beta - 1)), \
  "Martingale:" &bb(1)_((-epsilon, epsilon])[sigma(x_(tau))]
  tilde(b)(x_tau, lambda_tau) dif W_tau, quad &&cal(O)(epsilon^((beta - 1)/2)), \
  "Local time:" &dif L_(tau)^(z)(a), quad &&cal(O)(epsilon^beta).
  $
  No choice of $beta > 0$ brings all three contributions to the same order as $epsilon -> 0$.
]<lem-scaling-incompatibility>

#proof[
  The rescaling gives $dif t = epsilon^beta dif tau$ and $dif W_t = epsilon^(beta/2) dif W_tau$. The drift term in @eq-lam-sde carries a factor $epsilon^(-1)$ from the layer dynamics, yielding order $epsilon^(beta-1)$. The martingale term similarly yields order $epsilon^((beta-1)/2)$. By @lem-local-time-scaling, the local time contribution is $cal(O)(epsilon)$ in original time, hence $cal(O)(epsilon^(beta+1))$ after rescaling.

  The naive choice $beta = 1$ places drift and martingale at $cal(O)(1)$, but the local time term becomes $cal(O)(epsilon^2)$ and vanishes in the limit. Balancing drift and local time requires $beta - 1 = beta + 1$, which has no solution. Balancing martingale and local time requires $(beta-1)/2 = beta + 1$, giving $beta = -3$, which violates $beta > 0$.
]

This presents a fundamental technical hurdle: the three contributions to the dynamics of $lambda$ operate on incompatible scales. Any rescaling followed by $epsilon -> 0$ necessarily discards at least one contribution.

=== Objection II: Loss of physical interpretation.

Even granting mathematical well-posedness, the $epsilon -> 0$ limit produces an
object whose physical meaning has degenerated. As $epsilon -> 0$:

+ The layer $cal(D)_epsilon = {x in RR^d : |sigma(x)| <= epsilon}$ shrinks to
  the codimension-1 surface $cal(D) = {x : sigma(x) = 0}$.

+ The switching variable $lambda in [-1, 1]$ parametrises a convex interpolation
  between the vector fields $a^pm$ and noise coefficients $b^pm$. This
  interpolation only has meaning within the layer, where the dynamics
  transitions between the two regimes.

+ The stationary distribution $P_(ss)^epsilon (lambda | x)$ converges
  to some limiting distribution on $[-1, 1]$, but this limit lives on a domain
  whose connection to the original geometry has been lost.

In the deterministic case, the $epsilon -> 0$ limit yields a single value
$lambda^* (x)$, the Filippov sliding mode. The interpretation is clear: $lambda^*$ selects the unique convex combination that keeps trajectories on the discontinuity surface. In the stochastic setting, one retains a distribution over $lambda$ rather than a single value, but this distribution becomes detached from the layer on which $lambda$ was defined.

=== Objection III: Incompatibility with weak-noise analysis.

The most fundamental objection concerns the purpose of the analysis. The weak-noise framework treats $epsilon$ as the small parameter governing the asymptotic expansion. The objects of interest are not the typical paths obtained by minimising the Freidlin-Wentzell action functional, but the Gaussian fluctuations around the most probable path at order $cal(O)(sqrt(epsilon))$. These phenomena are intrinsically $epsilon$-dependent.

The stationary distribution $P_(ss)(lambda | x)$ at finite $epsilon$ encodes how noise selects among the continuum of Filippov solutions and determines the fluctuation structure near the discontinuity. Taking $epsilon -> 0$ collapses this to a deterministic Filippov vector field, eliminating precisely the phenomena we set out to analyse.

Consistency demands that $epsilon$ be preserved throughout the analysis, including in the treatment of the fast variable.

== The intermediate timescale resolution

The preceding objections share a common source: they arise from taking $epsilon -> 0$ in the layer dynamics. The resolution is to avoid this limit entirely. We introduce an intermediate timescale $delta$ satisfying

$
  epsilon << delta << 1.
$<eq-delta-ordering>

At fixed $epsilon > 0$, all quantities remain well-defined:

- The layer $cal(D)_epsilon$ has finite width and the boundaries $pm epsilon$ are well-separated. All probabilistic quantities, including local times, require no singular limits.

- The switching variable $lambda$ retains its meaning as parametrising the interpolation within a layer of finite width. The stationary distribution $P_(ss)(lambda | x)$ describes the equilibrium of $lambda$ within this layer.

- The parameter $epsilon$ appears throughout the dynamics, preserving the weak-noise structure required for fluctuation analysis.

The conditions on $delta$ encode a separation of timescales. The condition $delta >> epsilon$ ensures the fast variable $lambda$ equilibrates to $P_(ss)(lambda | x)$ within the $delta$-window. The condition $delta << 1$ ensures the slow variable $x$ remains approximately frozen over this window. A concrete realisation is $delta = epsilon^alpha$ for $alpha in (0, 1)$; the value of $alpha$ does not affect the limiting dynamics provided the relevant estimates hold uniformly.

= Estimates for the dynamics on the intermediate timescale

We proceed to establish the estimates that justify the timescale separation. On
the intermediate timescale $delta$ satisfying @eq-delta-ordering, the dynamics
of $x_t$ is frozen while the dynamics of $lambda_t$ equilibrates to a
steady-state distribution. Let us obtain estimates for the variation in the slow
variable $x_t$ on this time scale


#theorem(title: [Slow variation of $x_t$  in the $delta$-window])[

  Let $x_t in cal(D)_epsilon$ and $delta>0$ satisfying @eq-delta-ordering and
  $delta -> 0$ as $epsilon -> 0$, then

  $
    EE[ sup_(0<=s<=delta) |x_(t+s) - x_t|^2 ] <= C (delta^2  + epsilon delta),  \
    PP[sup_(0<=s<=delta) |x_(t+s) - x_t| > gamma ] <= C/gamma^2 (delta^2  + epsilon delta)
        
  $<eq-slow-var-x>
  for some $C,gamma>0$.
]<thm-slow-var>

#proof[

  We start by bounding the squared deviation in the $delta$ time window,
  $
    EE[ |x_(t+s) - x_t|^2]
      &= EE[ lr(|integral_t^(t+s) a(x_tau, lambda_(tau)) dif tau 
      + sqrt(epsilon) integral_t^(t+s) b(x_tau, lambda_tau) dif W_(tau) |)^2],  \
      &<= 2EE[ lr(|integral_t^(t+s) a(x_tau, lambda_tau) dif tau|) ^2 ]
      + 2 epsilon EE[ lr(|integral_t^(t+s) b(x_tau, lambda_s) dif W_tau |)^2]. \
  $

  We will bound each integral term separately, for the drift part we have
  $
    EE[lr(|integral_t^(t+s) a(x_tau, lambda_tau) dif tau|) ^2]
      &<= s integral_t^(t+s)  EE[lr(|a(x_tau, lambda_tau)  |) ^2] dif tau, \
      &<= s integral_t^(t+s)  C (1 + EE[ |x_tau|^2]) dif tau, \
      &<= C' s^2,
  $
  and for the martingale part we have  by Ito isometry
  $
    EE[ lr(|integral_t^(t+s) b(x_s, lambda_s) dif W_s |)^2] 
      &<=  integral_t^(t+s)EE[ lr(||b(x_s, lambda_s) ||)^2]dif s   \
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

The consequence of @thm-slow-var becomes more apparent when we choose any
mesoscopic scale $delta(epsilon)$ satisfying @eq-delta-ordering, e.g.
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

  Let $delta > 0$ satisfying @eq-delta-ordering, let $t in [t', t' + delta] subset
  [0, T]$ for some $t' in [0, T-delta]$. Let $x_t = x in cal(D)_epsilon$ be
  fixed (see @thm-slow-var). Then the backward generator $cal(A)_x$ of
  $lambda_t in [-1, 1]$ evolving according to @eq-lam-sde, acts on sufficently
  smooth test function $f: [-1, -1] mapsto RR$ via

  $
    (cal(A)_x f)(lambda) &= 1/epsilon partial_lambda
    f(lambda){partial_x sigma(x)^tns a(t, x, lambda)
    + epsilon/2 trc[b(t, x, lambda)^(tns) partial^2_(x x) sigma(x) b(t, x, lambda)] } \ 
      &+ 1/(2 epsilon) partial^2_(lambda lambda)f(lambda)
      partial_x sigma(x)^tns  b(t, x, lambda) b(t, x, lambda)^tns partial_x sigma(x),
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
  for some $t' in [0, T]$ and $delta>0$, (see @thm-slow-var). Let $f in C^2([-1, 1])$ and set an initial conditoin
  $lambda_(t') = lambda in [-1, 1]$. Applying Itō's lemma to $f(lambda_t)$ to
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
        tilde(a)(t, x, lambda_s) dif s, \
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
        tilde(b)(t, x, lambda_s) tilde(b)(t, x, lambda_s)^(tns) dif s, \
  $

  Taking expectations, the martingale term vanishes leaving

  $
    EE[f(lambda_t) - f(lambda)]
      = EE[I^((1))_t] + EE[I^((2))_t] + EE[I^((3))_t]. 
  $<eq-gen-decomp-expect>

  Interior terms. The terms $I^((1))_t$ and $I^((3))_t$ are the drift and
  diffusion on the interior of the [-1, 1], i.e. for $lambda in (-1, 1)$. Since
  the coefficients are continious in $t$, we have

  $
    lim_(t -> t') 1/t EE[I^((1))_t]
      = 1/epsilon partial_lambda f(lambda)
        tilde(a)(t, x, lambda), \
        lim_(t -> t') 1/t EE[II^((3))_t]
      = 1/(2 epsilon) partial^2_(lambda lambda) f(lambda)
        tilde(b)(t, x, lambda) tilde(b)(t, x, lambda)^(tns),
  $

  hence, the interior contribution to the generator is  the right-hand side
  of @eq-bwd-gen when we express $tilde(a)$ and $tilde(b)$ in terms of $a$ and
  $b$ using @eq-a-tilde-def and @eq-b-tilde-def.

  Local time term. For the local time contribution $I^((2))_t$, we know that
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

  Let $delta > 0$ satisfying @eq-delta-ordering, let $t in [t', t' + delta] subset
  [0, T]$ for some $t' in [0, T-delta]$. Let $x_t = x in cal(D)_epsilon$ be
  fixed (see @thm-slow-var). Then the forward generator $cal(A)^*_x$ of
  $lambda_t in [-1, 1]$ evolving according to @eq-lam-sde, acts on sufficently
  smooth probability density $P_t: [-1, -1] mapsto [0,  
 oo)$ via
  $
    (cal(A)^*_x P_t)(lambda) &= - 1/epsilon partial_lambda  ( 
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

]<lem-fwd-gen>


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

  Drift contribution. Integration by parts gives
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

  Diffusion contribution. Employing intrgration by parts twice yields
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
      P_t (lambda) tilde(a)(x_t, lambda)
      - 1/2 partial_lambda (
      P_t (lambda) tilde(b)(t, x, lambda) tilde(b)(t, x, lambda)^(tns)
      )
      ])_(-1)^1 = 0.
  $<eq-forward-zero-flux>

  Using @eq-forward-drift, @eq-forward-diffusion, and enforcing
  @eq-forward-zero-flux, we identify the forward operator as
  $
    (cal(A)^*_x P_t)(lambda)
      = - 1/epsilon partial_lambda [
          P_t (lambda) tilde(a)(t, x, lambda)
        ]
        + 1/(2epsilon) partial^2_(lambda lambda) [
          P_t (lambda) tilde(b)(t, x, lambda)
                       tilde(b)(t, x, lambda)^(tns)
        ].
  $<eq-forward-generator-final>

Substituting the definitions @eq-a-tilde-def
  @eq-b-tilde-def yields the defnition give in the lemma.
]

The operator $cal(A)^*_x$ is also called the Fokker–Planck or Kolmogorov forward
operator associated with the dynamics of the switching variable $lambda_t$, in
our case however, it is conditional on $x_t = x in cal(D)_(epsilon)$. 


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

  This ofcourse also means detailed balance is satisfied.
]<rem-db-lam>


== Bounds on the density of the switching variables

#todo[

  add stuff to say in this section we want obtain estimates of the density, and
  to do that we will start with the bounds on the coefficients.

]


Let us recapitulate the various coefficients that we discussed: $a^pm (t, x)$
and $b^pm (t, x)$ are the drift and noise coefficients for the SDE on either
side of the discontinuity from @def-ns-gen-sde; $a(t, x, lambda)$ and $b(t, x,
lambda)$ are the convex combination of the drift and nosie coefficients defined
in @eq-a-def and @eq-b-def respectively; $tilde(a)(t, x, lambda)$ and
$tilde(b)(t, x, lambda)$ are the convex combination of the drift and nosie
coefficients projected onto the (unscaled) normal of the discontiuity set and
are defined in @eq-a-tilde-def and @eq-b-tilde-def respectively; lastly it is
useful to define the projected diffusion coefficient
$
  tilde(d)(t, x, lambda) eqdef tilde(b)(t, x, lambda) tilde(b)(t, x, lambda)^tns
  = partial_x sigma(x)^tns b(t, x, lambda) b(t, x, lambda)^tns partial_x sigma(x).
$<eq-d-tilde-def>

All of these coefficients inherit the conditions of $a$ and $b$ as laid out in
@def-ns-gen-sde. We summarise these in the next lemma as they will then be used
in the later results.

#lemma(title: [Bounds on the coefficients])[

  Let $x in cal(D)_epsilon$ and $lambda in [-1, 1]$. Suppose the coefficients
  $a^pm$ and $b^pm$ satisfy assumptions (A1)–(A4) of @def-ns-gen-sde, and let
  $a(t, x, lambda)$ and $b(t, x, lambda)$ be the convex combinations defined in
  @eq-a-def and @eq-b-def, and let $tilde(b)(t, x, lambda)$ and $tilde(d)(x,
  lambda)$ be projected coefficents defined in @eq-b-tilde-def and
  @eq-d-tilde-def. Then there exist constants $C, K, tilde(M) > 0$,
  independent of $lambda$, such that the following bounds hold.

  1. Linear growth.  
  $
    ||a(t, x, lambda)|| + ||b(t, x, lambda)|| <= C (1 + ||x||).
  $<eq-ab-lin-growth-bound>

  2. Lipschitz continuity. For any $x, y in RR^d$ and any $t, s in [0, T]$,
  $
    ||a(t, x, lambda) - a(s, y, lambda)||
    + ||b(t, x, lambda) - b(s, y, lambda)|| <= K_x ||x - y|| + K_T |t - s|.
  $<eq-ab-lip-bound>

  3. Transversality. For any $x$ in $cal(D)_(epsilon)$ and $lambda in [-1, 1]$
     $
       || tilde(b)(t, x, lambda) || >= tilde(M) > 0, quad || tilde(d)(t, x, lambda) || >= tilde(M)^2 >  0.
     $<eq-ab-trans-bound>

]<lem-coeff-bounds>

#proof[

  1. Linear growth. Using the defintion of combined coefficients $a$  and the triangle
     inequality we obtain

    $
        ||a(t, x, lambda)|| 
        &<= 1/2 (1 + lambda)  ||a_+ (t, x)||  + 1/2 (1 - lambda)  ||a_- (t, x)|| , \
        &<= 1/2  [(1 + lambda) C_+ + (1 - lambda) C_- ]  (1 + |x|),  \
          &<= (C_+ + C_- )(1 + ||x||) .  
    $
     Carrying out the same steps for $b$ and combining thme gives bound for $b$
     is carried in the same manner gives the linear growth bound in the lemma
     where $C = C_+ + C_-$

  2. Lipschitz continuity. For any $x, y in RR^d$ and any $t, s in [0, T]$ we
     have by the triangle inequality $

    ||a(t, x, lambda) - a(s, y, lambda)|| &<= 1/2 (1 + lambda) ||a^+(t, x) - a^+(s, y)|| + 1/2 (1 - lambda) ||a^-(t, x) - a^-(s, y)||, \
      &<= 1/2 ||x - y|| [(1 + lambda) K^+_x  + (1 - lambda)K^+_x ], \ &+ 1/2 |t - s| [(1 + lambda) K^+_T  + (1 - lambda)K^+_T ] \
      &<=  ||x - y|| ( K^+_x  + K^-_x ) + |t - s| ( K^+_T  + K^-_T ). $

     Again carrying out the same procedure for $b$ and comining them gives the
     condition in the lemma where $K_T = K^+_T + K^+_T $ and $K_x = K^+_x + K^-_x$.

  3. Transversality. From the defintion of $tilde(b)(t, x, lambda)$ we have $
    || tilde(b)(t, x, lambda) || &= ||partial_x sigma(x)^tns b(t, x, lambda)|| ,\
      &= lr(||1/2 partial_x sigma(x)^tns [(1 + lambda)b^+ (t, x) + (1 - lambda)b^- (t, x)]||) ,\ 
      &>= 1/2 [(1 + lambda) M^+ + (1 - lambda)M^-] ,\ 
      &>= min(M^+, M^-). $
    Setting $tilde(M) = min(M^+, M^-)$ gives the bound in the lemma while taking
     the square gives the bound on the projected diffusion coefficent.

]


#lemma(title: [Invariant density of the switching variable])[

  
  #todo[
    convert this into an non autonomous,  The statement should say something like fixing the coefficients $tilde(a)$ and $tilde(b)$. 
  ]

  Let $x_t = x in cal(D)_epsilon$ be fixed (see @thm-slow-var), and let the
 forward generator of $A^*_x$ of $lambda_t$ be define in @eq-fwd-gen, then the
 invariant measure $P_ss (lambda)$ satifies $(cal(A)^*_x P_ss)(lambda) = 0$ and
 is given by

  $
    P_ss (lambda) = R(x) / (tilde(d)(t, x, lambda))
    exp(integral_(-1)^(lambda) (tilde(a)(x, nu)) / (tilde(d)(x, nu)) dif nu),
  $

  where
  $
    R(x) = [integral_(-1)^(1) 1/ (tilde(d)(t, x, lambda))
    exp(integral_(-1)^(lambda) (tilde(a)(x, nu)) / (tilde(d)(x, nu)) dif nu) dif lambda]^(-1),
  $<eq-p-inv-norm-const>

  is an $x$-dependent normalisation constant, $tilde(a)(t, x, lambda)$, and
  $tilde(b)(t, x, lambda)$ are defined in @eq-a-tilde-def and @eq-d-tilde-def,
  respectively.

]

#proof[

  The proof is a direct trivial calulation. We have
  $
    (cal(A)^* P_ss)(lambda)
      = - 1/epsilon partial_lambda [
          P_ss (lambda) tilde(a)(t, x, lambda)
        ]
        + 1/(2epsilon) partial^2_(lambda lambda) [
          P_ss (lambda) tilde(d)(t, x, lambda)
        ] = 0,
  $

  which immediately gives us the ordinary differential equation

  $
    P_ss (lambda) tilde(a)(t, x, lambda) &= 1/2 partial_lambda  [P_ss (lambda) tilde(d)(t, x, lambda)]. \
  $

  It follows then that  
  $
    (partial_lambda P_ss (lambda)) / (P_ss (lambda)) = (2 tilde(a)(t, x, lambda) - partial_lambda  tilde(d)(t, x, lambda)) / (tilde(d)(t, x, lambda)),
  $
  which after intergrating both sides with respect to $lambda$ we obtain 

  $
    ln P_ss (lambda) =  2 integral_(-1)^lambda (tilde(a)(x, nu)) / (tilde(d)(x, nu)) dif nu - ln tilde(d)(t, x, lambda) + ln R(x),
  $

  where $R(x)$ is an integration constant. Enforcing normalisation on the
  invariant density gives @eq-p-inv-norm-const.
  
]


#lemma(title: [Instantaneous smoothing of the switching variable density])[


  Let $x in cal(D)_(epsilon)$ fixed, and let $lambda_t$ ​ evolve according to the
  SDE with forward generator $A^*_x$ given by @lem-fwd-gen, let the diffusion
  coefficient satisfies $tilde(d)(t,x,lambda)≥C_2>0$ and let $lambda_0 in [-1,
  1]$ be any initial conditions giving a Dirac distribtion for the intial
  profile. Then for any $tau>0$, the occupation probability density of
  $P_(tau)(lambda_(tau) | x, lambda_0)$​ satisfies

  $
    R_("L")/sqrt(tau) exp[-(C'_(1)(lambda -lambda_0)^2 )/ tau] <= P_(tau)(lambda_(tau) | x, lambda_0) <= R_("U") / sqrt(tau) exp[-(C'_(1)(lambda - lambda_0)^2 )/ tau]
  $
  where $R_L$, $R_U$, $C'_(1)$  and $C'_(1)$ constants that depends on ... 
  #todo[
    add the constant dependence
  ]

]

#proof[
  The proof is a direct consequence from Aronson,  Theorem 1 in @aronson1967


  #todo[
    I dont need this I can justy use Nashs result cited in @aronson1967 6.
  ] 
]



#lemma(title: [Bounds on the Invariant Measure])[

  For all $x in cal(D)_epsilon$ fixed  and $||tilde(a)(t, x, lambda)|| <= tilde(C)_1 (x)$  and $||tilde(b)(t, x, lambda)|| >= tilde(C)_1 (x) > 0$, set $tilde(C)_(12)(x) = (tilde(C)_1 (x)) / (tilde(C)_2 (x))$


  $
    (tilde(C)_12 (x) exp[-tilde(C)_12 (x)(1 + 4 |lambda|)]) / sinh(tilde(C)_12 (x)) <= P_ss (lambda | x)
   <= (tilde(C)_12 (x) exp[tilde(C)_12 (x)(1 + 4 |lambda|)]) / sinh(tilde(C)_12 (x)) 

  $

  #todo[
    rewrite in a simpler form by introducing some auxiliary $G(x)$
  ]
]<lem-inv-meas-bound>



#proof[

  #todo[
    proof in notebook add it in.
  ]
  

  $
    (alpha ee^(-2 alpha (1 + 4 |lambda|)))/ sinh(alpha/2)
    <= P_ss (lambda)
   <= (alpha ee^(2 alpha (1 + 4 |lambda|)))/ sinh(alpha/2)

  $
]



= Norm tings


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

  _Drift term._ Integration by parts gives

  $
    I_1     &= lr(f(lambda) tilde(a)(t, x, lambda) g P_(ss)(lambda)|)_(-1)^1 
    - integral_(-1)^1 f(lambda) partial_lambda [tilde(a)(t, x, lambda) g(lambda) P_(ss)(lambda)] dif lambda.
  $<eq-drift-adj-1>

  _Diffusion term_ -- Let $tilde(d)(t, x, lambda) eqdef tilde(b)(t, x, lambda)
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

  Let $P: [-1, 1] -> (0, oo)$ be a probability density and $d: [-1, 1] -> (0,
  oo)$ satisfy $d(lambda) >= C_1 > 0$ and $P (lambda) >= C_2 > 0$ for all
  $lambda in [-1, 1]$. Then for all $f in C^1([-1, 1])$ with $integral_(-1)^(1)
  f(lambda) P (lambda) dif lambda = 0$

  $ integral_(-1)^(1) f^2(lambda) P (lambda) dif lambda <= 1/kappa
    integral_(-1)^(1) [partial_lambda f(lambda)]^2 d(lambda) P (lambda) dif
    lambda, $<eq-poincare-bound-def>

where $kappa = (C_1 C_2) \/ 2$.
]<thm-poincare-ineq>

#proof[
  By the fudnemental thorem of calculus we have
  $
    f(a) - f(b) = integral_a^b partial_lambda f(lambda) dif lambda.
  $

  Multiplying both sides by $P_ss (b)$ and integrating over the support gives

  $
    integral_(-1)^(1) f(a)P (b) dif b - integral_(-1)^(1) P (b) f(b) dif b
    = integral_(-1)^(1) P (b) integral_a^b partial_lambda f(lambda) dif lambda  dif b, 
  $

  where the first integral on the left hand side simplifies due to the
  normalisastion of probability, while the second vanishes to zero due to
  $EE[f(lambda)] = 0$, giving the relation  

  $
    f(a) = integral_(-1)^(1) P (b) integral_a^b partial_lambda f(lambda) dif lambda  dif b.
  $

  Taking the absolute value, squaring and employing Caychy-Schwarz twice gives
  us the bound

  $
    | f(a)|^2 &= (integral_(-1)^(1) P (b) lr(|integral_a^b partial_lambda f(lambda) dif lambda |) dif b)^2 \
      &<= (integral_(-1)^(1) P (b) dif b)
      (integral_(-1)^(1)  P (b)
      lr(integral_a^b |partial_lambda f(lambda) |^2 dif lambda) dif b), \
      &<= 
      (integral_(-1)^(1)  P (b)
      integral_(-1)^(1) |partial_lambda f(lambda) |^2 dif lambda dif b), \
      &<=  
      2 lr(integral_(-1)^(1) lr([partial_lambda f(lambda) ])^2 dif lambda) .
  $<eq-mod-f-bound>

  To obtain the desired bound from @eq-mod-f-bound, we simply multiply both by
  $P (a)$ and integrate over the interval to get
  $
    integral_(-1)^(1) |f(a)|^2 P (a) dif a = integral_(-1)^(1) f^2(a) P (a) dif a &<=2 lr(integral_(-1)^(1) lr([partial_lambda f(lambda) ])^2 dif lambda), \
    &<= 2 lr(integral_(-1)^(1)
    lr([partial_lambda f(lambda) ])^2 / (d(lambda) P (lambda))  d(lambda) P (lambda)dif lambda), \
      &<=2/(C_1 C_2) lr(integral_(-1)^(1)
      lr([partial_lambda f(lambda) ])^2   d(lambda) P (lambda)dif lambda), \
  $

  and letting $kappa = (C_1 C_2 )\/ 2$ yields @eq-poincare-bound-def.
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

  since from @lem-coeff-bounds that $|tilde(d)(t, x, lambda)|$ and similarly we know
  that $P_ss (lambda)$ is bounded from below from @lem-inv-meas-bound, therefore
  we can direcly apply @thm-poincare-ineq.
  
  $
    - integral_(-1)^(1) f(lambda) [partial_lambda f(lambda)  tilde(a)(t, x, lambda) + 1/2 partial^2_(lambda lambda) f(lambda) tilde(d)(t, x, lambda)] P_ss (lambda) dif lambda = integral_(-1)^1 d(t, x, lambda) [partial_lambda f(lambda)]^2 P_ss (lambda) dif lambda
  $

  then the rest follows via @thm-poincare-ineq

  #todo[
    in notebook complete it.
  ]
]


#lemma(title: [Dense sets in $L^2([-1, 1])$])[

  There exists a set $G subset dom(cal(A)_x)$ that are dense in $L^2$
  
]<lem-dense-l2>

#proof[
  #todo[
    Look at zeidler
  ]
]

#lemma(title: [zero mean observables in $L^2$])[

  Let $cal(A)_x$ be the backward generator defined in @eq-bwd-gen in
  @lem-bwd-gen, $cal(A)^*_x$ be its adjoint defined in @eq-fwd-gen, let $P_ss
  (lambda)$ be the steady-state probability density such that $(cal(A)^*_x
  P_ss)(lambda) =0$, then for all $g in L^2([-1, 1]; RR)$ satisfying

  $
    integral_(-1)^(1)g(lambda) P (lambda) dif lambda = 0 quad  forall P in dom(cal(A)^*_x),
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
    finish proof, it's in zeidler
  ]
]



#theorem(title: [Exponential mixing of the switching variable])[

  Let $x in cal(D)_epsilon$ be fixed, let $P_t in dom(cal(A))^*_x$ represent the
  occupation probability density of $lambda$ conditioned on $x$, let $P_ss$ be
  the invariant density, i.e $(cal(A)^*_x P_ss)(lambda) = 0$ which is uniformly
  bounded from below by $P_ss (lambda) >= C_1 > 0$ and let the diffusion
  coefficient satisfy $|tilde(d)(t, x, lambda)| >= C_2 > 0$ defined in
  @eq-d-tilde-def, be uniformaly bounded from below. Then for any $t, t_0 in [0,
  T]$ such that $0 < t_0 <= t$, and for any measurable $A subset [-1, 1]$, there
  exisits $kappa(x)>0$ and $C(x) > 0$ such that

  $
    lr(| integral_A [P_(t) (lambda | x) dif lambda - P_(ss)(lambda | x)]dif lambda |) <= C(x) ee^(-kappa(x) (t - t_0)).
  $
]<thm-exp-mixing>

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
    l2norm(zeta_t, L^2_(P_ss),  ,) <= l2norm(zeta_(t_0), L^2_(P_ss),  ,) ee^(- kappa (t - t_0)).
  $

  We know from @lem-inv-meas-bound that $0 < C_P (x) <= P_ss (lambda | x)$, hence

  $
    l2norm(zeta_0, L^2_(P_ss), 2,) &= integral^(1)_(-1)
    [P_(t_0) (lambda | x) - P_ss (lambda)]^2 / (P^2_(ss)(lambda |x))
    P_ss (lambda | x) dif lambda, \
      &<= 1/(C_(P)(x)) integral^(1)_(-1)
   [P^2_0 (lambda | x) + P^2_ss (lambda | x)] dif lambda \
      &<= 2/(C_(P)(x))\
  $

  defining $C(x) = sqrt(2 \/ C_(P)(x))$ yields the relation in the lemma.
]

#corollary(title: [Bound on expectations])[

  The bound on the differences between probability measures obtained in
  @thm-exp-mixing affords us a further bound, namely on the differences in
  expectations via

  $
    lr(|EE[f(lambda_t)] - integral^(1)_(-1) f(lambda)  P_ss (lambda) dif lambda |) 
      &= lr(|integral_(-1)^(1) f(lambda) zeta_t (lambda) P_ss (lambda) dif lambda|) ,\
      &= lr(|inprod(f,  zeta_t,L^2_(P_ss))|) ,\
      &<= l2norm(f, L^2_(P_ss),  ,) l2norm(zeta_t, L^2_(P_ss),  ,) ,\
      &<=  l2norm(f, L^2_(P_ss),  ,) l2norm(zeta_0, L^2_(P_ss),  ,) ee^(-kappa t), \
      &<=  sup_(lambda in [-1, 1]) | f(lambda) | l2norm(P_(t) (lambda) - P_(ss) (lambda), L^2_(P_ss),  ,) ee^(-kappa t).
  $

  The final supremum bound is of course only meaningful when we have bounded $f$
  on the interval $lambda in [-1, 1]$.
]<cor-exp-mixing-obs>


In summary we have estabilshed the existance of an intermediate timescale
$delta$ such that $epsilon << delta << 1$ where the following are satisfied:
- bounds on the coefficents, 
- justification of the transversality condition
- $EE[sup_s |x_(t+s) - x_t|^2] -> 0 $ as $epsilon -> 0$, that is the slow
  varible remains fixed.

- Exponentila mixing via Poincare inequality where the poincare constant is
  obtained from the lower bounds on the invariant measure and $tilde(d)$ bounds
- mixing bounds on the expectations of observables

= Averaging Principle
We are now ready to introduce the averaging principle for the slow dynamics

#definition(title: [The Reduced SDE])[

  Let $x_t in cal(D)_epsilon$ be a solution of the piecewise-smooth SDE given
  in @def-ns-gen-sde, and let $lambda in [-1, 1]$ be the switching variable
  parametrising the convex interpolation

  $
    a(t, x, lambda) &= 1/2(1 + lambda) a^+(t, x) + 1/2(1 - lambda) a^-(t, x), \
    b(t, x, lambda) &= 1/2(1 + lambda) b^+(t, x) + 1/2(1 - lambda) b^-(t, x),
  $

  between the piecewise-smooth coefficients $a^pm$ and $b^pm$. Let
  $P_(ss)(lambda | x)$ denote the stationary distribution of the
  switching variable conditional on $x$, satisfying $cal(A)_x^* P_(ss)
  = 0$ with zero-flux boundary conditions (see @lem-fwd-gen). The reduced SDE is

  $
    dif y_t = [macron(a)(t, y_t) + alpha epsilon macron(c)(t, y_t)] dif t + sqrt(epsilon)" "macron(b)(t, y_t) dif W_t,
  $<eq-reduced-sde>

  where the averaged coefficients are

  $
    macron(a)(t, x) &= integral_(-1)^(1) a(t, x, lambda) P_(ss)(lambda | x) dif lambda, \
    macron(b)(t, x) &= integral_(-1)^(1) b(t, x, lambda) P_(ss)(lambda | x) dif lambda, \
    macron(c)(t, x) &= integral_(-1)^(1) c(t, x, lambda) P_(ss)(lambda | x) dif lambda,
  $

  and

  $
    c(t, x, lambda) = sum_j J_x [b_j (t, x, lambda)] b_j (t, x, lambda)
  $

  is the Itō correction arising from the $alpha$-interpretation of the
  stochastic integral, with $b_j (t, x, lambda)$ denoting the $j$-th column of
  $b(t, x, lambda)$ and $J_x (dot)$ the Jacobian with respect to $x$.

]<def-reduced-sde>



Unsurprisingly, without any hidden term in the dynamics we require only the mean from the distribution $P_ss (lambda)$, 

#lemma(title: [Bounds on average Coefficients])[
  #todo[
    THis is almost a repetition as @lem-coeff-bounds
  ]
  
]<lem-avg-coeff-bounds>



#theorem(title: [Error estimates for the averaged SDE ])[

  Let $x_t$ evolve according to the, let $t in [0, T]$ for some $T>0$, and intiaila condition $x_0 = y_0 in RR^d$
  $
    PP[sup_(s in [0, t]) | x_s - y_s| > gamma ] <= C/gamma^2 R(epsilon, delta) t
  $

   


  #todo[
    finish typing out the statment
  ]
]

#proof[
  We know from Markov's inequality that 
  $
    PP[sup_(s in [0, t]) | x_s - y_s| > gamma ] <= 1/gamma^2 EE[sup_(s in [0, t]) |x_s - y_s|^2],
  $

  Let us define $xi_t eqdef x_t - y_t$, which gives us the initial condition $x_0 = 0$

  then substrituting in the definition for the  
  $
    xi_t = integral^t_0 a(s, x_s, lambda_s)  - macron(a)(s, y_s) dif s
    + sqrt(epsilon) integral^t_0 b(s, x_s, y_s) - macron(b)(s, y_s) dif W_s,
  $

  Note that $lambda_t$ evolves acording to its own SDE given by @eq-lam-sde, but
  for our purposes of bounding we do not have to explicitly take it into account
  here in the proof.

  Let us define the following integrands

  $
    I_a (s) &eqdef a(s, x_s, lambda_s) - macron(a)(s, x_s), \
    I I_a (s) &eqdef macron(a)(s, x_s) - macron(a)(s, y_s) ,
  $
  similarly for the noise coefficient we have
  $
    I_b (s) &eqdef b(s, x_s, lambda_s) - macron(b)(s, x_s), \
    I I_b (s) &eqdef macron(b)(s, x_s) - macron(b)(s, y_s) ,
  $

  Then consider 

  $
    ||xi_t||^2 &<= 2 lr(||integral^t_0   I_a (s) + I I_a (s) dif s||)^2
              + 2 epsilon lr(||integral^t_0   I_b (s) + I I_b (s) dif W_s||)^2, \
    &<= 2 t integral^t_0   ||I_a (s) + I I_a (s)||^2 dif s
              + 2 epsilon lr(||integral^t_0   I_b (s) + I I_b (s) dif W_s||)^2 \
  $

  where in the first instance we have used the inequality $|a + b|^2 <= 2 (|a|^2
  + |b|^2)$, and in the second case we have used Cauchy-Schwarz. Taking the
  expectation allows us to use Ito isometry on teh stochastic integral, 

  $
    EE[ ||xi_t||^2] <= 2 t EE[ integral^t_0   ||I_a (s) + I I_a (s)||^2 dif s]
    + 2 epsilon EE[integral^t_0   ||I_b (s) + I I_b (s)||^2 dif s],
  $
  and using Cauchy-Schwarz again on the intergrand gives

  $
    EE[||xi_t||^2] <= 2 t EE[ integral^t_0   ||I_a (s)||^2 + ||I I_a (s)||^2 dif s]
    + 2epsilon EE[integral^t_0   ||I_b (s)||^2 + ||I I_b (s)||^2 dif s].
  $

  From @lem-avg-coeff-bounds we know that $I I_a (s)$ and $I I_b (s)$ can be
  bounded as it is is just an application of Lipshitz, 

  $
    2t || I I_a (s) ||^2 + 2epsilon || I I_b (s) ||^2 &= 2t||macron(a)(s, x_s) - macron(a)(s, y_s) ||^2
    + 2epsilon||macron(b)(s, x_s) - macron(b)(s, y_s) ||^2 \
      &<=  2 macron(K)^2 ||xi_(s)||^2(t + epsilon).
  $ 

  For the other two integrals we consider first the parition of $[0, t]$ into
  the windows $[k delta, (k+1) delta]$ with $k = 0, 1, ... N-1$ where $N = t\/
  delta$ and then recasting the integrals as, for example,

  $
    integral_(0)^(t) ||I_a (s)|| dif s = sum_(k = 0)^(N-1) integral_(k delta)^((k+1)delta) ||I_a (s)|| dif s.
  $

  We then recast the integrand as

  $
    I_a (s) = underbrace(a(s, x_s, lambda_s)
    - a(s, x_(k delta), lambda_s), eqdef J^((1))_(a, k) (s))
    + underbrace(a(s, x_(k delta), lambda_s)
    - macron(a)(s, x_(k delta)), eqdef J^((2))_(a, k) (s))
    + underbrace(macron(a)(s, x_(k delta))
    - macron(a)(s, x_s), eqdef J^((2))_(a, k) (s))
  $
  Clearly,
  $
    ||I_a||^2 <= 4 (||J^((1))_(a, k)||^2 + ||J^((2))_(a, k)||^2 + ||J^((3))_(a, k)||^2)
  $

 Employing the Lipshitz conditions in @def-ns-gen-sde, and @lem-avg-coeff-bounds
  as well as @thm-slow-var we obtain

  $
    EE[ ||J^((1))_(a, k)||^2] &<= K^2 EE[ ||x_s - x_(k delta)||^2] &&<=  K^2 C (delta^2 + epsilon delta),  \
    EE[ ||J^((3))_(a, k)||^2] &<= macron(K)^2 EE[ ||x_s - x_(k delta)||^2] &&<= macron(K)^2 C (delta^2 + epsilon delta).
  $

  We rewrite $J^((2))_(a, k)(s)$ as
  $
    J^((2))_(a, k)(s) &= integral_(-1)^(1) a(s, x_(k delta), lambda)  [P_s (lambda | x_(k delta)) - P_ss (lambda | x_(k delta))] dif lambda
  $
  where $P_s (lambda | x_(k delta)) = ee^(s cal(A)^* (x)) delta(lambda - lambda_(k  delta))$. Defining $zeta_s (lambda) eqdef [P_s (lambda | x_(k delta)) - P_ss (lambda | x_(k delta))] \/ P_ss (lambda | x_(k delta))$, similar to the proof of @thm-exp-mixing, we then have 
  $
    J^((2))_(a, k)(s) &= inprod(a(s, x_(k delta), dot), zeta_s  , L^2_(P_ss)) \
      &<= l2norm(a(s, x_(k delta), dot), L^2_(P_ss), 2) l2norm(zeta_s, L^2_(P_ss), 2) \
      &<= l2norm(a(s, x_(k delta), dot), L^2_(P_ss), 2) l2norm(zeta_(k delta), L^2_(P_ss), 2) \
      &<= (C_a Q(x_(k delta)))/2 (1 + ||x_(k delta)||) ,
  $
  
  where $zeta_s = [P_s (lambda | x_(k delta)) - P_ss (lambda | x_(k delta))] \/ P_ss (lambda | x_(k delta))$ (from @thm-exp-mixing) and with $Q(x_(k delta)) \/ 2 >= l2norm(zeta_(k delta), L^2_(P_ss),  ,)$. Then we have
  $
    EE[ ||J^((2))_(a, k)(s)||^2] <= EE[Q^2(x_(k delta)) (1 + || x_(k delta)||^2)]
  $
  


]




#pagebreak()
= Needs Testing
// #lemma(title: [Concentration of $P_(ss)$ at tangency])[
// 
//   Let $x in cal(D)_epsilon$ and suppose the transversality condition (A4) holds.
//   Define
// 
//   $
//     nu^pm (x) = partial_x sigma(x)^tns a^pm (x),
//   $
// 
//   the normal components of the drift at $x$. Then:
// 
//   + If $nu^+ (x) < 0$ and $nu^- (x) > 0$ (sliding region), $P_(ss)(lambda | x)$
//     has support on $(-1, 1)$ with a density bounded away from zero.
// 
//   + If $nu^+ (x) -> 0$ with $nu^- (x) > 0$ fixed, then $P_(ss)(lambda | x) ->
//     delta_(lambda = 1)$ weakly.
// 
//   + If $nu^- (x) -> 0$ with $nu^+ (x) < 0$ fixed, then $P_(ss)(lambda | x) ->
//     delta_(lambda = -1)$ weakly.
// 
//   + If $nu^+ (x) > 0$ or $nu^- (x) < 0$ (crossing region), the process exits
//     $cal(D)_epsilon$ in finite time and no stationary distribution exists.
// 
//   In cases (ii) and (iii), the averaged coefficients satisfy
// 
//   $
//     macron(a)(x) -> a^pm (x), quad macron(b)(x) -> b^pm (x),
//   $
// 
//   recovering the smooth dynamics on the corresponding side of $cal(D)$.
// 
// ]<lem-tangency-concentration>

- whether we obtain the limits $P_ss -> delta(lambda pm 1)$ at the tangency points, in reverse time an forward time and backward w.r.t entry and exit points,  
  - Ovs i only needs to check one of the cases, then by time reversal symmetry the other will also hold. 
#pagebreak()

#bibliography("fixlib.bib")   

// Local Variables:
// typst-preview--master-file: "/home/seeralan/work/noty-vt/areas/nssde.typ"
// End:
