#import "@preview/theorion:0.4.1": *
#import "@preview/equate:0.3.2": equate
#import "@preview/gruvy:2.1.0": gruvbox, theme-colors, colors
#import "@preview/note-me:0.6.0": *
#import "@preview/cetz:0.4.2"
#import "@preview/smartaref:0.1.0": cref, Cref



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
  print: true,
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
    "1/1",
    both: true,
  )
])


#let remark(body) = [
  *Remark.* #body
]



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
#let eqs(..labels) = {
  let refs = labels.pos()
  if refs.len() == 1 {
    let el = query(refs.at(0)).first()
    let num = numbering(el.numbering, ..counter(math.equation).at(el.location()))
    [Eq. #link(refs.at(0))[(#num)]]
  } else if refs.len() == 2 {
    let el1 = query(refs.at(0)).first()
    let el2 = query(refs.at(1)).first()
    let num1 = numbering(el1.numbering, ..counter(math.equation).at(el1.location()))
    let num2 = numbering(el2.numbering, ..counter(math.equation).at(el2.location()))
    [Eqs. #link(refs.at(0))[(#num1)] and #link(refs.at(1))[(#num2)]]
  } else {
    let parts = refs.slice(0, -1).map(r => {
      let el = query(r).first()
      let num = numbering(el.numbering, ..counter(math.equation).at(el.location()))
      link(r)[(#num)]
    }).join([, ])
    let el_last = query(refs.at(-1)).first()
    let num_last = numbering(el_last.numbering, ..counter(math.equation).at(el_last.location()))
    [Eqs. #parts, and #link(refs.at(-1))[(#num_last)]]
  }
}

#let tns = math.op(text("T", font: "IBM Plex Sans"))
#let trc = math.op("Tr")
#let dom = math.op("Dom")
#let pm  = math.op($plus.minus$)
#let mp  = math.op($minus.plus$)
#let eqdef  = math.op($eq.triple$)
#let ee  = math.op($upright(e)$)
#let epsilon = math.epsilon.alt
#let sign = math.op(text("sign"))
#let ss   = math.op($oo$)
#let giv   = math.op($|$)
#let inprod(f, g, mu) = {
  [$chevron.l #f | #g chevron.r_(#mu)$]
}


#let lpnorm(f, mu, p) = {
  [$|#f|^(#p)_(#mu)$]
}


#set text(lang: "en")

#align(center)[
  #text(18pt, weight: "bold")[
    On stochastic differential equations with piecewise-smooth drift and
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
//     // grid((-5, -5, 5), (5, 5, 5), step: 0.5, stroke: black + 0.2pt) // 
//     
// 
//     line((0, 0, 0), (1, 0, 0), mark: (end: "stealth"), stroke: red)
//     line((0, 0, 0), (0, 1, 0), mark: (end: "stealth"), stroke: green)
//     line((0, 0, 0), (0, 0, 1), mark: (end: "stealth"), stroke: blue)
// 
//     // circle((0, 0), radius: 1.5, fill: blue.lighten(50%))
//     // // content((0, 0), [Cunt])
// 
//   })
// )


#outline(depth: 2)

#pagebreak()


// #let drafting(title: "TODO", children) = admonition(
//   icon-path: "icons/question.svg",
//   icon-alt: none,
//   title: title,
//   color: rgb(209, 36, 47),
//   children
// )

#let drafting(title: "Draft", children) = block(
  width: 100%,
  outset: (x: 5pt, y: 5pt),
  fill: rgb(87, 127, 230, 20))[
  #children
]


= Introduction

== Physical Motivation

We consider stochastic differential equations arising from the large-$N$ limit of
chemical master equations, which themselves emerge from coarse-graining of Potts
models. The resulting SDEs have two key features:

1. *Piecewise-smooth drift*: The deterministic dynamics has a discontinuity
   across a co-dimension-one switching manifold

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

- *Chen-Baule-Touchette-Just* @chenetal2013, Mostly asymptotics, and the system
   is very simple it is piecewise constant with additive noise.
  - Useful to check if my results match theirs in the limit.

- *Hill-Zanetell-Gemmer* @hilletal2022: Most probable paths via with
  mollification + $Gamma$-convergence, estimates are on the functional level,
  additive noise only. 
  - For some reason they did not cite @chiangsheu20002


The gap: No existing work treats multiplicative noise with discontinuous
noise coefficient and derives the Gaussian fluctuations near the
switching manifold. None have ever attempted to resolve the case when we have
hidden dynamics. 
- this requires one to obtain estimates for higher moments of $lambda$ not just the mean


== Literature Gap

The Freidlin-Wentzell theory of large deviations @freidlinwentzell1998book deals
with weak noise SDEs with smooth drift and noise coefficient.


== Main Contributions

1. Derivation of the switching variable dynamics via Meyer-Itō
2. Rigorous derivation of the fast Fokker-Planck equation with reflecting boundaries, via intermediate timescale
3. Averaging principle for the slow dynamics
4. Explicit formula for the Gaussian envelope including contributions from
   switching variable fluctuations




== Background <sec-background>

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
The switching parameter
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

  Let $T>0$, $t in [0, T]$, $epsilon>0$, $alpha in {0, 1\/2, 1}$, $sigma: bb(R)^d
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

 are, respectively, piecewise smooth drift and noise coefficients which switch
 on the discontinuity set $ cal(D) eqdef {x in RR^d | sigma(x) = 0} $ and satisfy
 following conditions:

  1. (A1 - Smoothnes) The constituent coefficients are sufficiently smooth
     $a^(pm) in C^2([0, T] times RR^d; RR^d)$ and $b^(pm)in C^2([0, T] times RR^d; RR^(d times m))$. #text(fill: red)[Why did I need it to be C2? (for the tubes?) $C^2 => "Lip"$]

  2. (A2 - Linear growth) We have $ ||a^(pm)(t, x)|| + ||b^(pm)(t, x)|| <= C^pm (1 +
  ||x||) $ for some $C>0$, where $||a^(pm)(dot, dot)||$ is the Euclidean norm and $
    ||b^(pm) (dot, dot)|| = sqrt(sum_(i j) |b_(i j) (dot, dot)|^2).
  $

  3. (A3 - Lipschitz continuity) For any $x, y in RR^d$ and any $s, t in [0, T],$ we have $
    ||a^(pm)(t, x) - a^(pm)(s, y)|| + ||b^(pm)(t, x) - b^(pm)(s, y)|| <= K^pm_(x) ||x - y|| + K^pm_(T) |t - s|,
  $ for some $K^pm >0$.

  4. (A4 - Transversality )  For any $x in cal(D)$ $
    ||partial_(x) sigma(x)^tns b^pm (x)||  >= M^pm > 0. 
  $ 

  5. (A5 - Bounded Jacobian)
     $
       lr(||J_(x)[b^(pm)_(j)(t, x)]||)  <= C^(pm)_(J) 
     $
  5. (A6 - Lipschitz Jacobian)
     $
       lr(||J_(x)[b^(+)_(j)(t, x) + b^(-)_(j)(t, x)][b^(+)_(j)(t, x) + b^(-)_(j)(t, x)]  \
        - 
        J_(x)[b^(+)_(j)(s, x) + b^(-)_(j)(t, x)][b^(+)_(j)(t, x) + b^(-)_(j)(t, x)]||) <= H_(x) ||x - y|| + H_(T) |t - s|,
     $
     where $b^(pm)_j$ is the $j^"th"$ column of the noise matrix $b^(pm)$. 

  


  #todo[
    It may be that we need the condition A4 for $x in cal(D)_(epsilon)$
  ]


  The $alpha$ is used to control evaluation point of the stochastic integral.

]<def-ns-gen-sde>

The conditions (A1-3) ensure that away from the discontinuity set, that is $x
in.not cal(D)$, we have the existence and uniqueness of solutions. In other
words away from $cal(D)$, one can employ standard methods of SDE theory to
analyse the dynamics, while near the discontinuity one can The stochastic
integral in @eq-gen-sde is understood in the α–sense, i.e. with evaluation point
$(1 - alpha) x_t + alpha x_(t+ Delta t)$. While the recasting of typical
$alpha$-SDE into an Itō form is straight forward, see for example,
@oksendal2013book or @gardiner2009book, one cannot naively follow the procedure
here as the noise coefficient does not have a continuous derivative.

Instead, we must first employ Filippov's convex construction @filippov2013book
for the drift and noise coefficient, with $lambda in [-1, 1]$ we define the
convex combinations

$
  a(t, x, lambda) eqdef 1/2(1 + lambda)a^+(t, x) + 1/2(1 - lambda)a^-(t, x),
$<eq-a-def>
and
$
  b(t, x, lambda)  eqdef 1/2(1 + lambda)b^+(t, x) + 1/2(1 - lambda)b^-(t, x),
$<eq-b-def>

which are smooth in $lambda$, as well as $x$ and $t$ as they inherit the
smoothness conditions given in @def-ns-gen-sde.  These definitions allow as to recast @eq-gen-sde into
the Itō analogue

$
  dif x_t = [a(t, x_t, lambda_t) + alpha epsilon c(t, x_t, lambda_t)] dif t + sqrt(epsilon) b(t, x_t, lambda_t) dif W_t,
$<eq-ito-sde>
where $lambda_t in [-1, 1]$ for all $t$ is a stochastic variable and where the
spurious drift is
$
  c(t, x, lambda)  eqdef sum_j J_x [b_j (t, x, lambda)] b_j (t, x, lambda).
$ <eq-ito-al-cor-term>

with $b_j (t, x, lambda)$ denoting the $j^#text("th")$ column of the matrix
$b(t, x, lambda)$ and $J_x (dot)$ is the Jacobian matrix of the vector argument
with respect to $x$. 

#lemma(title: [Regularity conditions on the spurious drift])[ Let $epsilon > 0$
  and $alpha in [0, 1]$, let $a: [0, T] times RR^d times [-1, 1] mapsto RR^d$
  and $b: [0, T] times RR^d times [-1, 1] mapsto RR^d$ be defined, respectively,
  according to @eq-a-def and @eq-ito-al-cor-term along with regularity
  conditions given in @def-ns-gen-sde. Then  
$
  a_(alpha, epsilon)(t, x, lambda) eqdef a(t, x, lambda) + alpha epsilon c(t, x, lambda),
$<eq-a-al-ep-def>

  satisfies the following regularity conditions:

  1. Linear growth

  $
    ||a_(alpha, epsilon)(t, x, lambda)|| <= C_(alpha) (1 + ||x||)
  $

  2. Lipschitz continuity
  $
    ||a_(alpha, epsilon)(t, x, lambda) - a_(alpha, epsilon)(s, y, lambda)|| <= K_(alpha) ||x - y|| + K_(alpha, T) |t - s|
  $
]<lem-a-spur-bound>

#proof[
  We verify linear growth and Lipschitz continuity for $a_(alpha, epsilon)(t, x, lambda) = a(t, x, lambda) + alpha epsilon c(t, x, lambda)$.

  Linear growth. From @eq-ab-lin-growth-bound we have $||a(t, x, lambda)|| <= C(1 + ||x||)$. For the spurious drift,
  $
    ||c(t, x, lambda)|| &= lr(||sum_j J_x [b_j (t, x, lambda)] b_j (t, x, lambda)||) \
      &<= sum_j ||J_x [b_j (t, x, lambda)]|| dot ||b_j (t, x, lambda)|| \
      &<= C_J sum_j ||b_j (t, x, lambda)|| \
      &<= C_J C (1 + ||x||),
  $
  using (A5) and linear growth of $b$. Thus $||a_(alpha, epsilon)(t, x, lambda)|| <= C(1 + ||x||) + alpha epsilon C_J C (1 + ||x||) <= C_(alpha)(1 + ||x||)$ with $C_(alpha) = C(1 + C_J)$.

  Lipschitz continuity. From @eq-ab-lip-bound we have the Lipschitz bound on $a$. For $c$, using (A6),
  $
    ||c(t, x, lambda) - c(s, y, lambda)|| &<= H ||x - y|| + H_T |t - s|.
  $
  Combining, $||a_(alpha, epsilon)(t, x, lambda) - a_(alpha, epsilon)(s, y, lambda)|| <= K_(alpha) ||x - y|| + K_(alpha, T) |t - s|$ with $K_(alpha) = K + alpha epsilon H$ and $K_(alpha, T) = K_T + alpha epsilon H_T$.
]

The regularity of $a_(alpha, epsilon)$ ensures that the system @eq-ito-sde
inherits existence and uniqueness from standard SDE theory away from $cal(D)$
where $lambda in {-1, 1}$. We now consider the behaviour near and on the
discontinuity set.

== Manifestation of hidden dynamics  <sec-hidden-dyn>

Notice purely by the fact that we have a non-Itō interpretation of the
multiplicative noise ($alpha != 0$), we have have nonlinear dependence on the
switching variable and that we must have a dynamics that exist on the
discontinuity set. To elucidate this we can decompose the drift into
$
  a_(alpha, epsilon)(t, x, lambda) 
    &= 1/2(1 + lambda){a^+(t, x, lambda) + (alpha epsilon)/2(1 + lambda) sum_(j) J_(x)[b^(+)_(j)(t, x)]b^(+)_(j)(t, x)} \
    &+ 1/2(1 - lambda){a^-(t, x, lambda) + (alpha epsilon)/2(1 - lambda) sum_(j) J_(x)[b^(-)_(j)(t, x)]b^(-)_(j)(t, x)} \
    &+ alpha epsilon (1 - lambda^2) h(t, x)), \
$<eq-a-al-ep-def-decomp>
where
$
  h(t, x)  eqdef 1/4 sum_(j) {J_(x)[b^(+)_(j)(t, x)]b^(-)_(j)(t, x) + J_(x)[b^(-)_(j)(t, x)]b^(+)_(j)(t, x)}.
$<eq-hidden-drift>


The first two terms are the expected contributions from either side of the
discontinuity, weighted by the convex interpolation. The third term,
proportional to $(1 - lambda^2)$, is qualitatively different. It vanishes when
$lambda = pm 1$ and attains its maximum when $lambda = 0$, i.e. precisely on the
discontinuity set $cal(D)$. We call $h(t, x)$ the *hidden drift*.


In deterministic piecewise-smooth systems, hidden terms of this form arise in
the analysis of sliding modes and can affect the dynamics on the discontinuity
set even though they vanish away from it @jeffrey2014, @jeffrey2018book,
@kuehn2015book. Here the situation is more subtle: the switching variable
$lambda_t$ is stochastic, and the contribution of $h(t, x)$ to the averaged
dynamics depends on the second moment $EE[lambda^2]$ under the stationary
distribution $P_(ss)(lambda | x)$ obtained as the long time limit of the fast
dynamics of $lambda$, whilst fixing $x$.


To see this, suppose we wish to replace @eq-ito-sde with an averaged counterpart.
The averaged drift takes the form
$
  macron(a)_(alpha, epsilon)(t, x) &= integral_(-1)^(1) a_(alpha, epsilon)(t, x, lambda) P_(ss)(lambda | x) dif lambda \
    &= 1/2(1 + macron(lambda))[a^+(t, x) + (alpha epsilon)/2 c^+(t, x)]
    + (alpha, epsilon)/4 (macron(lambda) + macron(lambda^2)) c^+(t, x)\
    &+ 1/2(1 - macron(lambda))[a^-(t, x) + (alpha epsilon)/2 c^-(t, x)]
    + (alpha, epsilon)/4 (macron(lambda) + macron(lambda^2)) c^-(t, x)\
    &+ (alpha epsilon)/4 (1 - macron(lambda^2)) h(t, x),
$<eq-avg-drift-with-hidden>

where $macron(lambda) = EE[lambda]$ and $macron(lambda^2) = EE[lambda^2]$ are
moments under $P_(ss)(lambda | x)$, and $c^pm (t, x) = sum_j J_x [b^pm_j]
b^pm_j$ are the Itō corrections on either side. We see that if we wish to
replace the @eq-ito-sde, with an average counterpart, averaged over the
differential inclusion, we must know at least the second moment, i.e.
$EE[lambda^2]$, if we wish to be faithful to the hidden dynamics.


// The magnitude of this contribution depends on the geometry of the problem. When
// the normal components of the drift $nu^pm (x) = partial_x sigma(x)^tns a^pm (t, x)$
// are both directed into the discontinuity set (sliding region), the stationary
// distribution $P_(ss)(lambda | x)$ has support on the full interval $(-1, 1)$
// with variance bounded away from zero. In this case the hidden drift makes a
// genuine $cal(O)(alpha epsilon)$ contribution to the averaged dynamics. When one
// of the normal components vanishes (tangency), the distribution concentrates
// near the corresponding boundary $lambda = pm 1$, and the hidden contribution
// vanishes as $(1 - macron(lambda^2)) -> 0$.

This is a crucial distinction from existing work on piecewise-smooth SDEs which
have been primarily interested in PWS drift fields and with smooth additive
noise. In such a case one has $b^+ = b^-$, the hidden drift $h(t, x)$ vanishes
identically and the averaged dynamics depends only on $macron(lambda)$. Whereas
we not only consider the case $b^+ != b^-$ but also when the noise is
multiplicative as is often the case in biological and chemical systems.

= Main Result

Our main result concerns the typical paths of the piecewise-smooth SDE
@eq-ito-sde. We show that $x_t$ is well-approximated by a reduced SDE
obtained by averaging over the fast switching dynamics, and that this
approximation is sufficiently strong to characterise both the typical paths and
the Gaussian fluctuations around them. In more formal terms, suppose $x_t$ solves
@eq-ito-sde with coefficients satisfying the regularity conditions of
@def-ns-gen-sde, and suppose $y_t$ solves the reduced SDE

  $
    dif y_t = macron(a)_(alpha, epsilon)(t, y_t) dif t + sqrt(epsilon) macron(b)(t, y_t) dif W_t,
  $
  where $macron(a)$ and $macron(b)$ are obtained by averaging against the
  stationary distribution $P_(ss)(lambda | x)$ of the switching variable
  obtained by fixing $x in cal(D)$. Then for any $T > 0$ and $gamma > 0$, we show that
  $
    PP[sup_(t in [0, T]) ||x_t - y_t|| > gamma] <= C_T / gamma^2 sqrt(epsilon),
  $
  where $C_T$ depends on $T$ and the regularity constants.

The reduced system has sufficiently smooth coefficients as wells as inheriting
the regularity conditions from @def-ns-gen-sde, it therefore satisfies a large
deviation principle via Freidlin-Wentzell theory with rate function
  $
    I_T [phi] = cases(
    1/2 integral_0^T ||[dot(phi)_t - macron(a)(t, phi_t)]^(tns)macron(d)(t, phi_t)^(-1)[dot(phi)_t - macron(a)(t, phi_t)]||^2 dif t quad & phi in "a.c. on " [0, T], 
    +oo quad & "otherwise",
    )
  $
  where $phi_0 = y_0$ and $
    macron(d)(t, x) eqdef macron(b)(t, x) macron(b)(t, x)^(tns).
  $

The $cal(O)(sqrt(epsilon))$ error bound on the paths of $x_t$ and $y_T$ is not
sufficient to transfer the LDP from $y_t$ to $x_t$. However, it does ensure that
the paths of $x_t$ lie in some neighbourhood around the typical paths of $y_t$
obtained by the minimisation of $I_T$. The bound is also sufficient to transfer
the Gaussian flucations around the typical, that is a Gaussian tube from $y_t$
on to $x_t$.

The remainder of the manuscript is organised as follows. In @sec-dyn-lam we
derive the dynamics of the switching variable via Meyer-Itô calculus. In
@sec-delta-time we introduce the intermediate timescale and justify the
separation between fast and slow dynamics. In @sec-est-on-del we establish
probabilistic estimates for the switching variable, including exponential
mixing. In @sec-avg-principle we prove the averaging principle for the
piecewise-smooth SDE. Finally, in @sec-typical-paths we show that the typical
paths of the original and averaged systems coincide.

// Lastly, it is important to note that in general the averaged coefficients
// depends on the full distribution $P^(ss)_(x)(lambda)$, not merely its mean, or
// at the very least on $EE[lambda^(n)]$ w.r.t $P^(ss)_(x)(lambda)$ for some
// positive integer $n$. In our case when $alpha != 0$, the spurious drift
// contributes terms quadratic in $lambda$, and the averaged dynamics retains
// information about $EE[lambda^2]$. In other cases one may have additional
// contribution that are not manifest from the noise coefficient @jeffrey2018book.
// This is the mechanism by which hidden dynamics on the discontinuity set manifest
// in the weak-noise asymptotics.
// 
// 
// Laslty, it is important to note that in general averaged coefficients depend on
// the full distribution $P_(ss)(lambda | x)$, not merely its mean. When $alpha !=
// 0$, the spurious drift contributes terms quadratic in $lambda$, and the averaged
// dynamics retains information about $EE[lambda^2]$. In other cases, additional
// hidden terms may arise from the noise coefficient @jeffrey2018book. In the
// deterministic setting, hidden dynamics can stabilise or destabilise sliding
// modes and create new equilibria on the discontinuity surface. In the stochastic
// case, these effects enter through the moments of $P_(ss)(lambda | x)$: the
// hidden drift $h(t, x)$ weighted by $(1 - EE[lambda^2])$ modifies the averaged
// dynamics, shifting or altering the stability of the typical paths.

// path approximation does not suffice to transfer the
// full large deviation principle from $y_t$ to $x_t$ as this would require
// exponential tightness of paths. However, it does ensure that, with probability
// tending to one, the paths of $x_t$ remain within any fixed neighbourhood of
// $y_t$. The typical paths of $x_t$ therefore coincide with the minimisers of
// $I_T$, and the Gaussian fluctuations around these paths are characterised by the
// linearisation of the averaged dynamics.





= Dynamics of the switching variable  <sec-dyn-lam>

The switching variable depends on the state and we will regularise the
definition given in @eq-lam-def as $ lambda = Lambda_(epsilon)[sigma(x)] $

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
i.e. when $x_t in cal(D)$.

Due to the dependent on the stochastic state variable $lambda$ is itself a
stochastic variable since it dependends on $x_t$ via $lambda_t =
Lambda_(epsilon)[sigma(x_t)]$, and, like its deterministic counterpart is
dynamic on the the much faster timescale $cal(O)(1\/epsilon)$. However, one
cannot simply employ Itō's Lemma on $lambda_t = Lambda_(epsilon)[sigma(x_t)]$ as
the latter is not a smooth function of $x_t$. Instead to study the dynamics of
$lambda_t$ we must first introduce two new concepts: local time of a
semi-martingale and the Meyer-Itō Theorem.

Local time of a semi-martingale $x_t$, denoted with $L^x_t (z)$is a measure of
the "visits" of the process on a given value $z$ for times up to $t$. It is
given via Tanaka's formula which we summarise in the following definition.

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
  @eq-ito-meyer is a Lebesgue-Stieltjes integral.
]<thm-ito-meyer>


#todo[ For $f in C^2$, the local time integral vanishes and @eq-ito-meyer
reduces to Itō's lemma. The local time $L^x_t(z)$ can equivalently be defined as

$
  L_t^x (z) = lim_(delta arrow.b 0) 1/(2delta) integral_0^t bb(1)_((z - delta, z + delta))(x_s) dif chevron.l x chevron.r_s,
$<eq-local-time-def>
which measures the accumulated quadratic variation of $x_t$ at level $z$.
 - convex functions of martingales are themselves martingales
 -  generalises itos theorem
   - add example about how it reduces to the standard itos lemma when $f$ has a second derivative
]


In order to apply @thm-ito-meyer to $lambda_t = Lambda_(epsilon)[sigma(x_t)]$, we
must first express the regulariser as a difference of convex functions which is
easily done as the following lemma shows.


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

where $delta(u)$ is the Dirac-delta distribution. Since $Lambda_epsilon$ is DC,
its left derivative and distributional second derivative are well-defined, which
we require for Meyer-Itō. For the latter, also need the dynamics of scalar
observable $sigma(x_t)$ which we state in the following lemma.

#lemma(title: [SDE for $z_t = sigma(x_t)$])[
  Let $lambda in [-1, 1]$, $sigma in C^2(RR^d, RR)$, $x_t$ be an Itō process
  according to @eq-ito-sde, supplemented by the regularity conditions in @def-ns-gen-sde and @lem-coeffs-lam
  then the random variable $z_t = sigma(x_t)$ evolves according to the SDE

  $
    dif z_t = tilde(a)_(alpha, epsilon)(t, x_t, lambda_t) dif t + sqrt(epsilon)tilde(b)(t, x_t, lambda_t) dif W_t,
  $<eq-z-sde>
  where
  $
    tilde(a)_(alpha, epsilon)(t, x, lambda) eqdef partial_x sigma(x)^(tns) [a(t, x, lambda) + alpha epsilon c(t, x, lambda)]
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

  then substitute for $dif x_t$ from @eq-ito-sde into @eq-dz-ito-lemma and
  apply Itō's product rule.

]


We are now in a position to consider the dynamics of the switching variable as
an SDE. By combining @thm-ito-meyer with @lem-z-sde we obtain the dynamics of
the switching variable. The dynamics of the switching variable, much like its
deterministic counterpart, operates on the faster timescale $cal(O)(1\/epsilon)$.
Unlike the deterministic case, however, the SDE contains local time terms at the
boundaries $lambda = pm 1$ which enforce reflection and keep $lambda_t$ in the
interval $[-1, 1]$. We formulate this precisely in the following theorem.

#theorem(title: [SDE for the switching variable $lambda$])[ 

  Let $epsilon>0$, $sigma in C^2[RR^d, RR]$ such that $cal(D)_(epsilon) = {x in
  RR^d | sigma(x) <= epsilon}$ is close set, let $x_t in cal(D)_epsilon$ evolve
  according to according to @eq-ito-sde, and let $Lambda_epsilon (u)$ be a be a
  family of regularisers of the sign function as defined in @eq-big-lam-def. then
  the switching variable $lambda_t = Lambda_epsilon [sigma(x_t)]$ evolves in the
  interval interval [-1, 1] according to the SDE
  $
    dif lambda_t &= 1/epsilon bb(1)_((-epsilon, epsilon])[sigma(x_t)] tilde(a)_(alpha, epsilon)(t, x_t, lambda_t) dif t
    + 1/sqrt(epsilon) bb(1)_((-epsilon, epsilon])[sigma(x_t)] tilde(b)(t, x_t, lambda_t) dif W_t \
      &+ 1/epsilon [dif L_t^(z)(-epsilon) - dif L_t^(z)(epsilon)]
    , \
  $<eq-lam-sde>

  where $tilde(a)_(alpha, epsilon)(t, x, lambda)$ and $tilde(b)(t, x, lambda)$ are defined in
  @eq-a-tilde-def and @eq-b-tilde-def respectively, $dif L_t^(z)(pm epsilon)$ is the
  change in the local time of $z_t$ at $z = pm epsilon$ where the evolution of
  $z_t$ is given by @eq-z-sde. 

]<thm-lam-sde>

#proof[Since $Lambda_epsilon (u)$ is a difference of convex functions, whose left
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
    lambda_t &= mu + integral_0^t 1/epsilon bb(1)_((-epsilon, epsilon])[sigma(x_s)] tilde(a)_(alpha, epsilon)(s, x_s, lambda_s) dif s
    + 1/sqrt(epsilon) integral_0^t bb(1)_((-epsilon, epsilon])[sigma(x)] tilde(b)(s, x_s, lambda_s) dif W_s \
      &+ 1/epsilon [ L_t^(z)(-epsilon) -  L_t^(z)(epsilon)].
$<eq-lam-sde-meyer-ito-full>
]

The local time terms in @eq-lam-sde enforce reflection at $lambda = pm 1$, or
analogously when $z_t$ reaches $pm epsilon$, the switching variable saturates and
the local time increments prevent escape from $[-1, 1]$. The dynamics of the
full system are then represented by a coupled SDE

#corollary(title: [Coupled slow-fast dynamics])[
  Under the conditions of @thm-lam-sde, the pair $(t, x_t, lambda_t)$ with $x_t in cal(D)_epsilon$ and $lambda_t = Lambda_(epsilon)(sigma(x_t)) in [-1, 1]$, satisfies the coupled system
$
  dif x_t &= a_(alpha, epsilon)(t, x_t, lambda_t) dif t
  + sqrt(epsilon) b(t, x_t, lambda_t) dif W_t, \
  dif lambda_t &= 1/epsilon bb(1)_((-epsilon, epsilon])[sigma(x_t)] tilde(a)_(alpha, epsilon)(t, x_t, lambda_t) dif t
  + 1/sqrt(epsilon) bb(1)_((-epsilon, epsilon])[sigma(x_t)] tilde(b)(t, x_t, lambda_t) dif W_t \
    &+ 1/epsilon [dif L_t^(z)(-epsilon) - dif L_t^(z)(epsilon)]
    , 
$<eq-x-lam-sde-pair>
  where $a_(alpha, epsilon)(t, x, lambda)$ and $b(t, x, lambda)$ are defined, respectively, in
 @eq-a-al-ep-def and @eq-b-def, while $tilde(a)_(alpha, epsilon)(t, x, lambda)$ and $tilde(b)(x,
lambda)$ are given defined in @eq-a-tilde-def and @eq-b-tilde-def respectively.
] <cor-coupled-sde>



The coupled system is a slow-fast stochastic system, and our goal is to obtain
controlled approximation for the dynamics of the slow process by closing the
dynamics of the switching variable $lambda_t$. Before we study that let us
recapitulate the various coefficients that we have discussed: $a^pm (t, x)$ and
$b^pm (t, x)$ are the drift and noise coefficients for the SDE on either side of
the discontinuity from @def-ns-gen-sde; $a(t, x, lambda)$ and $b(t, x, lambda)$
are the convex combination of the drift and noise coefficients defined in
@eq-a-def and @eq-b-def respectively; $a_(alpha, epsilon)(t, x, lambda)$ is
the full drift for the Itō SDE including the spurious drift, $tilde(a)_(alpha,
epsilon)(t, x, lambda)$ and $tilde(b)(t, x, lambda)$ are the convex combination
of the drift and noise coefficients projected onto the (unscaled) normal of the
discontinuity set and are defined in @eq-a-tilde-def and @eq-b-tilde-def
respectively; lastly it is useful to define the projected diffusion coefficient

$
  tilde(d)(t, x, lambda) eqdef tilde(b)(t, x, lambda) tilde(b)(t, x, lambda)^tns
  = partial_x sigma(x)^tns b(t, x, lambda) b(t, x, lambda)^tns partial_x sigma(x).
$<eq-d-tilde-def>


Despite the switching variable being random, because it is bounded, $lambda_t in
[-1, 1]$, all of these coefficients inherit the conditions of $a$ and $b$ as
laid out in @def-ns-gen-sde. We summerise these in the following lemma as they will
then be used in the later results.




#lemma(title: [Bounds on the coefficients])[
  Let $x in cal(D)_epsilon$ and $lambda in [-1, 1]$. Suppose the coefficients
  $a^pm$ and $b^pm$ satisfy assumptions (A1)–(A4) of @def-ns-gen-sde, and let
  $a(t, x, lambda)$ and $b(t, x, lambda)$ be the convex combinations defined in
  @eq-a-def and @eq-b-def, and let $tilde(b)(t, x, lambda)$ and $tilde(d)(x,
  lambda)$ be projected coefficients defined in @eq-b-tilde-def and
  @eq-d-tilde-def. Then there exist constants $C, K, tilde(M) > 0$,
  independent of $lambda$, such that the following bounds hold.

  1. Linear growth. For any $x in bb(R)^2$ and $t in[0, T]$,
  $
    ||a_()(t, x, lambda)|| + ||b(t, x, lambda)|| <= C (1 + ||x||).
  $<eq-ab-lin-growth-bound>
  2. Lipschitz continuity. For any $x, y in RR^d$ and any $t, s in [0, T]$,
  $
    ||a(t, x, lambda) - a(s, y, lambda)||
    + ||b(t, x, lambda) - b(s, y, lambda)|| <= K ||x - y|| + K_T |t - s|.
  $<eq-ab-lip-bound>

  3. Transversality. For any $x$ in $cal(D)_(epsilon)$ and $lambda in [-1, 1]$
     $
       || tilde(b)(t, x, lambda) || >= tilde(M) > 0, quad || tilde(d)(t, x, lambda) || >= tilde(M)^2 >  0.
     $<eq-ab-trans-bound>

]<lem-coeff-tilde-bounds>

#proof[

  1. Linear growth. Using the definition of combined coefficients $a$  and the triangle
     inequality we obtain

    $
        ||a(t, x, lambda)|| 
        &<= 1/2 (1 + lambda)  ||a_+ (t, x)||  + 1/2 (1 - lambda)  ||a_- (t, x)|| , \
        &<= 1/2  [(1 + lambda) C_+ + (1 - lambda) C_- ]  (1 + |x|),  \
          &<= (C_+ + C_- )(1 + ||x||) .  
    $
     Carrying out the same steps for $b$ and combining them gives bound for $b$
     is carried in the same manner gives the linear growth bound in the lemma
     where $C = C_+ + C_-$

  2. Lipschitz continuity. For any $x, y in RR^d$ and any $t, s in [0, T]$ we
     have by the triangle inequality $

       ||a(t, x, lambda) - a(s, y, lambda)|| &<= 1/2 (1 + lambda) ||a^+(t, x) - a^+(s, y)|| \ &+ 1/2 (1 - lambda) ||a^-(t, x) - a^-(s, y)||, \
      &<= 1/2 ||x - y|| [(1 + lambda) K^+  + (1 - lambda)K^+ ], \ &+ 1/2 |t - s| [(1 + lambda) K^+_T  + (1 - lambda)K^+_T ] \
      &<=  ||x - y|| ( K^+  + K^- ) + |t - s| ( K^+_T  + K^-_T ). $

     Again carrying out the same procedure for $b$ and combining them gives the
     condition in the lemma where $K_T = K^+_T + K^-_T $ and $K = K^+ + K^-$.

  3. Transversality. From the definition of $tilde(b)(t, x, lambda)$ we have $
    || tilde(b)(t, x, lambda) || &= ||partial_x sigma(x)^tns b(t, x, lambda)|| ,\
      &= lr(||1/2 partial_x sigma(x)^tns [(1 + lambda)b^+ (t, x) + (1 - lambda)b^- (t, x)]||) ,\ 
      &>= 1/2 [(1 + lambda) M^+ + (1 - lambda)M^-] ,\ 
      &>= min(M^+, M^-). $
    Setting $tilde(M) = min(M^+, M^-)$ gives the bound in the lemma while taking
     the square gives the bound on the projected diffusion coefficient.

]

The linear growth conditions also @lem-coeff-tilde-bounds imply finite
polynomial moments which we summerise in the following lemma.

#lemma(title: [Bounds on second moments])[ Let $x_t$ evolve according to
  @eq-x-lam-sde-pair, let $x_0 in RR^d$ be some initial condition, let $t in [0,
  T]$, then there exists a constant $C_(2, T) > 0$ such that

  $
    EE[ ||x_t||^2] <= C_(2, T) (1 + ||x_0||^2).
  $

  #todo[
    Maybe we can show by induction for all $k$.
  ]
]<lem-poly-mom-bound>


#proof[
By setting $f(x) = ||x||^2 = sum_i (x^((i)))^2$ and applying Itō's lemma to @eq-ito-sde-simp,
  $
    dif ||x_t||^2 &= sum_i 2 x^((i))_t dif x^((i))_t + sum_i dif chevron.l x^((i)) chevron.r_t \
      &= 2 x_t^tns [a_(alpha, epsilon)(t, x_t, lambda_t) dif t + sqrt(epsilon) b(t, x_t, lambda_t) dif W_t] \
      &quad + epsilon sum_(i,j) |b_(i j)(t, x_t, lambda_t)|^2 dif t \
      &= 2 x_t^tns a_(alpha, epsilon)(t, x_t, lambda_t) dif t + 2 sqrt(epsilon) x_t^tns b(t, x_t, lambda_t) dif W_t \
      &quad + epsilon ||b(t, x_t, lambda_t)||^2 dif t.
  $
  
  Taking expectations, the stochastic integral vanishes and we obtain
  $
    dif / (dif t) EE[ ||x_t||^2] = 2 EE[x_t^tns a_(alpha, epsilon)(t, x_t, lambda_t)] + epsilon EE[ ||b(t, x_t, lambda_t)||^2].
  $
  
  For the drift term, by Cauchy-Schwarz and the linear growth bound @eq-ab-lin-growth-bound,
  $
    EE[x_t^tns a_(alpha, epsilon)(t, x_t, lambda_t)] &<= EE[ ||x_t|| dot ||a_(alpha, epsilon)(t, x_t, lambda_t)||] \
      &<= C_(alpha) EE[ ||x_t|| (1 + ||x_t||)] \
      &= C_(alpha) EE[ ||x_t||] + C_(alpha) EE[ ||x_t||^2] \
      &<= C_(alpha) (1 + EE[ ||x_t||^2]).
  $
  where in the last step we used $EE[ ||x_t||] <= 1 + EE[ ||x_t||^2]$ which follows from $a <= 1 + a^2$ for $a >= 0$.
  
  For the diffusion term, again by linear growth,
  $
    EE[ ||b(t, x_t, lambda_t)||^2] &<= C^2 EE[(1 + ||x_t||)^2] \
      &= C^2 EE[1 + 2||x_t|| + ||x_t||^2] \
      &<= C^2 (3 + 3 EE[ ||x_t||^2]),
  $
  again by using $2a <= 1 + a^2$. Combining both estimates,
  $
    dif / (dif t) EE[ ||x_t||^2] &<= 2 C_(alpha) (1 + EE[ ||x_t||^2]) + 3 epsilon C^2 (1 + EE[ ||x_t||^2]) \
      &= (2 C_(alpha) + 3 epsilon C^2)(1 + EE[||x_t||^2]) \
      &<= C'(1 + EE[ ||x_t||^2]),
  $
  with $C' = 2 C_(alpha) + 3 C^2$ (taking $epsilon <= 1$). Finally, employing  
  Gronwall's inequality we obtain
  $
    EE[ ||x_t||^2 ] <= (||x_0||^2 + C' T) ee^(C' T) eqdef C_(2,T)(1 + ||x_0||^2).
  $
]




The moment bound ensure that all coefficients remain sufficiently controlled on $[0, T]$,
which we require for the intermediate timescale estimates.

= The intermediate timescale <sec-delta-time>

From @eq-lam-sde, the switching variable evolves on timescale
$cal(O)(1\/epsilon)$ while $x_t$ evolves on $cal(O)(1)$. The standard approach
would be to rescale time and take $epsilon -> 0$. We show this fails and
introduce an intermediate timescale $delta$ with $epsilon << delta << 1$ to
resolve the difficulty.

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
of the problem yields multiple objections concerning the mathematical subtleties
in the scaling, the physical interpretation, and the analysis of the original
problem given in @def-ns-gen-sde, that must be addressed individually.

=== Objection I: Incompatible scaling of the dynamics.

Before we attempt to rescale time we must first clarify the $epsilon$-order of
local time terms in @eq-lam-sde, which we do in the following lemma.

#lemma(title: [Scaling of local time terms])[
  Let $z_t$ be a stochastic processes defined in @eq-z-sde with quadratic variation $dif chevron.l z
  chevron.r_t = epsilon tilde(b)(t, x_t, lambda_t) tilde(b)(t, x_t, lambda_t)^(tns)
  dif t$. Then

  $
    dif EE[L^z_t (a)] = epsilon P^((z))(a, t) tilde(b)(t, x_t, lambda_t)tilde(b)(t, x_t, lambda_t)^(tns) dif t,
  $<eq-diff-ee-lt>
  where $P^((z))(a, t)$ denotes the density of $z_t$ at level $a$.
]<lem-local-time-scaling>

#proof[Taking the expectation of @eq-local-time-def,

$
  EE[L^z_t (a)] &= EE[ lim_(delta arrow.b 0) 1/(2delta) integral_0^t bb(1)_((a - delta, a + delta)) (z_s) dif chevron.l  z chevron.r_s], \
    &= lim_(delta arrow.b 0) 1/(2delta) integral_0^t EE[bb(1)_((a - delta, a + delta))   dif chevron.l  z chevron.r_s] \ 
    &= lim_(delta arrow.b 0) 1/(2delta)  integral_0^t epsilon EE[bb(1)_((a - delta, a + delta))tilde(b)(s, x_s, lambda_s) tilde(b)(s, x_s, lambda_s)^(tns) ]  dif s, \ 
    &=   epsilon  integral_0^t lim_(delta arrow.b 0) 1/(2delta)
    integral_(a - delta)^(a + delta) P^((z))(a, s) tilde(b) (x_s,  lambda_s) tilde(b)(x_s,  lambda_s)^(tns) dif s, \
    &=  epsilon integral_0^t P^((z))(a, s) tilde(b) (x_s,  lambda_s) tilde(b)(x_s,  lambda_s)^(tns) dif s, #<eq-local-time-exp-z-int>\
$
  
  The result follows by differentiation.
]

Having established the scaling of the local time terms, we now show that
no time rescaling can balance all contributions to the
$lambda$-dynamics in the following lemma.

#lemma(title: [Incompatibility of scaling])[

  Let $t = epsilon^beta tau$ for $beta > 0$. Under this rescaling, the terms in
  @eq-lam-sde scale as:

  $
    "Drift:" &bb(1)_((-epsilon, epsilon])[sigma(x_(tau))]
    tilde(a)_(alpha, epsilon)(x_tau, lambda_tau) dif tau, quad &&cal(O)(epsilon^(beta - 1)), \
  "Martingale:" &bb(1)_((-epsilon, epsilon])[sigma(x_(tau))]
  tilde(b)(x_tau, lambda_tau) dif W_tau, quad &&cal(O)(epsilon^((beta - 1)/2)), \
  "Local time:" &dif L_(tau)^(z)(a), quad &&cal(O)(epsilon^beta).
  $
  No choice of $beta > 0$ brings all three contributions to the same order as $epsilon -> 0$.
]<lem-scaling-incompatibility>

#proof[The rescaling gives $dif t = epsilon^beta dif tau$ and $dif W_t = epsilon^(beta/2) dif W_tau$. The drift term in @eq-lam-sde carries a factor $epsilon^(-1)$ from the layer dynamics, yielding order $epsilon^(beta-1)$. The martingale term similarly yields order $epsilon^((beta-1)/2)$. By @lem-local-time-scaling, the local time contribution is $cal(O)(epsilon)$ in original time, hence $cal(O)(epsilon^(beta+1))$ after rescaling.

  The naive choice $beta = 1$ places drift and martingale at $cal(O)(1)$, but
the local time term becomes $cal(O)(epsilon^2)$ and vanishes in the limit.
Balancing drift and local time requires $beta - 1 = beta + 1$, which has no
solution. Balancing martingale and local time requires $(beta-1)/2 = beta + 1$,
  giving $beta = -3$, which violates $beta > 0$.
]

This presents a fundamental technical hurdle: the three contributions to the
dynamics of $lambda$ operate on incompatible scales. Any rescaling followed by
$epsilon -> 0$ necessarily discards at least one contribution.

=== Objection II: Loss of physical interpretation.

Even granting mathematical well-posedness, the $epsilon -> 0$ limit produces an
object whose physical meaning has degenerated. As $epsilon -> 0$:

+ The layer $cal(D)_epsilon = {x in RR^d : |sigma(x)| <= epsilon}$ shrinks to
  the co-dimension-1 surface $cal(D) = {x : sigma(x) = 0}$.

+ The switching variable $lambda in [-1, 1]$ parametrises a convex interpolation
  between the vector fields $a^pm$ and noise coefficients $b^pm$. This
  interpolation only has meaning within the layer, where the dynamics
  transitions between the two regimes.

+ The stationary distribution $P_(ss)^epsilon (lambda | x)$ converges
  to some limiting distribution on $[-1, 1]$, but this limit lives on a domain
  whose connection to the original geometry has been lost.

In the deterministic case, the $epsilon -> 0$ limit yields a single value
$lambda^* (x)$, the Filippov sliding mode. The interpretation is clear:
$lambda^*$ selects the unique convex combination that keeps trajectories on the
discontinuity surface. In the stochastic setting, one retains a distribution
over $lambda$ rather than a single value, that is we furnish the differential
inclusion set with a probability measure, specifically a density distribution.
However, this but this distribution becomes detached from the layer on which
$lambda$ was defined.

=== Objection III: Incompatibility with weak-noise analysis.

The most fundamental objection concerns the purpose of the analysis. The
weak-noise framework treats $epsilon$ as the small parameter governing the
asymptotic expansion. For our purpose, the objects of interest are not only
typical paths obtained by minimising a hypothetical Freidlin-Wentzell action
functional, but the Gaussian fluctuations around the most probable path at order
$cal(O)(sqrt(epsilon))$. These phenomena are intrinsically $epsilon$-dependent.

The stationary distribution $P_(ss)(lambda | x)$ at finite $epsilon$ encodes how
noise selects among the continuum of Filippov solutions and determines the
fluctuation structure near the discontinuity. Taking $epsilon -> 0$ collapses
this to a deterministic Filippov vector field, eliminating precisely the
phenomena we set out to analyse. In other words, consistency demands that
$epsilon$ be preserved throughout the analysis, including in the treatment of
the fast variable.

== The intermediate timescale resolution
The preceding objections share a common source: they arise from taking $epsilon
-> 0$ in the layer dynamics. The resolution is to avoid this limit entirely. We
introduce an intermediate timescale $delta$ satisfying

$
  epsilon << delta << 1.
$<eq-delta-ordering>

At fixed $epsilon > 0$, all quantities remain well-defined:

- The layer $cal(D)_epsilon$ has finite width and the boundaries $pm epsilon$
  are well-separated

- The switching variable $lambda$ retains its meaning as parametrising the
  interpolation within a layer of finite width. The stationary distribution
  $P_(ss)(lambda | x)$ describes the steady-state density of $lambda$ within
  this layer for a fixed $x$.

- The parameter $epsilon$ appears throughout the dynamics, preserving the
  weak-noise structure required for fluctuation analysis.

The conditions on $delta$ encode a separation of timescales. The condition
$delta >> epsilon$ ensures the fast variable $lambda$ equilibrates to
$P_(ss)(lambda | x)$ within the $delta$-window. The condition $delta << 1$
ensures the slow variable $x$ remains approximately frozen over this window. A
concrete realisation is $delta = epsilon^alpha$ for $alpha in (0, 1)$; the value
of $alpha$ does not affect the limiting dynamics provided the relevant estimates
hold uniformly.


= Estimates for the dynamics on the intermediate timescale <sec-est-on-del>

We proceed to establish the estimates that justify the timescale separation. On
the intermediate timescale $delta$ satisfying @eq-delta-ordering, the dynamics
of $x_t$ is frozen while the dynamics of $lambda_t$ equilibrates to a
steady-state distribution. Let us obtain estimates for the variation in the slow
variable $x_t$ on this time scale


#theorem(title: [Slow variation of $x_t$  in the $delta$-window])[

  Let $x_t in cal(D)_epsilon$ and $delta>0$ satisfying @eq-delta-ordering and
  $delta -> 0$ as $epsilon -> 0$, then

  $
    //EE[ sup_(0<=s<=delta) |x_(t+s) - x_t|^2 ] <= C (delta^2  + epsilon delta),  \
    PP[sup_(0<=s<=delta) |x_(t+s) - x_t| > gamma ] <= C/gamma^2 (delta^2  + epsilon delta)
        
  $<eq-slow-var-x>
  for some $C,gamma>0$.
]<thm-slow-var>

#proof[We start by bounding the squared deviation in the $delta$ time window,
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
  and for the martingale part we have  by Itō isometry
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

The consequence of @thm-slow-var becomes more apparent when we choose any
mesoscopic scale $delta(epsilon)$ satisfying @eq-delta-ordering, e.g.
$delta(epsilon) = epsilon^beta$, for some $beta>0$ and then letting $epsilon ->
0$. The bound in @eq-slow-var-x ensures that for any fixed $gamma$, 

$
  PP[sup_(0<=s<=delta(epsilon))|x_(t+s) - x_t| > gamma]
 <= C/gamma^2 [delta^2(epsilon) + epsilon delta(epsilon)]


  -> 0,
$

and therefore the slow variable $x_t$, with probability tending to one, remains
constant on the entire interval $[t, t + delta(epsilon)]$. Simultaneously, we have

$
  delta(epsilon)/epsilon  = epsilon^(beta-1) limits(->) oo, quad #text[as] epsilon -> 0,
$

for $beta in (0, 1)$, which shows that the $delta$-window is arbitrarily large
on the fast $lambda$–timescale. Thus, in $delta$-interval, the slow variable may
be regarded as fixed while the fast variable has sufficient time to equilibrate.


Having now established the error associated with fixing the value of $x$ to its
value at the start of $delta$-time window, we now turn to demonstrate the mixing
of the switching variable, $lambda$, for a fixed $x in cal(D)_epsilon$. We start
by obtaining relevant facts about the dynamics of the former. Using @eq-lam-sde
from @thm-lam-sde, we can easily obtain the backward generator as the following
lemma shows.


// Theorem IV.1 (Generator on the Constraint Manifold).
// Let f:S→Rf: \mathcal{S} \to \mathbb{R}
// f:S→R be a smooth function. The infinitesimal generator is:
// 
// $$\boxed{(\mathcal{A}f)(\lambda, z) = \begin{cases}
// \tilde{a}^-(z) \partial_z f + \dfrac{\epsilon \tilde{d}^-(z)}{2} \partial_z^2 f & z < -\epsilon \[4mm]
// \dfrac{\tilde{a}0(\lambda)}{\epsilon} \partial\lambda f + \dfrac{\tilde{d}0(\lambda)}{2\epsilon} \partial\lambda^2 f & |\lambda| < 1 \[4mm]
// \tilde{a}^+(z) \partial_z f + \dfrac{\epsilon \tilde{d}^+(z)}{2} \partial_z^2 f & z > \epsilon
// \end{cases}}$$

#drafting[
  = Fast Makrovian Dyanmics 

  Although the dyanamics of $lambda_t$ can be described by the SDE in @eq-lam-sde,
  it is not a Markovian process, and hence there  

  == Orthogonal decomposition

  Let 
  $
    nu eqdef K_(sigma) partial_x sigma(x), quad K_(sigma) quad 1/(||partial_x sigma(x)||), 
  $
  be the unit normal to the discontiniuty surface then for a given $x in
  RR^d$, there exists a scalar $zeta$, such that $epsilon zeta$ is the
  shortest distance from $x$ to the discontinuity surface defined as
  $
    zeta eqdef sigma(x)/(epsilon ||partial_x sigma(x)||) = z/(epsilon ||partial_x sigma(x)||)
  $
  We can then decompose any $x in RR^d$ as 
  $
    x = xi + epsilon  nu zeta
  $

  where $$ $xi$ is a vector on the discontinuity surface, i.e. $sigma(xi) = 0$.
  Using Ito's lemma, it is straight forward to obtain the the SDE 

  - Fix $xi in cal(D)$

  - Assume for all $x in cal(D)_("sliding")$, $t in [0, T]$, we have
    $tilde(a)_(a, epsilon)(t,x, lambda = pm 1) = mp C$ for some $C>0$, that is the sliding condition.

  Then we have 

  $
    dif zeta_t = 1/epsilon tilde(a)_(alpha, epsilon)(t, xi_t, epsilon zeta_t, lambda_t) dif t
    + 1/sqrt(epsilon) tilde(b)(t, xi_t, epsilon zeta_t, lambda_t) dif W_t,
  $<eq-zeta-sde>

  $
    dif lambda_t &= 1/epsilon bb(1)_((-epsilon, epsilon])[sigma(x_t)]
    tilde(a)_(alpha, epsilon)(t, xi_t, epsilon zeta_t, lambda_t) dif t
    + 1/sqrt(epsilon) bb(1)_((-epsilon, epsilon])[sigma(x_t)]
    tilde(b)(t, xi_t, epsilon zeta_t, lambda_t) dif W_t \
      &+ 1/epsilon [dif L_t^(zeta)(-K_(sigma)) - dif L_t^(zeta)(K_(sigma))]
    , \
  $<eq-lam-sde-a>

  The proces $(lambda_t, zeta_t) in cal(S) eqdef [-1, 1] times RR$ is Markovian
  and takes place on an embedded 1D manifold.


  #figure(
    caption: [1D manifold embedded in $RR times [-1, 1]$],
    cetz.canvas({
      import cetz.draw: *
      set-style(
        stroke: 1.0pt,
        grid: (
          stroke: gray + 0.2pt,
          step: 0.5
        ),
        mark: (
          transform-shape: false,
          fill: black
        )
      )

      line((-5, 0), (5, 0), mark: (end: "stealth", start: "stealth"))
      content((), anchor: "north", $zeta$)
      line((0, -2), (0, 2), mark: (end: "stealth", start: "stealth"))
      content((), anchor: "east", $lambda$)

      line((-5, -1), (-1, -1), (1, 1), (5, 1))


    })
  )


  Let $f: cal(S) mapsto R$
// f:S→R be a smooth function. The infinitesimal generator is:
// 

  
  
// $$\boxed{(\mathcal{A}f)(\lambda, z) = \begin{cases}
// \tilde{a}^-(z) \partial_z f + \dfrac{\epsilon \tilde{d}^-(z)}{2} \partial_z^2 f & z < -\epsilon \[4mm]
// \dfrac{\tilde{a}0(\lambda)}{\epsilon} \partial\lambda f + \dfrac{\tilde{d}0(\lambda)}{2\epsilon} \partial\lambda^2 f & |\lambda| < 1 \[4mm]
// \tilde{a}^+(z) \partial_z f + \dfrac{\epsilon \tilde{d}^+(z)}{2} \partial_z^2 f & z > \epsilon
// \end{cases}}$$
  

  - Prove that $zeta_t$ with $xi$ fixed, permits a an invariant measure

  Conisder the three regions $cal(R)^- = (-oo, -1]$, $cal(R)^bullet = (1, -1)$,
  and $cal(R)^+ = [1, oo)$, 


    $
      tilde(a)^(pm)_(alpha, epsilon) (t, xi, zeta) &eqdef
      tilde(a)^(+)_(alpha, epsilon) (t, x = xi + epsilon nu zeta, lambda = pm 1) \
      tilde(b)^(pm)_(alpha, epsilon) (t, xi, zeta) &eqdef
      tilde(b)^(+)_(alpha, epsilon) (t, x = xi + epsilon nu zeta, lambda = pm 1) \
      tilde(d)^(pm)_(alpha, epsilon) (t, xi, zeta) &eqdef
      tilde(d)^(+)_(alpha, epsilon) (t, x = xi + epsilon nu zeta, lambda = pm 1)
    $


  Rescale time $tau = t / epsilon$, then get the invariant measure, or simply
  for $t in [0, delta]$

  We define the following potential funcitons

  $
    Psi^(-)(zeta) &eqdef 2 integral_(-oo)^(zeta) (tilde(a)^(-)_(alpha, epsilon) (t, xi, zeta'))/(tilde(d)^(-)_(alpha, epsilon) (t, xi, zeta')) dif zeta', quad &&zeta in (-oo, 1]\
    Psi^(bullet)(zeta) &eqdef 2 integral_(-1)^(zeta) (tilde(a)_(alpha, epsilon) (t, xi, zeta',  zeta'))/(tilde(d)_(alpha, epsilon) (t, xi, zeta', zeta')) dif zeta', quad &&zeta in (-1, 1) \
    Psi^(+)(zeta) &eqdef 2 integral_1^(zeta) (tilde(a)^(+)_(alpha, epsilon) (t, xi, zeta'))/(tilde(d)^(+)_(alpha, epsilon) (t, xi, zeta')) dif zeta', quad &&zeta in [1, oo)\ 
  $

  

  Then we have the invariant density

    $
      P^(oo)_(pm, xi)(zeta) = (C^(pm)(xi))/(tilde(d)^(pm)(t, xi, zeta))  ee^(Psi^(pm)(zeta))
    $

  


  #remark[We assume we start within the attracting basin of the sliding mode, in
    that sense we assume the coeffieints are local approximations which are
    extended to $pm oo$. As a result, given that $tilde(a)^+ < 0$ ensures that
    the $lim_(zeta -> inf) exp(Psi^+(zeta)) -> 0$. Similarl argument for
    $tilde(a)^-$, which ensures that the invariant density decays away from the
    discconiity.

  ]

  Matching Conditions:

  continuty of density (continuous sample paths $=>$ continuous density)

  $
    P^(oo)_(-, xi)(-1) &= P^(oo)_(bullet, xi)(-1)  &&=> C^(-) (xi) e^(Psi^(-)(-1))&&&= C^(bullet)(xi) \
    P^(oo)_(bullet, xi)(1) &= P^(oo)_(+, xi)(1)  &&=>  C^(bullet) (xi)e^(Psi^(bullet)(1)) &&& = C^(+)(xi) \
  $

  Let $C^-(xi) eqdef C(xi)$, then 

  $
    C^(bullet)(xi) = C(xi) ee^(Psi^(- )(-1)), quad C^(+)(xi) = C(xi) ee^(Psi^(-)(-1) + Psi^(bullet)(1))
  $

  Normalisation :

  $
    N^(-)(xi) &eqdef integral_(-oo)^(1) (ee^(Psi^(-)(zeta)))/(tilde(d)^(pm)(t, xi, zeta))  dif zeta, quad
    N^(bullet)(xi) &eqdef integral_(-1)^(1) (ee^(Psi^(bullet)(zeta)))/(tilde(d)^(bullet)(t, xi, zeta))  dif zeta,  quad
    N^(+)(xi) &eqdef integral_(1)^(oo) (ee^(Psi^(+)(zeta)))/(tilde(d)^(+)(t, xi, zeta))  dif zeta, 
  $

  Enforcing normalisation gives
  $
    C(xi) = (N^(-)(xi) + N^(bullet)(xi) ee^(Psi^-(-1)) + N^(+)(xi) ee^(Psi^-(-1) + Psi^+(1)))^(-1),
  $

  $
    P(zeta) =  Q^-(zeta) bb(1)_(cal(R)^-)(zeta)
    + Q^(bullet)(zeta) bb(1)_(cal(R)^bullet)(zeta)
    + Q^+(zeta) bb(1)_(cal(R)^+)(zeta)
  $

  We push forward the measure on $zeta$ to a measure $lambda$ under the mapping
  $lambda = Lambda_(epsilon)(epsilon K_sigma zeta)$

  $
    mu_(P^(oo)(lambda))(A) = rho^(+) bb(1)_(A)(1) + rho^(-) bb(1)_(A)(-1)
    + integral_(A)  P^(oo)_(bullet) (lambda) dif lambda
  $


  $
    rho_(+)(t) = integral_(-1)^(oo) P^(t)_(-) (t)
  $
  

  Exponential mixing
]




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


#proof[With $x_t = x$ fixed on the interval $t in [t' , t' + delta] subset [0, T]$
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
        tilde(a)_(alpha, epsilon)(t, x, lambda_s) dif s, \
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
        tilde(a)_(alpha, epsilon)(t, x, lambda), \
        lim_(t -> t') 1/t EE[II^((3))_t]
      = 1/(2 epsilon) partial^2_(lambda lambda) f(lambda)
        tilde(b)(t, x, lambda) tilde(b)(t, x, lambda)^(tns),
  $

  hence, the interior contribution to the generator is  the right-hand side
  of @eq-bwd-gen when we express $tilde(a)_(alpha, epsilon)$ and $tilde(b)$ in terms of $a$ and
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
  integral_-^1 P_(x)(t, lambda ) (cal(A)_x f_t ) (lambda) dif lambda = integral_-^1 (cal(A)^(*)_x P_(x))(t, lambda)  f_t (lambda) dif lambda , quad forall  f_t in dom(cal(A)_x)
$<eq-adjoint-def>

where $t in [t', t + delta] subset [0, T]$, $P_(x)(t, lambda)$ is the occupation
probability density of the switching variable $lambda$, and $cal(A)^*_x$ acts
only on the spatial argument of $P_x$. The forward generator is formulated in the
following lemma.

#lemma(title: [Forward generator of the switching variable])[
  Let $delta > 0$ satisfying @eq-delta-ordering, let $t in [t', t' + delta] subset
  [0, T]$ for some $t' in [0, T-delta]$. Let $x_t = x in cal(D)_epsilon$ be
  fixed (see @thm-slow-var). Then the forward generator $cal(A)^*_x$ of
  $lambda_t in [-1, 1]$ evolving according to @eq-lam-sde, acts on sufficently
  smooth probability density $P_x: [0, T] times [-1, 1] mapsto [0,  
 oo)$ via
  $
    (cal(A)^*_x P_x)(t, lambda) &= - 1/epsilon partial_lambda  ( 
      P_(x)(t, lambda) { partial_x sigma(x_t)^tns a(t, x, lambda)
    + epsilon/2 trc[b(t, x, lambda)^(tns) partial^2_(x x) sigma(x) b(t, x, lambda)] }) \ 
      &+ 1/(2 epsilon) partial^2_(lambda lambda)[P_(x)(t, lambda)
      partial_x sigma(x_t)^tns  b(t, x, lambda) b(t, x, lambda)^tns partial_x sigma(x_t)],
  $<eq-fwd-gen>
  with the domain 

  $
    dom(cal(A)_x^*) = {P_x in C^2([-1, 1]; [0, oo)) | J_t (pm 1) = 0},
  $<eq-dom-fwd-gen>
  where

  $
    J_t (lambda) &= 
P_x (t, lambda) { partial_x sigma(x_t)^tns a(t, x, lambda)
    + epsilon/2 trc[b(t, x, lambda)^(tns) partial^2_(x x) sigma(x) b(t, x, lambda)] } \ 
      &- 1/(2 ) partial_(lambda)[P_x (t, lambda)
      partial_x sigma(x_t)^tns  b(t, x, lambda) b(t, x, lambda)^tns partial_x sigma(x_t)],
  $

  is the scaled probability current.
]<lem-fwd-gen>


#proof[Let $P_(x)(t, lambda)$ denote the occupation density of the switching variable
  $lambda_t in [-1, 1]$, conditioned on a frozen value of $x_t = x in
  cal(D)_(epsilon)$. Inserting the backward generator from @lem-bwd-gen into 
  @eq-adjoint-def we obtain 


  $
    1/epsilon   integral_(-1)^1  P_(x)(t, lambda) 
    partial_lambda
    f(lambda) tilde(a)_(alpha, epsilon)(t, x, lambda) + 1/(2 epsilon) integral_(-1)^(1)
    P_(x)(t, lambda) partial^2_(lambda lambda)f(lambda)  tilde(b)(t, x, lambda) tilde(b)(t, x, lambda)^(tns) \
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
      P_(x)(t, lambda) tilde(a)_(alpha, epsilon)(t, x, lambda) dif lambda
      &= lr(1/epsilon f(lambda)
        P_(x)(t, lambda) tilde(a)_(alpha, epsilon)(t, x, lambda)|)_(-1)^1 \
      &- 1/epsilon integral_(-1)^1
        f(lambda)
        partial_lambda [
          P_(x)(t, lambda) tilde(a)_(alpha, epsilon)(t, x, lambda)
        ] dif lambda,
  $<eq-forward-drift>

  Diffusion contribution. Employing intrgration by parts twice yields
  $
    1/(2epsilon) integral_(-1)^1 partial^2_(lambda lambda) f(lambda)
      P_(x)(t, lambda) tilde(b)
      tilde(b)^(tns) dif lambda  &= 
       lr(1/(2epsilon)
        partial_lambda f(lambda)
        lr([
          P_(x)(t, lambda) tilde(b) tilde(b)^(tns)
           ])
      - 1/(2epsilon) f(lambda)
        partial_lambda [
          P_(x)(t, lambda) tilde(b) tilde(b)^(tns)
        ] |)_(-1)^1  \
      &+ 1/(2epsilon) integral_(-1)^1
        f(lambda) partial^2_(lambda lambda)
        [
         P_(x)(t, lambda) tilde(b) tilde(b)^(tns)
        ] dif lambda,
  $<eq-forward-diffusion>

  where the arguments $(t, x, lambda)$ are dropped in the notation of $tilde(a)_(alpha, epsilon)$
  and $tilde(b)$ for clarity. Since $f in dom(cal(A)) = { f in C^2([-1, 1]) |
  partial_lambda f(pm 1)=0 }$, all boundary terms proportional to
  $partial_lambda f(pm 1)$ vanish. The remaining boundary terms must also vanish
  to respect conservation of probability (i.e. zero probability flux through
  $lambda = pm 1$), giving the boundary condtion

  $
    lr({  P_(x)(t, lambda) tilde(a)_(alpha, epsilon)(x_t, lambda)
      - 1/2 partial_lambda [ P_(x)(t, lambda) tilde(b)(t, x, lambda) tilde(b)(t, x, lambda)^(tns)
]
 })_(-1)^1 = 0.
  $<eq-forward-zero-flux>

  Using #Cref(supplement: "Eqs.")[@eq-forward-drift @eq-forward-diffusion], and enforcing
  @eq-forward-zero-flux, we identify the forward operator as
  $
    (cal(A)^*_x P_(x))(t, lambda)
      = - 1/epsilon partial_lambda [
        P_(x)(t, lambda) tilde(a)_(alpha, epsilon)(t, x, lambda)
        ]
        + 1/(2epsilon) partial^2_(lambda lambda) [
          P_(x)(t, lambda) tilde(b)(t, x, lambda)
                       tilde(b)(t, x, lambda)^(tns)
        ].
  $<eq-forward-generator-final>

  Substituting the definitions in #Cref(supplement: "Eqs.")[@eq-a-tilde-def
  @eq-b-tilde-def] yields the defnition give in the lemma.

]

The operator $cal(A)^*_x$ is also called the Fokker–Planck or Kolmogorov forward
operator associated with the dynamics of the switching variable $lambda_t$, in
our case, however, it is conditional on $x_t = x in cal(D)_(epsilon)$. 


#remark[Since $P_t (lambda)$ is the one dimensional occupation probability density
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


#remark[Notice that our transversality condition from @def-ns-gen-sde, and subsequently
  the in @lem-coeff-tilde-bounds will give rise to ellipticity condition for the
  Fokker-Planck equation.
]


== Bounds on the probability density of the switching variable

With the forward generator in hand, we turn to quantitative estimates on the
density of the switching variable. Remember that our goal is to establish mixing
of the dynamics of $lambda$, for which we require various estimates for
$P_(x)(t, lambda)$.


Since we allow for the fact that at $t=0$ we allow a Dirac-delta initial
condition, i.e. $P_(x)(0, lambda) = delta(lambda - Lambda_(epsilon)(sigma(x_0)))$ where $x_0$ is
the initial condition of $x_t$, we must show that despite such initial
conditions, $P_(x)(t, lambda)$ becomes instantaneously smooth for any $t>0$. In
our case the ellipticity of the generator, which is guarrenteed by the
transversality condition in @def-ns-gen-sde, is key to show the instantaneous
smoothing as we show via the following lemma.


#lemma(title: [Instantaneous smoothing of the switching variable density])[
  Let $x in cal(D)_(epsilon)$ fixed, and let $lambda_t$ ​ evolve according to the
  SDE with forward generator $cal(A)^*_x$ given by @lem-fwd-gen, let $rho_(x)(t,
  lambda giv mu)$ with $mu in [-1, 1]$ be the Greens's function solution to
  $partial_t rho_(x) = cal(A)^*_x rho_x$ with the localised intial condition and let
  the diffusion coefficient satisfy

  $
    0 < C_1 (x) <= tilde(d)(t,x,lambda)<= C_2 (x).
  $Then for any $t in (0, T]$, the Greens's function satifies

  $
    (R_("L")(x))/sqrt(t) exp[-(C'_(1)(x)(lambda -mu)^2 )/ t]
    <= rho_(x)(t, lambda giv  mu)
    <= (R_("U")(x))/ sqrt(t) exp[-(C'_(2)(x)(lambda - mu)^2 )/ t],
  $

  where $R_"L"$, $R_"U"$, $C'_(1)$ and $C'_(2)$ constants that depends on $x$, upper
  and lower bounds for the diffusion coefficient which are dependnet on $x$ but
  uniform in $lambda$ and $T$.

]<lem-lam-smooth-denst>

#proof[

  The proof is a direct consequence from Aronson, Theorem 7 in @aronson1968 (see
  also @aronson1967). Let us work directly with the Fokker-Planck equation,
  using the definition of $cal(A)^*_x$ in @lem-fwd-gen we have
  $
    partial_t rho_(x)(t, lambda giv mu)
    = -1/epsilon partial_lambda [tilde(a)_(alpha, epsilon)(t, x, lambda) rho_(x)(t, lambda giv mu)] + 1/(2 epsilon) partial^2_(lambda lambda) [tilde(d)(t, x, lambda) rho_(x)(t, lambda giv  mu )],
  $
  with the Dirac-delta initial conditions $rho_(x)(0, lambda giv mu) =
  delta(lambda - mu)$ and where $tilde(a)_(alpha, epsilon)$ and $tilde(d)$ are defined in
  @eq-a-tilde-def and @eq-d-tilde-def respectively. Recasting in the divergence
  form we have
  $
    partial_t rho_(x)(t, lambda giv  mu)  = partial_(lambda)
    [A(t, x, lambda) rho_(x)(t, lambda giv mu)
    + B(t, x, lambda) partial_(lambda)rho_(x)(t, lambda giv mu) ],
  $

  where

  $
    A(t, x, lambda) = 1/epsilon [1/2 partial_lambda tilde(d)(t, x, lambda) - tilde(a)_(alpha, epsilon)(t, x, lambda)], quad
    B(t, x, lambda) = 1/(2 epsilon) tilde(d)(t, x, lambda).
  $

  In order to apply Aronson's result we must show that both $A$ and $B$ are
  bounded and that there exists a constant $nu >= 1$ such that the inequality
  $
    1/nu <= ||B(t, x, lambda)|| <= nu,
  $<eq-aronson-cond>

  is satisfied. Since we know that that $a_(alpha, epsilon)$ is bounded via
  linear growth bound in @lem-a-spur-bound, to bound $A$ we need only show that
  $partial_(lambda)tilde(d)$ is bounded. We have from the definition in
  @eq-d-tilde-def
  $
    partial_(lambda)tilde(d)(t, x, lambda)   &=
    partial_x sigma(x)^tns {partial_(lambda)[ b(t, x, lambda) b(t, x, lambda)^tns]} partial_x sigma(x) \
      &= 1/2 partial_x sigma(x)^tns ( [b^+(t, x) - b^-(t, x)]b(t, x, lambda)^(tns)  \
          &+
          b(t, x, lambda) [b^+(t, x) - b^-(t, x)]^(tns)  )
          partial_x sigma(x).
  $
  Taking the norm of both sides yeilds yeilds
  $
    || partial_(lambda)tilde(d)(t, x, lambda)||
      &<= ||partial_x sigma(x)||^2dot ||b(t, x, lambda)||dot||b^+(t, x) - b^-(t, x)|| \
      &<= ||partial_x sigma(x)||^2dot ||b(t, x, lambda)||dot||b^+(t, x) + b^-(t, x)||  \
      &<=  C' ||partial_x sigma(x)||^2 (1 + ||x||^2),
  $

  for some $C' > 0$, and where we have used @lem-coeff-tilde-bounds, and $||x||
  <= 1 + ||x||^2$. Moreover, since we know that $tilde(d)(t, x, lambda) <= 2 C^2
  (1 + ||x||^2)$ and $tilde(d)(t, x, lambda) >= tilde(M)^2 > 0$ by the linear
  growth bound and the transversality condition in @lem-coeff-tilde-bounds, we
  can satisfy @eq-aronson-cond by defining

  $
    nu(x) eqdef max[2 epsilon\/ tilde(M)^2, 2 C^2 (1 + ||x||^2), 1].
  $

]

While @lem-lam-smooth-denst gives us estimates for the transient density, we
also require the invariant density, that is quasi-stationary density obtained by
fixing $x$ to be static whilst allowing $lambda$ to be dynamics. We can compute
this invariant density explicitly.

#lemma(title: [Invariant density of the switching variable])[
  Let $x in cal(D)_epsilon$ be fixed, and let $tilde(a)_(alpha, epsilon)(x, lambda)
  eqdef tilde(a)_(alpha, epsilon)(t, x, lambda)|_(t = t_0)$ and $tilde(d)(x, lambda)
  eqdef tilde(d)(t, x, lambda)|_(t = t_0)$ denote the coefficients evaluated at
  some fixed time $t_0 in [0, T]$. Let the forward generator $cal(A)^*_x$ of
  $lambda_t$ be defined in @eq-fwd-gen with these frozen coefficients. Then the
  invariant measure $P_(ss)(lambda | x)$ satisfying $(cal(A)^*_x P_(ss))(lambda) = 0$
  is given by
  $
    P_(ss)(lambda | x) = (R(x)) / (tilde(d)(x, lambda))
    exp(2 integral_(-1)^(lambda) (tilde(a)_(alpha, epsilon)(x, nu)) / (tilde(d)(x, nu)) dif nu),
  $
  where
  $
    R(x) = [integral_(-1)^(1) 1 / (tilde(d)(x, lambda))
    exp(2 integral_(-1)^(lambda) (tilde(a)_(alpha, epsilon)(x, nu)) / (tilde(d)(x, nu)) dif nu) dif lambda]^(-1),
  $<eq-p-inv-norm-const>
  is an $x$-dependent normalisation constant.
]<lem-invariant-density>

#proof[
  The proof is a direct trivial calulation. We have
  $
    (cal(A)^* P_ss)(lambda)
      = - 1/epsilon partial_lambda [
          P_ss (lambda) tilde(a)_(alpha, epsilon)(t, x, lambda)
        ]
        + 1/(2epsilon) partial^2_(lambda lambda) [
          P_ss (lambda) tilde(d)(t, x, lambda)
        ] = 0,
  $

  which immediately gives us the ordinary differential equation

  $
    P_ss (lambda) tilde(a)_(alpha, epsilon)(t, x, lambda) &= 1/2 partial_lambda  [P_ss (lambda) tilde(d)(t, x, lambda)]. \
  $

  It follows then that  
  $
    (partial_lambda P_ss (lambda)) / (P_ss (lambda)) = (2 tilde(a)_(alpha, epsilon)(t, x, lambda) - partial_lambda  tilde(d)(t, x, lambda)) / (tilde(d)(t, x, lambda)),
  $
  which after intergrating both sides with respect to $lambda$ we obtain 

  $
    ln P_ss (lambda) =  2 integral_(-1)^lambda (tilde(a)_(alpha, epsilon)(x, nu)) / (tilde(d)(x, nu)) dif nu - ln tilde(d)(t, x, lambda) + ln R(x),
  $

  where $R(x)$ is an integration constant. Enforcing normalisation on the
  invariant density gives @eq-p-inv-norm-const.
  
]

The explicit formula for $P_(ss)(lambda | x)$ allows us to control its
dependence on the frozen variable $x$, whis is not required to show exponential
mixing but will be required later for averaging principle.



// #lemma(title: [Lipschitz continuity of $P_(ss)$ on $x$])[
//   Let $x, y in cal(D)_epsilon$ and let $P_(ss)(lambda | x)$ denote the invariant
//   density from @lem-invariant-density. Then there exists $K_P > 0$ such that
//   $
//     ||P_(ss)(dot | x) - P_(ss)(dot | y)||_("TV") <= K_P ||x - y||.
//   $
// ]<lem-pss-lipschitz>
// 
// #proof[
//   Define $g(x, lambda) = tilde(d)(x, lambda)^(-1) e^(phi(x, lambda))$ where
//   $phi(x, lambda) = 2 integral_(-1)^lambda tilde(a)(x, nu) / tilde(d)(x, nu) dif nu$.
//   From @lem-invariant-density, $P_(ss)(lambda | x) = R(x) g(x, lambda)$ where
//   $R(x) = (integral_(-1)^1 g(x, lambda) dif lambda)^(-1)$.
// 
//   We have
//   $
//     P_(ss)(lambda | x) - P_(ss)(lambda | y) = R(x)[g(x, lambda) - g(y, lambda)] + g(y, lambda)[R(x) - R(y)].
//   $
// 
//   For $g(x, lambda) - g(y, lambda)$, adding and subtracting $tilde(d)(x, lambda)^(-1) e^(phi(y, lambda))$,
//   $
//     |g(x, lambda) - g(y, lambda)| &<= tilde(d)(x, lambda)^(-1) |e^(phi(x, lambda)) - e^(phi(y, lambda))| + e^(phi(y, lambda)) |tilde(d)(x, lambda)^(-1) - tilde(d)(y, lambda)^(-1)|.
//   $
//   Using $|e^a - e^b| <= e^(max(a,b))|a - b|$, the lower bound $tilde(d) >= tilde(M)^2$, and Lipschitz continuity of $tilde(a)$ and $tilde(d)$,
//   $
//     |phi(x, lambda) - phi(y, lambda)| <= C'_phi ||x - y||, quad |tilde(d)(x, lambda)^(-1) - tilde(d)(y, lambda)^(-1)| <= K_d tilde(M)^(-4) ||x - y||.
//   $
//   Thus $|g(x, lambda) - g(y, lambda)| <= C_g ||x - y||$ for some $C_g > 0$.
// 
//   For $R(x) - R(y)$, let $G(x) = integral_(-1)^1 g(x, lambda) dif lambda$ so $R(x) = G(x)^(-1)$. Then
//   $
//     |R(x) - R(y)| = |G(x) - G(y)| / (G(x) G(y)) <= (2 C_g) / (G^2_min) ||x - y||,
//   $
//   where $G_min = 2 e^(-C_phi) / tilde(d)_max > 0$.
// 
//   Combining, with $R(x) <= G_min^(-1)$ and $g(y, lambda) <= e^(C_phi) / tilde(M)^2$,
//   $
//     |P_(ss)(lambda | x) - P_(ss)(lambda | y)| <= (C_g) / G_min ||x - y|| + (e^(C_phi)) / tilde(M)^2 dot (2 C_g) / G^2_min ||x - y||.
//   $
//   Integrating over $lambda in [-1, 1]$ and using $||dot||_("TV") = 1/2 ||dot||_(L^1)$ gives the result.
// ]


#lemma(title: [Lipschitz dependence of $P_(ss)$ on $x$])[
  Let $x, y in cal(D)_epsilon$ and let $P_(ss)(lambda | x)$ denote the invariant
  density from @lem-invariant-density. Then there exists $K_P > 0$ such that
  $
    ||P_(ss)(dot | x) - P_(ss)(dot | y)||_(L^1) <= K_P ||x - y||.
  $
]<lem-pss-lipschitz-2>

// #proof[
//   From @lem-invariant-density, $P_(ss)(lambda | x) = R(x)^(-1) g(x, lambda)$ where
//   $
//     g(x, lambda) = (tilde(d)(x, lambda))^(-1) exp(2 integral_(-1)^(lambda) (tilde(a)_(alpha, epsilon)(x, nu)) / (tilde(d)(x, nu)) dif nu).
//   $
// 
//   Define $phi(x, lambda) = 2 integral_(-1)^lambda (tilde(a)_(alpha, epsilon)(x, nu)) / (tilde(d)(x, nu)) dif nu$. By the Lipschitz bounds on $tilde(a)_(alpha, epsilon)$ and $tilde(d)$, and the uniform lower bound $tilde(d) >= tilde(M)^2$ from @lem-coeff-tilde-bounds,
//   $
//     |phi(x, lambda) - phi(y, lambda)| <= C_phi ||x - y||,
//   $
//   for some $C_phi > 0$ uniform in $lambda in [-1, 1]$.
// 
//   Similarly, since $tilde(d)$ is Lipschitz and bounded below,
//   $
//     |tilde(d)(x, lambda)^(-1) - tilde(d)(y, lambda)^(-1)| <= tilde(M)^(-4) |tilde(d)(x, lambda) - tilde(d)(y, lambda)| <= C_d ||x - y||.
//   $
// 
//   For $g(x, lambda)$, using $|ee^a - ee^b| <= ee^(max(a,b)) |a - b|$ and the uniform bound $|phi| <= C_phi$,
//   $
//     |g(x, lambda) - g(y, lambda)| &<= |tilde(d)(x, lambda)^(-1) - tilde(d)(y, lambda)^(-1)| ee^(phi(x, lambda)) + tilde(d)(y, lambda)^(-1) |ee^(phi(x, lambda)) - ee^(phi(y, lambda))| \
//       &<= C_d ee^(C_phi) ||x - y|| + tilde(M)^(-2) ee^(C_phi) C_phi ||x - y|| \
//       &<= C_g ||x - y||.
//   $
// 
//   For the normalisation $R(x) = integral_(-1)^1 g(x, lambda) dif lambda$,
//   $
//     |R(x) - R(y)| <= integral_(-1)^1 |g(x, lambda) - g(y, lambda)| dif lambda <= 2 C_g ||x - y||.
//   $
//   By @lem-doeblin, $P_(ss)(lambda | x) >= eta(x) > 0$, so $R(x) >= 2 eta_min > 0$ where $eta_min = inf_x eta(x)$. Thus
//   $
//     |R(x)^(-1) - R(y)^(-1)| <= (2 eta_min)^(-2) |R(x) - R(y)| <= C_g (2 eta^2_min)^(-1) ||x - y||.
//   $
// 
//   Finally,
//   $
//     |P_(ss)(lambda | x) - P_(ss)(lambda | y)| &<= |R(x)^(-1) - R(y)^(-1)| g(x, lambda) + R(y)^(-1) |g(x, lambda) - g(y, lambda)|.
//   $
//   Integrating over $lambda$ and using $integral g(x, lambda) dif lambda = R(x)$,
//   $
//     ||P_(ss)(dot | x) - P_(ss)(dot | y)||_(L^1) <= [C_g (2 eta^2_min)^(-1) R_max + (2 eta_min)^(-1) 2 C_g] ||x - y|| eqdef K_P ||x - y||,
//   $
//   where $R_max = sup_x R(x) < oo$ by the uniform bounds on the coefficients.
// ]

#proof[
  #text(fill: red)[
    The proof is straightforward via MD simulations.
  ]
]

With estimates for both the transient dynamics (@lem-lam-smooth-denst) and the
invariant density (@lem-invariant-density), we now establish convergence to
equilibrium. The strategy we will follow is standard. From
@lem-lam-smooth-denst, we will extract a uniform lower bound on the Green's
function (the Doeblin constant), use it to show contraction in total variation,
and iterate to obtain exponential mixing, see notes on convergence of Markov
processes in @hairer2021 for the procedure.

#lemma(title: [Doeblin Constant])[Let $x in cal(D)_epsilon $ be fixed, let
  $lambda_t$ evolve acccording to the forward generator $cal(A)^*_x$ given by
  @lem-fwd-gen and let $rho_(x)(t, lambda giv mu)$ denote the Green's function
  from @lem-lam-smooth-denst evaluated at time $t in [0, T]$. Then there exists
  a positif function $eta_(t)(x) > 0$ such that $ rho_(x)(t, lambda giv mu) >=
  eta_(t)(x) quad forall lambda, mu in [-1, 1], $ ]<lem-doeblin>

#proof[
  From @lem-lam-smooth-denst, we have 
  $
    rho_(x)(t, lambda giv  mu)
      &>= (R_("L")(x))/sqrt(t) exp[-(C'_(1)(x)(lambda -mu)^2 )/ t],\
  $
  from which we can define
  $
    eta_(t)(x) &eqdef inf_(lambda, mu) (R_("L")(x))/sqrt(t) exp[-(C'_(1)(x)(lambda -mu)^2 )/ t], \
      &= (R_("L")(x))/sqrt(t) exp[-(4 C'_(1)(x) )/ t].\
  $<eq-eta-def>
  
]

#corollary[Due to the existance of $eta_(t)(x)$ one can always decompose the Green's function into 
  $
    rho_(x)(t, lambda giv  mu) = eta_(t)(x) + r_(t)(lambda giv x, mu),
  $<eq-green-decomp>

  where $r_(t)(lambda giv x, mu) >= 0$ is the contribution to the kernel the
  spends on the initial condition $mu$ and the state $lambda$. Notice that we have
  $
    integral_(-1)^(1) r_(t)(lambda giv x, mu) dif lambda = 1 - 2 eta_(t) (x),
  $
  due to the normalisation of the Green's function over $lambda$, which inturn implies that $eta_(t)(x) in [0, 1\/2]$.
]<cor-lem-doeblin-res>


#corollary[Since $eta_(t)(x)$, when it exists it must be finite, and since we can always
  globally bound $||x_t||$ on the interval $[0, T]$, there must exist uniform lower
  bound on the interval $t in (0, T]$. We call this

  $
    C_(T, eta) eqdef inf_(t in [0, T]) eta_t (x_t).
  $

]<cor-lem-doeblin-min-eta-bound>

#todo[
  Reviewed to here. 
]





A useful object that we weill employ in our upcomoing proofs is the called the
transition operator. We will defnote it iwth $cal(T)_t$ and its action on action
on action on on a probability density $P_s in dom(cal(A)^*_x)$ is defined via
  $
    (cal(T)_t P_s)(x) eqdef integral_(-1)^(1) rho_(x)(t, lambda giv  mu) P_(s)(mu giv x) dif mu, 
  $<eq-def-trans-op>
from which 
  $
    P_(t + s) (lambda) = (cal(T)_s P_t)(x) ,
  $

follows. Note that by definition of the invarint density we must have
$P_(ss)(lambda) = cal(T)_s P_(ss) (lambda)$.


#lemma(title: [Total variation bound])[
  Let $x in cal(D)_(epsilon)$ be fixed, and let $lambda_t$ ​ evolve according to
  the SDE with forward generator $A^*_x$ given by @lem-fwd-gen, let $P_t (lambda
  giv x), Q_t (lambda giv x) in dom(cal(A)^*_x)$ with some normalised initial
  condition. Let $cal(T)_t$ be the transition operator defined in @eq-def-trans-op,
  and let $t,s>0$. Then the inequality

  $
    lpnorm(P_(s + t)(lambda giv x) - Q_(s + t)(lambda giv x), "TV", ,) 
      &<= [1 - 2 eta_(s)(x)] lpnorm(P_(t)(lambda giv x) - Q_(t)(lambda giv x), "TV", ,),
  $<eq-tv-bound>
  is ssatisfied where $eta(x)$ is the constant given in @lem-doeblin.

]<lem-tv-bound>
#proof[
  Using the definition of the transtion operator from @eq-def-trans-op we obtain 
  $
    (cal(T)_s P_t)(lambda) - (cal(T)_s Q_t)(lambda) = 
    integral_(-1)^(1)  rho_(x)(s, lambda giv  mu) (P_t (mu) - Q_t (mu))dif mu,
  $<eq-tv-bound-setup>
  where $rho_(x)(s, lambda giv  mu)$ is the Green's function for the forward
  equation, and where we have drop the conditional dependence on $x$ in the
  notation for clarity. Substituting in the decomposition given in
  @eq-green-decomp into @eq-tv-bound-setup yeilds

  $
    (cal(T)_s P_t)(lambda) - (cal(T)_s Q_t)(lambda) &= 
    eta(x) integral_(-1)^(1)   (P_(t)(mu) - Q_(t)(mu))dif mu 
    integral_(-1)^(1)  r_(s)(lambda, mu) (P_(t)(mu) - Q_(t)(mu))dif mu,  \
      &= integral_(-1)^(1)  r_(s)(lambda, mu) (P_(t)(mu) - Q_(t)(mu))dif mu .
  $

  Taking the absolute value on both sides allows to obtain the inequality
  $
    lr(|(cal(T)_s P_(t))(lambda) - (cal(T)_s Q_(t))(lambda)|) &= 
    lr(|integral_(-1)^(1)  r_(s)(lambda, mu) (P_(t)(mu) - Q_(t)(mu))dif mu|)\
      &<= integral_(-1)^(1)  r_(s)(lambda, mu) lr(|P_(t)(mu) - Q_(t)(mu)|)dif mu.
  $

  Finally, integrating both sides w.r.t $lambda$ allows to eliminate $r_s (lambda, mu)$ using @cor-lem-doeblin-res yeilding
  $
    integral_(-1)^(-1) lr(|(cal(T)_s P_(t))(lambda) - (cal(T)_s Q_(t))(lambda)|)dif lambda
      &<= integral_(-1)^(1)  lr(|P_(t)(mu) - Q_(t)(mu)|) lr((integral_(-1)^(1)  r_(s)(lambda, mu)  dif lambda)) dif mu,\
      &<= [1 - 2 eta_(s)(x)] integral_(-1)^(1)  lr(|P_(t)(mu) - Q_(t)(mu)|) dif mu,
  $
  or in the norm-notation
  $
    lpnorm((cal(T)_s P_(t))(lambda) - (cal(T)_s Q_(t))(lambda), L^1, ,) 
      &<= [1 - 2 eta_(s)(x)] lpnorm(P_(t)(mu) - Q_(t)(mu), L^1, ,).
  $<eq-l1-norm-tp>

  To write @eq-l1-norm-tp in terms of the total variation we employ the fact
  that that $P_t$ and $Q_t$ are absolutely continuous with respect to the
  Lebesgue measure, hence
  $
    lpnorm(P_(t) (lambda) - Q_(t) (lambda), "TV", ,)
       //&eqdef sup_(omega in cal(B)([-1, 1]))|P_(t) (lambda) - Q_(t) (lambda)|,\
      &= 1/2 integral_(-1)^(1) |P_(t) (lambda) - Q_(t) (lambda)| dif lambda.\
  $<eq-tv-def>
]
#lemma(title: [Upperbound on TV])[
  For any probability densities $P$, $Q$ on a compact set $Omega subset.eq RR^d$, 
  $
    lpnorm(P -Q, "TV", ,)  <= 1
  $
]<lem-tv-upper-bound>

#proof[
  We define $Omega^+ = {omega in Omega | P(omega) >= Q(omega) }$, and $Omega^- =
  {omega in Omega | P(omega) < Q(omega) }$ then 
  $
    integral_(Omega)|P(omega) - Q(omega)| dif omega = 
    integral_(Omega^+)[P(omega) - Q(omega)] dif omega
    +
    integral_(Omega^-)[Q(omega) - P(omega)] dif omega.
  $<eq-ubound-tv>
  By exploiting the normalisation of the densities we have that 
  $
   integral_(Omega^+)[P(omega) - Q(omega)] dif omega = 
    integral_(Omega^-)[Q(omega) - P(omega)] dif omega,
  $<eq-const-diff>
  and calling this common quantity $C$, we observe that  
  $
    C eqdef integral_(Omega^+)[P(omega) - Q(omega)] dif omega
    <= integral_(Omega^+)[P(omega)] dif omega
    <= integral_(Omega)[P(omega)] dif omega <= 1.
  $
  Then from the defintion of the the total variation we have
  $
    lpnorm(P -Q, "TV", ,)  = 1/2
    integral_(Omega)|P(omega) - Q(omega)| dif omega = C <= 1.
  $
]


#theorem(title: [Exponential mixing of the switching variable])[
  Let $x in cal(D)_epsilon$ be fixed, let $t >0$ and let $lambda_t$ evolve according to the SDE with the forward generate $cal(A)^*_x$ given by @lem-fwd-gen, and let $P_t in dom(cal(A))^*_x$ represent the
  occupation probability density with the initial condition $P_(0)(lambda giv x)$. Let $P_ss$ be
  the invariant density satisfying $(cal(A)^*_x P_ss)(lambda) = 0$. Then for any $tau in (0, t)$, there exisits constants $C(x) > 0$ and $kappa_(tau)(x) > 0$ such that
  $

    lpnorm(P_t (lambda giv x) - P_(ss)(lambda giv x), "TV", ,) <= C(x) ee^(-kappa_(tau)(x) t),
  $
  where $kappa_(tau)(x) = 2 eta_(tau)(x) / tau$ with $eta_(tau)(x)$ the Doeblin constant from @lem-doeblin.
]<thm-exp-mixing-doe>

#proof[
  Let $tau > 0$ such that $tau << t$ and let $m eqdef floor(t\/tau)$, then using
  the fact that $cal(T)_(tau) P_(ss )(lambda)$ and @lem-tv-bound we obtain
  $
    lpnorm(P_(m tau)(lambda) -  P_(ss)(lambda), "TV", ,) <=
    (1 - 2eta_(tau)(x))lpnorm(P_((m-1)tau)(lambda) -  P_(ss)(lambda), "TV", ,) ,
  $<eq-tv-bound-0>
  and using the same lemma repeatedly, that is utilising the semigrouip property,  yeilds
  $
    lpnorm(P_(m tau)(lambda) -  P_(ss)(lambda), "TV", ,) &<=
    (1 - eta_(tau)(x))^m lpnorm(P_(0)(lambda) -  P_(ss)(lambda), "TV", ,), \
      &<= (1 - 2eta_(tau)(x))^m,
  $<eq-tv-bound-1>

  where in the last step we have employed @lem-tv-upper-bound. We can rewrite
  the right hand side in @eq-tv-bound-1 using exponentials to obtain
  $
    (1 - eta_(tau)(x))^m &= e^(m ln[1 - eta(x)])
    <= ee^(-2eta(x) floor(t\/tau))
    <= ee^(-2eta(x) (t\/tau - 1))
    = C_1(x) ee^(-kappa_(tau)(x) t),
  $<eq-tv-bound-2>
  where $C_1(x) eqdef ee^(2eta_(tau)(x))$ and $kappa_(tau)(x) eqdef 2eta_(tau)(x) \/ tau$. With
  $eta_(tau)(x) in (0, 1\/2]$, we take $C_1(x) <= ee(1) = C$ to get the desired bound.
]


#corollary(title: [Bound on expectations])[

  The bound on the differences between probability measures obtained in
  @thm-exp-mixing-doe affords us a further bound, namely on the differences in
  expectations. As before with $0 < tau < t$ 

  $
    lr(|EE[f(lambda_t)] - integral^(1)_(-1) f(lambda)  P_ss (lambda) dif lambda |) 
      &<= integral_(-1)^(1) lr(|f(lambda) [P_(t)(lambda) - P_(ss)(lambda)]|) dif lambda ,\
      &<= lpnorm(f(lambda), L^oo, ,) lpnorm(P_(t)(lambda) - P_(ss)(lambda), L^1, ,) ,\
      &<= 2 C ee^(-kappa_(tau)(x) t) lpnorm(f(lambda), L^oo, ,) .\
  $

  where we have used Hoelder's inequality to obtain the penultimate bound .
  Obviously, the final supremum bound is of course only meaningful when we have
  bounded $f$ on the interval $lambda in [-1, 1]$.

]<cor-exp-mixing-obs-doe>

In summary, we have established the existence of an intermediate timescale
$delta$ with $epsilon << delta << 1$ on which $x_t$ remains approximately fixed
while $lambda_t$ rapidly equilibrates to $P_(ss)(lambda | x)$. We also proved
that equilibration occurs through exponential mixing for which the
transversality was crucial. These estimates are essential to derive the
averaging principle in the next section.

= Averaging Principle <sec-avg-principle>

We are now ready to introduce the averaging principle for the fast switching
variable dynamics

#definition(title: [The Reduced SDE])[Let $x_t in cal(D)_epsilon$ be a solution
  of the piecewise-smooth SDE given in @def-ns-gen-sde, and let $lambda in [-1,
  1]$ be the switching variable parametrising the convex interpolation

  $
    a(t, x, lambda) &= 1/2(1 + lambda) a^+(t, x) + 1/2(1 - lambda) a^-(t, x), \
    b(t, x, lambda) &= 1/2(1 + lambda) b^+(t, x) + 1/2(1 - lambda) b^-(t, x),
  $

  between the piecewise-smooth coefficients $a^pm$ and $b^pm$. Let
  $P_(ss)(lambda | x)$ denote the stationary distribution of the
  switching variable conditional on $x$, satisfying $cal(A)_x^* P_(ss)
  = 0$ with zero-flux boundary conditions (see @lem-fwd-gen). The reduced SDE is

  $
    dif y_t = [macron(a)_(alpha, epsilon)(t, y_t) + alpha epsilon macron(c)(t, y_t)] dif t + sqrt(epsilon)" "macron(b)(t, y_t) dif W_t,
  $<eq-reduced-sde>

  where the averaged coefficients are

  $
    macron(a)_(alpha, epsilon)(t, x) &= integral_(-1)^(1) a_(alpha, epsilon)(t, x, lambda) P_(ss)(lambda | x) dif lambda, #<eq-avg-a> \
    macron(b)(t, x) &= integral_(-1)^(1) b(t, x, lambda) P_(ss)(lambda | x) dif lambda, #<eq-avg-b> \
    macron(c)(t, x) &= integral_(-1)^(1) c(t, x, lambda) P_(ss)(lambda | x) dif lambda, #<eq-avg-c>
  $

  and

  $
    c(t, x, lambda) = sum_j J_x [b_j (t, x, lambda)] b_j (t, x, lambda)
  $

  is the Itō correction arising from the $alpha$-interpretation of the
  stochastic integral, with $b_j (t, x, lambda)$ denoting the $j$-th column of
  $b(t, x, lambda)$ and $J_x (dot)$ the Jacobian with respect to $x$.

]<def-reduced-sde>


Unsurprisingly, without any hidden term in the dynamics we require only the mean
from the distribution $P_ss (lambda)$,


#lemma(title: [Bounds on average Coefficients])[ Let $x in cal(D)_epsilon$, $t
  in [0, T]$, and let $macron(a)_(alpha, epsilon)(t, x)$, $macron(b)(t, x)$, and $macron(c)(t,
  x)$ be the averaged coefficients defined in #Cref(supplement: "Eqs.")[@eq-avg-a @eq-avg-b @eq-avg-c].
  Then $macron(a)$, $macron{b}$, and $macron(c)$ inherit the regularity of the
  convex combinations $a(t, x, lambda)$, $b(t, x, lambda)$, and $c(t, x,
  lambda)$. Specifically, there exist constants $macron(C), macron(K),
  macron(K)_T > 0$ such that:

  1. Linear growth. For any $x in bb(R)^2$ and $t in[0, T]$,
     $
       ||macron(a)_(alpha, epsilon)(t, x)|| + ||macron(b)(t, x)|| <= C(1 + ||x||).
     $
  2. Lipschitz continuity. For any $x, y in bb(R)^2$ and $s, t in[0,T]$
     $
       ||macron(a)_(alpha, epsilon)(t, x) - macron(a)_(alpha, epsilon)(t, y)|| + ||macron(b)(t, x) - macron(b)(t, y)|| <= C(1 + ||x||).
     $
  
]<lem-avg-coeff-bounds>

#proof[
  Linear growth. For $macron(a)_(alpha, epsilon)(t, x)$, using the definition @eq-avg-a and the linear growth bound on $a(t, x, lambda)$ from @lem-coeff-tilde-bounds,
  $
    ||macron(a)_(alpha, epsilon)(t, x)|| &= lr(||integral_(-1)^(1) a(t, x, lambda) P_(ss)(lambda | x) dif lambda||) \
      &<= integral_(-1)^(1) ||a(t, x, lambda)|| P_(ss)(lambda | x) dif lambda \
      &<= integral_(-1)^(1) C(1 + ||x||) P_(ss)(lambda | x) dif lambda \
      &= C(1 + ||x||),
  $
  since $P_(ss)(lambda | x)$ is a probability density. The same argument applies to $macron(b)(t, x)$ using the linear growth bound on $b(t, x, lambda)$, giving
  $
    ||macron(b)(t, x)|| <= C(1 + ||x||).
  $
  For $macron(c)(t, x)$, using @eq-avg-c and the bound on $c(t, x, lambda)$ from @lem-coeffs-lam,
  $
    ||macron(c)(t, x)|| <= integral_(-1)^(1) ||c(t, x, lambda)|| P_(ss)(lambda | x) dif lambda <= C_J C (1 + ||x||).
  $
  Thus all averaged coefficients satisfy linear growth with $macron(C) = max{C, C_J C}$.

  Lipschitz continuity. For $macron(a)_(alpha, epsilon)(t, x) - macron(a)_(alpha, epsilon)(s, y)$, we write
  $
    macron(a)_(alpha, epsilon)(t, x) - macron(a)_(alpha, epsilon)(s, y) &= integral_(-1)^(1) a_(alpha, epsilon)(t, x, lambda) P_(ss)(lambda | x) dif lambda - integral_(-1)^(1) a_(alpha, epsilon)(s, y, lambda) P_(ss)(lambda | y) dif lambda, \
  &= integral_(-1)^(1) [a_(alpha, epsilon)(t, x, lambda) - a_(alpha, epsilon)(s, y, lambda)] P_(ss)(lambda | x) dif lambda \
      &quad + integral_(-1)^(1) a_(alpha, epsilon)(s, y, lambda) [P_(ss)(lambda | x) - P_(ss)(lambda | y)] dif lambda.
  $
  
  For the first integral, using the Lipschitz bound in @eq-ab-lip-bound yeilds
  $
    lr(||integral_(-1)^(1) [a(t, x, lambda) - a(s, y, lambda)] P_(ss)(lambda | x) dif lambda||) &<= integral_(-1)^(1) ||a(t, x, lambda) - a(s, y, lambda)|| P_(ss)(lambda | x) dif lambda \
      &= K ||x - y|| + K_T |t - s|.
  $
  
  For the second integral, we employ the Lipschitz bound from @lem-pss-lipschitz
  for the invariant density to obtain

  $
    lr(||integral_(-1)^(1) a(s, y, lambda) [P_(ss)(lambda | x) - P_(ss)(lambda | y)] dif lambda||) &<= sup_(lambda) ||a(s, y, lambda)|| lpnorm(P_(ss)(dot | x) - P_(ss)(dot | y), L^1, ,) \
      &<= C(1 + ||y||) K_P ||x - y||.
  $

  Combining both terms,
  $
    ||macron(a)_(alpha, epsilon)(t, x) - macron(a)_(alpha, epsilon)(s, y)|| <= [K + C(1 + ||y||) K_P] ||x - y|| + K_T |t - s|.
  $
  For $x, y$ in a bounded set (which holds on $[0, T]$ by @lem-poly-mom-bound), we obtain
  $
    ||macron(a)_(alpha, epsilon)(t, x) - macron(a)_(alpha, epsilon)(s, y)|| <= macron(K) ||x - y|| + macron(K)_T |t - s|,
  $

  with $macron(K) = K + C(1 + R_T) K_P$ and $macron(K)_T = K_T$, where $R_T$
  bounds $||x||$ and $||y||$ on the interval. The same argument applies to
  $macron(b)$, giving the Lipschitz bounds claimed in the lemma.
]


With the averaged coefficients satisfying the same regularity as the original
system given in @def-ns-gen-sde, we can state the main error estimate for
substituting in place of the pws system which is implicitly multi-timescale
system with its averaged counterpart.


#theorem(title: [Error estimate for the averaged SDE ])[Let $(t, x_t, lambda_t)$
  evolve according to the coupled slow-fast system of @cor-coupled-sde with
  coefficients satisfying regularity conditions @lem-coeff-tilde-bounds, and let
  $y_t$ evolve according to the reduced SDE given in @def-reduced-sde, with
  coefficients satisfying conditions given in @lem-avg-coeff-bounds. Let $T > 0$, $gamma, epsilon>0$ and assume $x_0 = y_0 in cal(D)_epsilon$ with $lambda_0 in [-1, 1]$. Then there exists a constant $C^"er"_T >0$, depending on $T$, Lipschitz constants and the linear growth constants, such that 
  $
    PP[sup_(t in [0, T]) ||x_t - y_t|| > gamma ] <= 1/gamma^2 C^"er"_T sqrt(epsilon).
  $
]<thm-avg-principle>

#proof[
  Let us define $xi_t eqdef x_t - y_t$, which gives us the initial condition $x_0 = 0$

  then substrituting in the definition for the  
  $
    xi_t = integral^t_0 a_(alpha, epsilon)(s, x_s, lambda_s)  - macron(a)_(alpha, epsilon)(s, y_s) dif s
    + sqrt(epsilon) integral^t_0 b(s, x_s, y_s) - macron(b)(s, y_s) dif W_s,
  $

  Let us define the following integrands

  $
    I_a (s) &eqdef a_(alpha, epsilon)(s, x_s, lambda_s) - macron(a)_(alpha, epsilon)(s, y_s), \
    I I_a (s) &eqdef macron(a)_(alpha, epsilon)(s, x_s) - macron(a)_(alpha, epsilon)(s, y_s) ,
  $
  similarly for the noise coefficient we have
  $
    I_b (s) &eqdef b(s, x_s, lambda_s) - macron(b)(s, y_s), \
    I I_b (s) &eqdef macron(b)(s, x_s) - macron(b)(s, y_s) ,
  $

  Then consider 

  $
    ||xi_t||^2 &<= 2 lr(||integral^t_0   I_a (s) + I I_a (s) dif s||)^2
              + 2 epsilon lr(||integral^t_0   I_b (s) + I I_b (s) dif W_s||)^2, \
    &<= 2 t integral^t_0   ||I_a (s) + I I_a (s)||^2 dif s
              + 2 epsilon lr(||integral^t_0   I_b (s) + I I_b (s) dif W_s||)^2, \
  $

  where in the first instance we have used the inequality $|a + b|^2 <= 2 (|a|^2
  + |b|^2)$ (Cauchy-Schwarz), and in the second case we have also used
  Cauchy-Schwarz. Taking the expectation allows us to use Itō isometry on teh
  stochastic integral,

  $
    EE[ ||xi_t||^2] <= 2 t EE[ integral^t_0   ||I_a (s) + I I_a (s)||^2 dif s]
    + 2 epsilon EE[integral^t_0   ||I_b (s) + I I_b (s)||^2 dif s],
  $
  and using Cauchy-Schwarz again on the intergrand gives

  $
    EE[ ||xi_t||^2] <= 4 t EE[ integral^t_0   ||I_a (s)||^2 + ||I I_a (s)||^2 dif s]
    + 4epsilon EE[integral^t_0   ||I_b (s)||^2 + ||I I_b (s)||^2 dif s].
  $

  From @lem-avg-coeff-bounds we know that $I I_a (s)$ and $I I_b (s)$ can be
  bounded as it is is just an application of Lipschitz, 

  $
    4t || I I_a (s) ||^2 + 4epsilon || I I_b (s) ||^2 &= 4t||macron(a)_(alpha, epsilon)(s, x_s) - macron(a)_(alpha, epsilon)(s, y_s) ||^2
    + 4epsilon||macron(b)(s, x_s) - macron(b)(s, y_s) ||^2 \
      &<=  4 macron(K)^2(T + epsilon) ||xi_(s)||^2.
  $ 

  For the other two integrals we consider first the parition of $[0, t]$ into
  the windows $[k delta, (k+1) delta]$ with $k = 0, 1, ... N-1$ where $N = t\/
  delta$ and then recasting the integrals as, for example,

  $
    integral_(0)^(t) ||I_a (s)||^2 dif s = sum_(k = 0)^(N-1) integral_(0)^(delta) ||I_(a, k) (k delta + s)||^2 dif s,
  $

  where $I_(a,k)(s) eqdef I_(a)(k delta + s)$ with $s in [0, delta)$. We then recast
  the integrand as

  $
    I_(a, k)(s) = sum_(ell = 1)^5 J^((ell))_(a, k) (s), 
  $<eq-i1-5-sum>

  where 

  $
    J^((1))_(a, k)(s) &eqdef 
      a(k delta + s, x_(k delta + s), lambda_(k delta + s))
    - a(k delta + s, x_(k delta ), lambda_(k delta + s)),   \
    J^((2))_(a, k)(s) &eqdef 
      a(k delta + s, x_(k delta), lambda_(k delta + s))
    - a(k delta , x_(k delta ), lambda_(k delta + s)) ,  \
    J^((3))_(a, k)(s) &eqdef 
      a(k delta , x_(k delta), lambda_(k delta + s))
      - macron(a)(k delta , x_(k delta )) ,  \
      J^((4))_(a, k)(s) &eqdef 
    macron(a)(k delta , x_(k delta))
      - macron(a)(k delta  + s, x_(k delta )),   \
      J^((5))_(a, k)(s) &eqdef 
    macron(a)(k delta + s , x_(k delta))
    - macron(a)(k delta  + s, x_(k delta +s)) .  \
  $

  Each term in the sum in @eq-i1-5-sum, is associated with a particular type of
  error: $J^((1))_(a, k)$ and $J^((5))_(a, k)$ are the spatial error associated
  with fixing $x$ with its value at the start of the interval for the drift and
  avergaed drift fields respectively; $J^((2))_(a, k)$ and $J^((4))_(a, k)$ are
  the temporal error associated, respectivley, with drift and avergaed drift
  fields by fixing the explicit time dependence to the start of the interval;
  $J^(3)(a, k)$ is the mixing error as all other dependences are fixed. Clearly,

  $
    ||I_(a,k)(s)||^2 <= 5 sum_(ell = 1)^5 ||J^((ell))_(a, k)(s)||^2,
  $

  by Cauchy-Schwarz, and it only remains to bound the various squared error
  terms. Employing the Lipschitz conditions in @def-ns-gen-sde, and
  @lem-avg-coeff-bounds as well as @thm-slow-var we obtain

  $
    EE[ ||J^((1))_(a, k)(s)||^2] &<= K^2_(x) EE[ ||x_s - x_(k delta)||^2] &&<=  K^2 _(x)C (delta^2 + epsilon delta),  \
    EE[ ||J^((5))_(a, k)(s)||^2] &<= macron(K)^2_x EE[ ||x_s - x_(k delta)||^2] &&<= macron(K)^2_x C (delta^2 + epsilon delta),
  $
  which control the spatial error, and similarly, 
  $
    EE[ ||J^((2))_(a, k)(s)||^2] &<= K^2_(T) s^2 &<= K^2_(T) delta^2,  quad
    EE[ ||J^((4))_(a, k)(s)||^2] &<= macron(K)^2_T s^2 &<= macron(K)^2_T delta^2 ,
  $
  which controls the temporal error, Using these uniform bounds we can integrate over time to yeild
  $
    sum_(k=0)^(N -1) integral_(0)^(delta) EE[ ||J^((1))_(a, k)(s)||^2] dif s
      &<= K^2_x C (delta - epsilon delta) t &&<= K^2_x T C (delta - epsilon delta),  \
    sum_(k=0)^(N -1) integral_(0)^(delta) EE[ ||J^((3))_(a, k)(s)||^2] dif s
      &<= macron(K)^2_(x)C (delta - epsilon delta) t &&<= macron(K)^2_(x)T C (delta - epsilon delta),
  $
  and for the temporal errors we obtain
  $
    sum_(k=0)^(N -1) integral_(0)^(delta) EE[ ||J^((2))_(a, k)(s)||^2] dif s
      &<= K^2_T t delta^2  &&<= K^2_T T delta^2 ,\
    sum_(k=0)^(N -1) integral_(0)^(delta) EE[ ||J^((5))_(a, k)(s)||^2] dif s
      &<= macron(K)^2_(T) t delta^2 &&<= macron(K)^2_(T) T delta^2,
  $

  We now turn to the mixxing error contribution, we first rewrite $J^((3))_(a, k)(s)$ as
  $
    J^((3))_(a, k)(s) &= integral_(-1)^(1) a(k delta, x_(k delta), lambda)  [P_s (lambda | x_(k delta)) - P_ss (lambda | x_(k delta))] dif lambda
  $

  where $P_s (lambda | x_(k delta)) = ee^(s cal(A)^*_(x_(k delta))) P_0(lambda
  giv x_(k delta) )$ with $P_0(lambda | x_(k delta))$ being formally, the
  normalised probability density of $lambda$ at the start of the interval $[k
  delta, (k+1) delta]$. Recall that for $k = 0$ we have $P_0 (lambda giv x_(0))
  = delta(lambda - lambda_0)$ but for all $k > 0$ the initial probability
  density $P_0 (lambda giv x_(k delta))$ corresponds to some conditional
  probability density at $t = k delta > 0$ that by @lem-lam-smooth-denst, is in
  $L^2_(P_ss)$. With $J^((2))_(a, k, i)(s)$ denoting the $i^"th"$ component of the vector, we have for $k>0$
  $
    ||J^((2))_(a, k)(s)||^2 = sum_i |J^((2))_(a, k, i)(s)|^2 &= sum_i
    lr(|integral_(-1)^(1) a_(i)(k delta, x_(k delta), lambda)
    [P_s (lambda | x_(k delta)) - P_ss (lambda | x_(k delta))] dif lambda|)^2 \
      &<= 
      [sum_i  lpnorm(a_(i)(k delta, x_(k delta), lambda), L^oo, 2)]
      lpnorm(P_s (lambda | x_(k delta)) - P_ss (lambda | x_(k delta)), L^1, 2) ,\
      &= sup_(lambda) ||a(k delta, x_(k delta), lambda)||^2
      lpnorm(P_s (lambda | x_(k delta)) - P_ss (lambda | x_(k delta)), L^1, 2) ,\
      &= 4 sup_(lambda) ||a(k delta, x_(k delta), lambda)||^2
      lpnorm(P_s (lambda | x_(k delta)) - P_ss (lambda | x_(k delta)), "TV", 2) ,\
      &<= C_0 (1 + ||x||^2)
      lpnorm(P_s (lambda | x_(k delta)) - P_ss (lambda | x_(k delta)), "TV", 2) ,\
  $

  where we have exploited the linear growth condition in @def-ns-gen-sde, and
  where $C_0 eqdef 8 (C_++ C_-)^2$. Employing @thm-exp-mixing-doe with $tau =
  epsilon$ on the total variation term yeilds the bound

  $
    lpnorm(P_s (lambda | x_(k delta)) - P_ss (lambda | x_(k delta)), "TV", 2)
    <=
    ee ee^(-2 eta_(epsilon)(x_(k delta)) s \/ epsilon). 
  $

  For the $k=0$ term we have a Dirac distriubtion but the total variation is
  still bounded from above by one via @lem-tv-upper-bound. Putting it together we obtain 

  $
    sum_(k=0) ^(N-1) integral_(k delta)^((k+1) delta) EE[ ||J^((2))_(a, k)(s)||^2]
      &<= EE[C_0 (1 + ||x||^2) (delta  + ee sum_(k=1) ^(N-1)
      integral_(0)^(delta) ee^(-2 eta_(epsilon)(x_(k delta)) s \/ epsilon) dif s)],\
      &<= EE[C_0 (1 + ||x||^2) (delta  + ee epsilon sum_(k=1) ^(N-1)
      (1 - ee^(-2 eta_(epsilon)(x_(k delta)) delta \/ epsilon))/(2 eta_(epsilon) (x_(k delta)))
      )],\
      &<= EE[C_0 (1 + ||x||^2) (delta  + ee epsilon sum_(k=1) ^(N-1)
      (1)/(2 eta_(epsilon) (x_(k delta))))].
  $

  This estimate can be made uniform the interval $t = [0, T]$ by inovking
  @lem-poly-mom-bound and @cor-lem-doeblin-min-eta-bound to yeild

  $
    sum_(k=0) ^(N-1) integral_(k delta)^((k+1) delta) EE[ ||J^((2))_(a, k)(s)||^2]
      &<= C_T [delta  + (ee epsilon (N - 1)) / (2 C_(T, eta))]  \
      &<= C_T [delta  + (ee epsilon N ) / (2 C_(T, eta))], \
      &<= M_T [delta  + (epsilon t ) / (delta)] \
      &<= M_T [delta  + (epsilon T ) / (delta)],
  $

  where $M_T eqdef max{C_T, C_T ee \/ (2 C_(T, eta)) }$. A similar procedure can
  be used to obtain the same bounds for $||I_(b)(s)||^2$. Assembling the various inequalities together yeilds
  $
    EE[ ||xi_t||^2] &<= 20(T + epsilon) [T C(delta^2 + epsilon delta) (K^2_x + macron(K)^2_x)
    + T delta^2(K^2_T + macron(K)^2_T)  + M_T (delta + (epsilon T)/delta)] \ &+ 4macron(K)^2(T + epsilon) integral_0^t EE[ ||xi_s||^2 ] dif s. 
  $
  Notice that we can simplify the various terms before we apply Gronwall's inequality, by taking $delta^2 <= delta$ and $epsilon delta <= delta$, which allows us to write
  $
    EE[ ||xi_t||^2] &<= D^"er"_(T) (delta + (epsilon T)/delta) + K^"er"_T integral_0^t EE[ ||xi_s||^2 ] dif s,
  $<eq-error-bound-pre-gron>
  where we define the constants
  $
    D^"er"_T eqdef 20  T (T+1)[C(K^2_x + macron(K)^2_x) + K^2_(T) + macron(K)^2_(T) ] + M_T, quad "and" quad
    K^"er"_T eqdef 4 macron(K)^2_(x)(T + 1).
  $

  Both $D^"er"_T$ and $K^"er"_T$ depends on the Lipschitz constants and the
  interval size $T$, while the former also on depends on the linear growth
  constants. Applying to @eq-error-bound-pre-gron yeilds

  $
    EE[ ||xi_t||^2]  <= D^"er"_(T) (delta + (epsilon T) / delta) ee^(K^"er"_(T) t).
  $<eq-error-bound-at-t>

  To connect the error bound at $t in [0, T]$ to the supremum bound we employ
  Doob's Maringale inequality (see e.g. Proposition 1.5 in Chapter II of
  @revuzyor19993book) to obtain

  $
    EE[sup_(t in [0, T]) ||xi_t||^2] &<= 4 sup_(t in [0, T])lr({EE[ ||xi_t||^2]})
    <= D^"er"_(T) (delta + (epsilon T) / delta) ee^(K^"er"_(T) T).
  $<eq-error-bound-no-op>

  From @eq-error-bound-no-op we can observe that the minimal error is achieved
  when $delta^* eqdef op("argmin", limits: #true)_(delta)[(delta + (epsilon T) /
  delta)] = sqrt(T epsilon)$, thus absorbing all of the constants together we
  can define

  $
    C^"er"_T eqdef D^"er"_T sqrt(T) ee^(K^"er"_(T) T),
  $
  which after applying the inequality yeilds 
  $
    PP[sup_(t in [0, T]) ||x_s - y_s|| > gamma ] <= 1/gamma^2 EE[sup_(t in [0, T]) ||x_s - y_s||^2] <= 1/gamma^2 C^"er"_T sqrt(epsilon),
  $
  which is the desired bound.
]


= Typical paths and Gaussian fluctuations <sec-typical-paths>

The reduced SDE given in @def-reduced-sde is a weak-noise SDE for which we have
an LDP via Freidlin-Wentzell theory (see Chap. 5, Sec. 3 of
@freidlinwentzell1998book, and Chap. 5, Sec. 6 of @dembozeitouni2010book, for
proofs and discussions). We state the relevant results here.

#theorem(title: [Large deviation principle for the averaged system])[
  Let $y_t$ solve the reduced SDE @eq-reduced-sde with coefficients satisfying
  the regularity conditions of @lem-avg-coeff-bounds, and let $y_0 = x_0$. Then
  $y_t$ satisfies a large deviation principle with rate function
  $
    I_T [phi] = cases(
    1/2 integral_0^T ||[dot(phi)_t - macron(a)(t, phi_t)]^(tns)macron(d)(t, phi_t)^(-1)[dot(phi)_t - macron(a)(t, phi_t)]||^2 dif t quad & phi in "a.c. on " [0, T], 
    +oo quad & "otherwise",
    )
  $
  where $ phi_0 = y_0$ and $
    macron(d)(t, x) eqdef macron(b)(t, x) macron(b)(t, x)^(tns).
  $

  Hence, for any measurable set $A$ of paths,
  $
    -inf_(phi in A^circle.small) I_T [phi] <= liminf_(epsilon -> 0) epsilon log PP[y in A]
    <= limsup_(epsilon -> 0) epsilon log PP[y in A] <= -inf_(phi in macron(A)) I_T [phi],
  $
  where $A^circle.small$ and $macron(A)$ denote the interior and closure of $A$.
]<thm-ldp-averaged>

In the limit $epsilon -> 0$, the paths of $y_t$ concentrate around the typical
paths of $y_t$ which correspond to the minimisers of $I_T$. These satisfy the
Euler-Lagrange equations

$
  dif / (dif t) (partial cal(L)) / (partial dot(phi)) - (partial cal(L)) / (partial phi) = 0,
$

where the Lagrangian is
$
  cal(L)(t, phi, dot(phi)) eqdef ||[dot(phi)_t - macron(a)(t, phi_t)]^(tns)
  macron(d)(t, phi_t)^(-1)[dot(phi)_t - macron(a)(t, phi_t)]||^2
$
For a minimiser $phi^*_t$ with $phi^*_0 = x_0$, Freidlin-Wentzell theory states that for any $gamma > 0$,  

$
  PP[sup_(t in [0, T]) ||y_t - phi^*_t|| > gamma] -> 0 quad "as" epsilon -> 0.
$

The fluctuations of $y_t$ around a typical path $phi^*_t$ are Gaussian at scale
$sqrt(epsilon)$. Defining $zeta^(epsilon)_t eqdef (y_t - phi^*_t) \/ sqrt(epsilon)$, we
have $zeta^(epsilon)_t -> zeta_t$ in distribution as $epsilon -> 0$, where $zeta_t$ is
the Ornstein-Uhlenbeck process

$
  dif zeta_t = J_(x)[macron(a)_(alpha, 0)(t, phi^*_t)] zeta_t dif t
  + macron(b)(t, phi^*_t) dif W_t,
$<eq-gauss-dev-sde>

where $macron(a)_(alpha, 0)(t, x) = lim_(epsilon -> 0) macron(a)_(alpha,
epsilon)(t, x)$, $J_(x)[macron(a)_(alpha, 0)(t, phi^*_t)]$ is the Jacobian of
the average drift evaluated on the typical path. From @eq-gauss-dev-sde one can
construct a Gaussian tube by solving the corresponding Lyapunov equation for the
covariance of $zeta_t$

#proposition(title: [Typical paths of $x_t$ and $y_t$ coincide])[ Let $(t, x_t,
  lambda_t)$ evolve according to the coupled slow-fast system of
  @cor-coupled-sde with coefficients satisfying regularity conditions
  @lem-coeff-tilde-bounds, and let $y_t$ evolve according to the reduced SDE
  given in @def-reduced-sde, with coefficients satisfying conditions given in
  @lem-avg-coeff-bounds. Let $T > 0$, $gamma, epsilon>0$ and assume $x_0 = y_0
  in cal(D)_epsilon$ with $lambda_0 in [-1, 1]$. Let $phi^*_t in C^2([0, T];
  RR^d)$ be a minimiser of the Freidlin-Wentzell rate function given
  @thm-ldp-averaged with $phi^*_0 = x_0 = y_0$. Then for any $gamma > 0$,
  $
    PP[sup_(t in [0, T]) ||x_t - phi^*_t|| > gamma] -> 0 quad "as" epsilon -> 0.
  $
]<cor-typical-paths>



#proof[
  By the triangle inequality,
  $
    sup_(t in [0, T]) ||x_t - phi^*_t|| <= sup_(t in [0, T]) ||x_t - y_t|| + sup_(t in [0, T]) ||y_t - phi^*_t||,
  $
  which yeilds 
  $
    PP[sup_(t in [0, T]) ||x_t - phi^*_t|| > gamma] <= 
    PP[sup_(t in [0, T]) ||y_t - phi^*_t|| > gamma/2] +
    PP[sup_(t in [0, T]) ||x_t - y_t|| > gamma/2].
  $

  From @thm-ldp-averaged, we know that $PP[sup_(t in [0, T]) ||y_t - phi^*_t|| >
  gamma/2] -> 0$ as $epsilon -> 0$. For the second term, the @thm-avg-principle gives
  $
    PP[sup_(t in [0, T]) ||x_t - y_t|| > gamma / 2] <= (4 C_T) / gamma^2 sqrt(epsilon) -> 0,
  $
  as $epsilon -> 0$.

]

#remark[A full transfer of the LDP of the reduced (averaged) system, $y_t$ to the full
  system with $x_t$, requires a sharper controll of the error, specifically on
  requires exponential equivalence in distribution

  $
    lim_(epsilon -> 0) epsilon ln(PP[sup_(t in [0, T])||x_t - y_t|| > gamma]) = -oo.
  $

  However, for computing typical paths and their Gaussian fluctuations, that are
  the primary objects of intereset in applications of weak-noise SDE's, our
  averaging principle with $cal(O)(sqrt(epsilon))$ error affords complete
  characterisation.
]

= Examples

== Toy example

Let us consider a 1D system with $sigma(x) = x$

$
  a_(+)(t, x) = -x, quad a_(-)(t, x) = x,
$
and


$
  b_(+)(t, x) = r_(+) + x, quad b_(-)(t, x) = r_(-) - x, quad r_+, r_- > 0, r_+ != r_-, 
$


let $hat(r)_+ eqdef r_+ + r_-$, and $hat(r)_- eqdef r_+ + r_-$, when 
$
  a(t, x, lambda) = -lambda x, quad b(t, x, lambda) = 1/2 [hat(r)_+ + lambda (hat(r)_-  + 2 x)]
$


$
$


$
  a_(alpha, epsilon)(t, x) &= -lambda x +  (alpha epsilon lambda)/2 b(t, x, lambda) \
    &= lambda/2 (alpha epsilon hat(r)_+ - 2 x) + (alpha epsilon lambda^2)/2 (hat(r)_- + 2 x)
$





$
  dif x_t &=   a_(alpha, epsilon) dif t + b(t, x, lambda) dif W_t
$

$
  tilde(a)_(alpha, epsilon) = a_(alpha, epsilon), quad
  tilde(d)  = b^2 =  1/4[hat(r)_+^2 + 2 lambda hat(r)_+(hat(r)_ + + 2x) + lambda^2(hat(r)_ + + 2x)^2]
$




#pagebreak()
= Notes
== Tasks
- Fix the $alpha in [0, 1] -> alpha in {0, 1/2, 1}$ giving Itō, strato anti-ito 
- Chagne the line about Eq. 171
- Change the equation above 178
- Fix $P_ss -> P^(ss)_x $ make x paramterised than using the given notation.

== Application Ideas

- Sustained Oscillations Generated by Mutually Inhibiting Neurons with Adaptation Biol. Cybern. 52, 367-376 (1985)
  - feead back control neurons
    - usefull for central pattern generators
    - cost of control in weak noise environments??


- Numerical simulation of piecewise-linear models of gene regulatory networks
  using complementarity systems. Phys D 269:103–119
  - IRM

- Analysis and generic properties of gene regulatory networks with graded
  response functions Physica D 201 (2005) 150–176
  - might be the first analysis of gene regulatory netowrks with piecewise smooth functions




== Needs Testing
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




// = Poincaré Bounding (Useless)
// 
// Before we proceed it is usefull to introduce a defnition for the $L^2$ space that we will be working in, but first let us consider the general defintion of an $L^2$ inner product and norms on real functions.
// 
// #definition(title: [$L^2$-norm])[
// 
//   Let $(Omega, cal(F), mu)$ be a measurable space, where $Omega$ is the set,
//   $cal(F)$ is the sigma algebra of $Omega$, and $mu$ the measure. Then for any
//   $cal(F)$-measurable $f, g: Omega mapsto RR$, the $L^2$ inner product is defined as 
// 
//   $
//     chevron.l f, g chevron.r_(L^2_(mu)) eqdef  integral_(Omega) f(omega) g(omega) dif mu(omega),
//   $<eq-l2-inner-prod-def>
// 
//   which induces the $L^2$-norm
//   
//   $
//     lpnorm(f, L^2_(mu), ,) eqdef chevron.l f \, f chevron.r_(L^2_(mu))^(1/2)  =
//     (integral_(Omega) f^2(omega) dif mu(omega))^(1/2).
//   $<eq-l2-norm-def>
// ]
// 
// #corollary(title: [$L^2$-bound to supremum bound])[
// 
//   It is straight forward to bound an $L^2$-norm using a supremum bound via the
//   argument
// 
//   $
//     lpnorm(f, L^2_(mu), ,)
//     = (integral_(Omega) f^2(omega) dif mu(omega))^(1/2) 
//       &= (integral_(Omega) |f(omega)|^2 dif mu(omega))^(1/2), \
//       &<= sup_(x in Omega) |f(omega)| (integral_(Omega)  dif mu(omega))^(1/2), \
//       &<= sqrt(mu(Omega))sup_(x in Omega) |f(omega)|.
//   $
// ]<cor-l2-to-sup>
// 
// Since we have a probability density we will use the notation $inprod(., .,
// L^2_(P_t))$ and $lpnorm(., L^2_(P_t), ,)$ where $P_t in dom(cal(A)^*_x)$. 
// 
// #lemma(title: [Symmetry of $cal(A)_x$])[
//   Let $x in cal(D)_epsilon$ be fixed, $cal(A)_x$ be the backward generator
//   given in @lem-bwd-gen, and $P_(ss)(lambda | x)$ satisfy $cal(A)^*_x
//   P_(ss) = 0$ with $J_(ss)(pm 1) = 0$. Then $cal(A)_x$ is
//   symetric in $L^2_(P_(ss))$, that is satisfying the ralation 
// 
//   $
//     chevron.l cal(A)_x f, g chevron.r_(L^2_(P_oo)) =
//     chevron.l f, cal(A)_x g chevron.r_(L^2_(P_oo)) ,
//   $<eq-self-adjoint-def>
// 
//   for all $f, g in dom(cal(A)_x)$.
// 
// ]<lem-self-adjoint>
// 
// #proof[
//   We proceed by substituting @eq-bwd-gen into the left hand side of @eq-self-adjoint-def gives,
// 
//   $
//     integral_(-1)^(1)  (cal(A)_x f)(lambda) g(lambda) P_(ss)(lambda) = I_1 + I_2
//   $<eq-adj-setup>
//   where
//   $
//     I_1  &eqdef integral_(-1)^1 partial_lambda
//     f(lambda) tilde(a)_(alpha, epsilon)(t, x, lambda) g(lambda) P_(ss)(lambda) dif lambda ,\
//     I_2 &eqdef 1/2 integral_(-1)^1 partial^2_(lambda lambda) f(lambda) tilde(d)(t, x, lambda) g(lambda) P_(ss), dif lambda
//   $
//   are, respectively, the drift and diffusion contributions which we treat separately.
// 
//   _Drift term._ Integration by parts gives
// 
//   $
//     I_1     &= lr(f(lambda) tilde(a)_(alpha, epsilon)(t, x, lambda) g P_(ss)(lambda)|)_(-1)^1 
//     - integral_(-1)^1 f(lambda) partial_lambda [tilde(a)_(alpha, epsilon)(t, x, lambda) g(lambda) P_(ss)(lambda)] dif lambda.
//   $<eq-drift-adj-1>
// 
//   _Diffusion term_ -- Let $tilde(d)(t, x, lambda) eqdef tilde(b)(t, x, lambda)
//    tilde(b)(t, x, lambda)^tns$. Integrating by parts twice yeilds
// 
//   $
//     I_2 &= 1/2 lr(partial_lambda f(lambda) d(t, x, lambda) g(lambda) P_(ss)(lambda)|)_(-1)^1
//     - 1/2 integral_(-1)^1 partial_lambda f(lambda) partial_lambda [d(t, x, lambda) g(lambda) P_(ss)(lambda)] dif lambda, \
//       &= -1/2 lr(f(lambda) partial_lambda  [d(t, x, lambda) g(lambda) P_(ss)(lambda)]|)_(-1)^1
//       + 1/2 integral_(-1)^1 f(lambda) partial^2_(lambda lambda) [d(t, x, lambda) g(lambda) P_(ss)(lambda)] dif lambda,
//   $<eq-diff-adj-1>
// 
//   where the boundary term on the first line vanishes as $partial_lambda f(pm 1)
//   = 0$. Considering only the boundary terms from @eq-drift-adj-1 and @eq-diff-adj-1, 
// 
//   $
//     &lr(f(lambda)  {tilde(a)_(alpha, epsilon)(t, x, lambda) g(lambda) P_(ss)(lambda) - 
//     1/2 partial_lambda [tilde(d)(t, x, lambda) g(lambda) P_(ss)(lambda)]}|)_(-1)^1, \
//       &= lr(f(lambda)  {tilde(a)_(alpha, epsilon)(t, x, lambda) g(lambda) P_(ss)(lambda) - 
//     1/2 partial_lambda g(lambda) tilde(d)(t, x, lambda) g(lambda) P_(ss)(lambda) - g(lambda) partial_lambda [tilde(d)(t, x, lambda)  P_(ss)(lambda)]}|)_(-1)^1, \
//       &= lr(f(lambda) g(lambda) {tilde(a)_(alpha, epsilon)(t, x, lambda)  P_(ss)(lambda) 
//       -  partial_lambda [tilde(d)(t, x, lambda)  P_(ss)(lambda)]}|)_(-1)^1 = 0,
//   $
//   where we have used $partial_lambda g(pm 1) = 0$ and the condition in @eq-db-on-lam. Expanding the integrand of the first integral gives
// 
//   $
//     f(lambda)  partial_lambda [g(lambda) tilde(a)_(alpha, epsilon)(t, x, lambda) P_(ss)(lambda)] = 
//     f(lambda) [partial_lambda g(lambda) tilde(a)_(alpha, epsilon)(t, x, lambda)  P_(ss)(lambda) + g(lambda)partial_lambda [tilde(a)_(alpha, epsilon)(t, x, lambda) P_(ss)(lambda)]],
//   $
//   similarlay for the second integrand
//   $
//     1/2 f(lambda) partial^2_(lambda lambda) [d(t, x, lambda) g(lambda) P_(ss)(lambda)]  &= 
//     1/2 f(lambda) {
//       partial^2_(lambda lambda) g(lambda) tilde(d)(t, x, lambda)  P_(ss)(lambda) \
//         &+ 2 partial_lambda g(lambda) partial_lambda [tilde(d)(t, x, lambda)  P_(ss)(lambda)]  \
//         &+ g(lambda) partial^2_(lambda lambda) [tilde(d)(t, x, lambda)  P_(ss)(lambda)] }.
//   $
// 
//   The coeffcients of $g(lambda)$ vanish due to the steady-state condition on $P_(ss)(lambda)$, ie. $(cal(A)^(*)_x P_(ss))(lambda) = 0$, leaving
//   $
//     &1/2 f(lambda) partial^2_(lambda lambda) [d(t, x, lambda) g(lambda) P_(ss)(lambda)] - f(lambda)  partial_lambda [g(lambda) tilde(a)_(alpha, epsilon)(t, x, lambda) P_(ss)(lambda)] \
//       &= f(lambda) lr({
//         partial_lambda g(lambda) [tilde(a)_(alpha, epsilon)(t, x, lambda)
//       + 1/2 partial^2_(lambda lambda) g(lambda) tilde(d)(t, x, lambda) ] P_(ss)(lambda)  \
//             &quad  quad quad  - 2 partial_lambda g(lambda) [tilde(a)_(alpha, epsilon)(t, x, lambda) P_(ss)(lambda) - 1/2 partial_lambda [tilde(d)(t, x, lambda)  P_(ss)(lambda)]]
//       }).
//   $
// 
//   The second term vanishes due to the zero current condition leaving only
//   $(cal(A)_x g)(lambda)$ in the integrand, thus
// 
//   $
//     integral_(-1)^1 (cal(A)_x f) g P_(ss) dif lambda 
//     = integral_(-1)^1 f (cal(A)_x g) P_(ss) dif lambda.
//   $
// 
// ]
// 
// 
// 
// #todo[
//   add text here to say we need to introduce this theorem without proof, for the proof see .... 
// ]
// 
// 
// #theorem(title: [Poincaré inequality])[
// 
//   Let $P: [-1, 1] -> (0, oo)$ be a probability density and $d: [-1, 1] -> (0,
//   oo)$ satisfy $d(lambda) >= C_1 > 0$ and $P (lambda) >= C_2 > 0$ for all
//   $lambda in [-1, 1]$. Then for all $f in C^1([-1, 1])$ with $integral_(-1)^(1)
//   f(lambda) P (lambda) dif lambda = 0$
// 
//   $ integral_(-1)^(1) f^2(lambda) P (lambda) dif lambda <= 1/kappa
//     integral_(-1)^(1) [partial_lambda f(lambda)]^2 d(lambda) P (lambda) dif
//     lambda, $<eq-poincare-bound-def>
// 
// where $kappa = (C_1 C_2) \/ 2$.
// ]<thm-poincare-ineq>
// 
// #proof[
//   By the fudnemental thorem of calculus we have
//   $
//     f(a) - f(b) = integral_a^b partial_lambda f(lambda) dif lambda.
//   $
// 
//   Multiplying both sides by $P_ss (b)$ and integrating over the support gives
// 
//   $
//     integral_(-1)^(1) f(a)P (b) dif b - integral_(-1)^(1) P (b) f(b) dif b
//     = integral_(-1)^(1) P (b) integral_a^b partial_lambda f(lambda) dif lambda  dif b, 
//   $
// 
//   where the first integral on the left hand side simplifies due to the
//   normalisastion of probability, while the second vanishes to zero due to
//   $EE[f(lambda)] = 0$, giving the relation  
// 
//   $
//     f(a) = integral_(-1)^(1) P (b) integral_a^b partial_lambda f(lambda) dif lambda  dif b.
//   $
// 
//   Taking the absolute value, squaring and employing Caychy-Schwarz twice gives
//   us the bound
// 
//   $
//     | f(a)|^2 &= (integral_(-1)^(1) P (b) lr(|integral_a^b partial_lambda f(lambda) dif lambda |) dif b)^2 \
//       &<= (integral_(-1)^(1) P (b) dif b)
//       (integral_(-1)^(1)  P (b)
//       lr(integral_a^b |partial_lambda f(lambda) |^2 dif lambda) dif b), \
//       &<= 
//       (integral_(-1)^(1)  P (b)
//       integral_(-1)^(1) |partial_lambda f(lambda) |^2 dif lambda dif b), \
//       &<=  
//       2 lr(integral_(-1)^(1) lr([partial_lambda f(lambda) ])^2 dif lambda) .
//   $<eq-mod-f-bound>
// 
//   To obtain the desired bound from @eq-mod-f-bound, we simply multiply both by
//   $P (a)$ and integrate over the interval to get
//   $
//     integral_(-1)^(1) |f(a)|^2 P (a) dif a = integral_(-1)^(1) f^2(a) P (a) dif a &<=2 lr(integral_(-1)^(1) lr([partial_lambda f(lambda) ])^2 dif lambda), \
//     &<= 2 lr(integral_(-1)^(1)
//     lr([partial_lambda f(lambda) ])^2 / (d(lambda) P (lambda))  d(lambda) P (lambda)dif lambda), \
//       &<=2/(C_1 C_2) lr(integral_(-1)^(1)
//       lr([partial_lambda f(lambda) ])^2   d(lambda) P (lambda)dif lambda), \
//   $
// 
//   and letting $kappa = (C_1 C_2 )\/ 2$ yields @eq-poincare-bound-def.
// ]
// 
// 
// #corollary[
//   #todo[
//     add a small statement about how for $t >0$ $P_t in L 2$
//   ]
// ]<cor-l2-of-pt>
// 
// 
// #lemma(title: [Bounding zero-mean observables])[
//   Let $cal(A)_x$ be the backward generator defined in @eq-bwd-gen in
//   @lem-bwd-gen, $cal(A)^*_x$ be its adjoint defined in @eq-fwd-gen, let $P_ss
//   (lambda)$ be the steady-state probability density such that $(cal(A)^*_x
//   P_ss)(lambda) =0$, then for all $f in dom(cal(A))_x$ satisfying
// 
//   $
//     integral_(-1)^(1)f(lambda) P_ss (lambda) dif lambda = 0,
//   $ we have the inequality
// 
//   $
//     lpnorm(f, L^2_(P_ss), 2) <= 1/kappa inprod(-cal(A)_x f, f, L^(2)_(P_ss)) 
//   $
// ]<lem-zero-mean-bound>
// 
// #proof[
//   We need only show that 
//   $
//     inprod(-cal(A)_x f, f, L^(2)_(P_ss)) =  1/kappa
//     integral_(-1)^1  [partial_lambda f(lambda)]^2 tilde(d)(t, x, lambda) P_ss (lambda) dif lambda,
//   $
// 
//   since from @lem-coeff-tilde-bounds that $|tilde(d)(t, x, lambda)|$ and similarly we know
//   that $P_ss (lambda)$ is bounded from below from @lem-inv-meas-bound, therefore
//   we can direcly apply @thm-poincare-ineq.
//   
//   $
//     - integral_(-1)^(1) f(lambda) [partial_lambda f(lambda)  tilde(a)_(alpha, epsilon)(t, x, lambda) + 1/2 partial^2_(lambda lambda) f(lambda) tilde(d)(t, x, lambda)] P_ss (lambda) dif lambda = integral_(-1)^1 d(t, x, lambda) [partial_lambda f(lambda)]^2 P_ss (lambda) dif lambda
//   $
// 
//   then the rest follows via @thm-poincare-ineq
// 
//   #todo[
//     in notebook complete it.
//   ]
// ]
// 
// 
// #lemma(title: [Dense sets in $L^2([-1, 1])$])[
//   There exists a set $G subset dom(cal(A)_x)$ that are dense in $L^2$
//   
// ]<lem-dense-l2>
// 
// #proof[
//   #todo[
//     Look at zeidler
//   ]
// ]
// 
// #lemma(title: [zero mean observables in $L^2$])[
//   Let $cal(A)_x$ be the backward generator defined in @eq-bwd-gen in
//   @lem-bwd-gen, $cal(A)^*_x$ be its adjoint defined in @eq-fwd-gen, let $P_ss
//   (lambda)$ be the steady-state probability density such that $(cal(A)^*_x
//   P_ss)(lambda) =0$, then for all $g in L^2([-1, 1]; RR)$ satisfying
// 
//   $
//     integral_(-1)^(1)g(lambda) P (lambda) dif lambda = 0 quad  forall P in dom(cal(A)^*_x),
//   $ we have the inequality
// 
//   $
//     lpnorm(g, L^2_(P_ss),2)<= 1/kappa inprod(-cal(A)_x g, g, L^(2)_(P_ss)) 
//   $
// 
// ]<lem-zero-mean-bound-l2>
// 
// #proof[
//   By @lem-dense-l2, there exisits a sequence smooth function in $f_n in dom(cal(A)_x)$  such that 
//   $
//     lim_(n -> 0) ||f_n - g||  = 0,
//   $
// 
//   We can approximte any $g$ in $L^2([-1, 1])$, by a sequance of smooth $f in
//   dom(cal(A)_x)$, hence we can extend the  @lem-zero-mean-bound to generic $g$ 
// 
//   #todo[
//     finish proof, it's in zeidler
//   ]
// ]
// 
// 
// 
// #theorem(title: [Exponential mixing of the switching variable])[
// 
//   Let $x in cal(D)_epsilon$ be fixed, let $P_t in dom(cal(A))^*_x$ represent the
//   occupation probability density of $lambda$ conditioned on $x$, let $P_ss$ be
//   the invariant density, i.e $(cal(A)^*_x P_ss)(lambda) = 0$ which is uniformly
//   bounded from below by $P_ss (lambda) >= C_1 > 0$ and let the diffusion
//   coefficient satisfy $|tilde(d)(t, x, lambda)| >= C_2 > 0$ defined in
//   @eq-d-tilde-def, be uniformaly bounded from below. Then for any $t_0 > 0$ and $t in [0,
//   T]$ such that $ t_0 <= t$, and for any measurable $A subset [-1, 1]$, there
//   exisits $kappa(x)>0$ and $C(x) > 0$ such that
// 
//   #todo[
//     fix statement
//   ]
// 
//   $
//     lr(| integral_A [P_(t) (lambda | x) dif lambda - P_(ss)(lambda | x)]dif lambda |) <= C(x) ee^(-kappa(x) (t - t_0)).
//   $
// ]<thm-exp-mixing>
// 
// #proof[
//   To aid in the proof we define $xi_t (lambda) eqdef P_t (lambda) - P_ss
//   (lambda)$, and $zeta_t (lambda) eqdef xi_t (lambda) \/ P_ss (lambda)$, where
//   we have dropped the conditional argument in the noation. Clearly $zeta_t
//   (lambda)$ has
// 
//   $
//     lr(|integral_A [P_t (lambda) - P (lambda)] dif lambda |)
//       &= lr(|integral_A zeta_t (lambda) P_ss (lambda) dif lambda|), \
//       &<= integral_(-1)^1 |zeta_t (lambda)| P_ss (lambda) dif lambda,  \
//       &<= (integral_(-1)^1 zeta^2_t (lambda) P_ss (lambda) dif lambda )^(1\/2)
//           (integral_(-1)^1  P_ss (lambda) dif lambda)^(1\/2), \ 
//       &<= (integral_(-1)^1 zeta^2_t (lambda) P_ss (lambda) dif lambda )^(1\/2), \
//       &= lpnorm(zeta_t, L^2_(P_ss),  ,).
//   $
//   Then it only remains to bound $lpnorm(zeta_t, L^2_(P_ss),  ,)$
//   $
//     dif /(dif t) lpnorm(zeta_t, L^2_(P_ss), 2 ,) &= 2 integral_(-1)^1  zeta_t (lambda) partial_t zeta_t (lambda) P_ss (lambda) dif lambda, \ 
//       &= -2 inprod(-cal(A)_x zeta_t,  zeta_t, L^2_(P_ss)), \
//       &<= -2kappa lpnorm(zeta_t, L^2_(P_ss),  2,), quad #text[(by @lem-zero-mean-bound-l2)].
//   $ 
// 
//   Then by Gronwall's inequality we have 
// 
//   $
//     lpnorm(zeta_t, L^2_(P_ss),  ,) <= lpnorm(zeta_(t_0), L^2_(P_ss),  ,) ee^(- kappa (t - t_0)).
//   $
// 
//   We know from @lem-inv-meas-bound that $0 < C_P (x) <= P_ss (lambda | x)$, hence
//   $
//     lpnorm(zeta_(t_0), L^2_(P_ss), 2,) &= integral^(1)_(-1) {
//     [P_(t_0) (lambda | x) - P_ss (lambda)]^2 / (P^2_(ss)(lambda |x))
//     P_ss (lambda | x)} dif lambda, \
//       &<= 2/(C_(P)(x)) integral^(1)_(-1)
//       [P^2_(t_0) (lambda | x) + P^2_ss (lambda | x)] dif lambda \
//       &<= 4/(C_(P)(x)),\
//   $
// 
//   where we have implicitly used that fact that $lpnorm(P_(t_0), L^2_(P_ss), ,)<
//   oo$ for any $t_0 > 0$ and for any initial distribution of $lambda$ which is
//   guaranteed by @lem-lam-smooth-denst. Finally, defining $C(x) = 2  \/ sqrt(
//   C_(P)(x))$ yields desired result.]
// 
// 
//  ignore   below
//   $
//     lpnorm(zeta_0, L^2_(P_ss), 2,) &= integral^(1)_(-1) {
//     [P_(t_0) (lambda | x) - P_ss (lambda)]^2 / (P^2_(ss)(lambda |x))
//     P_ss (lambda | x) }dif lambda,  \
//       &<= integral^(1)_(-1) {[(P_(t_0) (lambda | x)) /(P_ss (lambda | x))  - 1]^2 P_ss (lambda | x)} dif lambda \
//       &<= 2 integral^(1)_(-1) [(P_(t_0) (lambda | x)) /(P_ss (lambda | x))  - 1]^2 P_ss (lambda | x) dif lambda \
//     
//   $
// 
// #corollary(title: [Bound on expectations])[
// 
//   The bound on the differences between probability measures obtained in
//   @thm-exp-mixing affords us a further bound, namely on the differences in
//   expectations via
// 
//   $
//     lr(|EE[f(lambda_t)] - integral^(1)_(-1) f(lambda)  P_ss (lambda) dif lambda |) 
//       &= lr(|integral_(-1)^(1) f(lambda) zeta_t (lambda) P_ss (lambda) dif lambda|) ,\
//       &= lr(|inprod(f,  zeta_t,L^2_(P_ss))|) ,\
//       &<= lpnorm(f, L^2_(P_ss),  ,) lpnorm(zeta_t, L^2_(P_ss),  ,) ,\
//       &<=  lpnorm(f, L^2_(P_ss),  ,) lpnorm(zeta_0, L^2_(P_ss),  ,) ee^(-kappa t), \
//       &<=  sup_(lambda in [-1, 1]) | f(lambda) | lpnorm(P_(t) (lambda) - P_(ss) (lambda), L^2_(P_ss),  ,) ee^(-kappa t).
//       &<= lpnorm(f, L^2_(P_ss),  ,) lpnorm(zeta_t, L^2_(P_ss),  ,) ,\
//       &<=  sup_(lambda in [-1, 1]) | f(lambda) |  lpnorm(zeta_(t_0), L^2_(P_ss),  ,) ee^(-kappa (t - t_0)), \
//       &<=  C(x) ee^(-kappa (t - t_0)) sup_(lambda in [-1, 1]) | f(lambda) |  , 
//   $
// 
//   where we have used @cor-l2-to-sup. Obviously, the final supremum bound is of
//   course only meaningful when we have bounded $f$ on the interval $lambda in
//   [-1, 1]$.
// ]<cor-exp-mixing-obs>
// 
// #lemma(title: [Bounds on the invariant measure])[
//   For all $x in cal(D)_epsilon$ fixed  and $||tilde(a)_(alpha, epsilon)(t, x, lambda)|| <= tilde(C)_1 (x)$  and $||tilde(b)(t, x, lambda)|| >= tilde(C)_1 (x) > 0$, set $tilde(C)_(12)(x) = tilde(C)_1 (x) \/ tilde(C)_2 (x)$
// 
// 
//   $
//     (tilde(C)_12 (x) exp[-tilde(C)_12 (x)(1 + 4 |lambda|)]) / sinh(tilde(C)_12 (x)) <= P_ss (lambda | x)
//    <= (tilde(C)_12 (x) exp[tilde(C)_12 (x)(1 + 4 |lambda|)]) / sinh(tilde(C)_12 (x)) 
// 
//   $
// 
//   #todo[
//     rewrite in a simpler form by introducing some auxiliary $G(x)$
//   ]
// ]<lem-inv-meas-bound>
// 
// 
// 
// #proof[
// 
//   #todo[
//     proof in notebook add it in.
//   ]
//   
// 
//   $
//     (alpha ee^(-2 alpha (1 + 4 |lambda|)))/ sinh(alpha/2)
//     <= P_ss (lambda)
//    <= (alpha ee^(2 alpha (1 + 4 |lambda|)))/ sinh(alpha/2)
// 
//   $
// ]
// 
// #corollary[
// 
//   It is useful to have the following uniform lower bound for invariant density
// 
//   $
//     (tilde(C)_12 (x) exp[-tilde(C)_12 (x)(1 + 4 |lambda|)]) /
//     sinh(tilde(C)_12 (x)) &>= 1/2 tilde(C)_12 (x) ee^(-tilde(C)_(12)(x) (2 + 4|lambda|)) \
//       &>=1/2 C(1 + ||x||) ee^(-tilde(C)_(12)(x) (2 + 4|lambda|)), \
//       &=C_P (x)
//   $
//   where it is uesful to define
//   $
//     C_P (x) eqdef 1 / 2 C ee^(-6C) (1 + ||x||) ee^(-6 C ||x||)
//   $
// 
//   $
//     C_P (x) eq.def 1 / 2 C ee^(-6C) (1 + ||x||) ee^(-6 C ||x||)
//   $
// 
// ]


#pagebreak()

#bibliography("fixlib.bib")   

// Local Variables:
// typst-preview--master-file: "/home/seeralan/work/noty-vt/areas/nssde.typ"
// End:
