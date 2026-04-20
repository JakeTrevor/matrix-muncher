= Cloning an encrypted qubit


#let tens = math.times.o
#let bigtens = math.times.o.big
#let ket = x => [|#x #math.chevron.r]

To begin with, let's define

The bell $phi.alt^+$, which is defined as so:

$ ket(phi.alt^+) := 1/(sqrt(2)) (ket(00) + ket(11)) $

Then, given some initial state $ket(alpha)$ the starting state for our algorithm is:


$ rho_n := ket(alpha) tens (bigtens_n ket(phi.alt^+)) $

Note that for some $n$, the qubit count is $2n +1$.


We then need to define some utility funcitons, which we use in the rest of this document:

#grid(
  columns: (1fr, 1fr),
  $
     alpha (n,mu) & :: NN -> {0,1,2,3} -> CC \
    alpha (\_, 0) & = 1 \
    alpha (\_, 1) & = i \
     alpha (n, 2) & = -(i^(n+1)) ? "or" = (-i)^(n+1) \
    alpha (\_, 3) & = i
  $,
  $
    sigma_mu & :: {0, 1, 2, 3} -> CC^(2 times 2) \
     sigma_0 & = "ID"_2 \
     sigma_1 & = X \
     sigma_2 & = Y \
     sigma_3 & = Z
  $,
)

And we can now define our Matrix function


$
  "enc"^(n) :=
  [2/sqrt(2) "ID"_(n+1) - i 2/sqrt(2) (bigtens_(n+1) X)]
  [2/sqrt(2) "ID"_(n+1) - i 2/sqrt(2) (bigtens_(n+1) Z)]
$



$
  "enc"^(n) := 1/2 sum_(mu = 0)^3 (1/(alpha(n, mu)) sigma_mu tens (bigtens_(i=1)^n sigma_mu tens "ID" ))
$
