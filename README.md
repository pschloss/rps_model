
#For either neighbourhood, if an empty lattice point is selected for updating, the probability that it is filled with a cell of type i (with iset symbol{C,S,R}) is given by fi, the fraction of its neighbourhood occupied by strain i.
#
#If an occupied lattice point in state i is selected, it is killed with probability Deltai. Although DeltaC and DeltaR are fixed values, DeltaS is not; it is equal to DeltaS,0 + tau(fC), where DeltaS,0 is the probability of death of an S cell without any neighbouring C cells, and tau measures the toxicity of neighbouring C cells.
#
#If memory serves, the following transitions in a focal point were possible (written "X-->Y  :  P", where X is the state of the point before the transition, Y is the state after, and P is the probability of transition)
#
#S-->0  :  delta_S = delta_S,0 + tau * f_C
#S-->S :  1 - delta_S
#R-->0 :  delta_R
#R-->R :  1 - delta_R
#C-->0 :  delta_C
#C-->C : 1 - delta_C
#0-->S : f_S
#0-->R : f_R
#0-->C : f_C
#0-->0 : 1 - f_S - f_R - f_C
#
#The state "0" is an empty point, bolded quantities are parameters, and the italics quantity is a function. For each unique initial state, the probabilities sum to 1 (as they should).
#
#So there are really three kinds of transitions: deaths (X-->0), survivals (X-->X) or births (0-->X), where X is in {S,R,C}.  One cell type cannot directly replace another cell type in this model (i.e., the transition S-->C does not occur during one asynchronous update).  Also, there is no fecundity differences in our three players in this model (rate of occupying an empty site is simply proportional to the fraction of the neighborhood of that empty site occupied by type X, i.e., f_X).  Thus, most of the action occurs through viability differences.

#resistant = 1
#sensitive = 2
#colicin = 3
#empty = 0

#original
#f9b94e18164fd5fb2159391335284ba55440dce2
library(rbenchmark)
benchmark(source('rps.R'), replications=1)
#             test replications elapsed relative user.self sys.self user.child
#1 source("rps.R")            1   5.512        1     5.505    0.006          0
#  sys.child
#1         0


#synchronus
#7b1917b661c36c4132b125f8b7cd267b780bad21
library(rbenchmark)
benchmark(source('rps.R'), replications=1)
#             test replications elapsed relative user.self sys.self user.child
#1 source("rps.R")            1   5.438        1      5.43    0.007          0
#  sys.child
#1         0
