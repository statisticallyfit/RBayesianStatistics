# 1. In bn.mle compare distribution of O|A with corresponding one from querygrain
junction.mle <- compile(as.grain(bn.mle))
jage.young <- setEvidence(junction.mle, "A", states="young")
jage.old <- setEvidence(junction.mle, "A", states="old")
# P(O)
querygrain(junction.mle, nodes = "O")$O
# P(O | A = young)
querygrain(jage.young, nodes="O")$O
# P(O | A = old)
querygrain(jage.old, nodes="O")$O


# 2. How many random observations are needed for cpquery to make estimates of
# parameters of these two distributions with precision +- 0.01?
set.seed(123)
cpquery(bn.mle, event = (O == "emp"), 
        evidence = list(A = "young"), method = "lw", 
        n = 10^3) # enough for likelihood weighting
cpquery(bn.mle, event = (O == "emp"), 
        evidence = (A == "young"), method = "ls", 
        n = 10^4) # enough for logic sampling


# 3. Extract dag from bn.mle
dag.ex3 <- bn.net(bn.mle)
graphviz.plot(dag.ex3)


# 4. Which nodes d-separate Age and Occupation?
sapply(nodes(dag), function(z) dsep(dag, "A", "O", z))
# ... dsep(dag, "A", "O", "T")

