
### Conditional Independence Tests: identify presence of individual arcs (individual)

# if H0 for conditional indep is rejected, arc is included in DAG
df <- (nlevels(survey[, "T"]) - 1) * (nlevels(survey[, "E"]) - 1) * 
      nlevels(survey[, "O"]) * nlevels(survey[, "R"]); df

# Test if conditionally independent
# H0 = T is independent of E | (O & R)
ci.test("T", "E", c("O", "R"), test="mi", data=survey) # G^2 tests
ci.test("T", "E", c("O", "R"), test="x2", data=survey) # chi-square test

# Should we remove O -> T? What about R-> T?
# H0 = T is independent of O|R 
ci.test("T", "O", "R", test = "x2", data=survey)

# Test all - strength = p-value
# reports p-value of criterion difference in models: dag_before vs dag_after
# where dag_after has that particular arc removed. 
arc.strength(dag, data = survey, criterion = "x2")




### Network Scores: how well DAG mirrors dependence structure of data (as whole)
# Types of scores: BIC, BDeu

score(dag, data = survey, type="bic")
score(dag, data = survey, type="bde", iss=10)

# does dag fit data better BEFORE or AFTER adding arc E -> T?
dag4 <- set.arc(dag, from="E", to="T")
nparams(dag4, survey)
score(dag4, data = survey, type="bic")

rnd <- random.graph(nodes = c("A", "S", "E", "O", "R", "T"))
modelstring(rnd)
score(rnd, data = survey, type="bic") # so dag is better than random! explains some structure


# Hill-climbing algorithm to find the DAG that maximizes network score
learned <- hc(survey) # the algorithm returns a DAG
modelstring(learned); graphviz.plot(learned)
arc.strength(learned, data = survey, criterion = "x2")
arc.strength(dag, data = survey, criterion = "x2")
# reports change in bic score caused by respective arc removal
arc.strength(learned, data = survey, criterion = "bic") #before score < after arc removal score
arc.strength(dag, data = survey, criterion = "bic") #more after scores are better...

learned2 <- hc(survey, score="bde")
modelstring(learned2)
graphviz.plot(learned2)
