# 1. Create an object of class bn for the dag for the survey
dag.ex <- model2network(modelstring(dag))
graphviz.plot(dag.ex)

# 2. Use the functions in bnlearn and dag.ex to extract nodes and arcs of the DAG.
# Also extract parents and children of each node. 
nodes(dag)
arcs(dag)
# did not work...
#pars <- sapply(nodes(dag.ex), parents, X=dag.ex)
#chld <- sapply(nodes(dag.ex), children, X=dag.ex)
dag$nodes$A$children
dag$nodes # ANSWER

# 3. print the model formula from the bn
modelstring(dag)

# 4. Fit the parameters of the network from the survey data
# using their Bayesian estimators and save result in bn.fit.ex
survey.ex <- read.table("../data/survey.txt", header=TRUE)
head(survey.ex)
bn.fit.ex <- bn.fit(dag, survey.ex, method="bayes")
bn.fit.ex

# 5. Remove the arc from Education to Occupation
dag.ex2 <- drop.arc(dag.ex, from = "E", to = "O")
graphviz.plot(dag.ex2)
graphviz.plot(dag.ex)

# 6. Fit the parameters of the modified network. Which local
# distributions change, and how?
bn.fit.ex2 <- bn.fit(dag.ex2, survey.ex, method="bayes")
bn.fit.ex2
bn.fit.ex

coef(bn.fit.ex2$O) # removed arc
coef(bn.fit.ex$O)  # old arc
