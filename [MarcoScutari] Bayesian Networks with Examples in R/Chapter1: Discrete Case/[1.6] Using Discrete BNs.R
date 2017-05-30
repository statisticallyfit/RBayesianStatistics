#install.packages("gRain") #didn't work
#biocLite("gRain")
library(lattice)


#Are two ndoes directly separated? (d-separated)
graphviz.plot(dag)
dsep(dag, x = "S", y = "R")
dsep(dag, x = "O", y = "R")
path(dag, from = "S", to="R")

# now conditioning on E, S->E->R is blocked so S -> R directly separated is TRUE
# serial connection
dsep(dag, x = "S", y = "R", z = "E")

# divergent connection
dsep(dag, x = "O", y = "R")
dsep(dag, x = "O", y = "R", z = "E")

# convergent connection: only dsep when not conditioning on E, pointed to by A and S
dsep(dag, x = "A", y = "S")
dsep(dag, x = "A", y = "S", z = "E")



#-------------------------------------------------------------------------------------

### Using conditional prob queries and most likely explanation queries by
### exact and approximate inference

# Exact Inference, EXAMPLE1 - different preference in transport of women vs all?
junction <- compile(as.grain(bn)) #transform bn into junction tree to do inference
junction
# This is P(T)
jsex <- setEvidence(junction, nodes = "S", states = "F"); jsex
querygrain(junction, nodes = "T")$T ###### P(T)
querygrain(jsex, nodes = "T")$T     ###### P(T | S = female)
# conclude: no major differences in probabilities ==> so women show same preferences
# towards car and train use as interviewees as a whole


# Exact Inference, EXAMPLE2 - how living in small city affects car and train use
jres <- setEvidence(junction, nodes = "R", states = "small")
querygrain(junction, nodes = "T")$T ###### P(T)
querygrain(jres, nodes = "T")$T     ###### P(T | R=small)
#conclude: so people in small residence prefer train more  and car less
# and 'other' more than all people combined


# Exact Inferece, EXAMPLE3 - relation between S and T conditioning on E=high
jedu <- setEvidence(junction, nodes = "E", states = "high"); jedu
SandT.cpt <- querygrain(jedu, nodes = c("S", "T"), type = "joint")
SandT.cpt # This is P(S and T | E = high)

#illustration of types of distributions
querygrain(jedu, nodes = c("S", "T"), type = "marginal") # P(S|E=high) with P(T|E=high)
querygrain(jedu, nodes = c("S", "T"), type = "conditional") # P(S|T|E=high)

# for the last one, we discovered: 
# P(S=F | T=t | E=high) = P(S=F | E=high)
# P(S=M | T=t | E=high) = P(S=M | E=high), which means
# S is independent of T conditional on E. Gender does not tell of transport
# preferences if we know person's education

# method 1 to confirm the above
dsep(bn, x = "S", y = "T", z = "E") 

# method 2 to confirm the above
SandT.ct = SandT.cpt * nrow(survey) # make contingency table
SandT.ct
chisq.test(SandT.ct) # ha absolutely independent! pvalue=1



### Approximate Inference (monte carlo)

# recompute first cell of SandT.cpt table P(S=M and T=car | E=high)
SandT.cpt[[1]]
cpquery(bn, event = (S == "M") & (T == "car"), evidence = (E == "high"))

#improve approximationg by increasing number of random observations
cpquery(bn, event = (S == "M") & (T == "car"), evidence = (E == "high"), n = 10^6)

# likelihood weighting (lw) is more accurate
cpquery(bn, event = (S == "M") & (T == "car"), 
        evidence = list(E = "high"), method="lw") #evidence list holds all conditioning variables

# example of more complex query: P(S=M, T=car | {A=young, E=uni} OR {A=adult})
cpquery(bn, even = (S == "M") & (T == "car"), 
        evidence = ((A == "young") & (E == "uni")) | (A == "adult"))



# Using cpdist: returns df containing random obs for variables in nodes that 
# match evidence
# Can be used for any type of inference: since it just returns the counts
SxT <- cpdist(bn, nodes = c("S", "T"), evidence = (E == "high"))
head(SxT)

# example: produce S and T table and compare to querygrain table
prop.table(table(SxT))
SandT.cpt



### Plotting DAGs
graphviz.plot(dag) #default layout="dot
graphviz.plot(dag, layout="fdp")
graphviz.plot(dag, layout="circo")

# makeing S-E-R black and E grey filled
hlight <- list(nodes = nodes(dag), arcs = arcs(dag), col="grey", textCol = "grey")
pp <- graphviz.plot(dag, highlight = hlight)
edgeRenderInfo(pp) <-
      list(col = c("S~E" = "dodgerblue", "E~R" = "purple"), 
           lwd = c("S~E" = 3, "E~R" = 3))
nodeRenderInfo(pp) <- 
      list(col = c("S" = "dodgerblue", "E" = "gold", "R" = "purple"), 
           textCol = c("S" = "black", "E" = "black", "R" = "black"), 
           fill = c("E" = "yellow"))
renderGraph(pp)

# Plotting conditional probability distributions (tables)
bn.fit.barchart(bn.mle$T, main = "Travel", 
                xlab = "P(T | R and O)", ylab = "")
bn.fit.dotplot(bn.mle$T, main = "Travel", 
               xlab = "P(T | R and O)", ylab = "")


# Plotting this info: 
# This is P(T)
#jsex <- setEvidence(junction, nodes = "S", states = "F")
#querygrain(junction, nodes = "T")$T ###### P(T)
#querygrain(jsex, nodes = "T")$T     ###### P(T | S = female)
#querygrain(jres, nodes = "T")$T     ###### P(T | R=small)
Evidence <- factor(c(rep("Unconditional",3), rep("Female", 3), 
                     rep("Small City", 3)), 
                   levels = c("Unconditional", "Female", "Small City"))
Travel <- factor(rep(c("car", "train", "other"), 3), 
                 levels = c("other", "train", "car"))
distr <- data.frame(Evidence = Evidence, Travel = Travel, 
                    Prob = c(0.5618, 0.2808, 0.15730, 
                             0.5620, 0.2806, 0.15730, 
                             0.4838, 0.4170, 0.0990))
distr

# layout=c(3,1) but I changed it to 1,1
barchart(Travel ~ Prob | Evidence, data=distr, 
         layout = c(3,1), xlab = "probability", 
         scales = list(alternating = 1, tck = c(1, 0)), 
         strip = strip.custom(factor.levels = 
                                    c(expression(P(T)), 
                                      expression(P({T} * "|" * {S == F})),
                                      expression(P({T} * "|" * {R == small})))), 
         panel = function(...) {
               panel.barchart(...)
               panel.grid(h = 0, v = -1)
         })