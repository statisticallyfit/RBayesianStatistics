# 1. plot
graphviz.plot(dag)


# 2. plot, highlight nodes, args that are part of one or more v-structures
vs <- vstructs(dag, arcs=TRUE); vs
hl <- list(nodes = unique(as.character(vs)), arcs=vs)
graphviz.plot(dag, highlight = hl)

# 3. highlight path A->O
hl <- matrix(c("A", "E", "E", "O"), ncol = 2, byrow = TRUE)
graphviz.plot(dag, highlight = list(arcs = hl))

# 4. plot conditional prob table of education
bn.fit.barchart(bn$E)

# 5. compare graphically distributions of Education for male and female
# interviewees
junction <- compile(as.grain(bn))
jmale <- setEvidence(junction, "S", states="M")
jfemale <- setEvidence(junction, "S", states="F")
library(gridExtra)
      # P(E | sex = male)
p1 <- barchart(querygrain(jmale, nodes="E")$E,  
               main = "Male", xlim = c(0, 1))
      # P(E | sex = female)
p2 <- barchart(querygrain(jfemale, nodes = "E")$E, 
               main = "Female", xlim = c(0, 1))
grid.arrange(p1, p2, ncol = 2)
