# 5. Implement function for BN inference through rejection sampling using
# the description provided in section 1.4

rejection.sampling <- function(BN, numSimulations, event.node, 
                               event.value, evidence.node, evidence.value){
      
      sims <- rbn(BN, numSimulations)
      m1 <- sims[sims[, evidence.node] == evidence.value, ]
      m2 <- m1[m1[, event.node] == event.value, ]
      return(nrow(m2)/nrow(m1))
}

rejection.sampling(bn.mle, numSimulations = 10^4, 
                   event.node = "O", event.value = "emp", 
                   evidence.node = "A", evidence.value = "young")
