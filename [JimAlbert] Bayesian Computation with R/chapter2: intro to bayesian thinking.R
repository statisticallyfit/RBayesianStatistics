library(LearnBayes)
library(ggplot2)

#library(devtools)
library(ggfortify)


# USING A DISCRETE PRIOR

# create the prior and set up the probabilities
p = seq(0.05, 0.95, by=0.1)
prior = c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0)
prior = prior/sum(prior) # normalize to probability form

# plot the prior
dataOfPriors = data.frame(p, prior)
graph = ggplot(aes(x=p, y=prior), data=dataOfPriors)
graph + geom_point(shape=19) 

plot(p, prior, type="h", lwd=10, main="The Prior Distribution")

# find the posterior
data = c(11,16) # since 11/27 students sleep at least 8 hours
post = pdisc(p, prior, data)
round(cbind(p, prior, post), 2)


# plot the posterior
library(lattice)
PRIOR = data.frame("prior", p, prior); PRIOR
POST = data.frame("posterior", p, post); POST
names(PRIOR) = c("Type", "P", "Probability")
names(POST) = c("Type", "P", "Probability")
data = rbind(PRIOR, POST); data
xyplot(Probability~P|Type, data=data, layout=c(1,2), type="h", 
       lwd=5, col="dodgerblue")





# USING A BETA PRIOR

# Beta distribution is used because it is updated easily with arguments a+s and b+f
# formula = p^(a-1) * (1-p)^(b-1)
# mean = a/(a+b), variance= m(1-m)/(a+b+1)

quantile2 = list(p=0.9, x=0.5)
quantile1 = list(p=0.5, x=0.3)
parameters = beta.select(quantile1, quantile2)
a = parameters[1]
b = parameters[2]

# posterior density is: g(p|data) ~ p^(a+s-1)*(1-p)^(b+f-1), 
# where s = 11, and f = 16
a; b; s = 11; f = 16

priorDist <- ggdistribution(dbeta, seq(0,1,0.01), shape1=a, shape2=b, col="red")
likDist <- ggdistribution(dbeta, seq(0,1,0.01), shape1=s+1, shape2=f+1, col="limegreen", p=priorDist)
ggdistribution(dbeta, seq(0, 1, by=0.01), shape1=a+s, shape2=b+f, col="blue", p=likDist)

# OR
curve(dbeta(x, a+s, b+f), from=0, to=1, 
      xlab="p", ylab="Density", lty=1, lwd=4)
curve(dbeta(x, s+1, f+1), add=TRUE, lty=2, lwd=4)
curve(dbeta(x, a, b), add=TRUE, lty=3, lwd=4)
legend(0.7, 4, c("Prior", "Likelihood", "Posterior"), 
       lty=c(3,2,1), lwd=c(3,3,3))

# Is it likely that proportion of heavy sleepers > 0.5?
# solution: find P(p >= 0.5|data)
1 - pbeta(0.5, a+s, b+f) # unlikely

# 90% interval estimate for p is: 
qbeta(c(0.05, 0.95), a+s, b+f)

# Another way to summarize a posterior density: simulation
psim = rbeta(1000, a+s, b+f)
graph = ggplot(aes(psim), data=data.frame(psim))
graph + geom_density(fill="lightskyblue")
graph + geom_histogram(binwidth=0.05, fill="lightskyblue")

# Estimate probability that proportion > 0.5
sum(psim >= 0.5)/1000

# 90% confidence interval
quantile(psim, c(0.05, 0.95))




# USING HISTOGRAM PRIOR

midpt = seq(0.05, 0.95, by=0.1)
prior = c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0)
prior = prior/sum(prior)

# the prior
curve(histprior(x,midpt,prior), from=0, to=1, ylab="Prior density",ylim=c(0,.3))
# the posterior
curve(histprior(x, midpt, prior) * dbeta(x, s+1, f+1), 
      from=0, to=1, ylab="Posterior density")

# to sample from the posterior, make grid of values of p