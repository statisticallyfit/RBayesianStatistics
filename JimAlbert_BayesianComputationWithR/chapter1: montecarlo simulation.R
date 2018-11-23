
# Monte Carlo Simulation: finding the true significance 
#level for t-stat when populations violate normality and 
#constant variance assumptions

# Procedure: 
# 1 simulate random sample from first and second populations
# 2 find t-statistic for both
# 3 decide if |t| exceeds critical point. Repeat N times
# alpha_true = number of null rejections/N = proportion of times the means of sampled values weren't significantly different

tstatistic <- function(x,y) {
  n1 = length(x)
  n2 = length(y)
  # compute std pooled
  sp = sqrt(((n1-1)*sd(x)^2 + (n2-1)*sd(y)^2)/(n1 + n2 - 2)); sp
  t.stat = (mean(x) - mean(y))/(sp*sqrt(1/n1 + 1/n2))
  return(t.stat)
}

t.simulation = function(){
  tstatistic(rnorm(n1, mean=0, sd=1), rnorm(n2, mean=0, sd=1))
}



# call
alpha=0.1; num.reps = 10000
x = rnorm(n1, mean=10, sd=2)
y = rexp(n2, rate=1/30)

tstat.vector = replicate(num.reps, t.simulation())
num.rejects = length(tstat.vector[abs(tstat.vector) > qt(1-alpha/2, n1+n2-2)])
true.alpha = num.rejects/num.reps

plot(density(tstat.vector), xlim=c(-5, 8), ylim=c(0, 0.4), lwd=4)
curve(dt(x, df=18), add=TRUE, lwd=2, col="mediumpurple3")





# -------------------- EXERCISES ----------------------
library(LearnBayes)
library(ggplot2)

data("studentdata")
head(studentdata)
attach(studentdata)

# 1 Movie dvds
hist(Dvds)
summary(Dvds)
table(Dvds)
barplot(table(Dvds))

# 2 Student heights
output = boxplot(Height~Gender, plot=F) # y~x
output

graph <- ggplot(aes(x=Gender, y=Height), data=studentdata)
graph + geom_boxplot(aes(fill=Gender))
graph + geom_boxplot(aes(fill=Gender), alpha=0.4)
graph + geom_boxplot(aes(fill=Gender)) + scale_fill_manual(values=c("dodgerblue", "pink"))
graph + geom_boxplot(aes(fill=Gender)) + scale_fill_manual(values=c("purple", "yellow"))

# 3 Sleeping times
model = lm(WakeUp~ToSleep)
plot(ToSleep, WakeUp, pch=20); abline(WakeUp~ToSleep)
abline(model, lwd=3, col="dodgerblue")
coeffs = model$coefficients; coeffs
coeffs[1] + coeffs[2]*0

graph = ggplot(aes(x=ToSleep, y=WakeUp), data=studentdata)
graph + geom_point(shape=19) + geom_smooth(method="lm", lwd=1, col="dodgerblue")


# 4 Performance of traditional confint for prop
binomialConfInterval = function(X, n) {
  z = qnorm(0.95)
  phat = X/n
  se = sqrt(phat*(1-phat)/n)
  return(c(phat-z*se, phat+z*se))
}

confSimulation = function(){
  X = rbinom(size=simulations, 1, p=0.5); 
  binomialConfInterval(X, simulations) 
}


makeDataFrameOfConfIntervals = function() {
  lowerVec = {}; upperVec = {}
  
  for(i in 1:simulations){
    confint = conf.simulation()
    lowerVec[i] = confint[1]
    upperVec[i] = confint[2]
  }
  return(data.frame(lowerVec, upperVec))
}

findNumOfCoverages = function(){
  coverages = 0
  
  dataFrame = makeDataFrameOfConfIntervals()
  for(i in 1:simulations){
    lower = dataFrame[i, 1]
    upper = dataFrame[i, 2]
    if(p >= lower & p <= upper)
      coverages = coverages + 1
  }
  return(coverages/simulations)
}


# call
m=1000

# -------- n = 10 ------
n = 10; p = 0.05
coverages = replicate(m, findNumOfCoverages())
mean(coverages) # a snapshot of contents in the coverages vector

n = 10; p = 0.25
coverages = replicate(m, findNumOfCoverages())
mean(coverages) # a snapshot of contents in the coverages vector

n = 10; p = 0.50
coverages = replicate(m, findNumOfCoverages())
mean(coverages) # a snapshot of contents in the coverages vector

# -------- n = 25

n = 25; p = 0.05
coverages = replicate(m, findNumOfCoverages())
mean(coverages) # a snapshot of contents in the coverages vector

n = 25; p = 0.25
coverages = replicate(m, findNumOfCoverages())
mean(coverages) # a snapshot of contents in the coverages vector

n = 25; p = 0.50
coverages = replicate(m, findNumOfCoverages())
mean(coverages) # a snapshot of contents in the coverages vector

# -------- n = 100 -------
n = 100; p = 0.05
coverages = replicate(m, findNumOfCoverages())
mean(coverages) # a snapshot of contents in the coverages vector

n = 100; p = 0.25
coverages = replicate(m, findNumOfCoverages())
mean(coverages) # a snapshot of contents in the coverages vector

n = 100; p = 0.50
coverages = replicate(m, findNumOfCoverages())
mean(coverages) # a snapshot of contents in the coverages vector
