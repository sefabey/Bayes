# This is me following the the workshop called "Introduction to Bayesian inference with JAGS"
# which was presented in "useR! International R User 2017 Conference"
# URL:"https://channel9.msdn.com/Events/useR-international-R-User-conferences/useR-International-R-User-2017-Conference/Introduction-to-Bayesian-inference-with-JAGS"

library(tidyverse)
glm.out <- glm(carat~color+price+cut, family = poisson(), data = diamonds)

diamonds
summary(glm.out)
rsq(glm.out)
qplot(glm.out)
install.packages("rsq")
library(rsq)

qplot(diamonds$carat, bins=50)
density(diamonds$carat) %>% 
  plot()

qplot(diamonds$price,diamonds$carat)
predict(glm.out)

install.packages("rjags", dependencies = T)
library(rjags)
m <- jags.model("blocker.bug", diamonds, inits, n.chains = 2)
?jags.model()


# ==============================================
library(rjags)
set.seed(1355)
y <- rnorm(50, mean = 3.7)
m <- jags.model("location1.bug", data = list(y=y, N=length(y)))
class(m)
print(m)

m$data()
# to extract samples from posterior distribution
s <- coda.samples(m, "theta", n.iter=1000) 
s <- unlist(s)

# exercises
# calculte sample mean 
mean(s)
# estimate std error of the sample mean
sem <- sd(s)/sqrt(length(s)-1)
sem

# how many samples we need for accuracy of 0.005
1000*(sem/0.005)

# plot densith of theta using the samples in s
hist(s)
plot(density(s)) #posterior distribution
abline(v=mean(y), col="grey")

# use a qq plot to verify that the samples are from a normal dist
qqnorm(s)

# use the Shapiro-Wilk test to formally test normality

shapiro.test(s) #normal (no evidence against normality)

# shapiro.test(rnorm(100, mean = 5, sd = 9)) # normal
# shapiro.test(runif(100, min = 2, max = 4)) # not normal


inits1=list(".RNG.name"="base::Wichmann-Hill",
            ".RNG.seed"=7453210)
inits2=list(".RNG.name"="base::Wichmann-Hill",
            ".RNG.seed"=33543)

ma <- jags.model("location1.bug", data=list(y=y, N=length(y)),
                 n.chains = 2, inits = list(inits1,inits2))

mb <- jags.model("location1.bug", data=list(y=y, N=length(y)),
                 n.chains = 2, inits = list(inits1,inits2))

ma$state(internal = TRUE)[[1]] #state of the model, internal argument will return the RNG
mb$state(internal = TRUE)[[1]]

#Shrinkage

m <- jags.model("location2.bug", data = list(y=y, N=50,tau=1)) 
# tau is the prior precision
s <- coda.samples(m,"theta", n.iter=1000000)
s <- unlist(s)
plot(density(s), col="blue", xlim = c(0,4.5))

m2 <- jags.model("location2.bug", data = list(y=y, N=50,tau=10))
s2 <- coda.samples(m2,"theta", n.iter=1000000)
s2 <- unlist(s2)
lines(density(s2), col="black") #posterior distribution shifted towards 0
# stronger prior distribution
# incresing prior precision, you make prior dominate over posterior

m3 <- jags.model("location2.bug", data = list(y=y, N=50,tau=20))
s3 <- coda.samples(m3,"theta", n.iter=1000000)
s3 <- unlist(s3)
lines(density(s3), col="red") #posterior distribution shifted even closer to 0

# this phenomenon is called shrinkage and it is a well known phenomenon in Bayesian stats.
# if you put a lot of prior information in your model, you need a lot of data to overcome it.
# in the 3rd case, we are putting such a strong prior on the paremeter theta, the true value
# for the simulated data, which is 3.7, actually excluded by the posteriror.

(post.precision <- 1/var(s))
(post.precision2 <- 1/var(s2))
(post.precision3 <- 1/var(s3))


plot(c(1,10,20), c(post.precision,post.precision2,post.precision3))
abline(50,1)

# ===================================================================
# Bugs Language

model{
  for (i in 1:N){
    r[i]~dbin(p[i], n[i])
    p[i]~dunif(0,1)
  }
}

#bugs code for linear regression

for (i in 1:N) {
  y[i]~dnorm(mu[i], tau)
  mu[i] <- alpha+beta*x[i]
}
alpha ~ dnorm(m.alpha,p.alpha)
beta ~ dnorm (m.beta, p.beta)
log.sigma ~ dunif (a,b)
sigma <- exp(log.sigma)
sigma.sq <- power(signa,2)
tau <- 1/sigma.sq

#Surgical Example 

# no of ops on infants and number of deaths
library(rjags)
source(file="surgical-data.R") #not working
?read.jagsdata()
# cardica surgeries is (n)
# number of deaths is (r)

# Which hospital has the lowest death rate?
# Which hospital has the highest death rate?

m <- jags.model("surgical.bug", data = surgical) #beta binomial model 
update(m,1000)#burn in 
s <- coda.samples(m,"p", n.iter = 1000)

class(s)
head(s)
tail(s)
summary(s)

summary(s)[[1]]#Moments
plot(s)
windows()
devAskNewPage(TRUE)
plot(s)
HPDinterval(s, prob = .9)

# =====================================================================
#r2jags package
#install.packages("R2jags")
library(R2jags)

r2jags.out <- jags(data=surgical,parameters.to.save = "p",
                   model.file = "surgical.bug", n.chains = 2)
r2jags.out
plot(r2jags.out) # according to the graph, number 8 seems to be the worst hospital 
#and the number q seems to be the best hospita

# ====================================================================
# runjags package

install.packages('runjags')
library(runjags)

runjags.out <- run.jags(model = "surgical.bug", monitor = "p",
                        data=surgical,n.chains = 2)
runjags.out
plot(runjags.out)


# ====================================================================
# jagsui package
#install.packages("jagsUI")
library(jagsUI)
jagsui.out <- jagsUI::jags(data = surgical, parameters.to.save = "p",
                           model.file = "surgical.bug", n.chains = 2,
                           n.iter = 1000)
jagsui.out #if you are starting out with MCMC, it might be useful to use this package because it will
            # guide you through the output in detail

plot(jagsui.out)


# =============================================================

# graphical models

# in a graphical model, random variables are represented as nodes
# and the relations between them by edges

#two kind of graphs
# directed acyclic graphs
# conditional independence graphs

# solutions to poor mixing
# thinning and running for longer. 
#taking every 20 iterations and running the simulation longer


