########Lab10(new_Methods)########
#Example (1): Estimate the rate parameter of an exponential distribution
library (MASS)
n= 1000
sample=rexp(n, 0.5)
### New Method ###
rate_estimation=fitdistr(sample,"exponential")
rate_estimation
confint(rate_estimation,level= 0.95)

### OLD Method ###
mle = optim(par = c(rate = .5 ), fn = NLL, data = sample,
              control = list(parscale = c(rate = .5)))
NLL = function (pars, data){
# Extract parameters from the vector
rate = pars[1]
# Calculate Negative Log LIkelihood
NLL= -sum(dexp(x = data, rate = rate, log = TRUE))
}
mle$par

### OLD Method ###
p = seq (.1 , 1 , by = 0.01)
Llikhood=numeric(length( p ))

likva = function (p, data){
  sum(dexp(x = data, rate =p, log = TRUE))
}

for (i in 1 :length(p))
{
  Llikhood[i]=likva(p[i],sample)
}
phat= p[Llikhood == max(Llikhood)]
likeResults=data.frame(proportion = p, Llikhood = Llikhood)
Chi_value=qchisq( 0.05 , 1 , lower.tail= FALSE)/2
lower < max( p[p <= phat & Llikhood <= max(Llikhood)-1.92])
upper < min( p[p >= phat & Llikhood <= max(Llikhood)-1.92])
lower
upper

##_______________________________________________________________________##
#Example (2): Estimate the mean and standard deviation parameters of normal distribution
library(MASS)
n=1000
sample=rnorm(n, 1, 2)
### New Method ###
rate_estimation=fitdistr(sample,"normal")
rate_estimation
confint(rate_estimation,level=0.96)

##_______________________________________________________________________##
#Example (3): Estimate the mean and standard deviation parameters of normal distribution
library(MASS)
n=1000
sample=rgeom(n, 0.25)
### New Method ###
rate_estimation=fitdistr(sample,"geometric")
rate_estimation
confint(rate_estimation,level=0.96)
  