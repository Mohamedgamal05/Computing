##########################Lab7##########################
# Bern. pop ---------------------------------------------------------------------
n=1000000
x=rbinom(n,1,0.71)
p=sum(x)/n
p

# Nor. pop ----------------------------------------------------------------
n=10000000
x=rnorm(n, mean = 10, sd = 3)
mean_est=sum(x)/n
var_est=(sum(x^2)/n)-(mean_est)^2
mean_est
var_est

# unif. pop ---------------------------------------------------------------
n=100
x=runif(n, min = 1, max = 5)
m1=sum(x)/n
m2=sum(x^2)/n
a_est=m1-sqrt(3*(m2-m1^2))
b_est=m1+sqrt(3*(m2-m1^2))
a_est
b_est

library(ggplot2)

# Exp. pop ----------------------------------------------------------------
sample = rexp(100000,rate=10)
mle = optim(par = c(rate = .5), fn = NLL, data = sample,
            control = list(parscale = c(rate = .5)))
NLL = function(pars, data) {
  # Extract parameters from the vector
  rate = pars[1]
  # Calculate Negative Log-LIkelihood
  NLL= -sum(dexp(x = data, rate = rate, log = TRUE))
}
mle$par


# Nor. pop ----------------------------------------------------------------

sample = rnorm(100000,mean=4,sd=2)
mle = optim(par = c(mu = 0.2, sigma = 1.5), fn = NLL, data = sample,
            control = list(parscale = c(mu = 0.2, sigma = 1.5)))
NLL = function(pars, data) {
  # Extract parameters from the vector
  mu = pars[1]
  sigma = pars[2]
  # Calculate Negative Log-LIkelihood
  NLL= -sum(dnorm(x = data, mean = mu, sd = sigma, log = TRUE))
}
mle$par

# Confidence int ----------------------------------------------------------


sample = rbinom(1000, size = 32, p = 0.7)
p <- seq(.4, .9, by = 0.01)
Llikhood=numeric(length( p ))

for (i in 1:length(p))
{
  Llikhood[i]=likva(p[i],sample)
}
likva = function(p, data) {
  sum(dbinom(x = data, size=32,prob= p, log = TRUE))
}
likeResults=data.frame(Proportion = p, Llikhood = Llikhood)
ggplot(data=likeResults, aes(x = Proportion,y=Llikhood))+
  geom_line()+geom_hline(yintercept = max(Llikhood), linetype = "dashed")
phat= p[Llikhood == max(Llikhood)]

Chi_value= qchisq(0.05, df=1, lower.tail=FALSE)/2
lower <- max( proportion[proportion <= phat & Llikhood <= max(Llikhood) - 1.92] )
upper <- min( proportion[proportion >= phat & Llikhood <= max(Llikhood) - 1.92] ) 
lower
upper
######################################################################################
#Exercise 1
x1=c(1.3403,1.8915,4.1428,3.0095,
     1.8450,2.1611,4.8945,3.4758,
     2.8798,2.1612,1.8716,1.3810,
     1.4057,3.5031,1.5061,3.2319,
     1.4956,1.7329,3.1107,1.3254
     )
n = length(x1)
x1_mean = mean(x1)
x1_var = var(x1)
x1_mean
x1_var
m1=mean(x1)
m2=sum(x1^2)/n
bita_est=((m2-m1^2)/m1) 
alpha_est=m1/bita_est
bita_est
alpha_est
#############################################################################################
#Exercise 2
x2 = c(0.5708,0.3957,0.1829,0.9300,
       0.8252,1.4966,0.2008,2.7691,
       0.1393,0.3103,0.0413,0.0559,
       0.4433,0.2934,0.4825,0.6840,
       0.3299,0.5206,1.0475,0.5522)
n2 = length(x2)
x2_mean = mean(x2)
x2_var = var(x2)
x2_mean
x2_var
lambda_est = 1/(x2_mean)
lambda_est
z_alpha_2 = qnorm(1 - (0.05 / 2))
CI_lower = lambda_est - z_alpha_2 * lambda_est / sqrt(n)
CI_upper = lambda_est + z_alpha_2 * lambda_est / sqrt(n)
CI_lower
CI_upper



