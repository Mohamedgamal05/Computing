install.packages("shiny")
library(statsr)
library(dplyr)

##
#-----------------Normal t-test-----------------#
##
##

data(nc)
names(nc)#shows the name of columns in dataset

mean(nc$weight)

# t-test for single column
t.test(nc$weight,mu=7)

# t-test between 2 groups
by(nc$weight,nc$habit,summary)
by(nc$weight,nc$habit,mean)

t.test(weight ~ habit , data=nc, conf.level = .95)

# inference do the same with t test but it draws more histograms and plots
inference(y=weight,x=habit,data=nc,statistic = "mean",type="ht",null=0,
          alternative = "twosided",method="theoretical")

# Another Example 
# Weight of the mice before treatment 
before = c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)

# Weight of the mice after treatment
after = c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)

# Create a data frame
my_data = data.frame( group = rep(c("before", "after"), each = 10),
                      weight = c(before, after) )

t.test(weight ~ group , data = my_data ,paired = TRUE, conf.level = 0.95)

#-------------------------------------------------------------------------------#
##
#-----------------Empirical likelihood-----------------#
##
install.packages("emplik",dependencies = T)

library(emplik)

#_________________________________________________________#
## Example for normal distribution
n = 25

#(1) generating the data from N(0,1)
X = rnorm(n,0,1)

el.obj = el.test(X,mu=0)

el.obj$Pval

t.test(X,mu=0)$p.value

X<-rexp(n,rate=1)-1

#_________________________________________________________#
## Example for exponential distribution
X<-rexp(n,rate=1)-1

el.obj<-el.test(X,mu=0)

el.obj$Pval

t.test(X,mu=0)$p.value
####________________After comparing Normal & Exponential distributions using
####Empirical Likelihood we found empirical is more accurate than t test as t test works 
####just with normal distribution________________####

#_________________________________________________________#
## Another Example 
data=c(6.1, -8.4, 1.0, 2.0, 0.7, 2.9, 3.5, 5.1, 1.8, 3.6, 7.0, 3.0, 9.3, 7.5, -6.0)

t.test(data,mu=0)
# from histogram data seems not to be normal so we used empirical as better result
hist(data)

el.test(data,mu=0)

#Finding Confidence Interval for Empirical Likelihood
#This function gives us -2LLR which we use in calculations of upper and lower limits
myfun6 = function(theta, data1){
  el.test(data1,mu=theta) 
}
#We must not forget that we use chi-square dist in finding upper and lower limit for empirical 
#likelihood initializing steps and function which give us -2LLR
findUL(step=0.5, fun=myfun6, MLE=0, data1=data,level = qchisq(0.95, df=1))

#_________________________________________________________#
#In the following example we used 2 samples in empirical first we install new library
#which deal with 2 samples
install.packages("EL",dependencies = T)
library(EL)

x=c(96.8,57.2,37.4,44.0,55.0,41.8,46.2,41.8,41.8,59.4,44.0,52.8,33.0,52.8,41.8,44.0,52.8
     ,59.4,37.4,77.0,39.6,57.2,57.2, 41.8,39.6)
y=c(26.4,33.0,30.8,35.2,44.0,48.4,61.6,41.8,26.4,28.6,55.0, 61.6,63.8,24.2,37.4,48.4,52.8
     ,46.2,57.2,68.2,46.2,37.4,46.2, 52.8,35.2)
# we used t test and empirical to compare and found that empirical is better and accurate
t.test(x, y) 

EL.means(x, y)

