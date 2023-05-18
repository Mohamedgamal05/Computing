# Installing required libraries
install.packages("detzrcr",dependencies = TRUE)
# Importing Libraries
library(dplyr)#data manipluation including filter and arrange
library(ggplot2)#
library(detzrcr)#Inlcuding DKW_Inequality and graphs related as lower and upper bound for ecdf
library(gcookbook)

# Importing Data and calculate mean and std for population
data("heightweight")
heightweight
population = heightweight$heightIn #can be written as heightweight[,"heightIn"]
mu = mean(population); mu
sigma = sd(population); sigma
pop_var = var(population); pop_var

# PUtting our data in DataFrame as sampling in following lines takes data from dataframe
population = data.frame(heightweight)
sample = sample_n(population,size=50)
mean_sample=mean(sample[,"heightIn"])
mean_sample
# defining z* ,lower pound and upper pound
z_star_95 <- qnorm(0.975)
lower = mean_sample - z_star_95 * sigma/sqrt(50) 
upper = mean_sample + z_star_95 * sigma/sqrt(50)
# Start to taking sample and apply basic calculations for (Emprical)ecdf
#population = data.frame(heightweight) 
sample=sample_n(population,size = 200) 
ggplot(sample, aes(x = heightIn)) + stat_ecdf() 
# mean_est=mean(sample[,"heightIn"]) #sum(sample$heightIn)/length(sample$heightIn) 
# mean_est

calc_ecdf=calc_dkw(sample,column="heightIn",alpha=.05)
calc_ecdf
calc_ecdf=calc_ecdf[2:201,]
ggplot()+
  stat_ecdf(data=heightweight, aes(x = heightIn),colour="#0072B2", size=.7)+
  geom_step(data = calc_ecdf, aes (x=x,y=low)) +
  geom_step(data = calc_ecdf, aes (x=x,y=high))


mean_est=sum(sample$heightIn)/length(sample$heightIn)
mean_est

var_est=var(sample$heightIn)
var_est



