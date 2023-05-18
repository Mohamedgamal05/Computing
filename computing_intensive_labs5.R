######Lab5(Bootstrap confidence Interval using different methods)######
# install.packages("boot",dependencies = TRUE)
library(boot)

datasize=900
y =rnorm(datasize,0,1)
ybar = mean(y)
citheor_lower = ybar - 1.96/sqrt(datasize)
citheor_upper = ybar + 1.96/sqrt(datasize)
citheor_lower
citheor_upper

boot_data_calc_mean=function(data){
  boot_samp= sample(data,replace=TRUE)
  mean(boot_samp)
}

boot_mean_reps=replicate(n=200,boot_data_calc_mean(y))
standard_error_boot=sd(boot_mean_reps)
standard_error_boot

#or using this function#
boot_data_calc_mean=function(data,idx){
  mean(data[idx])
}
set.seed(61)
bootstrap_data <- boot(y, boot_data_calc_mean, R = 200)
bootstrap_data


#Calculate the bootstrap confidence interval using percentile
cibootstrap_per = quantile(boot_mean_reps,c(0.025,0.975))
cibootstrap_per

#or using boot.ci function
boot.ci(boot.out = bootstrap_data, type = "perc") 

#Calculate the bootstrap confidence interval using normal distribution
citboot_lower = mean(boot_mean_reps) - 1.96*standard_error_boot
citboot_upper = mean(boot_mean_reps) + 1.96*standard_error_boot
citboot_lower
citboot_upper

#Calculate the bootstrap confidence interval using empirical method
xbar=mean(y)
deltastar = boot_mean_reps - xbar
d = quantile(deltastar,c(0.025,0.975))
ci = xbar - c(d[2],d[1])
ci


#install.packages("gcookbook", dependencies = TRUE)
library(gcookbook)
#View(heightweight)

#calculate the difference in sample means
mean(heightweight$weightLb[heightweight$sex=="m"])
mean(heightweight$weightLb[heightweight$sex=="f"])
Obs.Diff.In.Means = mean(heightweight$weightLb[heightweight$sex=="m"]) -
  mean(heightweight$weightLb[heightweight$sex=="f"]) 
Obs.Diff.In.Means

#Using Bootstrap
m=heightweight$weightLb[heightweight$sex=="m"]
f=heightweight$weightLb[heightweight$sex=="f"]
boot_data_calc_mean=function(data){ 
  boot_samp= sample(data,replace=TRUE)
  mean(boot_samp)
}
boot_mean_m=replicate(n=200,boot_data_calc_mean(m)) 
boot_mean_f=replicate(n=200,boot_data_calc_mean(f))

#calculate the the "PERCENTILE" bootstrap confidence interval
Boot.Diff.In.Means = boot_mean_m - boot_mean_f 
length(Boot.Diff.In.Means)
quantile(Boot.Diff.In.Means, prob=0.025)
quantile(Boot.Diff.In.Means, prob=0.975)
