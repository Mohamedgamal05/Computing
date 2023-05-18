sim_norm_calc_mean = function(){
  data=rnorm(n=20,mean=3,sd=2)
  sum(data)/length(data)
}

sim_norm_mean = replicate(n=2500,sim_norm_calc_mean())

hist(sim_norm_mean,probability=T)
grid()
box()
curve(dnorm(x,3,sqrt(4/20)),add=TRUE,col='red')

plot(ecdf(sim_norm_mean))
curve(pnorm(x,3,sqrt(4/20)),add=TRUE,col='red')



plot(ecdf(sim_norm_mean))
par(mfrow=c(1,3))
replicate(n=50,sim_norm_calc_mean())->sim_norm_mean1
replicate(n=500,sim_norm_calc_mean())->sim_norm_mean2
replicate(n=2500,sim_norm_calc_mean())->sim_norm_mean3
plot(ecdf(sim_norm_mean1))
curve(pnorm(x,3,sqrt(4/20)),add=TRUE,col='red')
plot(ecdf(sim_norm_mean2))
curve(pnorm(x,3,sqrt(4/20)),add=TRUE,col='yellow')
plot(ecdf(sim_norm_mean3))
curve(pnorm(x,3,sqrt(4/20)),add=TRUE,col='green')







install.packages("palmerpenguins",dependencies = TRUE)
library(palmerpenguins)

peng=na.omit(penguins)
mass=peng$body_mass_g


boot_data_calc_median=function(data){
  boot_samp= sample(data,replace=TRUE)
  median(boot_samp)
}

boot_median_reps=replicate(n=5000,boot_data_calc_median(mass))

par(mfrow=c(1,1))
hist(boot_median_reps,probability=T)
grid()
box()
