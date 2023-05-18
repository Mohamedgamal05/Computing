# install.packages("bootstrap",dependencies = TRUE)
library(bootstrap)
data=c(3.56,0.69,0.1,1.84,
       3.93,1.25,0.18,1.13,0.27,0.5,0.67,0.01,0.61,0.82,
       1.70,0.39,0.11,1.2,1.21,0.72)
# Sample mean
mean(data)
# Jackknife the mean
jackmean <- jackknife(data,mean)
# Sample mean
jack_es=mean(jackmean$jack.values)
# Bias-corrected jackknife estimate
meanjack = mean(data)- jackmean$jack.bias
jack_es
mean(data)
jackmean$jack.bias
meanjack
jackmean$jack.se

# Calculate 95% confidence interval for the sample mean
LowerCI= meanjack-1.96* jackmean$jack.se
upperCI= meanjack+1.96* jackmean$jack.se
LowerCI
upperCI