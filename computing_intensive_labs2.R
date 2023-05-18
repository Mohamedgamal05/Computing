 install.packages("dplyr", dependencies = TRUE)
 install.packages("ggplot2", dependencies = TRUE)
 install.packages("ggpubr", dependencies = TRUE)
 install.packages("gcookbook", dependencies = TRUE)

library(dplyr)
library(ggplot2)
library(ggpubr)
library(gcookbook)

p1 = ggplot(data.frame(x = c(-3, 3)), aes(x = x)) +
  stat_function(fun = pnorm, colour="#D55E00", size=1)
p1

df = data.frame(sampl=rnorm(10))

p2 = ggplot(df, aes(x=sampl)) +
  stat_ecdf(geom="point", colour="#0072B2") +
  stat_ecdf(colour="#0072B2", size=1)

p2

figure <- ggarrange(p1, p2, labels = c("tcdf", "ecdf"), ncol = 1, nrow = 2)
figure



data(heightweight)
heightweight

ggplot(heightweight, aes(x = weightLb, col=sex)) +
  stat_ecdf()
