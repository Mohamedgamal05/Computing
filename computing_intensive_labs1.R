library(statsr)
library(dplyr)
library(shiny)
library(ggplot2)

data(ames)

ggplot(data = ames, aes(x = area)) +geom_histogram(binwidth = 200)

print("hello world")
"hello world" %>% print()

ames %>%
  summarise(mu = mean(area), pop_med = median(area), 
            sigma = sd(area), pop_iqr = IQR(area),
            pop_min = min(area), pop_max = max(area),
            pop_q1 = quantile(area, 0.25), # first quartile, 25th percentile
            pop_q3 = quantile(area, 0.75)) # third quartile, 75th percentile

sampl <- ames %>%
  sample_n(size = 50)

ggplot(data = sampl, aes(x = area)) +
  geom_histogram(binwidth = 100)

sampl %>%
  summarise(mu_sample = mean(area), sample_med = median(area), 
            sigma_sample = sd(area), sample_iqr = IQR(area),
            sample_min = min(area), sample_max = max(area),
            sample_q1 = quantile(area, 0.25), # first quartile, 25th percentile
            sample_q3 = quantile(area, 0.75)) # third quartile, 75th percentile



sample_means50 <- ames %>%
  rep_sample_n(size = 1500, reps = 15000, replace = TRUE) %>%
  summarise(x_bar = mean(area))
ggplot(data = sample_means50, aes(x = x_bar)) + geom_histogram(binwidth = 20)


print(mean(sample_means50$x_bar))
print(sd(sample_means50$x_bar))














dev.off()
