# Question 2

rayleigh <- function(u,sigma){
  1-exp(-(u**2)/(2*(sigma**2)))
  
}

rayleigh_inverse <- function(y,sigma){
  sigma*(-2*log(1-y))**(0.5)
}

# generate 1000 random uniforms
unif <- runif(1000)

# set distribution parameters
sigma <- 1 # scale

# generate random variables
ray <- sapply(unif,rayleigh_inverse, sigma = sigma)

cdf <- ecdf(ray)
t <- seq(0, 6, .001)
plot(cdf)

hist(ray, prob = TRUE, xlim = c(0,7),xlab= "values" ,ylab = "Density")
lines(t, (t/sigma^2)*exp((-t^2)/(2*sigma^2)))
?hist
# use base R to generate the same values!
library("VGAM")
library("ggplot2")
library("tibble")
set.seed(1)
rrayleigh_vals <- rrayleigh(1000, scale = 1)

tibble(`Inverse-transform-generated` = xsim, `Base-R generated` = rrayleigh_vals) %>%
  pivot_longer(everything(), names_to = "method") %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 50) +
  scale_x_continuous(limits = c(-25, 25)) +
  facet_wrap(~ method) +
  theme_bw() +
  labs(x = "x", y = "Frequency")

# Question 3
