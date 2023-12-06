# Install and load packages
install.packages("ggplot2")
library(ggplot2)

# Load data
growth_data <- read.csv("experiment1.csv")

# Define logistic growth function
logistic_fun <- function(t) {
  N <- (N0*K*exp(r*t))/(K-N0+N0*exp(r*t))
  return(N)
}

# Define exponential growth function
exponential_fun <- function(t) {
  N <- N0*exp(r*t)
  return(N)
}

# Define population parameters
N0 <- 986.5074723 
r <- 0.0100086 
K <- 6.00e+10

# Create a plot to compare logistic and exponential models

ggplot(aes(t, N), data = growth_data) +
  geom_function(fun = logistic_fun, aes(colour = "Logistic")) +
  geom_function(fun = exponential_fun, aes(colour = "Exponential")) +
  ylim(0, 6.00e+10) +
  xlim(0, 3511) +
  labs(title = "Comparison of Logistic and Exponential Growth Models",
       x = "Time",
       y = "Population") +
  scale_color_manual(values = c(Logistic = "red", Exponential = "blue"), name = "Growth Model") +
  theme_minimal()
