# QUESTION 1

1. Plot the logistic growth data

Load the ggplot2 package and the simulated data for a logistic growth curve of _Escherichia coli._ 
```{r}
install.packages("ggplot2")
library(ggplot2)

growth_data <- read.csv("experiment1.csv")
```
Using ggplot, plot population size over time, to observe the pattern of growth for the population of _E. coli._ 
```{r}
install.packages("ggplot2")
library(ggplot2)

ggplot(aes(t,N), data = growth_data) +
  geom_point() +
  xlab("t") +
  ylab("y") +
  theme_bw()
```
The population growth is exponential and a logistical growth curve is observed. Therefore, the next step is to perform a log transformation. 
```{r}
ggplot(aes(t,N), data = growth_data) +
  geom_point() +
  xlab("t") +
  ylab("y") +
  scale_y_continuous(trans='log10')
```
By doing so, the relationship between time and population size is linearized.



2. Estimate the model parameters using a linear approximation

Load dplyr package and population growth data.
```{r}
install.packages("dplyr")
library(dplyr)

growth_data <- read.csv("experiment1.csv")
```

Log transform the data in order to apply a linear model. 

To calculate N0 and r, subset the data by time to only include data from before the population began to grow significantly in size.
```{r}
#Case 1. K >> N0, t is small

data_subset1 <- growth_data %>% filter(t<1500) %>% mutate(N_log = log(N))

model1 <- lm(N_log ~ t, data_subset1)
summary(model1)
```
To calculate K, subset the data by time to only include data points from when population size has stabilised.
```{r}
#Case 2. N(t) = K

data_subset2 <- growth_data %>% filter(t>2500)

model2 <- lm(N ~ 1, data_subset2)
summary(model2)
```



3. Plot the data and model

Load the data
```{r}
growth_data <- read.csv("experiment1.csv")
```
Define a function for logistic growth
```{r}
logistic_fun <- function(t) {
  N <- (N0*K*exp(r*t))/(K-N0+N0*exp(r*t))
  return(N) 
}
```
Define the values for initial population size, initial population growth rate, and carrying capacity.
```{r}
N0 <- 986.5074723
r <- 0.0100086 
K <- 6.00e+10 
```

Plot the data and a logistic model based on the calculated population metrics, to see how well the model fits the real data.
```{r}
ggplot(aes(t,N), data = growth_data) + 
  geom_function(fun=logistic_fun, colour="red") + 
  geom_point() +
  scale_y_continuous(trans='log10')
```
[logistic growth model](https://github.com/anonbiologist/logistic_growth/assets/153086380/e7279d91-ee2a-44d8-8329-9005eda8d34c)




# QUESTION 2

Calculate the population size at t = 4980 minutes, assuming that the population grows exponentially.


Define a function for exponential growth:
```{r}
exponential_fun <- function(N0, r, t) {
  N <- N0 * exp(r * t)
  return(N)
}
```
Define the values
```{r}
N0 <- 986.5074723
r <- 0.0100086
t <- 4980
```
Calculate population size at t = 4980 minutes for exponential growth
```{r}
population_exp <- exponential_fun(N0, r, t)
```

- Population size for exponential growth = 4.370846e+24

- Population size for logistic growth = 6.00e+10

Therefore, the exponential growth model produced a far greater population size. This is unsurprising, as by t = 4980 minutes, population growth had plateaued for the logistic model. 

The crucial difference between the two models is that carrying capacity (K) is only considered in the logistic growth model, whereas exponential growth does not consider limiting factors upon population size and therefore models infinite growth.






