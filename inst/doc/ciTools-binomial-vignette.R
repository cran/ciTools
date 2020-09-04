## ----message = F--------------------------------------------------------------
set.seed(20171011)
library(dplyr)
library(ciTools)

## -----------------------------------------------------------------------------
df <- data.frame(
  x = runif(30, -1, 1),
  n = rbinom(30, 6, .8)) %>%
  mutate(y = rbinom(30, n, (exp(x)/(1 + exp(x)))))

## -----------------------------------------------------------------------------
head(df)

## -----------------------------------------------------------------------------
glm(y/n ~ x, data = df, family = "binomial", weights = n)

## -----------------------------------------------------------------------------
dfProb <- mutate(df, prob = y/n)
head(dfProb)
glm(prob ~ x, data = dfProb, family = "binomial", weights = n)

## ----echo = F-----------------------------------------------------------------
get_box <- function(myRow){
  data.frame(x = rep(myRow$x, myRow$n),
             y = c(rep(1, myRow$y), rep(0, myRow$n - myRow$y)))
}

out <- NULL
for(i in 1:nrow(df)){
  out <- bind_rows(out, get_box(df[i,]))
}

dfTall <- out

## -----------------------------------------------------------------------------
head(dfTall)
glm(y ~ x, data = dfTall, family = "binomial")

## ----echo = F-----------------------------------------------------------------
fit <- glm(y/n ~ x, data = df, family = "binomial", weights = n)

## -----------------------------------------------------------------------------
head(add_quantile(df, fit, p = 0.9))

