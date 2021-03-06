## ---- include = F-------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ciTools)
library(lme4)
set.seed(20170627)

x_gen_mermod <- function(ng = 8, nw = 5){
  n <- ng * nw
  x1 <- base::sample(letters[1:2], n, replace = T)
  x2 <- runif(n)
  group <- rep(as.character(1:ng), each = nw)
  return(data.frame(x1 = x1,
                    x2 = x2,
                    group = group))
}

mm_pipe <- function(df, ...){
  model.matrix(data = df, ...)
}

get_validation_set <- function(df, sigma, sigmaG, beta, includeRanef, groupIntercepts){
  vm <- sample_n(df, 5, replace = F)[rep(1:5, each = 100), ]
  vf <- bind_rows(vm, df) %>%
    select(-group) %>%
    mm_pipe(~.*.)
  vf <- vf[1:500, ]
  vGroups <- if(!includeRanef) rnorm(500, 0, sigmaG) else groupIntercepts[as.numeric(vm$group)]
  vm[["y"]] <- vf %*% beta + vGroups + rnorm(500, mean = 0, sd = sigma)
  vm
}

y_gen_mermod <- function(df, sigma = 1, sigmaG = 1, delta = 1, includeRanef = FALSE, validationPoints = FALSE){
  groupIntercepts <- rnorm(length(unique(df$group)), 0, sigmaG)
  tf <- df %>%
    dplyr::select(-group) %>%
    mm_pipe(~.*.)
  beta <- rep(delta, ncol(tf))
  if(validationPoints)  {
    vm <- get_validation_set(df, sigma, sigmaG, beta, includeRanef, groupIntercepts)
  }
  df[["y"]] <- tf %*% beta + groupIntercepts[as.numeric(df$group)] + rnorm(nrow(df), mean = 0, sd = sigma)
  df[["truth"]] <- tf %*% beta + groupIntercepts[as.numeric(df$group)] * (includeRanef)
  if(validationPoints) return(list(df = df, vm = vm)) else return(df)
}


## -----------------------------------------------------------------------------
set.seed(20170812)
df <- x_gen_mermod(10, 20) %>%
  y_gen_mermod()

## -----------------------------------------------------------------------------
fit2 <- lmer(y ~ x1 * x2 + (1|group) , data = df)
fit2

## -----------------------------------------------------------------------------
df %>% add_ci(fit2, type = "parametric", includeRanef = T) %>% head()

## ---- fig.width = 8, fig.heither = 5------------------------------------------
df %>%
  filter(group %in% c(1:3)) %>%
  add_ci(fit2, type = "parametric", includeRanef = T, names = c("LCB", "UCB")) %>%
  ggplot(aes(x = x2)) +
  geom_ribbon(aes(ymin = LCB, ymax = UCB), colour = "black", fill = "red1", alpha = 0.2) +
  geom_line(aes(y = pred), colour = "red1", size = 2) +
  facet_grid(x1 ~ group)

## -----------------------------------------------------------------------------
set.seed(20170628)
df %>%
  add_ci(fit2, includeRanef = T, type = "parametric", names = c("Lpar", "Upar")) %>%
  add_ci(fit2, includeRanef = T, type = "boot", names = c("Lboot", "Uboot")) %>% head()

## ---- fig.width = 8, fig.heither = 5------------------------------------------
set.seed(20170628)
df %>%
  filter(group %in% c(1:3)) %>%
  add_ci(fit2, includeRanef = T, type = "parametric", names = c("Lpar", "Upar")) %>%
  add_ci(fit2, includeRanef = T, type = "boot", names = c("Lboot", "Uboot")) %>%
  ggplot(aes(x = x2)) +
  geom_ribbon(aes(ymin = Lpar, ymax = Upar), colour = "black", fill = "blue1", alpha = 0.2) +
  geom_line(aes(y = pred), colour = "blue1", size = 2) +
  geom_ribbon(aes(ymin = Lboot, ymax = Uboot), colour = "black", fill = "red1", alpha = 0.2) +
  geom_line(aes(y = pred), colour = "red1", size = 2) +
  facet_grid(x1 ~ group)

## -----------------------------------------------------------------------------
df %>% add_ci(fit2, includeRanef = F, type = "parametric") %>% head()

## ---- fig.width = 8, fig.heither = 5------------------------------------------
set.seed(20170628)
df %>%
  filter(group %in% c(1:3)) %>%
  add_ci(fit2, includeRanef = F, type = "parametric", names = c("LU", "UU")) %>%
  add_ci(fit2, includeRanef = T, type = "parametric", names = c("LC", "UC")) %>%
  ggplot(aes(x = x2)) +
  geom_ribbon(aes(ymin = LU, ymax = UU), colour = "black", fill = "blue1", alpha = 0.2) +
  geom_ribbon(aes(ymin = LC, ymax = UC), colour = "black", fill = "red1", alpha = 0.2) +
  facet_grid(x1 ~ group)

## ---- fig.width = 8, fig.heither = 5------------------------------------------
set.seed(20170628)
df %>%
  filter(group %in% c(1:3)) %>%
  add_ci(fit2, includeRanef = F, type = "parametric", names = c("Lpar", "Upar")) %>%
  add_ci(fit2, includeRanef = F, type = "boot", names = c("Lboot", "Uboot")) %>%
  ggplot(aes(x = x2)) +
  geom_ribbon(aes(ymin = Lpar, ymax = Upar), colour = "black", fill = "blue1", alpha = 0.2) +
  geom_line(aes(y = pred), colour = "blue1", size = 2) +
  geom_ribbon(aes(ymin = Lboot, ymax = Uboot), colour = "black", fill = "red1", alpha = 0.2) +
  geom_line(aes(y = pred), colour = "red1", size = 2) +
  facet_grid(x1 ~ group)

## ---- fig.width = 8, fig.heither = 5------------------------------------------
df %>%
  add_ci(fit2, type = "parametric", includeRanef = T, names = c("LCB", "UCB")) %>%
  filter(group %in% c(1:3)) %>%
  ggplot(aes(x = x2)) +
  geom_ribbon(aes(ymin = LCB, ymax = UCB), colour = "black", fill = "red1", alpha = 0.2) +
  geom_line(aes(y = pred), colour = "red1", size = 2) +
  geom_point(aes(y = y), size = 2) +
  facet_grid(x1 ~ group)

## ---- fig.width = 8, fig.heither = 5------------------------------------------
df %>%
  add_ci(fit2, type = "parametric", includeRanef = F, names = c("LCB", "UCB")) %>%
  filter(group %in% c(1:3)) %>%
  ggplot(aes(x = x2)) +
  geom_ribbon(aes(ymin = LCB, ymax = UCB), colour = "black", fill = "red1", alpha = 0.2) +
  geom_line(aes(y = pred), colour = "red1", size = 2) +
  geom_point(aes(y = y), size = 2) +
  facet_grid(x1 ~ group)

## ---- fig.width = 6, fig.heither = 4------------------------------------------
df %>%
  add_ci(fit2, type = "parametric", includeRanef = F, names = c("LCB", "UCB")) %>%
  ggplot(aes(x = x2)) +
  geom_ribbon(aes(ymin = LCB, ymax = UCB), colour = "black", fill = "red1", alpha = 0.2) +
  geom_line(aes(y = pred), colour = "blue", size = 2) +
  facet_grid( ~ x1) +
  geom_hline(yintercept = 1.5, colour = "red1", size = 2, linetype = 2, alpha = .5)

## -----------------------------------------------------------------------------
df %>% add_pi(fit2, includeRanef = T, type = "parametric") %>% head()

## ---- fig.width = 8, fig.heither = 5------------------------------------------
df %>%
  filter(group %in% c(1:3)) %>%
  add_ci(fit2, type = "parametric", includeRanef = T, names = c("LCB", "UCB")) %>%
  add_pi(fit2, type = "parametric", includeRanef = T, names = c("LPB", "UPB")) %>%
  ggplot(aes(x = x2)) +
  geom_ribbon(aes(ymin = LCB, ymax = UCB), colour = "black", fill = "red1", alpha = 0.2) +
  geom_ribbon(aes(ymin = LPB, ymax = UPB), colour = "black", fill = "blue1", alpha = 0.2) +
  geom_line(aes(y = pred), colour = "red1", size = 2) +
  facet_grid(x1 ~ group)

## -----------------------------------------------------------------------------
df %>% add_pi(fit2, type = "parametric",includeRanef = F) %>% head()

## ---- fig.width = 6, fig.heither = 4------------------------------------------
df %>%
  add_pi(fit2, type = "parametric", includeRanef = F, names = c("LPB", "UPB")) %>%
  ggplot(aes(x = x2)) +
  geom_ribbon(aes(ymin = LPB, ymax = UPB), colour = "black", fill = "blue1", alpha = 0.2) +
  geom_line(aes(y = pred), colour = "red1", size = 2) +
  geom_point(aes(y = y), size = 2) +
  facet_grid( ~x1 )+
  geom_hline(yintercept = 0, colour = "red1", size = 2, linetype = 2)

## ---- fig.width = 8, fig.heither = 5------------------------------------------
df %>%
  filter(group %in% c(1:3)) %>%
  add_ci(fit2, type = "parametric", includeRanef = T, names = c("LCB", "UCB")) %>%
  add_pi(fit2, type = "parametric", includeRanef = F, names = c("LPB", "UPB")) %>%
  ggplot(aes(x = x2)) +
  geom_ribbon(aes(ymin = LCB, ymax = UCB), colour = "black", fill = "red1", alpha = 0.2) +
  geom_ribbon(aes(ymin = LPB, ymax = UPB), colour = "black", fill = "blue1", alpha = 0.2) +
  geom_line(aes(y = pred), colour = "red1", size = 2) +
  geom_point(aes(y = y), size = 2) +
  facet_grid(x1 ~ group)

## ---- include = F-------------------------------------------------------------
pm <- read.csv("lmer_unconditional_coverage.csv")
wm <- read.csv("lmer_unconditional_width.csv")

## ---- echo = F, fig.width = 8, fig.heither = 4--------------------------------
pm %>%
  ggplot(aes(x = groupSize, y = CoverageProb, colour = Method)) +
  geom_line(size = 2) + facet_grid(~numberOfGroups) +
  scale_y_continuous(breaks = seq(0, 1, length.out = 6), limits = c(0, 1)) +
  scale_x_log10(breaks = c(5, 10, 20, 50)) +
  ggtitle("Coverage probabilities for unconditional confidence intervals") +
  geom_hline(yintercept = 0.8)


## ---- echo = F, fig.width = 8, fig.heither = 4--------------------------------
pm %>%
  ggplot(aes(x = groupSize, y = CoverageProb, colour = Method)) +
  geom_line(size = 2) + facet_grid(~numberOfGroups) +
  scale_y_continuous(breaks = seq(0, 1, length.out = 6), limits = c(0, 1)) +
  scale_x_log10(breaks = c(5, 10, 20, 50)) +
  ggtitle("Coverage probabilities for unconditional confidence intervals") +
  geom_hline(yintercept = 0.8)


## ---- include = F-------------------------------------------------------------
pm <- read.csv("lmer_conditional_coverage.csv")
wm <- read.csv("lmer_conditional_width.csv")

## ---- echo = F, fig.width = 8, fig.heither = 4--------------------------------
pm %>%
  ggplot(aes(x = groupSize, y = CoverageProb, colour = Method)) +
  geom_line(size = 2) + facet_grid(~numberOfGroups) +
  scale_y_continuous(breaks = seq(0, 1, length.out = 6), limits = c(0, 1)) +
  scale_x_log10(breaks = c(5, 10, 20, 50)) +
  ggtitle("Coverage probabilities for unconditional confidence intervals") +
  geom_hline(yintercept = 0.8)

## ---- echo = F, fig.width = 8, fig.heither = 4--------------------------------
wm %>%
  ggplot(aes(x = groupSize, y = IntWidth, colour = Method)) +
  scale_x_log10(breaks = c(5, 10, 20, 50)) +
  ggtitle("Interval widths for conditional confidence intervals") +
  geom_line(size = 2) + facet_grid(~numberOfGroups)

## ---- echo = F, fig.width = 8, fig.heither = 4--------------------------------
pm %>%
  ggplot(aes(x = groupSize, y = CoverageProb, colour = Method)) +
  geom_line(size = 2) + facet_grid(~numberOfGroups) +
  scale_y_continuous(breaks = seq(0, 1, length.out = 6), limits = c(0, 1)) +
  scale_x_log10(breaks = c(5, 10, 20, 50)) +
  ggtitle("Coverage probabilities for unconditional prediction intervals") +
  geom_hline(yintercept = 0.8)


## ---- echo = F, fig.width = 8, fig.heither = 4--------------------------------
wm %>%
  ggplot(aes(x = groupSize, y = IntWidth, colour = Method)) +
  scale_x_log10(breaks = c(5, 10, 20, 50)) +
  ggtitle("Interval widths for unconditional prediction intervals") +
  geom_line(size = 2) + facet_grid(~numberOfGroups)

## ---- include = F-------------------------------------------------------------
pm <- read.csv("lmer_conditional_pi_coverage.csv")
wm <- read.csv("lmer_conditional_pi_width.csv")

## ---- echo = F, fig.width = 8, fig.heither = 4--------------------------------
pm %>%
  ggplot(aes(x = groupSize, y = CoverageProb, colour = Method)) +
  geom_line(size = 2) + facet_grid(~numberOfGroups) +
  scale_y_continuous(breaks = seq(0, 1, length.out = 6), limits = c(0, 1)) +
  scale_x_log10(breaks = c(5, 10, 20, 50)) +
  ggtitle("Coverage probabilities for conditional prediction intervals") +
  geom_hline(yintercept = 0.8)

## ---- echo = F, fig.width = 8, fig.heither = 4--------------------------------
wm %>%
  ggplot(aes(x = groupSize, y = IntWidth, colour = Method)) +
  scale_x_log10(breaks = c(5, 10, 20, 50)) +
  ggtitle("Interval widths for conditional prediction intervals") +
  geom_line(size = 2) +
  facet_grid(~numberOfGroups)

