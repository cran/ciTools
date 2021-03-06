# Copyright (C) 2017 Institute for Defense Analyses
#
# This file is part of ciTools.
#
# ciTools is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ciTools is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with ciTools. If not, see <http://www.gnu.org/licenses/>.

#' Confidence Intervals for the Survivor Function of Accelerated
#' Failure Time Models
#'
#' This function is one of the methods of \code{add_probs} and is
#' automatically called when an object of class \code{survreg} is
#' passed to \code{add_probs}.
#'
#' Confidence intervals may be produced for estimated probabilities of
#' accelerated failure time models. Presently, confidence intervals
#' may be computed for lognormal, weibull, exponential, and
#' loglogistic failure time models. If \code{comparison = "<"},
#' confidence intervals are made for the probability that a failure
#' will be observed before \code{q}. Similarly, if \code{comparison =
#' ">"}, confidence intervals will be formed for the probability that
#' a unit fails after \code{q}. In the survival literature,
#' \code{comparison = ">"} corresponds to estimating the survivor
#' function, \emph{S(q)}.
#'
#' Confidence intervals are produced parametrically via the Delta
#' Method. Simulations show that under a mild
#' to moderate amount of censoring, this method performs
#' adequately.
#'
#' The logistic transformation is applied to ensure
#' that confidence interval bounds lie between \eqn{0} and \eqn{1}.
#'
#' Note: Due to a limitation, the \code{Surv} object must be specified in
#' \code{survreg} function call. See the examples section for one way
#' to do this.
#'
#' Note: \code{add_probs.survreg} cannot inspect the convergence of
#' \code{fit}. Poor maximum likelihood estimates will result in poor
#' confidence intervals. Inspect any warning messages given from
#' \code{survreg}.
#'
#' @param df A data frame of new data.
#' @param fit An object of class \code{survreg}. Predictions are made
#'     with this object.
#' @param q A double. A quantile of the survival time distribution. In
#'     survival applications this is the time of event.
#' @param confint A logical. If \code{TRUE}, confidence intervals for
#'     the estimated probabilities will be calculated and appended to
#'     \code{df}.
#' @param alpha A number. Control the confidence level of the
#'     confidence intervals if \code{confint = TRUE}.
#' @param name \code{NULL} or a string. If \code{NULL}, probabilities
#'     automatically will be named by \code{add_probs()}, otherwise,
#'     the probabilities will be named \code{name} in the returned
#'     data frame.
## #' @param method A string. One of either \code{"parametric"} or
## #'     \code{"boot"}.
#' @param yhatName A string. Name of the vector of predictions.
#' @param comparison A character vector of length one. If
#'     \code{comparison = "<"}, then \eqn{Pr(Y|X < q)} is
#'     calculated. If \code{comparison = ">"}, the survivor function
#'     at time \code{q} is calculated.
## #' @param nSims Number of simulations used if \code{method = "boot"}
#' @param ... Additional arguments.
#'
#' @return A dataframe, \code{df}, with predicted medians, probabilities,
#'     and confidence intervals for predicted probabilities attached.
#'
#' @seealso \code{\link{add_ci.survreg}} for confidence intervals for
#'     \code{survreg} objects, \code{\link{add_pi.survreg}} for
#'     prediction intervals of \code{survreg} objects, and
#'     \code{\link{add_quantile.survreg}} for response quantiles of
#'     \code{survreg} objects.
#'
#' @references
#' For the logistic transformation of estimated probabilities and error bounds:
#' Meeker, William Q., and Luis A. Escobar. Statistical methods for reliability data. John Wiley & Sons, 2014. (Chapter 8)
#'
#' For a discussion of forming confidence intervals for survival probabilities:
#' Harrell, Frank E. Regression modeling strategies. Springer, 2015. (Chapter 17)
#'
#' @examples
#' ## Define a data set.
#' df <- survival::stanford2
#' ## remove a covariate with missing values.
#' df <- df[, 1:4]
#' ## next, create the Surv object inside the survreg call:
#' fit <- survival::survreg(survival::Surv(time, status) ~ age + I(age^2),
#'                          data = df, dist = "lognormal")
#' ## Calculate the level 0.75 quantile wit CIs for that quantile
#' add_probs(df, fit, q = 500, name = c("Fhat", "lwr", "upr"))
#'
#' ## Try a weibull model for the same data:
#' fit2 <- survival::survreg(survival::Surv(time, status) ~ age + I(age^2),
#'                           data = df, dist = "weibull")
#' ## Calculate the level 0.75 quantile with CIs for the quantile
#' add_probs(df, fit2, q = 500, name = c("Fhat", "lwr", "upr"))
#'
#'
#' @export


add_probs.survreg <- function(df, fit, q,
                              name = NULL, yhatName = "median_pred",
                              comparison = "<",
                              confint = TRUE,
                              alpha = 0.05,
                              ...){

    if (is.null(name) & (comparison == "<" || comparison == ">=")){

        name[1] <- paste("prob_less_than", q, sep="")
        name[2] <- paste("lcb")
        name[3] <- paste("ucb")
    }
    if (is.null(name) & (comparison == ">" || comparison == ">=")){

        name[1] <- paste("prob_greater_than", q, sep="")
        name[2] <- paste("lcb")
        name[3] <- paste("ucb")
    }
    if ((name[1] %in% colnames(df)))
        warning ("These probabilities may have already been appended to your dataframe. Overwriting.")

    if (!is.null(fit$weights))
        if (var(fit$weights) != 0)
            stop("weighted regression is unsupported.")

    if (!(fit$dist %in%
          c("loglogistic", "lognormal", "loggaussian", "exponential", "weibull")))
        stop("Unsupported distribution")

    if(any(is.na(df)))
        stop("Check df for missingness")

    ## if (method == "parametric") {
        parametric_ci_survreg_prob(df, fit, q, name, yhatName, comparison,
                                   confint, alpha)
    ## }
    ## else if (method == "boot") {
    ##     boot_ci_survreg_prob(df, fit, q, name, yhatName, comparison,
    ##                          confint, alpha, nSims)
    ## }
    ## else
    ##     stop("method must be either boot or parametric")
}

survreg_calc_probs <- function(df, fit, q, comparison){
    form <- formula(fit)
    m <- model.frame(form, df)
    mat <- model.matrix(form, m)

    dist <- survival::survreg.distributions[[fit$dist]][["dist"]]
    fn_list <- survival::survreg.distributions[[dist]][["density"]]
    cdf <- function(x) fn_list(x)[,1]

    pred <- predict(fit, df, type = "linear")
    scale <- fit$scale
    zeta <- (log(q) - pred) / scale

    if (comparison == "<" || comparison == "<=")
        F <- cdf(zeta)
    else if (comparison == ">" || comparison == ">=")
        F <- 1 - cdf(zeta)
    else
        stop("invalid comparison")

    return(list(
        F = F,
        mat = mat,
        zeta = zeta
    ))
}

parametric_ci_survreg_prob <- function(df, fit, q,
                                       name, yhatName,
                                       comparison,
                                       confint,
                                       alpha
                                       ){

    collect <- survreg_calc_probs(df = df, fit = fit, q = q,
                                  comparison = comparison)

    F <- collect[["F"]]
    zeta <- collect[["zeta"]]
    mat <- collect[["mat"]]

    if (confint){
        dist <- survival::survreg.distributions[[fit$dist]][["dist"]]
        fn_list <- survival::survreg.distributions[[dist]][["density"]]
        dens <- function(x) fn_list(x)[,3]
        scale <- fit$scale
        crit_val <- qnorm(p = 1 - alpha/2, mean = 0, sd = 1)
        cov_mat <- vcov(fit)

        if (fit$dist == "exponential")
            cov_mat <- cbind(rbind(cov_mat, 0), 0)

        d_g <- rep(NA, dim(mat)[1])
        seF <- rep(NA, dim(mat)[1])

        for(i in 1:dim(mat)[1]){
            d_g_beta <- dens(zeta[i]) * (-mat[i,] / scale)
            d_g_delta <- dens(zeta[i]) * (-zeta[i])
            d_g_vec <- c(d_g_beta, d_g_delta)
            seF[i] <- sqrt(t(d_g_vec) %*% cov_mat %*% d_g_vec)
        }

        w <- exp(crit_val * seF / (F * (1 - F)))
        lwr <- F / (F + (1 - F) * w)
        upr <- F / (F + (1 - F) / w)
    }

    if(is.null(df[[yhatName]]))
        df[[yhatName]] <- predict(fit, df, type = "quantile" , p = 0.5)

    df[[name[1]]] <- as.numeric(F)

    if (confint){
        df[[name[2]]] <- as.numeric(lwr)
        df[[name[3]]] <- as.numeric(upr)
    }

    data.frame(df)
}

boot_ci_survreg_prob <- function(df, fit, q,
                                 name, yhatName,
                                 comparison,
                                 confint,
                                 alpha,
                                 nSims){
    pred <- predict(fit, df, type = "quantile", p = 0.5)

    collect <- survreg_calc_probs(df = df, fit = fit, q = q,
                                  comparison = comparison)

    F <- collect[["F"]]

    if (confint){
        dat <- model.frame(fit)
        unsurv <- as.matrix(dat[,1])
        dat <- cbind(dat, unsurv)
        nBoot <- NROW(dat)
        boot_mat <- matrix(NA, nrow = nSims, ncol = NROW(df))
        for (i in 1:nSims){
            temp <- dat[sample(1:nBoot, size = nBoot, replace = TRUE),]
            boot_fit <- survival::survreg(formula(fit$terms), data = temp,
                                          dist = fit$dist)
            boot_mat[i,] <- survreg_calc_probs(df = df, fit = boot_fit, q = q,
                                               comparison = comparison)[["F"]]
        }
        lwr = apply(boot_mat, 2, quantile, probs = alpha / 2)
        upr = apply(boot_mat, 2, quantile, probs = 1 - alpha / 2)
    }

    if(is.null(df[[yhatName]]))
        df[[yhatName]] <- pred

    df[[name[1]]] <- F

    if (confint){
        df[[name[2]]] <- lwr
        df[[name[3]]] <- upr
    }

    data.frame(df)
}
