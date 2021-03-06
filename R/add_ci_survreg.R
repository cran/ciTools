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

#' Confidence Intervals for the Mean Survival Time of Accelerated
#' Failure Time Models.
#'
#' This function is one of the methods for \code{add_ci}, and is
#' called automatically when \code{add_ci} is used on a \code{fit} of
#' class \code{survreg}.
#'
#' \code{add_ci.survreg} calculates confidence intervals for the mean
#' survival time of several accelerated failure time (AFT) models
#' including exponential, lognormal, weibull, and loglogistic
#' models. AFT models must be fit with the \code{survreg} function in
#' the \code{survival} package. Confidence intervals are formed
#' parametrically via the Delta method.
#'
#' \code{add_ci.survreg} will compute confidence intervals for the
#' following mean survival time point estimates:
#'
#' Exponential: \eqn{E[Y|X] = \exp{X\beta}}
#'
#' Weibull: \eqn{E[Y|X] = \exp{X\beta}\Gamma(1 + \sigma)}
#'
#' Lognormal: \eqn{E[Y|X] = \exp{X\beta + \frac{\sigma^2}{2}}}
#'
#' Loglogistic: \eqn{E[Y|X] = \exp{X\beta}\Gamma(1 + \sigma)(1 - \sigma)}
#'
#' Traditionally, survival time predictions are made with the median
#' survival time. For forming confidence intervals for the median
#' survival time (or any quantile of the survival time distribution),
#' see \code{\link{add_quantile.survreg}}.
#'
#' Note: The expected survival time of a loglogistic model with scale
#' >= 1 does not exist. Otherwise, expected survival times exist for
#' each of the four AFT models considered in \code{add.ci_survreg}.
#'
#' Note: Due to a limitation, the \code{Surv} object must be specified in
#' \code{survreg} function call. See the examples section for one way
#' to do this.
#'
#' Note: \code{add_ci.survreg} cannot inspect the convergence of
#' \code{fit}. Poor maximum likelihood estimates will result in poor
#' confidence intervals. Inspect any warning messages given from
#' \code{survreg}.
#'
#' @param df A data frame of new data on which to form
#'     predictions and confidence intervals.
#' @param fit An object of class \code{survreg}. Predictions are made
#'     with this object.
#' @param names \code{NULL} or a string of length 2. If \code{NULL}, quantiles
#'     automatically will be named by \code{add_quantile}, otherwise,
#'     they will be named \code{names}.
#' @param yhatName A string. Name of the vector of predictions. The
#'     default name is \code{mean_pred}.
#' @param alpha A number between 0 and 1. 1 - \code{alpha} is the
#'     confidence level of the intervals.
## #' @param method A string. One of either \code{"parametric"} or
## #'     \code{"boot"}. If \code{method = "parametric"}, Delta method
## #'     intervals are calculated. If \code{method = "boot"}
## #'     nonparametric bootstrap intervals are calculated.
## #' @param nSims A positive integer. Set the number of simulated draws
## #'     to use. A value greater than or equal to 2000 is recommended.
#' @param ... Additional arguments.
#'
#' @return A dataframe, \code{df}, with predicted expected values and
#'     level \emph{1 - alpha} level confidence levels attached.
#'
#' @seealso \code{\link{add_quantile.survreg}} for quantiles of the
#'     survival time distribution of \code{survreg} objects,
#'     \code{\link{add_pi.survreg}} for prediction intervals of
#'     \code{survreg} objects, and \code{\link{add_probs.survreg}} for
#'     survival probabilities of \code{survreg} objects.
#'
#' @examples
#' ## Define a data set.
#' df <- survival::stanford2
#' ## remove a covariate with missing values.
#' df <- df[, 1:4]
#' ## next, create the Surv object inside the survreg call:
#' fit <- survival::survreg(survival::Surv(time, status) ~ age + I(age^2),
#'                          data = df, dist = "lognormal")
#' add_ci(df, fit, alpha = 0.1, names = c("lwr", "upr"))
#'
#' ## Try a different model:
#' fit2 <- survival::survreg(survival::Surv(time, status) ~ age + I(age^2),
#'                           data = df, dist = "weibull")
#' add_ci(df, fit2, alpha = 0.1, names = c("lwr", "upr"))
#'
#' @references
#' For descriptions of the log-location scale models supported:
#' Meeker, William Q., and Luis A. Escobar. Statistical methods for reliability data. John Wiley & Sons, 2014. (Chapter 4)
#'
#' For a description of the multivariate Delta method:
#' Meeker, William Q., and Luis A. Escobar. Statistical methods for reliability data. John Wiley & Sons, 2014. (Appendix B.2)
#'
#' @export

add_ci.survreg <- function(df, fit,
                           alpha = 0.1,
                           names = NULL,
                           yhatName = "mean_pred",
                           ...){

    if (is.null(names)){
        names[1] <- paste("LCB", alpha/2, sep = "")
        names[2] <- paste("UCB", 1 - alpha/2, sep = "")
    }

    if ((names[1] %in% colnames(df))) {
        warning ("These quantiles may have already been appended to your dataframe. Overwriting.")
    }

    if(any(is.na(df)))
        stop("Check df for missingness")

    if (!(fit$dist %in%
          c("loglogistic", "lognormal", "loggaussian", "exponential", "weibull")))
        stop("Unsupported distribution")

    if (!is.null(fit$weights))
        if (var(fit$weights) != 0)
            stop("weighted regression is unsupported.")

    ## if(method == "boot")
    ##     boot_ci_survreg_expectation(df, fit,
    ##                                 alpha,
    ##                                 names,
    ##                                 yhatName,
    ##                                 nSims)
    ## else if(method == "parametric")
    parametric_ci_survreg_expectation(df, fit,
                                      alpha,
                                      names,
                                      yhatName)
    ## else
    ##     stop("method must be either 'boot' or 'parametric'")
}


calc_surv_mean <- function(mat, distr, beta, scale){

    if (distr == "weibull")
        pred <- exp(mat %*% beta) * gamma(1 + scale)
    else if (distr == "exponential")
        pred <- exp(mat %*% beta)
    else if (distr == "loglogistic")
        pred <- exp(mat %*% beta) * gamma(1 + scale) * gamma(1 - scale)
    else if ((distr == "lognormal") || (distr == "loggaussian"))
        pred <- exp(mat %*% beta + (scale^2) / 2)

    pred
}

parametric_ci_survreg_expectation <- function(df, fit,
                                              alpha,
                                              names,
                                              yhatName){
    distr <- fit$dist

    if (distr == "loglogistic" && (fit$scale >= 1))
        stop("Expected value is undefined for loglogistic distribution with scale >= 1")

    form <- formula(fit)
    m <- model.frame(form, df)
    mat <- model.matrix(form, m)


    nPred <- dim(df)[1]
    beta <- coef(fit)
    scale <- fit$scale

    pred <- calc_surv_mean(mat = mat, distr = distr,
                           beta = beta, scale = scale)

    crit_val <- qnorm(p = 1 - alpha/2, mean = 0, sd = 1)
    cov_mat <- vcov(fit)

    if (distr == "exponential")
        cov_mat <- cbind(rbind(cov_mat, 0), 0)

    d_g <- rep(NA, nPred)
    seYhat <- rep(NA, nPred)

    for(i in 1:nPred){
        if (distr == "weibull"){
            d_g_beta <- c(exp(mat[i,] %*% beta)) * mat[i,] * gamma(1 + scale)
            d_g_delta <- exp(mat[i,] %*% beta) *
                digamma(1 + scale) * gamma (1 + scale) * scale
        }
        if (distr == "exponential"){
            d_g_beta <- c(exp(mat[i,] %*% beta)) * mat[i,]
            d_g_delta <- 0
        }
        if (distr == "loglogistic"){
            d_g_beta <- c(exp(mat[i,] %*% beta)) * mat[i,] *
                gamma(1 + scale) * gamma(1 - scale)
            d_g_delta <- c(exp(mat[i,] %*% beta)) *
                (gamma(1 + scale) * digamma(1 - scale) * gamma(1 - scale) * (-scale) +
                 gamma(1 - scale) * digamma(1 + scale) * gamma(1 + scale) * scale)
        }
        if (distr == "lognormal"){
            d_g_beta <- exp(c(mat[i,] %*% beta) + (scale^2) / 2) * mat[i,]
            d_g_delta <- exp(c(mat[i,] %*% beta) + (scale^2) / 2) * (scale^2)
        }
        d_g_vec <- c(d_g_beta, d_g_delta)
        seYhat[i] <- sqrt(t(d_g_vec) %*% cov_mat %*% d_g_vec)
    }

    w <- exp(crit_val * seYhat / pred)
    lwr <- pred / w
    upr <- pred * w

    if(is.null(df[[yhatName]]))
        df[[yhatName]] <- c(pred)

    df[[names[1]]] <- as.numeric(lwr)
    df[[names[2]]] <- as.numeric(upr)

    data.frame(df)
}


surv_boot_mean <- function(df, fit){
    distr <- fit$dist

    if (distr == "loglogistic" && (fit$scale >= 1)){
        pred <- NA
        return(pred)
    }

    form <- formula(fit)
    m <- model.frame(form, df)
    mat <- model.matrix(form, m)
    nPred <- dim(df)[1]
    beta <- coef(fit)
    scale <- fit$scale

    pred <- calc_surv_mean(mat = mat, distr = distr,
                           beta = beta, scale = scale)
    pred
}

boot_ci_survreg_expectation <- function(df, fit,
                                        alpha,
                                        names,
                                        yhatName,
                                        nSims){

    distr <- fit$dist

    if (distr == "loglogistic" && (fit$scale >= 1))
        stop("Expected value is undefined for loglogistic distribution with scale >= 1")

    form <- formula(fit)
    m <- model.frame(form, df)
    mat <- model.matrix(form, m)

    if(any(is.na(mat)))
        stop("Check df for missingness")

    nPred <- dim(df)[1]
    beta <- coef(fit)
    scale <- fit$scale

    pred <- calc_surv_mean(mat = mat, distr = distr,
                           beta = beta, scale = scale)

    boot_mat <- matrix(NA, nrow = nSims, ncol = nPred)

    for (i in 1:nSims){
        temp <- df[sample(1:nPred, size = nPred, replace = TRUE),]
        boot_fit <- survival::survreg(formula(fit$terms), data = temp,
                                      dist = fit$dist)
        boot_pred <- surv_boot_mean(df, boot_fit)
        boot_mat[i,] <- boot_pred
    }

    lwr = apply(boot_mat, 2, quantile, probs = alpha / 2)
    upr = apply(boot_mat, 2, quantile, probs = 1 - alpha / 2)
    if (is.null(df[[yhatName]]))
        df[[yhatName]] <- as.numeric(pred)

    df[[names[1]]] <- lwr
    df[[names[2]]] <- upr
    data.frame(df)

}
