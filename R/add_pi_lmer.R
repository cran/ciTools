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

#' Prediction Intervals for Linear Mixed Model Fitted Values
#'
#' This function is one of the methods in \code{add_pi}, and is
#' called automatically when \code{add_pi} is used on a \code{fit} of
#' class \code{lmerMod}.
#'
#' It is recommended that one use parametric prediction intervals when
#' modeling with a random intercept linear mixed model. Otherwise,
#' prediction intervals may be simulated via a parametric bootstrap
#' using the function \code{lme4.simulate()}.
#'
#' @param df A data frame of new data
#' @param fit An object of class \code{lmerMod}.
#' @param alpha A real number between 0 and 1. Controls the confidence
#'     level of the interval estimates.
#' @param names \code{NULL} or character vector of length two. If
#'     \code{NULL}, prediction bounds automatically will be named by
#'     \code{add_pi}, otherwise, the lower prediction bound will be
#'     named \code{names[1]} and the upper prediction bound will be
#'     named \code{names[2]}.
#' @param yhatName A string. Name of the predictions vector.
#' @param type A string, either \code{"parametric"} or
#'     \code{"boot"}. Determines the method used to calculate the
#'     prediction intervals.
#' @param includeRanef A logical. Set whether the predictions and
#'     intervals should be conditioned on the random effects. If
#'     \code{FALSE}, random effects will not be included.
#' @param nSims A positive integer. If \code{type = "boot"},
#'     \code{nSims} will determine the number of bootstrap simulations
#'     to perform.
#' @param log_response A logical, indicating if the response is on log
#'     scale in the model fit. If \code{TRUE}, prediction intervals
#'     will be returned on the response scale.
#' @param ... Additional arguments.
#' @return A dataframe, \code{df}, with predicted values, upper and lower
#'     prediction bounds attached.
#'
#' @seealso \code{\link{add_ci.lmerMod}} for confidence intervals
#'     for \code{lmerMod} objects, \code{\link{add_probs.lmerMod}} for
#'     conditional probabilities of \code{lmerMod} objects, and
#'     \code{\link{add_quantile.lmerMod}} for response quantiles of
#'     \code{lmerMod} objects.
#'
#' @examples
#' dat <- lme4::sleepstudy
#' # Fit a (random intercept) linear mixed model
#' fit <- lme4::lmer(Reaction ~ Days + (1|Subject), data = lme4::sleepstudy)
#' # Add 50% prediction intervals to the original data using the default
#' # method.
#' add_pi(dat, fit, alpha = 0.5)
#'
#' # Add 50% prediction intervals to the original data using the
#' # parametric bootstrap method. Form prediction intervals at the population
#' # level (unconditional on the random effects).
#' add_pi(dat, fit, alpha = 0.5, type = "boot", includeRanef = FALSE)
#'
#' @export

add_pi.lmerMod <- function(df, fit,
                           alpha = 0.05, names = NULL, yhatName = "pred",
                           type = "parametric", includeRanef = TRUE,
                           log_response = FALSE, nSims = 10000, ...) {
  if (is.null(names)){
    names[1] <- paste("LPB", alpha/2, sep = "")
    names[2] <- paste("UPB", 1 - alpha/2, sep = "")
  }
  if ((names[1] %in% colnames(df))) {
    warning ("These PIs may have already been appended to your dataframe. Overwriting.")
  }

  if(type == "parametric")
        parametric_pi_lmermod(df, fit, alpha, names, includeRanef, log_response, yhatName)
    else if(type == "boot")
        boot_pi_lmermod(df, fit, alpha, names, includeRanef, nSims, log_response, yhatName)
    else
        stop("Incorrect type specified!")

}


parametric_pi_lmermod <- function(df, fit, alpha, names, includeRanef, log_response, yhatName){

    rdf <- get_resid_df_mermod(fit)
    seGlobal <- get_pi_mermod_var(df, fit, includeRanef)

    if(includeRanef)
        re.form <- NULL
    else
        re.form <- NA

    out <- predict(fit, df, re.form = re.form)
    if(is.null(df[[yhatName]]))
        df[[yhatName]] <- out
    df[[names[1]]] <- out + qt(alpha/2,df = rdf) * seGlobal
    df[[names[2]]] <- out + qt(1 - alpha/2, df = rdf) * seGlobal
    if (log_response){
        df[[yhatName]] <- exp(out)
        df[[names[1]]] <- exp(df[[names[1]]])
        df[[names[2]]] <- exp(df[[names[2]]])
    }
    data.frame(df)
}



boot_pi_lmermod <- function(df, fit, alpha, names, includeRanef, nSims, log_response, yhatName) {

    if (includeRanef)
        reform = NULL
    else
        reform = NA

    gg <- simulate(fit, newdata = df, re.form = reform, nsim = nSims)
    gg <- as.matrix(gg)
    lwr <- apply(gg, 1, FUN = quantile, probs = alpha/2)
    upr <- apply(gg, 1, FUN = quantile, probs = 1 - alpha / 2)

    out <- predict(fit, df, re.form = reform)

    if(is.null(df[[yhatName]]))
        df[[yhatName]] <- out
    df[[names[1]]] <- lwr
    df[[names[2]]] <- upr

    if (log_response){
        df[[yhatName]] <- exp(out)
        df[[names[1]]] <- exp(df[[names[1]]])
        df[[names[2]]] <- exp(df[[names[2]]])
    }
    data.frame(df)
}
