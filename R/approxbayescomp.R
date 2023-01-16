
#' approxbayescomp
#'
#' Approximate Bayesian Computation (ABC) is a statistical method to fit a
#' Bayesian model to data when the likelihood function is hard to compute.
#' The `approxbayescomp` package implements an efficient form of ABC — the
#' sequential Monte Carlo (SMC) algorithm. While it can handle any general
#' statistical problem, we built in some models so that fitting insurance
#' loss distributions is particularly easy.
#'
#' @docType package
#' @author Patrick Laub <patrick.laub@gmail.com>
#' @import Rcpp RcppEigen
#' @importFrom Rcpp evalCpp
#' @useDynLib approxbayescomp, .registration = TRUE
#' @name approxbayescomp
#' @references Pierre-Olivier Goffard, Patrick J. Laub (2021), Approximate Bayesian Computations to fit and compare insurance loss models, Insurance: Mathematics and Economics, 100, pp. 350-371
#' @seealso{
#'   \url{https://pat-laub.github.io/approxbayescomp-r/} and \url{https://pat-laub.github.io/approxbayescomp/}
#' }
NULL
