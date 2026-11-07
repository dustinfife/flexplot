#' Stored demonstration models
#'
#' Fitted objects distributed with flexplot for examples and regression tests.
#' Loading these objects does not fit new models.
#' @name model_fixtures
#' @aliases gamma_fit gaussian_fit logistic_fit logistic_fit_interaction
#'   poisson_fit mixed_logistic small_mixed_mod small_randomForest small_rf
#' @docType data
#' @format A collection of separately loadable fitted model objects:
#' \describe{
#'   \item{gamma_fit}{A Gamma GLM, \code{y_gam ~ x + z}, fitted to \code{small}.}
#'   \item{gaussian_fit}{A Gaussian GLM, \code{y_bin ~ x + z}, fitted to \code{small}.}
#'   \item{logistic_fit}{A binomial GLM, \code{y_bin ~ x + z}, fitted to \code{small}.}
#'   \item{logistic_fit_interaction}{A binomial GLM, \code{y_bin ~ x * z},
#'     fitted to \code{small}.}
#'   \item{poisson_fit}{A Poisson GLM, \code{y_pois ~ x + z}, fitted to \code{small}.}
#'   \item{mixed_logistic}{A binomial mixed model with formula
#'     \code{y_binary ~ x + a + (x | id)}, fitted to 51 observations.}
#'   \item{small_mixed_mod}{A linear mixed model with formula
#'     \code{y ~ x + a + (1 | id)}, fitted to \code{small_mixed}.}
#'   \item{small_randomForest}{A regression forest of class \code{randomForest}.}
#'   \item{small_rf}{A conditional inference forest of class \code{RandomForest}
#'     from the party package.}
#' }
#' @seealso \code{\link{small}}, \code{\link{small_mixed}}
NULL

#' Small binary-response example data
#'
#' Alternative encodings of a binary outcome, used to exercise factor ordering
#' and binary-response plotting.
#' @format A data frame with 100 rows and six columns:
#' \describe{
#'   \item{x}{Numeric predictor.}
#'   \item{y}{Numeric response.}
#'   \item{y_char}{Binary outcome encoded with text labels.}
#'   \item{y_numb}{Binary outcome encoded numerically.}
#'   \item{y_ord}{Ordered-factor encoding of the binary outcome.}
#'   \item{y_ord_rev}{Ordered-factor encoding with reversed levels.}
#' }
"small_logistic"

#' Small clustered example data
#'
#' A small dataset used for mixed-model plotting examples and regression tests.
#' @format A data frame with 51 rows and six columns:
#' \describe{
#'   \item{y}{Numeric outcome.}
#'   \item{x}{Numeric predictor.}
#'   \item{a}{Categorical predictor.}
#'   \item{b}{Categorical predictor.}
#'   \item{z}{Numeric predictor.}
#'   \item{id}{Cluster identifier.}
#' }
#' @seealso \code{\link{model_fixtures}}
"small_mixed"

#' Legacy logical example object
#'
#' A stored logical constant retained for compatibility. Prefer \code{TRUE} in
#' new code, since names such as \code{T} can be reassigned in an R session.
#' @format A logical vector of length one containing \code{TRUE}.
"T"
