# The R interface is kept outside the generated R6 definitions.
#' General Linear Model
#'
#'
#' @param data .
#' @param out .
#' @param preds .
#' @param graphic .
#' @param graphicassump .
#' @param estimates .
#' @param se .
#' @param line .
#' @param center .
#' @param modelTerms Main effects and interactions. NULL retains the additive
#'   model; an empty list fits an intercept only.
#' @param compare Fit and compare two models on the same complete
#'   observations.
#' @param comparisonTerms Independent main effects and interactions for Model
#'   2. NULL uses all selected main effects.
#' @param comparisonX Predictor used on the comparison x axis; defaults to the
#'   first numeric model predictor, otherwise the first model predictor.
#' @param comparisonPanels Additional model predictors displayed in panels,
#'   not on a second axis.
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$assumpplot} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$plot} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$glmcat} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$diff} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$rsq} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$comparisonplot} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$comparison} \tab \tab \tab \tab \tab a table \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$glmcat$asDF}
#'
#' \code{as.data.frame(results$glmcat)}
#'
#' @export
glmbasic = function(
    data,
    out,
    preds,
    graphic = TRUE,
    graphicassump = FALSE,
    estimates = TRUE,
    se = TRUE,
    line = "Loess",
    center = "Median + quartiles",
    modelTerms = NULL,
    compare = FALSE,
    comparisonTerms = NULL,
    comparisonX = NULL,
    comparisonPanels = NULL) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("glmbasic requires jmvcore to be installed (restart may be required)")

    if ( ! missing(out)) out = jmvcore::resolveQuo(jmvcore::enquo(out))
    if ( ! missing(preds)) preds = jmvcore::resolveQuo(jmvcore::enquo(preds))
    if ( ! missing(comparisonX)) comparisonX = jmvcore::resolveQuo(jmvcore::enquo(comparisonX))
    if ( ! missing(comparisonPanels)) comparisonPanels = jmvcore::resolveQuo(jmvcore::enquo(comparisonPanels))
    if (missing(data))
        data = jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(out), out, NULL),
            `if`( ! missing(preds), preds, NULL),
            `if`( ! missing(comparisonX), comparisonX, NULL),
            `if`( ! missing(comparisonPanels), comparisonPanels, NULL))

    if (inherits(modelTerms, "formula")) modelTerms = jamovi_glm_terms(modelTerms)
    if (inherits(comparisonTerms, "formula")) comparisonTerms = jamovi_glm_terms(comparisonTerms)

    options = glmbasicOptions$new(
        out = out,
        preds = preds,
        graphic = graphic,
        graphicassump = graphicassump,
        estimates = estimates,
        se = se,
        line = line,
        center = center,
        modelTerms = modelTerms,
        compare = compare,
        comparisonTerms = comparisonTerms,
        comparisonX = comparisonX,
        comparisonPanels = comparisonPanels)

    analysis = glmbasicClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}
