#' kmPval
#'
#' kmPval provides log-rank p-value provided in the survdiff() call.
#'
#' @param model is a survdiff() model object
#'
#' @return pvalue shown in survdiff() model as a character string
#'
#' @examples
#' library(survival)
#' mod <- survdiff(Surv(runif(100, .1, 5),
#'                 sample(c(0, 1), 100, replace = T, prob = c(.2, .8))
#'                 ) ~ sample(c('no', 'yes'), 100, replace = T))
#' kmPval(mod)
kmPval <- function(model) {
  return(round(1 - pchisq(model$chisq,
                          length(model$n)-1),
               digits = 3))
}
