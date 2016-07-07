#Provides p values for lmer models
#' lmerPvals
#'
#' lmerPvals provides p-values for lmer() models using the normal distribution as an approximation
#'
#' @param model A lmer() model object
#'
#' @return returns the model coefficients with the approximated p-vales appended
#' @export
#'
#' @examples
lmerPvals <- function(model) {
  coefs <- data.frame(coef(summary(model)))
  # use normal distribution to approximate p-value
  coefs$p.z <- round(2 * (1 - pnorm(abs(coefs$t.value))),digits = 3)
  coefs$p.z <- ifelse(coefs$p.z == 0,"< 0.001",
                      coefs$p.z)
  return(coefs)
}