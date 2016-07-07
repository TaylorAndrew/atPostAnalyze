#' logistic_boot
#'
#' logistic_boot provides bootstrapped  with 95% confidence intervals odds ratios
#'
#' @param data data.frame containing variables to be modeled
#' @param formula is a formula object specifying the model
#' @param R is the number of random samples
#' @param seed seed to be set for replicability
#'
#' @return A data.frame with the original OR and the bootstrapped 95% CIs
#' @export
#'
#' @examples
logistic_boot <- function(data, formula, R = 10000, seed=12345) {

  logit.bootstrap <- function(data, indices) {
    d <- data[indices, ]
    fit <- glm(formula, data = d, family = "binomial")
    return(coef(fit))
}
  logit.boot <- boot(data=data, statistic=logit.bootstrap, R=R)

  getci <- function(index) {
    ci <- boot.ci(logit.boot, type="bca", index=index)
    out <- data.frame(OriginalOR = exp(logit.boot$t0[index]),
                      lower.ci = exp(ci$bca[4]),
                      upper.ci = exp(ci$bca[5]))
  }
  output <- do.call(rbind, lapply(1:length(logit.boot$t0), getci))
  return(output)
}
