#' posthocChiSq
#'
#' posthocChiSq computes standard pairwise chi-squared tests and provides raw and adjusted p-values.
#'
#' @param chiObject
#' @param control
#' @param digits
#'
#' @return data.frame containing raw and adjusted p-values.
#' @export
#'
#' @examples
#' example_df <- data.frame(x1 = sample(c('m', 'f'), 50, replace = T),
#'                          x2 = sample(letters[1:4], 50, replace = T))
#' test <- with(example_df, chisq.test(x2, x1))
#' posthocChiSq(chiObject = test)
#' test2 <- with(example_df, chisq.test(x1, x2))
#' posthocChiSq(chiObject = test2, control="holm")
posthocChiSq <- function (chiObject,
                         control = c("none",
                                     "fdr",
                                     "BH",
                                     "BY",
                                     "bonferroni",
                                     "holm",
                                     "hochberg",
                                     "hommel"),
                         digits = 4)
{
  control <- match.arg(control)
  tbl <- chiObject$observed
  if (length(tbl[,1])==2) {
    tbl <- t(tbl)
    varNames <- strsplit(chiObject$data.name, " and ")
    print(paste0("Pairwise variable (",
                 varNames[[1]][1],
                 ") currently has only 2 levels. Transposing data such that ",
                 varNames[[1]][2],
                 " will be used for pairwise comparison"))
  }
  popsNames <- rownames(tbl)
  prs <- combn(1:nrow(tbl), 2)
  tests <- ncol(prs)
  pvals <- numeric(tests)
  lbls <- character(tests)
  for (i in 1:tests) {
    pvals[i] <- chisq.test(tbl[prs[, i], ])$p.value
    lbls[i] <- paste(popsNames[prs[, i]], collapse = " vs. ")
  }
  if (control == "none") {
    adj.pvals <- pvals
  } else {
    adj.pvals <- p.adjust(pvals, method = control)
  }
  return(data.frame(
                comparison = lbls,
                raw.p = round(pvals, digits),
                adj.p = round(adj.pvals, digits)
  ))
}