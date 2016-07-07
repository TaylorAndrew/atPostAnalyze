#' marascuiloPHA
#'
#' marascuiloPHA performs the Marascuilo PHA procedure, which is a conservative chi-square pairwise comparison tests. Specifically, given a level of a grouping variable of interest, this procedure will test whether or not the proportion of patients that are that level differ pairwise across all levels of another variable.
#'
#' @param data data.frame containing all data
#' @param group Variable name of the grouping variable
#' @param var Variance name for the variable of interest
#' @param pval Significance level. Default of 0.05
#' @param dec Number of decimals to round descriptive statistics to.
#'
#' @return A list of matrices.
#' @export
#'
#' @examples
#' example_df <- data.frame(x1 = sample(letters[1:3], 30, replace = T),
#'                          x2 = sample(letters[10:14], 30, replace = T))
#' example_df
#' with(example_df, table(x1, x2))
#' marascuiloPHA(data = example_df, group = "x1", var = "x2")
marascuiloPHA <-
  function(data,
           group,
           var,
           pval = .05,
           dec = 3) {
    #Table of column percentages. Proportion for each level of 'group' per level of 'var'.
    tab <-
      cbind(row.names(prop.table(table(data[, group],
                                       data[, var]), 2)),
            prop.table(table(data[, group],
                             data[, var]), 2))
    forOneLevel <- function(ref) {
    #Proporions of reference group (compared to non-reference group), per level of 'var'
    tabv2 <- subset(tab, tab[, 1] == ref)
    tabv3 <- tabv2[,-1]
    len <- length(levels(factor(data[, var])))
    props <- matrix(ncol = 1, nrow = len)
    props[, 1] <- as.numeric(tabv3)

    row.names(props) <- levels(factor(data[, var]))
    colnames(props) <- paste0("Proportion: ", ref)

    d <- matrix(nrow = len, ncol = len)

    for (i in 1:len) {
      for (j in 1:len) {
        if (i == j)
          d[i, j] <- NA
        if (i != j)
          d[i, j] <- abs(props[i] - props[j])
      }
    }
    row.names(d) <- levels(factor(data[, var]))
    colnames(d) <- levels(factor(data[, var]))
    tabsum <- table(data[, var])
    x2 <- qchisq(1 - pval, len - 1)
    critt <- matrix(nrow = len, ncol = len)
    for (i in 1:len) {
      for (j in 1:len) {
        if (i == j)
          critt[i, j] <- NA
        if (i != j)
          critt[i, j] <-
            sqrt(x2) *
            sqrt(((props[i] * (1 - props[i])) / tabsum[i]) +
                   ((props[j] * (1 - props[j])) / tabsum[j]))
      }
    }
    output <- matrix(nrow = len, ncol = len + 1)
    for (i in 1:len) {
      for (j in 1:len) {
        if (i == j)
          output[i, j + 1] <- NA
        if (i != j)
          output[i, j + 1] <-
            ifelse(d[i, j] >= critt[i, j],
                   paste(round(d[i, j], digits = dec), "*"),
                   round(d[i, j], digits = dec))
      }
    }
    output[, 1] <- round(props, digits = dec)
    row.names(output) <- levels(factor(data[, var]))
    colnames(output) <- c(paste0("Proportion: ", ref), levels(factor(data[, var])))
    return(output)
    }
    lvls <- levels(factor(data[, group]))
    outList <- do.call(list, lapply(lvls, forOneLevel))
    return(outList)
  }