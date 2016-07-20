#' summaryModels
#'
#' summaryModels returns model estimates of interst for: lm, glm, gee, lmer, coxph and aov model objects
#'
#' @param x Model object
#' @param lmer If TRUE, the model object will be considered as an lmer() model object
#' @param digits Number of digits to round estimates to
#'
#' @return R data.frame object containing model estimates and p-values
#' @export
#'
#' @examples
#' example_df <- data.frame(a = sample(c(0, 1), 100, replace = T),
#'                          b = sample(letters[24:26], 100, replace = T),
#'                          c = rnorm(100),
#'                          d = rnorm(100, 15, 3))
#' lmMod <- lm(c ~ d, data = example_df)
#' logMod <- glm(a ~ b*c, data = example_df, family = binomial)
#' aovMod <- aov(c ~ a, data = example_df)
#' summaryModels(lmMod)
#' summaryModels(logMod)
#' summaryModels(aovMod)
summaryModels <-
  function(x,lmer = FALSE, digits = 3) {
    if (lmer == FALSE) {
      if(as.character(x$call[1]) == "polr") {
        sumMod <- summary(x)
        ctable <- coef(sumMod)
        p <-
          round(pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2, 3)
        cis <- round(exp(confint.default(x)), 2)
        l = length(cis[, 1])
        cis <-
          c(paste0("(", cis[, 1], ', ', cis[, 2], ")"), rep("", length(p) - length(cis[, 1])))
        sums <- data.frame(
          Variables = names(p),
          OR = paste0(round(exp(ctable[, 1]), 2), " ", cis),
          p.value = p
        )
        sums <- sums[c(1:l),]
        return(sums)
      }
      if (as.character(x$call[1]) == "coxph") {
        y <- summary(x)
        if (length(row.names(y$conf.int)) > 1) {
          sums <-
            data.frame(
              row.names(y$conf.int),round(y$conf.int[,c(1,3,4)],
                                          digits = digits),
                                          round(y$coeff[,5],digits = digits)
            )
        }
        if (length(row.names(y$conf.int)) == 1) {
          sums <-
            data.frame(t(c(
              row.names(y$conf.int),round(y$conf.int[,c(1,3,4)],
                                          digits = digits),
                                          round(y$coeff[,5],digits = digits)
            )))
        }
        sums$HR <- paste0(sums[,2], " (",sums[,3],", ",sums[,4],")")
        sums <- data.frame(sums[,c(1,6,5)])
        names(sums) <- c("Variables","HR (95% CI)","pvalue")
          return(sums)
      }
      if (as.character(x$call[1]) %in% c("lm","glm")) {
        if (is.null(x$call$family))
          x$call$family <- "identity"
        if (x$call$family == "identity") {
          y <- summary(x)
          sums <-
            data.frame(
              row.names(y$coefficients),paste0(
                round(y$coefficients[,1],digits)," (",round(y$coefficients[,2],2),")"
              ),round(y$coefficients[,4],digits)
            )
          names(sums) <- c("Variables","Estimate (Std. Error)","pvalue")
         return(sums)
        }
        if (x$call$family == "binomial") {
          LogisticOR <- function(model) {
            k <-
              as.data.frame(cbind(paste0(
                round(exp(cbind(
                  coef(model)
                )),digits = digits)," (",round(exp(cbind(
                  confint(model)[,1]
                )),digits = digits),", ",round(exp(cbind(
                  confint(model)[,2]
                )),digits = 2),")"
              )))
            rownames(k) <- rownames(cbind(coef(model)))
            return(k)
          }
          OR <- LogisticOR(x)
          y <- summary(x)
          sums <-
            data.frame(row.names(OR),OR,round(y$coefficients[,4], digits))
          names(sums) <- c("Variables","OR (95% CI)","pvalue")


            return(sums)
        }
      }
      if (as.character(x$call[1]) %in% c("aov")) {
        y <- summary(x)
        etasq <- function(Sumaov) {
          ssb <- Sumaov[[1]][["Sum Sq"]][-length(Sumaov[[1]][["Sum Sq"]])]
          sse <- Sumaov[[1]][["Sum Sq"]][length(Sumaov[[1]][["Sum Sq"]])]
          eta <- ssb / sse
          len <- length(Sumaov[[1]][["Df"]]) - 1
          library(MBESS)
          get.cis <- function(i = 1) {
            lower <-
              ci.pvaf(
                F.value = Sumaov[[1]][["F value"]][i], df.1 = Sumaov[[1]][["Df"]][i], df.2 =
                  Sumaov[[1]][["Df"]][length(Sumaov[[1]][["Df"]])], N = (sum(Sumaov[[1]][["Df"]]) +
                                                                           1), conf.level = .95
              )$Lower.Limit.Proportion.of.Variance.Accounted.for
            upper <-
              ci.pvaf(
                F.value = Sumaov[[1]][["F value"]][i], df.1 = Sumaov[[1]][["Df"]][i], df.2 =
                  Sumaov[[1]][["Df"]][length(Sumaov[[1]][["Df"]])], N = (sum(Sumaov[[1]][["Df"]]) +
                                                                           1), conf.level = .95
              )$Upper.Limit.Proportion.of.Variance.Accounted.for
            cisi <- c(lower,upper)
            return(cisi)
          }
          ciss <- do.call(rbind, lapply(1:len,get.cis))
          EffectSize <-
            data.frame(row.names(Sumaov[[1]])[-length(row.names(Sumaov[[1]]))],paste0(
              round(eta,digits)," (",round(ciss[,1],3),", ",round(ciss[,2],digits),")"
            ))
          names(EffectSize) <- c("Variables","Effect Size")
          return(EffectSize)
        }
        etaES <- etasq(y)
        sums <-
          data.frame(etaES[,1],etaES[,2],round(y[[1]][["Pr(>F)"]][-length(y[[1]][["Pr(>F)"]])],digits))
        names(sums) <- c("Variables","Effect Size (95% CI)","P values")
          return(sums)
      }
      if (as.character(x$call[1]) %in% c("gee")) {
        if (as.character(x$family[1]) %in% c("gaussian")) {
          coefn <- coef(summary(x))
          ps <- round(2 * pnorm(abs(coefn[,5]), lower.tail = FALSE),digits)
          sums <- data.frame(
            Source = row.names(coefn),
            Estimate = paste0(round(coefn[,c(1)],2)," (",round(coefn[,c(4)],digits),")"),pval =
              ps
          )
            return(sums)
        }
        if (as.character(x$family[1]) %in% c("binomial")) {
          coefn <- coef(summary(x))
          OR <- exp(coefn[,1])
          ps <- round(2 * pnorm(abs(coefn[,5]), lower.tail = FALSE),digits)
          se <- summary(x)$coefficients[,4]
          ub <- exp(coefn[,1] + (se * qnorm(0.975)))
          lb <- exp(coefn[,1] - (1 * se * qnorm(0.975)))
          sums <- data.frame(
            Source = row.names(coefn),
            `OR (95% CI)` = paste0(
              round(OR,2)," (",round(lb,2),", ",round(ub,digits),")"
            ),pval = ps
          )
            return(sums)
        }
      }
    }
    if (lmer == TRUE) {
      y <- summary(x)
      coefs <- data.frame(coef(y))
      # use normal distribution to approximate p-value
      coefs$p.z <-
        round(2 * (1 - pnorm(abs(coefs$t.value))),digits = digits)
      coefs$p.z <- ifelse(coefs$p.z == 0,"< 0.0001",
                          coefs$p.z)
      sums <- data.frame(
        Source = row.names(coefs),
        Estimate = paste0(round(coefs[,c(1)],2)," (",round(coefs[,c(2)],digits),")"),pval =
          coefs$p.z
      )
        return(sums)
    }
    return(sums)
  }
