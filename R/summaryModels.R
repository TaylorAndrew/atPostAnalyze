#' summaryModels
#'
#' summaryModels returns model estimates of interst for: lm, glm, gee, lmer, coxph and aov model objects
#'
#' @param x Model object
#' @param lmer If TRUE, the model object will be considered as an lmer() model object
#' @param fn Filename for the output document. If NULL, no output word document will be created
#' @param digits Number of digits to round estimates to
#' @param noprint If TRUE, output will not be retained as an R data.frame object.
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
  function(x,lmer = FALSE,fn = NULL, digits = 3, noprint = FALSE) {
    if (!is.null(fn)) {
      quick.print <- function(PrintType = "Table",
                              table = FALSE,
                              filename = "RPrintout.doc",
                              tableintro = "Table X",
                              row.names = FALSE,
                              fontsize = 10,
                              column.margins = 1,
                              plotfunc = FALSE,
                              plotargs = FALSE,
                              plotwidth = 6,
                              plotheight = 6,
                              resolution = 300) {
        #Get package and load
        get.packages <- function(list) {
          new.packages <- list[!(list %in% installed.packages()[,"Package"])]
          old.packages <- list[(list %in% installed.packages()[,"Package"])]
          if (length(old.packages)) {
            #       print(paste(rbind(old.packages, "is already installed.")))
          }
          if (length(new.packages)) {
            #       print(paste(rbind(new.packages, "not yet installed. Attempting to install now")))
            install.packages(new.packages)
          }
          lapply(list,function(x) {
            library(x,character.only = TRUE)
          })
          #     print(paste(rbind(list, "has been loaded")))
        }
        get.packages("rtf")
        if (PrintType == "Table") {
          type <- class(table)
          tabtypes <- c("table", "data.frame", "matrix")
          match <- type %in% tabtypes
          if (match != 1)
            print("Error: You have specified a non-Table-Type Object as a Table-Type Object")
          else {
            #Write to rtf
            rtf <- RTF(
              filename, width = 8.5, height = 11, font.size = 10,
              omi = c(1, 1, 1, 1)
            )
            addParagraph(rtf, tableintro)
            len <- length(table[1,])
            columnwid <- rep(column.margins,len)
            if (row.names == TRUE)
              columnwid <- c(columnwid,column.margins)
            addTable(
              rtf, table, font.size = fontsize, row.names = row.names, NA.string = "-",
              col.widths = columnwid
            )
          }
          if (PrintType == "Plot") {
            rtf <- RTF(
              filename, width = 8.5, height = 11, font.size = 10,
              omi = c(1, 1, 1, 1)
            )
            addParagraph(rtf, tableintro)
            addPlot(
              rtf,plot.fun = plotfunc,width = plotwidth,height = plotheight,res = resolution, plotargs
            )
          }
          done(rtf)
        }
      }
    }
    if (lmer == FALSE) {
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
        if (!is.null(fn))
          quick.print(table = sums, filename = fn, row.names = F)
        if (noprint == F)
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
          if (!is.null(fn))
            quick.print(table = sums, filename = fn)
          if (noprint == F)
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
          if (!is.null(fn))
            quick.print(table = sums, filename = fn, row.names = F)
          if (noprint == F)
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
        if (noprint == F)
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
          if (!is.null(fn))
            quick.print(table = sums, filename = fn)
          if (noprint == F)
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
          if (noprint == F)
            return(sums)
          if (!is.null(fn))
            quick.print(table = sums, filename = fn)
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
      if (!is.null(fn))
        quick.print(table = sums, filename = fn)
      if (noprint == F)
        return(sums)
    }
  }