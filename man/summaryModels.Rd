% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summaryModels.R
\name{summaryModels}
\alias{summaryModels}
\title{summaryModels}
\usage{
summaryModels(x, lmer = FALSE, digits = 3)
}
\arguments{
\item{x}{Model object}

\item{lmer}{If TRUE, the model object will be considered as an lmer() model object}

\item{digits}{Number of digits to round estimates to}
}
\value{
R data.frame object containing model estimates and p-values
}
\description{
summaryModels returns model estimates of interst for: lm, glm, gee, lmer, coxph and aov model objects
}
\examples{
example_df <- data.frame(a = sample(c(0, 1), 100, replace = T),
                         b = sample(letters[24:26], 100, replace = T),
                         c = rnorm(100),
                         d = rnorm(100, 15, 3))
lmMod <- lm(c ~ d, data = example_df)
logMod <- glm(a ~ b*c, data = example_df, family = binomial)
aovMod <- aov(c ~ a, data = example_df)
summaryModels(lmMod)
summaryModels(logMod)
summaryModels(aovMod)
}

