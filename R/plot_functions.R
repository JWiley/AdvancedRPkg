globalVariables(c("fit", "se.fit"))

##' Function to plot data and mean summary
##'
##' This is a simple function designed to facilitate plotting raw
##' data along with dots indicating the mean at each x-axis value.
##'
##' Although this function can be used with any type of data that works
##' with \code{plot}, it works best when the x-axis values are discrete,
##' so that there are several y-values at the same x-axis value so that
##' the mean of multiple values is taken.
##'
##' @param formula A formula specifying the variable to be used on the
##'   y-axis and the variable to be used on the x-axis.
##' @param d A data.frame class object containing the variables specified
##'   in the \code{formula}.
##' @return Called for the side effect of creating a plot.
##' @author Wiley
##' @export
##' @importFrom graphics plot points
##' @importFrom stats formula
##' @keywords plot
##' @examples
##' # example usage of meanPlot
##' meanPlot(mpg ~ factor(cyl), d = mtcars)
meanPlot <- function(formula, d) {
  v <- all.vars(formula)
  d <- d[order(d[, v[2]]), ] ## sorting first
  m <- tapply(d[, v[1]], d[, v[2]],
              FUN = mean, na.rm = TRUE)

  plot(formula, data = d, type = "p")
  points(x = unique(d[, v[2]]), y = m,
         col = "blue", pch = 16, cex = 2)
}

##' Method for plotting linear models
##'
##' Simple method to plot a linear model using ggplot
##' along with 95% confidence intervals.
##'
##' @param data The linear model object from \code{lm}
##' @param mapping Regular mapping, see \code{ggplot} and \code{aes} for details.
##' @param vars A list of variable values used for prediction.
##' @param \ldots Additional arguments passed to \code{ggplot}
##' @return A ggplot class object.
##' @export
##' @importFrom stats predict qnorm
##' @import ggplot2
##' @examples
##' ggplot2::ggplot(
##'   lm(mpg ~ hp * qsec, data = mtcars),
##'   ggplot2::aes(hp, mpg, linetype = factor(qsec)),
##'   vars = list(
##'     hp = min(mtcars$hp):max(mtcars$hp),
##'     qsec = round(mean(mtcars$qsec) + c(-1, 1) * sd(mtcars$qsec)), 1)) +
##'   ggplot2::geom_ribbon(ggplot2::aes(ymin = LL, ymax = UL), alpha = .2) +
##'   ggplot2::geom_line() +
##'   ggplot2::theme_bw()
ggplot.lm <- function(data, mapping, vars, ...) {
  newdat <- do.call(expand.grid, vars)
  yvar <- as.character(formula(data)[[2]])
  d <- as.data.frame(predict(data, newdata = newdat, se.fit = TRUE))
  d <- within(d, {
    LL <- fit + qnorm(.025) * se.fit
    UL <- fit + qnorm(.975) * se.fit
  })
  colnames(d)[1] <- yvar
  data <- cbind(newdat, d[, c(yvar, "LL", "UL")])
  ggplot(data = data, mapping = mapping, ...)
}
