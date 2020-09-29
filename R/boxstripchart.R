##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param y values
##' @param x groups
##' @param ... will be parsed to boxplot and stripchart
##' @return
##' @author Wenjian Yang

boxstripchart <- function(y, x, ...) {

  user.args <- list(...)

  x.num <- as.numeric(factor(x))
  x.lim <- c(min(x.num, na.rm=TRUE) - 0.5, max(x.num, na.rm=TRUE) + 0.5)

  y.sorted <- unique(sort(y))
  pstep <- quantile(y.sorted[-1] - y.sorted[-length(y.sorted)], 0.1)

  ## get min/max
  pp <- boxplot(y ~ x, plot=FALSE)
  pmin <- min(pp$stats) - pstep
  pmax <- max(pp$stats) + pstep

  boxplot.args <- list(formula=y ~ x, outline=FALSE, ylim=c(pmin, pmax))
  boxplot.args <- modifyList(boxplot.args, user.args)

  do.call('boxplot', boxplot.args)

  strip.args <- list(x=y ~ x, vert=T, add=T, col="red", pch=16, method="jitter", xlim=x.lim)
  strip.args <- modifyList(strip.args, user.args)
  do.call('stripchart', strip.args)
}

