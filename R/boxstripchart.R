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

boxplot.prx <- function(formula, xaxis.at=1:3, xaxis.label=c('AA', 'AB', 'BB'), point.col='red', pch=16, alpha=0.5, jitter.factor=0.2, ...) {
    boxplot(formula, axes=FALSE, axes=FALSE, ...)
    box(bty='l')
    axis(2); axis(1, at=xaxis.at, label=xaxis.label)

    yx <- eval(attributes(terms(formula))$variables, parent.frame())
    
    points(jitter(yx[[2]], factor=jitter.factor)+1, yx[[1]], col=trans.col(point.col, alpha), pch=pch)
}

boxviolin <- function (y, x, srt=0, xtext.adj=NULL, ...) 
{
    if (!requireNamespace("viopoints", quietly = TRUE)) {
      stop("Package \"viopoints\" needed for this function to work. Please install it.", call. = FALSE)
    }
    require("viopoints")
    user.args <- list(...)

    missing <- is.na(y) | is.na(x)
    x <- x[!missing]
    y <- y[!missing]

    x.factor <- factor(x)
    x.count <- table(x.factor)
    x.levels <- levels(x.factor)
    
    x.num <- as.numeric(factor(x))
    x.lim <- c(min(x.num, na.rm = TRUE) - 0.5, max(x.num, na.rm = TRUE) + 
        0.5)
    y.sorted <- unique(sort(y))
    pstep <- quantile(y.sorted[-1] - y.sorted[-length(y.sorted)], 
        0.1)
    pp <- boxplot(y ~ x, plot = FALSE)
    pmin <- min(pp$stats) - pstep
    pmax <- max(pp$stats) + pstep
    boxplot.args <- list(formula = y ~ x, outline = FALSE, ylim = c(pmin, 
        pmax))
    boxplot.args <- modifyList(boxplot.args, user.args)
    boxplot.args$axes=FALSE
    do.call("boxplot", boxplot.args)
    box(bty='L')
    axis(2)
    if (is.null(xtext.adj)) {
	axis(1, at=1:nlevels(x.factor), label=paste0(levels(x.factor), "\n(n=", x.count, ")"), srt=srt)
    } else {
        axis(1, at=1:nlevels(x.factor), label=FALSE)
        text(x=1:nlevels(x.factor), y=xtext.adj,labels=paste0(levels(x.factor), "\n(n=", x.count, ")"), srt=srt, adj=1, xpd=TRUE)
   }
    vio.args <- list(y, groups=x, col = "red", add=TRUE, 
        pch = 16, method = "violin") ##, xlim = x.lim)
    vio.args <- modifyList(vio.args, user.args)
    do.call("viopoints", vio.args)
    boxplot.args$add <- TRUE
    boxplot.args$col <- NULL
    do.call("boxplot", boxplot.args)
}

