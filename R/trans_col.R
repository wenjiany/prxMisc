##' .. content for \description{} (no empty lines) ..
##' return rgb color coding for transparent color
##' .. content for \details{} ..
##' @title create transparent color
##' @param col
##' @param alpha
##' @return
##' @author Yang

trans_col <- function(col, alpha=1) {
    do.call(rgb, as.list(c(col2rgb(col)/255, alpha)))
}


