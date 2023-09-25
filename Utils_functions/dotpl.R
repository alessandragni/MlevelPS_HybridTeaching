# for dotplot 
dotpl <- function (x, data, main = TRUE, ...) {
  prepanel.ci <- function(x, y, se, subscripts, ...) {
    if (is.null(se)) 
      return(list())
    x <- as.numeric(x)
    hw <- 1.96 * as.numeric(se[subscripts])
    list(xlim = range(x - hw, x + hw, finite = TRUE))
  }
  panel.ci <- function(x, y, se, subscripts, pch = 16, horizontal = TRUE, 
                       col = dot.symbol$col, lty = dot.line$lty, lwd = dot.line$lwd, 
                       col.line = dot.line$col, levels.fos = unique(y), groups = NULL, 
                       ...) {
    x <- as.numeric(x)
    y <- as.numeric(y)
    dot.line <- trellis.par.get("dot.line")
    dot.symbol <- trellis.par.get("dot.symbol")
    sup.symbol <- trellis.par.get("superpose.symbol")
    panel.abline(h = levels.fos, col = col.line, lty = lty, 
                 lwd = lwd)
    panel.abline(v = 0, col = col.line, lty = lty, lwd = lwd)
    if (!is.null(se)) {
      se <- as.numeric(se[subscripts])
      panel.segments(x - 1.96 * se, y, x + 1.96 * se, y, 
                     col = "black")
    }
    panel.xyplot(x, y, pch = pch, ...)
  }
  f <- function(nx, ...) {
    xt <- x[[nx]]
    ss <- stack(xt)
    mtit <- if (main) 
      nx
    ss$ind <- factor(as.character(ss$ind), levels = colnames(xt))
    ss$.nn <- rep.int(reorder(factor(rownames(xt)), xt[[1]], 
                              FUN = mean, sort = sort), ncol(xt))
    se <- NULL
    if (!is.null(pv <- attr(xt, "postVar"))) 
      se <- unlist(lapply(1:(dim(pv)[1]), function(i) sqrt(pv[i, i, ])))
    #############################################################
    # Next line is the inseerted line to deal with clmm objects
    #############################################################
    if (!is.null(cv <- attr(xt, "condVar"))) se <- as.vector(unlist(cv))
    dotplot(.nn ~ values | ind, ss, se = se, prepanel = prepanel.ci, 
            panel = panel.ci, xlab = NULL, main = mtit, ...)
  }
  setNames(lapply(names(x), f, ...), names(x))
}