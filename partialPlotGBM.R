partialPlotGBM <- function (x, pred.data, x.var, which.class, w, n.trees, plot = TRUE, add = FALSE, 
          n.pt = min(length(unique(pred.data[, xname])), 51), rug = TRUE, 
          xlab = deparse(substitute(x.var)), ylab = "", main = paste("Partial Dependence on", 
                                                                     deparse(substitute(x.var))), ...) 
{
  if (is.null(x$trees)) 
    stop("The object must contain the trees.\n")
  x.var <- substitute(x.var)
  xname <- if (is.character(x.var)) 
    x.var
  else {
    if (is.name(x.var)) 
      deparse(x.var)
    else {
      eval(x.var)
    }
  }
  xv <- pred.data[, xname]
  n <- nrow(pred.data)
  if (missing(w)) 
    w <- rep(1, n)
  if (is.factor(xv) && !is.ordered(xv)) {
    x.pt <- levels(xv)
    y.pt <- numeric(length(x.pt))
    for (i in seq(along = x.pt)) {
      x.data <- pred.data
      x.data[, xname] <- factor(rep(x.pt[i], n), levels = x.pt)
      y.pt[i] <- weighted.mean(predict(x, x.data, n.trees), 
                                    w, na.rm = TRUE)
    }
    if (add) {
      points(1:length(x.pt), y.pt, type = "h", lwd = 2, 
             ...)
    }
    else {
      if (plot) 
        barplot(y.pt, width = rep(1, length(y.pt)), col = "blue", 
                xlab = xlab, ylab = ylab, main = main, names.arg = x.pt, 
                ...)
    }
  }
  else {
    if (is.ordered(xv)) 
      xv <- as.numeric(xv)
    x.pt <- seq(min(xv), max(xv), length = n.pt)
    y.pt <- numeric(length(x.pt))
    for (i in seq(along = x.pt)) {
      x.data <- pred.data
      x.data[, xname] <- rep(x.pt[i], n)
        y.pt[i] <- weighted.mean(predict(x, x.data, n.trees), 
                                 w, na.rm = TRUE)
    }
    if (add) {
      lines(x.pt, y.pt, ...)
    }
    else {
      if (plot) 
        plot(x.pt, y.pt, type = "l", xlab = xlab, ylab = ylab, 
             main = main, ...)
    }
    if (rug && plot) {
      if (n.pt > 10) {
        rug(quantile(xv, seq(0.1, 0.9, by = 0.1)), side = 1)
      }
      else {
        rug(unique(xv, side = 1))
      }
    }
  }
  invisible(list(x = x.pt, y = y.pt))
}
