# ================================================================================
# autograph.continuous
# ================================================================================
# Types of graphs:
#  1. Purely continuous. Histogram.
#  2. Continuous with point mass(es). Histogram with a black bar denoting a point mass.
#     This is determined within autograph.continuous;
#     currently for zero only when values are nonnegative.
#     E.g. "income" (0, 1231, 15000, 0, 50000, ...)
#  3. Continous with a gap (with point masses or not)  [not yet implemented]
#     E.g. income with a wide gap (0, 1231, 5000000, ...)
#  4. Mixed continuous/discrete [not yet well implemented] --> mixture
#  Options:
#   - bins    : bin breaks
#   - borders : bin borders
#   - title   : field label
#   - 
ag_plot.ag_continuous <- function (x)
{
  GA <- graphattr(x)
  Attr <- function (...) ag_get(GA, ...)
  name <- Attr("data")
  y <- uncoded <- x[[name]]
  arg <- Attr(
    which   = c("title", "#bins", "#borders"),
    default = list(title=name, bins=10, borders=NULL)
  )
  plot.arg  <-  Attr("plot", default=list(xlab=""))
  borders <- arg$borders
  y <- na.omit(recode(y, coerce=TRUE, napp=NA))
  n <- length(y)
  # Assume here that y does not contain any NAs.
  ifnull(borders) <- pretty(y, n=arg$bins)
  ag_setmargins()
  hist.arg <- plot.arg
  hist.arg$x <- y
  hist.arg$main <- list(NULL)
  ifnull(hist.arg) <- list(breaks=borders, labels=borders, freq=TRUE, col='red')
  ifnull(hist.arg, "ylab") <- if (hist.arg$freq) "Frequency" else "Proportion"
  do.call("hist", hist.arg)
  ag_top_label(uncoded, ly1=1, ly2=2)
  title(arg$title, line=3, cex.main=1)
}  ### end 

ag_plot.ag_mixed <- function (x)
{
  GA <- graphattr(x)
  Attr <- function (...) ag_get(GA, ...)
  name <- Attr("data")
  arg <- Attr(
    which  =c("title", "#bins", "#borders"),
    default=list(title=name, bins=10, borders=NULL)
  )
  plot.arg  <-  Attr("plot", default=list(xlab=""))
  borders <- arg$borders
  y <- uncoded <- x[[name]]
  y <- recode(y, apply.na=TRUE, napp=NA)
  y <- na.omit(y)
  n <- length(y)
  # Assume here that y does not contain any NAs.
  has.positive <- any(y>0)
  has.negative <- any(y<0)
  
  mixed.posneg <- has.positive & has.negative
  all.one.sign <- (! mixed.posneg)
  zeros <- (y==0)
  ifnull(borders) <- pretty(y, n=arg$bins)
  bin.width <- (borders[2]- borders[1])
  ag_setmargins()
  hist.arg <- plot.arg
  ifnull(hist.arg) <- list(breaks=borders, labels=borders, freq=TRUE, col='red')
  ifnull(hist.arg, "ylab") <- if (hist.arg$freq) "Frequency" else "Percentage"
  hist.arg$x <- y[!zeros]
  hist.arg$xaxt <- "n"
  hist.arg$yaxt <- "n"
  ifnull(hist.arg, "main") <- ""
  hist0 <- do.call("hist", hist.arg)
  hist0zero <- hist(y, breaks=borders, plot=FALSE)
  counts   <- hist0$counts
  n.counts <- sum(hist0zero$counts)
  y.max    <- max(counts/n.counts)
  y.pretty <- pretty (c(0, y.max))
  y.pretty <- y.pretty[y.pretty<=y.max]
  axis(2, y.pretty*n.counts, 100*y.pretty, mgp=c(1.7, 0.2, 0), par(tcl=-0.3))
  axis(1, at=borders, labels=borders, mgp=c(1.7, 0.2, 0), par(tcl=-0.3))
  n.zeros <- sum(zeros)
  if ( any(zeros) ) {
    if (has.negative & has.positive) {
      polygon(0.1*bin.width*c(-4, 4, 4, -4), c(0, 0, n.zeros, n.zeros), col="black")
    } else {
      polygon(0.1*bin.width*c(-4, 0, 0, -4), c(0, 0, n.zeros, n.zeros), col="black")
    }
  }
  ag_top_label(uncoded, ly1=1, ly2=2)
  title(arg$title, line=3, cex.main=1)
}  ### end 
