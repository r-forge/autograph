# ================================================================================
# discrete and ordered categorical plots
# ================================================================================
# Types: 
# 1. discrete: 
#   - equally spaced (bounds specified)
#   - upper and lower bounds
#   - may have gaps
#   E.g. "# of previous classes" (0, 1, 2, 3, 1.5, ...)
#   E.g. "# of siblings"
#   E.g. "2,3,5,6,8"... => _||_||_|
#
# 2. ordered categorical
#   - unequally spaced numbers
#   - does not have gaps
# 
#   
# Options:
#   label
#   plot.xlab
#   bins
#   borders
#   show.na
#   scale
ag_plot.ag_discrete <- function (x)
{
  GA <- graphattr(x)
  Attr <- function (...) ag_get(GA, ...)
  name <- Attr("data")
  y <- uncoded <- x[[name]]
  arg <- Attr(
    which  =c("title", "#bins", "#borders"),
    default=list(title=name, bins=10, borders=NULL)
  )
  plot.arg  <-  Attr("plot", default=list(xlab=""))
  scale <- Attr("scale", default=NULL)
  y <- recode(y, apply.na=TRUE, napp=NA, coerce=TRUE)
  if (is.factor(y)) {
    print(y)
    stop("Could not coerce factor to numeric!?")
  }
  y <- na.omit(y)
  n <- length(y)
  if (length(scale)>0) {
    ifnull(names(scale)) <- paste(seq(along=scale))
    g <- function (x, a) if (is.null(b <- x[[a]])) return(NA) else return(b)
    scale.values  <- asNumeric(sapply(scale, g, "score"))
    scale.names  <- sapply(scale, g, "label")
    ord.scale.values <- order(scale.values)[c(1,length(scale.values))]
    slowval <- min(scale.values, na.rm=TRUE)
    suppval <- if (length(na.omit(scale.values))>1) max(scale.values, na.rm=TRUE) else NA
    slow    <- scale.names[ ord.scale.values[1] ]
    supp    <- scale.names[ ord.scale.values[2] ]
  } else {
    slowval <- NA
    suppval <- NA
  }
  # Assume here that y does not contain any NAs.
  lower.boundary.specified <- !is.na(slowval)
  upper.boundary.specified <- !is.na(suppval)
  .from <- if (lower.boundary.specified) slowval else min(y)
  .to   <- if (upper.boundary.specified) suppval else max(y)
  ## DEBUG  here check that we don't have too wide a range.
  table0 <- NULL
  breaks <- seq(from=.from, to=.to)
  for (i in breaks) {
    table0 <- c(table0, sum(y==i))
  }
  names(table0) <- as.character(breaks)
  ## .adj <- (-1+1.e-5) * (table0 !=0)
  ag_setmargins()
  plot.arg$height <- (table0*100/n)
  ifnull(plot.arg) <- list(col="red", ylab="Percentage")
  do.call("barplot", plot.arg)
  usr <- par("usr")
  .cex <- if (lower.boundary.specified & upper.boundary.specified) 0.8 else 0.9
  if (lower.boundary.specified) {
    .x1 <- (slowval-0.8)
    mtext(slow, cex=.cex, side=1, line=2, at=.x1, adj=0)
  }
  if (upper.boundary.specified) {
    .x2 <- usr[2]
    mtext(supp, cex=.cex, side=1, line=2, at=.x2, adj=1)
  }
  title(arg$title, line=3, cex.main=1)
  ag_top_label(uncoded, ly1=1, ly2=2)
}  ### end
