# ================================================================================
# ag_plot.ag_ordered_categorical
# ================================================================================
ag_plot.ag_ordered_categorical <- function (x)
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
  scale <- as.list( ag_get(cb, "scale", default=NULL) )
  y <- recode(y, apply.na=TRUE, napp=NA, coerce=TRUE)
  # May be 
  y <- na.omit(y)
  n <- length(y)
  ifnull(names(scale)) <- paste(seq(along=scale))
  scale <- unlist(scale)
  if (length(scale)>0) {
    if (is.numeric(y)) {
      scale.values <- asNumeric(scale)
      scale.names <- names(scale)
      slowval <- min(scale.values)
      suppval <- max(scale.values)
      slow    <- scale.names[ scales==slowval ]
      supp    <- scale.names[ scales==suppval ]
    } else if (is.factor(y)) {
      scale.values <- asNumeric(names(scale))
      ord.values <- order(scale.values)
      scale.names <- as.character(scale)
      slowval <- min(scale.values)
      suppval <- max(scale.values)
      slow    <- scale.names[ ord.values[1] ]
      supp    <- scale.names[ ord.values[length(scale.names)] ]
    } else stop("Can't do non-factor non-numeric ordered categorical variables yet.")
  } else {
    slowval <- NA
    suppval <- NA
  }
  # Assume here that y does not contain any NAs.
  lower.boundary.specified <- !is.na(slowval)
  upper.boundary.specified <- ! is.na(suppval)
  .from <- if (lower.boundary.specified) slowval else min(y)
  .to   <- if (upper.boundary.specified) suppval else max(y)
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
  .cex <- if (lower.boundary.specified & upper.boundary.specified) 0.6 else 0.8
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
} ### end ag_plot.ag_ordered_categorical
