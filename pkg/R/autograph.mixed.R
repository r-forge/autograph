## mixtures
ag_mixed_table <- function (x, grouping=rep(0, length(x)))
{
  # x : vector
  groups <- sort(unique(grouping))
  tables <- as.list(rep(NA, length=length(groups)))
  ord <- ag_bargraph_ordering(x)
  ordnum <- match(ord$order, c("freq", "value", "graphbook", "label"))
  data <- NULL
  colors <- NULL
  breaks <- NULL
  borders <- NULL
  labels <- NULL
  has.numeric <- FALSE
  for (i in seq(along=groups)) {
    ind <- (grouping==groups[i])
    y <- x[ind]
    # We use as.character since we want to get rid of the categories that have zero obs's.
    if (all(isNumber(as.character(y)))) {
      y      <- asNumeric(y)  ### These are supposed to be numbers, quoted.
      hist.y <- hist(y, plot=FALSE)
      y      <- hist.y$counts
      n      <- length(y)
      y.breaks <- hist.y$breaks
      y.labels <- as.character(y.breaks)
      names(y) <- rep("", n)
      has.numeric <- TRUE
    } else {
      y <- table(as.character(y)) # DEBUG: kluge.
      n <- length(y)
      if (ordnum==1) {
        y.order <- order(y) # Order given as indices.
      } else if (ordnum==2) { # value order
        labels <- unique(names(cl[order(tolower(cl))])) # Order given as vector of names.
        y.order <- labels[ labels %in% names(y) ]
      } else if (ordnum==3) { # Graphbook order
        labels <- unique(names(cl))   # Order given as vector of names.
        y.order <- labels[ labels %in% names(y) ]
      } else if (ordnum==4) { # label order
        y.order <- order(tolower(names(y)))
      }
      if (!ord$ascending) y.order <- rev(y.order)
      y <- y[y.order]
      y.breaks <- NULL
      y.labels <- names(y)
    }
    j <- length(data)
    data   <- if (is.null(data)) y else c(data,0,y)
    colors <- if (is.null(colors)) rep("red",n) else c(colors,"white",rep("red",n))
    borders <- if (is.null(borders)) rep("black",n) else c(borders,"white",rep("black",n))
    labels <- if (is.null(labels)) y.labels else c(labels,"", y.labels)
    breaks[[i]] <- list(where=seq(from=j, to=length(data)), labels=y.breaks)
  }
  borders[data==0] <- "white"
  list(data=data,
    colors=colors,
    borders=borders,
    labels=labels,
    breaks=breaks,
    has.numeric=has.numeric)
}
ag_plot.ag_mixture <- function (x)
{
  GA <- graphattr(x)
  Attr <- function (...) ag_get(GA, ...)
  name <- Attr("data")
  arg <- Attr(
    which  =c("title", "#bins", "#borders"),
    default=list(title=name, bins=10, borders=NULL)
  )
  plot.arg  <-  Attr("plot", default=list(xlab=""))
  y <- uncoded <- x[[name]]
  grouping <- ag_grouping(y)
  y <- recode(y, apply.na=TRUE, napp=NA)
  tbl <- ag_grouped_table(y, grouping)
  table0  <- tbl$data
  colors  <- tbl$colors
  borders <- tbl$borders
  ## DEBUG: decide how big to make the labels & whether they should be horizontal or vertical
  label.expansion <- 1
  las.value <- 1
  if (length(table0)>3) {
    las.value <- 3
    name <- ""
    if (length(table0)>8) label.expansion <- 0.7
  }
  n <- sum(table0)
  names.arg <- names(table0)
  ## .adj <- (-1+1.e-5) * (table0 != 0)
  adj.table <- (table0)*100/n
  breaks  <- tbl$breaks
  has.a.numeric.group <- tbl$has.numeric
  # space=0 is necessary when we have numeric groups;
  #         otherwise the numeric labels won't be properly aligned
  space <- if (has.a.numeric.group) 0 else 0.2
  ag_setmargins(las.value, names.arg)
  plot.arg$height <- adj.table
  ifnull(plot.arg) <- list(space=space, ylab="Percentage", col=colors, names.arg=names.arg,
    las=las.value, cex.lab=1, cex.axis=1, cex.names=label.expansion)
  do.call("barplot", plot.arg)
  ag_top_label(uncoded, ly1=1, ly2=2)
  title(arg$title, line=3, cex.main=1)
  if (has.a.numeric.group) {
    for (i in seq(along=breaks)) {
      break0 <- breaks[[i]]
      if (is.null(break0$labels)) next
      axis(1, at=break0$where, labels=break0$labels, line=1) 
##      for (count in seq(along=break0$labels)) {
##        mtext(break0$labels[count], side=1, at=break0$where[i], line=1, cex=0.8)
##      }
    }
  }
  ## axis(1, at=1:length(breaks), labels=breaks))
  invisible(NULL)
} ### end of function 
ag_plot.ag_logscale_continuous <- function (x)
{
  GA <- graphattr(x)
  Attr <- function (...) ag_get(GA, ...)
  name <- Attr("data")
  arg <- Attr(
    which   = c("label", "#bins", "#borders"),
    default = list(label=name, bins=10, borders=NULL)
  )
  plot.arg  <-  Attr("plot", default=list(xlab=""))
  borders <- arg$borders
  y <- uncoded <- x[[name]]
  y <- recode(y, apply.na=TRUE, napp=NA, coerce=TRUE)
  y <- na.omit(y)
  n <- length(y)
  if (is.factor(y)) {
    print(y)
    stop("Could not coerce y to numeric")
  }
  y <- log(y)
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
  title(arg$label, line=3, cex.main=1)
  invisible(NULL)
}  ### end 
