# ================================================================================
# ag_plot.ag_unordered_categorical
# ================================================================================
# Options:
#   label
#   plot.xlab
#   bins
#   borders
#   show.na
#   scale
#   order = value/code | label [ascending descending]
#
ag_plot.ag_unordered_categorical <- function (x)
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
