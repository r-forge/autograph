# ================================================================================
# routines common to all plots
# ================================================================================
ag_top_label <- function(x, ly1=0.7, ly2=1.4)
{
  cex <- 0.75
  prop_applicable <- mean(is.applicable(x))
  perc_applicable <- round(100*prop_applicable, 2)
  mtext(paste("Applicable to ", perc_applicable, "% of the cases",sep=""), line=ly1, cex=cex)
  prop_observed_applicable <- mean(is.observed(applicable(x)))
  perc_observed_applicable <- round(100*prop_observed_applicable, 2)
  mtext(paste(perc_observed_applicable,"% of applicable cases were observed",sep=""),line=ly2, cex=cex)
  invisible()
}
# ================================================================================
# set margins of a plot
# ================================================================================
ag_setmargins <- function (las=1, names.arg=NULL)
{
  mar <- c(5,4,4,2)
  if (!is.null(names.arg)) {
    if (las==3) {
      maxchar <- max(sapply(names.arg, nchar))
      mar[1] <- min(10, trunc(mar[1] + maxchar/4))
    }
  }
  par(mar=0.1+mar)
}
ag_defaultmargins <- function ()
{
  par(mar=0.1+c(10,5,5,2), oma=0.1+c(0,2,6,2))
}

# ================================================================================
# preparing the barplot
# ================================================================================
#
ag_trim_string <- function(x) { 
  # Trims leading & trailing blanks in a string
  gsub('^[[:space:]]+|[[:space:]]+$', '', x)
}
ag_bargraph_ordering <- function (x)
{
  ord <- list(order="label", ascending=FALSE)
  my_order <- graphattr(x, "order", default="label") # DEBUG
  if (is.character(my_order)) {
    my_order.byValue <- (regexpr("value", my_order)>0)
    my_order.byLabel <- (regexpr("label", my_order)>0)
    my_order.ascending <- (regexpr("ascend", my_order)>0)
    #
    if (my_order.byValue) ord$order <- "value"
    if (my_order.byLabel) ord$order <- "label"
    ord$ascending <- (ord$order!="freq")
    if (my_order.ascending) order.ascending <- TRUE
  }
  return(ord)
}
ag_merge_values <- function(x)
{
  y <- NULL
  for (i in seq(along=x)) {
    if (!(x[i] %in% y)) y <- c(y, x[i])
  }
  return(y)
}
ag_grouped_table <- function (x, grouping=rep(0, length(x)))
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
