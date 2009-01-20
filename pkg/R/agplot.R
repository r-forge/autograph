# ================================================================================
# ag_plot
# ================================================================================
ag_plot <- function (x)
{
  UseMethod("ag_plot")
}
ag_plot.default <- function (x)
{
  print(x)
  stop("Unknown data type:", x$type)
}
