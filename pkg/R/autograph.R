# ========================================================================================
# autograph 
# ========================================================================================
autograph <- function (x, ...)
{
  UseMethod('autograph')
}

ag <- autograph

autograph.default <- function (x, name=deparse(substitute(x)), ...)
{
  x <- as.data.frame(x)
  cb <- graphbook(x)
  autograph(cb, name=name, data=x, ...)
}

autograph.character <- function (x, name=deparse(substitute(x)), ...)
{
  
  autograph(x, name=name, ...)
}
autograph.data.frame <- function (x, name=deparse(substitute(x)), ...)
{
  cb <- graphbook(x)
  autograph(cb, name=name, data=x, ...)
}
autograph.list <- function (x, name=deparse(substitute(x)), ...)
{
  cb <- graphbook(x)
  autograph(cb, name=name, data=x, ...)
}
autograph.graphable <- function (x, name=deparse(substitute(x)), ...)
{
  cb <- graphbook(x)
  autograph(cb, name=name, data=x, ...)
}
autograph.matrix <- function (x, name=deparse(substitute(x)), ...)
{
  x <- as.data.frame(x)
  cb <- graphbook(x)
  autograph(cb, name=name, data=x, ...)
}
