# ========================================================================================
# codebook (x, ...)
# ========================================================================================
codebook <- function (x, ...)
{
  UseMethod("codebook")
}
codebook.codebook <- function (x, ...)
{
  return(x)
}
codebook.list <- function (x, ...)
{
  cb <- lapply(x, codebook)
  names(cb) <- names(x)
  return(cb)
}
codebook.default <- function (x, ...)
{
  cb <- attr(x, "codebook")
  if (is.null(cb)) ag_default_codebook(x) else cb
}
codebook.graphbook <- function (x, ...)
{
  if (length(x)<1) return(empty_codebook())
  lapply(x, codebook)
}
codebook.graphbook_item <- function (x, ...)
{
  # (Should) already be a 'codebook' but just in case use as.codebook to ensure
  # that the output object is a codebook.
  #
  # NOTE returns only one codebook, not a list 
  # (although in the future there may be several codebooks in a graphbook item)
  as.codebook(x$code)
}
codebook.graphable <- function (x, ...)
{
  # Returns a list of codebooks.
  #
  gb <- attr(x, "graphbook")
#browser()
  if (is.list(x)) {
    cb <- list()
    for (name in names(x)) {
 ## DEBUG: extracting graphbook items (change routine name)
      CB  <- codebook(gb[name])
      if (length(CB)>0) {
        cb[[name]] <- CB[[1]]
      }
    }
  } else {
    cb <- list(codebook(x))
  }
  # Returns a list of codebooks, named after the (column) names of the data frame or list
  # If the list doesn't have an entry for a column, this means
  # the column wasn't present in the graphbook in the first place!
  # If the column was there in the graphbook but has an 'empty codebook', then
  # it simply doesn't have a codebook.
  return(cb)
}
ag_default_codebook <- function (x)
{
  cb <- lapply(levels(x),
    function (level) {
      list(from=level, to=level, .default=TRUE)
  })
  if (is.null(cb)) cb <- list()
  class(cb) <- "codebook"
  return(cb)
}
print.codebook <- function (x, ...)
{
  if (length(x)==0) {
    cat("Empty codebook\n")
  } else {
    cat("Codebook:\n")
    cat(ag_output_code(x, ...), sep="\n")
  }
}
# ========================================================================================
# testing codebook
# ========================================================================================
is.codebook <- function (x)
{
  inherits(x, "codebook")
}
no.codebook <- function (x)
{
  (!is.codebook(attr(x, "codebook", exact=TRUE)))
}
# ========================================================================================
# coercing to codebook format
# ========================================================================================
as.codebook <- function (x, ...)
{
  UseMethod("as.codebook")
}
as.codebook.codebook <- function (x, ...)
{
  return(x)
}
as.codebook.character <- function (x, split="\n", ...)
{
  if (length(x)==1 && file.exists(x)) {
    x <- readLines(x)
  } else if (any(regexpr(split, x)>0)) {
    x <- unlist(strsplit(x, split))
  }
  CB <- list()
  k <- 1
  for (i in seq(along=x)) {
    L <- ag_parse_one_codebook_line(x[i])
    for (j in seq(along=L)) {
      CB[[k]] <- L[[j]]
      k <- k + 1
    }
  }
  class(CB) <- "codebook"
  return(CB)
}
as.codebook.list <- function (x)
{
  if (length(x)<1) return(empty_codebook())
  namez <- unlist(lapply(x, names))
  ok <- ((!is.null(namez)) && all(namez %in% c("from", "to", "range.from", "range.to", ".default")))
  if (!ok) stop("The list is not in proper codebook format")
  class(x) <- "codebook"
  return(x)
}
as.codebook.NULL <- function (x, ...)
{
  empty_codebook()
}
empty_codebook <- function ()
{
  structure(list(), class="codebook")
}
# ========================================================================================
# codebook<- (x, ..., value)
# ========================================================================================
"codebook<-" <- function(x, check=TRUE, value)
{
  UseMethod('codebook<-')
}
"codebook<-.default" <- function(x, check=TRUE, value)
{
  attr(x, "codebook") <- as.codebook(value)
  return(x)
}
