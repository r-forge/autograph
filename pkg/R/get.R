# ================================================================================
# ag_get - retrieve attributes 
# ================================================================================
# The graphbook of an object is embedded in the attribute 'graphbook'.
#
# ag_get(x, "#option", default) : coerces the string into a numeric vector
#                                 "1,2,3" will become c(1,2,3), "1, 2 3" will also.
# ag_get(x, "?option", default) : coerces the string into a logical value;
#                                 ("yes", "y", "true", "t") is TRUE.
#                                 ("no", "n", "false", "f") is FALSE.
#                                 Otherwise NA.
# ag_get(x, "!option", default) : coerces the string into a TRUE or FALSE value
#                                 if option is not one of the TRUE values, returns FALSE.
#
ag_get <- function (x, which, default=NULL)
{
  # x : graphbook (list)
  # Extract an option from a list
  #
  # "#" numeric vector
  # "?" logical, true, false, or NA
  # "!" logical, true iff 'which' is contained in yes, true, y, t.
  if (is.null(x)) return(NULL)
  single <- (length(which)<=1)
  A <- list()
###if (any(regexpr("bord", which)>0)) browser()
  for (i in seq(along=which)) {
    w <- which[i] 
    if ((y <- match(substr(w, 1, 1), c("#", "?", "!"), nomatch=0))) {
      w <- substr(w, 2, nchar(w))
    }
    a <- x[[w]]
    if (is.null(a)) {
      a <- if (single) default else default[[w]]
    } ##else if (is.null(names(a))) {
      ##a <- unlist(x[w]) # preserves the name (=which)
      ##}
    if (y==1) {
      if (is.character(a)) {
        a <- unlist(lapply(strsplit(a, "[, ]+"), asNumeric))
      }
    } else if (y==2) {
      a <- is.true(a, na=NA)
    } else if (y==3) {
      a <- is.true(a, na=FALSE)
    }
    if (single) return(a)
    A[[w]] <- a 
  }
  return(A)
}
