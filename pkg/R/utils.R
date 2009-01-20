# ========================================================================================
# autograph utilities
# ========================================================================================
asNumeric <- function (x)
{
  if (is.factor(x)) x <- as.character(x)
  ow <- getOption("warn")
  on.exit(options(warn=ow))
  options(warn=-1)
  as.numeric(x)
}
isNumeric <- function (x)
{
  # is_numeric - true if a vector component is a number, NaN or NA
  #
  a <- asNumeric(x) # Non-numbers are turned into NAs.
  return((!is.na(a)) | is.nan(a) | is.na(x))
}
isNumber <- function (x)
{
  # isNumber - true if a vector component is a number but not NaN or NA
  # ARGUMENTS
  a <- asNumeric(x) # Non-numbers are turned into NAs.
  (!is.na(a))
}
####
.splitnames <- function (x)
{
  # decompose a vector of groups (values are unique group names,
  # names are a comma-separated strings of values.
  # e.g. if x is
  #    structure(c("1","2"), names=c("a,b", "c,d,e"))
  # then this returns
  #   structure(c("a", "b", "c", "d", "e"), names=c("1", "1", "2", "2", "2"))
  y <- c()
  nx <- names(x)
  for (i in seq(along=x)) {
      a <- strsplit(nx[i], "[, ]+")[[1]]
      y <- c(y, structure(rep(x[i], length(a)), names=a))
  }
  y
}
.has.extension <- function(x, ext)
{
  # True if the string x ends in user-specified filename extension
  rgx <- paste('\\.',ext,'$',sep='')
  (regexpr(rgx, x)>0)
}
is.true <- function (x, na=NA)
{
  #
  # translate various outputs to TRUE/FALSE
  #
  a <- as.logical(x)
  if (length(a)<1) return(na)
  if (!is.na(a)) return(a)
  x <- toupper(paste(x))
  if (x %in% c("YES", "TRUE", "Y", "T"))
     return(TRUE)
  else if (x %in% c("NO", "FALSE", "N", "F"))
     return(FALSE)
  else return(na)
}
"ifnull<-" <- function (x, which, value)
{
  if (missing(which)) {
    namess <- names(value)
    if (is.list(value) && length(namess)>0) {
      for (name in namess) {
        if (is.null(x[[name]])) x[[name]] <- value[[name]]
      }
    } else {
      if (is.null(x)) x <- value
    }
  } else {
    if (is.null(x[[which]])) x[[which]] <- value
  }
  return(x)
}
