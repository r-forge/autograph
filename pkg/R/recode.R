# ========================================================================================
# readcode - read a data file, along with a graphbook
# ========================================================================================
.has.extension <- function(x, ext)
{
  # True if the string x ends in user-specified filename extension
  rgx <- paste('\\.',ext,'$',sep='')
  (regexpr(rgx, x)>0)
}
read.gb <- function(x, graphbook.source=NULL, header=TRUE, ...)
{
  if (.has.extension(x,'csv')) {
    df <- read.csv(x, sep=",", header=header, ...)
  } else if (.has.extension(x, 'dta')) {
    require("foreign")
    df <- read.dta(x, ...)
  } else if (.has.extension(x, 'txt')) {
    df <- read.table(x, header=header, ...)
  }
  graphbook.name <- if (!is.character(graphbook.source)) deparse(substitute(graphbook.source)) else graphbook.source
  cb <- graphbook(graphbook.source)
  if (is.null(cb)) {
    if (file.exists(codefile <- paste(x, "txt", sep="."))) {
      cb <- read.graphbook(codefile)
    } else {
      cb <- graphbook(df) # default
    }
  }
  attr(cb, "graphbook.name") <- graphbook.name
  attr(cb, "datafile") <- x
  graphbook(df) <- cb
  class(df) <- c("graphable", class(df))
  return(df)
}
# graphbook.R

#####################################################################################
#
# recode etc. 
#
#####################################################################################
ag_level_split <- function (x, sep=",")
{
  # .levelsplit - split factor levels into "real" levels
  # if the factors are indeed "multiple" factors encoded into one (by commas)
  #
  if (!is.factor(x)) stop("x must be a factor")
  lev <- levels(x)
  splitlev <- strsplit(lev, split=sep)
  empty.strings <- (sapply(splitlev, length)==0)
  splitlev[empty.strings] <- ""
  names(splitlev) <- lev
  return(splitlev)
}
ag_expand_multiple_levels <- function (x)
{
  if (!is.factor(x)) stop("x must be a factor")
  cb <- codebook(x)
  splitlev <- ag_level_split(x)
  if (is.null(splitlev)) return(NULL)
  splitlev <- lapply(splitlev, ag_trim_string)
  # NOTE Could be more efficient...
  a <- lapply(as.list(as.character(x)), function (x) {
    if (is.na(x)) return(x) else splitlev[[x]]
  })
  lengths <- sapply(a, length)
  all.choices <- unlist(a)
  b <- as.factor(all.choices)
  attr(cb, "lengths") <- lengths
  attr(cb, "multiple") <- FALSE
  codebook(b) <- cb
  return(b)
}
is.multiple_factor <- function (x)
{
  cb <- codebook(x)
  if (is.null(mu <- attr(cb, "multiple"))) return(FALSE) else return(is.true(mu))
}
ag_category_grouping <- function (x)
{
  cb <- codebook(x)
  if (is.null(groups <- attr(cb, "group"))) {
     L <- regroupNumeric(x)     
     return(NULL)
  }
  unlist(lapply(seq(along=groups), function (i) structure(rep(i, length(groups[[i]])), names=groups[[i]])))
}
ag_grouping <- function (x)
{
  groupmap <- ag_category_grouping(x)
  cx <- as.character(x)
  if (length(groupmap)<1) {
    group.ids <- rep(0, length(cx))
  } else {
    group.ids <- groupmap[cx]
    group.ids[is.na(group.ids)] <- groupmap["*"]  # May be again an NA
    group.ids[is.na(group.ids)] <- Inf
  }
  return(group.ids)
}
ag_expand_factor <- function (x)
{
  if (!is.factor(x)) return(x)
  if (is.multiple_factor(x)) {
    x <- ag_expand_multiple_levels(x)
  }
  return(x)
}
ag_make_code_converter <- function (cb)
{
  if (is.null(cb)) return(function (x) x)
  codemap <- unclass(cb)
  # DEBUG: Expand this later so 'code' is a list of 
  # codebooks for different vectors in the data frame x
  # (if x is not a df then there's only one codebook)
  #
  map <- lapply(codemap, function (y) {
    if (is.null(from <- y$from)) return(NULL)
    if (is.null(to <- y$to[1])) to <- NA
    map <- rep(to, length(from))
    names(map) <- from
    return(map)
  })
  map <- unlist(map)
  fmap <- lapply(codemap, function (y) {
    range.from <- y$range.from
    range.to <- y$range.to
    if (is.null(range.from) && is.null(range.to)) return(NULL)
    if (is.null(range.from)) range.from <- (-Inf)
    if (is.null(range.to)) range.to <- (+Inf)
    if (is.null(to <- y$to)) to <- NA
    function (x) {
      w <- (x>=range.from & x<=range.to)
      list(which=w, to=to)
    }
  })
  fmap[sapply(fmap, is.null)] <- NULL
  mapping <- function (x) {
    if (length(map)>0) {
      convert <- (x %in% names(map))
      rest <- x[!convert]
      x[convert] <- map[x[convert]]
    } else {
      convert <- rep(FALSE, length(x))
    }
    if (length(fmap)<1) return(x)
    y <- asNumeric(x)
    convert <- (convert | is.na(y))
    for (f in fmap) {
      if (all(convert)) break
      z <- f(y[!convert]) # use 'y' since we compare only numbers
      convert.this <- z$which # length of sum(!convert)...
      if (any(convert.this)) {
        x[!convert][convert.this] <- z$to
        convert[!convert][convert.this] <- TRUE
      }
    }
    return(x)
  }
  return(mapping)
}
recode <- function (x, apply.na=TRUE, coerce=TRUE, napp="NaN", ...)
{
  UseMethod("recode")
}
recode.list <- function (x, ...)
{
  L <- list()
  for (i in seq(along=x)) {
    data <- x[[i]]
#browser()
    CB <- codebook(x[i])
    if (length(CB)>0) {
      codebook(data) <- CB[[1]]
    }
    L[[i]] <- recode(data, ...)
  }
  names(L) <- names(x)
  return(L)
}
recode.data.frame <- function (x, ...)
{
  L <- recode.list(x, ...)
  lgths <- sapply(L, length)
  if (all(lgths==lgths[1])) {
    as.data.frame(L)
  } else return(L)
}
recode.default <- function (x, apply.na=TRUE, coerce=TRUE, napp="NaN", ...)
{
  # Assume that 'x' is a factor that does NOT have 'multiple' levels within the levels.
  # (e.g. no levels such as 'a,b,c' and 'a,b')
  # na_collapse : collapse "missing" categories into "NA"
  # napp_collapse : collapse "not applicable" categories into "NaN"
  # na_impute : impute 'NA's whenever a value is 'missing'
  if (!is.factor(x)) {
    if (is.character(x) || is.numeric(x)) {
      x <- factor(x)
      if (is.numeric(x)) coerce <- TRUE
    } else {
      return(x)
    }
  }
  convert <- ag_make_code_converter(codebook(x))
  if (is.multiple_factor(x)) {
    x <- ag_expand_multiple_levels(x)
  }
  new_levels <- convert(levels(x))
  if (apply.na) {
    na_lev <- attr(codebook(x), "na")
    napp_lev <- attr(codebook(x), "napp")
    new_levels <- na_levels(new_levels, na_levels=na_lev, napp_levels=napp_lev, napp=napp, ...)
  }
  levels(x) <- new_levels
  if (coerce && all(isNumeric(levels(x)))) {
    return(asNumeric(x))
  }
  return(x)
}
nalevels <- function () {
}
na_levels <- function (x, na_levels = NULL, napp_levels = NULL, napp="NaN")
{
  if (is.null(na_levels)) na_levels <- c("NA", "N/A", "missing")
  if (is.null(napp_levels)) napp_levels <- c("NaN", "NAPP", "N/APP", "not applicable")
  x[x%in%na_levels]   <- NA
  x[x%in%napp_levels] <- napp
  return(x)
}
regroupNumeric <- function (x)
{
  # Assume that 'x' is a factor that does NOT have 'multiple' levels within the levels.
  # (e.g. no levels such as 'a,b,c' and 'a,b')
  # characters will be coerced to vectors
  #
  if (is.character(x)) {
    x <- factor(x)
  }
  if (!is.factor(x)) stop("Need a factor")
  num <- isNumber(x)
  nas <- is.na(x)
  numbers <- asNumeric(x[num])
  miss    <- asNumeric(x[nas])
  lx <- levels(x)
  lx[isNumeric(lx)] <- NA
  levels(x) <- lx
  rest <- x[!is.na(x)]
  list(num = numbers, nas = miss, rest = rest)
}
