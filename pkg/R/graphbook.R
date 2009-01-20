# ================================================================================
# reading the graphbook
# ================================================================================
# 
.filterChars <- function (text)
{
  # This unfortunately does not work if there are characters outside this locale...
  gsub("[^[:print:][:space:]]", "", text)
}
.trimstring <- function(x) { 
  # Trims leading & trailing blanks in strings
  gsub('^[[:space:]]+|[[:space:]]+$', '', x)
}
.errorCannotParse <- function (text, lineNo)
{
  cat("SYNTAX ERROR.\n")
  cat(" Line: #", lineNo, ": [", text, "]\n", sep="")
}
.fixrgx <- function (pattern)
{
  pattern <- gsub(" ", "[[:blank:]]", pattern)
  gsub("[\\]d", "[[:digit:]]", pattern)
}
regexmatch <- function (pattern, text, ...)
{
  if (length(text)==0) return(logical(0))
  pattern <- .fixrgx(pattern)
  (regexpr(pattern, text, ...)>0) 
}
regexsub <- function (pattern, ...)
{
  pattern <- .fixrgx(pattern)
  sub(pattern, ...)
}
regexsplit <- function (pattern, text, ...)
{
  pattern <- .fixrgx(pattern)
  if (regexpr(pattern, text)<0) return(character(0))
  escaped.parentheses <- gregexpr("\\\\\\(", pattern)[[1]]
  parentheses <- gregexpr("\\(", pattern)[[1]]
  n.pars <- sum(parentheses>0)-sum(escaped.parentheses>0)
  replacement <- paste("\\", 1:n.pars, sep="", collapse="\n")
  x <- sub(pattern, replacement, text, ...)
  unlist(strsplit(x, "\n"))
}
.mergelist <- function (x)
{
  .names <- unique(unlist(lapply(x, names)))
  y <- list()
  for (n in .names) {
    y[[n]] <- unlist(lapply(x, "[[", n))
  }
  y
}
splitintowords <- function (x)
{
  a <- gsub("(([^\" ]+)|\"([^\"]+)\") *", "\\2\\3\n", x)
  unlist(strsplit(a, "\n"))
}
outputwords <- function (x)
{ 
  quoted <- paste("\"", x, "\"", sep="")
  no.blank <- (regexpr("[[:blank:]]", x)<=0)
  quoted[no.blank] <- x[no.blank]
  return(quoted)
}
d0 <- function (x)
{
  namez <- .trimstring(unlist(strsplit(x[-1], ";")))
  structure(list(namez), names=c(x[1]), header=TRUE)
}
d1 <- function (x)
{
  structure(list(x[-1]), names=x[1])
}
d1e <- function (x)
{
  warning("Warning: unknown entry '", x[1], "'\n")
  print(  structure(list(x[-1]), names=x[1], unknown=TRUE) )
  structure(list(x[-1]), names=x[1], unknown=TRUE)
}
d2 <- function (x)
{
  y <- splitintowords(x[-1])
  structure(list(y), names=x[1])
}
d3 <- function (x)
{
  y <- regexsplit("^ *([^[:blank:]]+) *(.*)$", x[2])
  y <- structure(list(y[2]), names=y[1])
  structure(list(y), names=x[1])
}
ag_one_codebook_line <- function (x)
{
  lhs <- splitintowords(x[1])
  rhs <- .trimstring(x[-1])
  list(code=list(from=lhs, to=rhs))
}
ag_parse_one_codebook_line <- function (x)
{
  ag_one_codebook_line(regexsplit("^ *([^=]+) *= *(.*) *$", x))
}
d5 <- function (x)
{
  list(.blank=NA)
}
d6scale <- function (x)
{
  list(scale=list(score=asNumeric(x[2]), label=x[3]))
}
.PARSER_LIST <- list(
  "^ *(data) +(.*)"=d0,
  "^ *(tag|type|title|label|order|codebook) +(.*)"=d1,
  "^ *(skip|multiple|show.na) +(.*)"=d1,
  "^ *(group) +(.*)"=d2,
  "^ *(na|napp) +(.*)"=d1,
  "^ *(plot) +(.*)"=d3,
  "^ *(scale) +([^[:blank:]]+) +(.*)"=d6scale,
  "^ *([^=]+) *= *(.*)"=ag_one_codebook_line,
  "^ *([[:alnum:]]+) +(.*)"=d1e,
  "^( *)$"=d5
)
.processGraphbookLines <- function (textlines)
{
  L <- as.list(rep(NA, length(textlines)))
  rgx <- names(.PARSER_LIST)
  escaped <- (1+regexpr("\\\\#", textlines))
  escaped[escaped==0] <- Inf
  bang <- regexpr("#", textlines)
  comment <- (bang>0 & bang<escaped)
  bang[!comment] <- (1+nchar(textlines)[!comment])
  txt <- .trimstring(substr(textlines, 1, bang-1))
  cmt <- .trimstring(substr(textlines, bang+1, nchar(textlines)))
  cmtch <- rep(FALSE, length(txt))
  for (i in seq(along=rgx)) {
    mtch <- (regexpr(rgx[i], txt)>0)
    umtch <- (mtch & !cmtch)
    if (any(umtch)) {
      f <- function (x) .PARSER_LIST[[i]](regexsplit(rgx[i], x, ignore.case=TRUE))
      L[umtch] <- lapply(txt[umtch], f)
    }
    cmtch <- (cmtch | mtch)
  }
  for (i in which(nchar(cmt)>0)) {
    comment(L[[i]][[1]]) <- cmt[i]
  }
  cmdline <- (!sapply(L, is.na))
  section.start <- sapply(L, function (x) !is.na(x) && !is.null(attr(x, 'header')))
  # Now adjust the section start so that preceding comments are attached to the section
  k <- 1
  for (i in which(section.start)) {
    if (i>k) {
      s <- i
      for (j in seq(from=i-1, to=k)) {
        if (cmdline[j]) break
        if (comment[j]) s <- j
      }
      if (s==i) next
      section.start[seq(from=s+1, to=i)] <- FALSE
      section.start[s] <- TRUE
      k <- (i+1)
    }
  }
  section.indicator <- factor(cumsum(section.start))
  L <- split(L, section.indicator)
  L <- lapply(L, unlist, recursive=FALSE)
  return(L)
}

.ag_nocollapse <- function (x) return(x)

.ag_collapse <- function (x) unlist(x, recursive=FALSE)

.ag_codecollapse <- function (x)
{
  as.codebook.list(x)
}

cbk.collapse <- function (xname, xlist)
{
  f <- switch(xname,
   "code"=.ag_codecollapse,
   "scale"=.ag_nocollapse,
   "group"=.ag_nocollapse,
   "na"=.ag_nocollapse,
   "napp"=.ag_nocollapse,
   .ag_collapse)
  f(xlist)
}

#
# The following needs to changed so that
# it will be possible to embed several codebooks into
# one graphbook_item (if we have a multivariate plot)
#
.collapseGraphbookLines <- function (L)
{
  A <- L
  for (i in seq(along=A)) {
    data.names <- A[[i]]$data
    for (na in unique(names(A[[i]]))) {
      # Entries such as '.blank' are not collapsed since they
      # represent empty lines which may contain comments
      no.collapse <- (substr(na, 1, 1)==".")
      w <- which(names(A[[i]])==na)
      if (no.collapse) {
        for (j in w) {
          la <- A[[i]][[j]]
          A[[i]][[j]] <- cbk.collapse(na, la)
        }
      } else {
        la <- A[[i]][w]
        first <- w[1]
        rest <- w[-1]
        names(la) <- NULL
        A[[i]][[first]] <- cbk.collapse(na, la)
        A[[i]][rest] <- NULL
      }
    }
    class(A[[i]]) <- "graphbook_item"
  }
  return(A)
}
# ========================================================================================
# get codebook
# ========================================================================================
# ========================================================================================
# error messages
# ========================================================================================
# ========================================================================================
# readGraphbook - read a data file, along with a graphbook
# ========================================================================================
graphbook.character <- function (x)
{
  txt <- readLines(x)
  ag_parse_graphbook(txt)
}
ag_parse_graphbook <- function (txt)
{
  cb <- .processGraphbookLines(txt)
  CB <- .collapseGraphbookLines(cb)
  class(CB) <- c("graphbook")
  return(CB)
}


# ========================================================================================
# graphbook
# ========================================================================================
graphbook <- function (x, ...)
{
  UseMethod("graphbook")
}
graphbook.graphbook <- function (x, ...)
{
  return(x)
}
graphbook.graphable <- function (x, ...)
{
  attr(x, "graphbook")
}
graphbook.default <- function (x)
{
  if (is.null(x)) {
    return(NULL)
  }
  stop("No graphbook() method for objects of class ", class(x))
}
graphbook.list <- function (x)
{
  name <- deparse(substitute(x))
  if (is.null(cb <- attr(x, "graphbook", exact=TRUE))) {
    cb <- new_graphbook(x)
    attr(cb, ".default") <- TRUE
  } else {
    attr(cb, ".default") <- FALSE
  }
  return(cb)
}
graphbook.data.frame <- function (x)
{
  name <- deparse(substitute(x))
  if (is.null(cb <- attr(x, "graphbook", exact=TRUE))) {
    cb <- new_graphbook(x)
    attr(cb, ".default") <- TRUE
  } else {
    attr(cb, ".default") <- FALSE
  }
  return(cb)
}
empty_graphbook <- function (x)
{
  new_graphbook(NULL)
}
new_graphbook <- function (x)
{
  # x : data frame
  cb <- list()
  data.columns <- names(x)
  for (i in seq(along=x)) {
    # argument is a data frame
    cb[[i]] <- ag_default_graphbook_item(x[[i]], name=data.columns[i])
  }
  names(cb) <- data.columns
  class(cb) <- "graphbook"
  return(cb)
}
ag_default_code <- function (x, type)
{
  lev <- levels(x)
  code <- lapply(lev,
    function (level) {
      list(from=level, to=level, .default=TRUE)
  })
  as.codebook(code)
}
ag_default_graphbook_item <- function (x, name=deparse(substitute(x)), type=NULL)
{
  # default is an univariate summary
  # x : a vector, not a list
  #
  type <- ag_default_datatype(x)
  f <- switch(type,
    "continuous"=ag_default_graphbook_item_continuous,
    "unordered-categorical"=ag_default_graphbook_item_unordered_categorical,
    ag_default_graphbook_item_blank
  )
  cbi <- f(x)
  cbi$data <- name
  cbi$type <- type
  cbi$code <- ag_default_code(x, type)
  return(cbi)
}

ag_minimumNumericLevelsToBeDeemedMixture <- 5

ag_default_datatype <- function (x)
{
  x <- recode(x, coerce=TRUE) # Attempt to coerce to number
  if (is.factor(x)) {
    L <- regroupNumeric(x)
    if (length(L$num)>=ag_minimumNumericLevelsToBeDeemedMixture) {
      type <- "mixture"
    } else {
      type <- "unordered-categorical"
    }
  } else {
    type <- ag_datatype_vector(x)
  }
  return(type)
}

ag_standardize_graphbook <- function (x)
{
  # Not used yet
  gb <- graphbook(x)
  default <- new_graphbook(x)
  for (i in seq(along=gb)) {
    item <- gb[[i]]
    if (is.null(data.columns <- item$data)) next
    for (d in data) {
      gb[[i]] <- ag_merge_graphbook_items(item, default)
    }
  }
}

ag_merge_graphbook_items <- function (user.specified, default)
{
  item <- user.specified
  data.columns <- item$data
  user.type <- item$type
  for (i in seq(along=data.columns)) {
    data.name <- item$data[i]
    data.type <- item$type[i]
    if (is.null(data.type) || is.na(data.type)) {
      def.data.type <- default[[data.name]]$type
      item$type[i] <- def.data.type
    }
  }
  return(item)
}
# ========================================================================================
# [.graphbook
# ========================================================================================
"[.graphbook" <- function (x, ix, strict=FALSE)
{
  # Extract graphbook items by 'data' items (RENAME THIS ROUTINE)
  #
  if (is.character(ix)) {
    L <- unlist(lapply(seq(along=x), function (i) {
      data <- make.names(x[[i]]$data)
      structure(rep(i, length(data)), names=data)
    }))
    ix <- L[make.names(ix)]
  }
  if (is.null(ix) || is.na(ix)) return(empty_graphbook())
  cl <- class(x)
  x <- unclass(x)[ix]
  class(x) <- cl
  return(x)
}
# ========================================================================================
# graphattr
# ========================================================================================
graphattr <- function (x, which=NULL,  ...)
{
  ci <- attr(x, "graphattr")
  if (is.null(ci)) {
    return(NULL)
  }
  if (is.null(which)) return(ci) else ag_get(ci, which, ...)
}

ag_get_codebook <- function (x, name)
{
  # extracts the codebook corresponding to the data name 'name' from graphbook_item 'x'
  # Now, we don't have multivariate graphs yet so there are no multiple codebooks
  # within a graphbook_item. Thus return the only one that exists.
  cb <- codebook(x)
  multiple <- is.true(x$multiple, na=FALSE)
  attr(cb, "multiple") <- multiple
  attr(cb, "group") <- x$group
  attr(cb, "na") <- unlist(x$na)
  attr(cb, "napp") <- unlist(x$napp)
  return(cb)
}

"graphattr<-" <- function (x, which=NULL, value)
{
  # DESCRIPTION
  #   Assigns a graphbook_item (value) to a list (x),
  #   with named components matching exactly to those in x$data.
  #   Merges the information with a 'default' graphbook item, ensuring that
  #   all essential information is there.
  #
  if (!is.null(which)) {
    gc <- attr(x, "graphattr")
    gc[[which]] <- value
    attr(x, "graphattr") <- gc
    return(x)
  }
  gc <- value    
  if (!inherits(gc, "graphbook_item")) {
    stop("Can only assign graphbook items to objects")
  }
  xnames <- names(x)
  types <- c(gc$type, rep(NA, length(xnames)))[seq(along=xnames)]
  for (i in seq(along=xnames)) {
    # loop through the data list items 
    name <- xnames[i]
    y <- x[[name]]
    cb <- ag_get_codebook(gc, name)
    codebook(y) <- cb
    if (is.na(types[i])) types[i] <- ag_default_datatype(y)
    x.def <- ag_default_graphbook_item(y)
    x[[name]] <- y
  }
  gc$type <- types
  attr(x, "graphattr") <- gc
  plotclass <- paste("ag", paste(gc$type, collapse="__"), sep="_")
  plotclass <- gsub("-", "_", plotclass)
  class(x) <- unique(c("graphable_item", plotclass, class(x)))
  return(x)
}
# ========================================================================================
# 
# ========================================================================================
# ========================================================================================
# 
# ========================================================================================
is.graphbook <- function (x)
{
  inherits(x, "graphbook")
}

no.graphbook <- function (x)
{
  (!is.graphbook(attr(x, "graphbook", exact=TRUE)))
}
#
# graphbook [ extraction
#
#
# graphbook<- assignment
#
"graphbook<-" <- function(x, check=TRUE, value)
{
   UseMethod('graphbook<-')
}

"graphbook<-.default" <- function(x, check=TRUE, value)
{
  stop("Graphbooks can only be assigned to data frames")
}

ag_as_graphable <- function (x)
{
  if (!("graphable" %in% class(x))) {
    class(x) <- c("graphable", class(x))
  }
  return(x)
}

ag_data_names <- function (x, column.names=TRUE)
{
  if (column.names) {
    lapply(x, function (x) make.names(x[["data"]]))
  } else {
    lapply(x, function (x) x[["data"]])
  }
}

"graphbook<-.data.frame" <- function(x, check=TRUE, value)
{
  cb <- value
  if (is.null(cb)) {
    attr(x, "graphbook") <- NULL
    return(x)
  }
  if (!is.graphbook(cb)) {
    stop("Can only assign graphbooks with graphbook(x) <- ...")
  }
  data.columns <- names(x)
  graph.data.names <- ag_data_names(cb)
  good.graphs <- sapply(graph.data.names,
     function (name) {
       if (is.null(name)) return(TRUE)
       all(name %in% data.columns)
     })
  if (check) {
    gdn <- unlist(graph.data.names)
    unused.graphs <- (!(gdn %in% data.columns))
    if (any(unused.graphs)) {
      txt <- paste(gdn[unused.graphs], collapse=", ")
      warning("The following data vectors do not exist in the data frame: ", txt)
    }
    unused.data <- (!(data.columns %in% gdn))
    if (any(unused.data)) {
      txt <- paste(data.columns[unused.data], collapse=", ")
      cat("Note: the following data columns do not exist in the graphbook: ", txt, "\n")
    }
  }
  if (any(!good.graphs)) {
    cb_attr <- attributes(cb)
    if (!is.null(names(cb))) {
      cb_attr$names <- cb_attr$names[good.graphs]
    }
    cb <- cb[good.graphs]
    attributes(cb) <- cb_attr
  }
  attr(x, "graphbook") <- cb
  ag_as_graphable(x)
}

# ================================================================================
# default_graphbook
# ================================================================================
#ag_reorder_list <- function (x, order=NULL)
#{
#  if (is.null(order) || is.null(names(x))) return(x)
#}
ag_default_graphbook_item_blank <- function (x)
{
  structure(list(), class="graphbook_item")
}

ag_default_graphbook_item_continuous <- function (x)
{
  cb <- ag_default_graphbook_item_blank()
  #Order <- c("nbins", "borders", "plot")
  return(cb)
}

ag_default_graphbook_item_unordered_categorical <- function (x)
{
  cb <- ag_default_graphbook_item_blank()
  return(cb)
}
#========================================================================
# splicing graphbooks
#========================================================================
"[.graphable" <- function (x, ...)
{
  gb <- graphbook(x)
  cx <- class(x)
  class(x) <- cx[cx!="graphable"]
  x <- x[...]
  graphbook(x, check=FALSE) <- gb
  return(x)
}
# end of graphbook.R
