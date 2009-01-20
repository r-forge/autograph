#========================================================================
# outputting
#========================================================================
ag_output <- function (x, ...)
{
  UseMethod("ag_output")
}
ag_output.default <- function (x, ...)
{
  print(x)
  stop("No method for x of class ", class(x))
}
ag_output_blank <- function (x, ...) {
  if (is.null(co <- comment(x))) {
    return("")
  } else {
    paste("#", co, collapse=" ")
  }
}
ag_output_header <- function (x, name) {
  ag_output_default(x, name)
}
ag_output_default <- function (x, name) {
  txt <- paste(x, collapse=" ") # Just in case, but x should be of length 1. 
  if (is.null(co <- comment(x))) {
    paste(name, txt, sep=" ", collapse=" ")
  } else {
    paste(name, txt, paste("#", co, sep=" "), sep=" ", collapse=" ")
  }
}
ag_output_singleliners <- function (x, name) {
  unlist(lapply(x, function (y) {
    ag_output_default(y, name)
  }))
}
ag_output_code <- function (x, ...) {
  txt <- rep(NA, length(x))
  indent <- " "
 # DEBUG: check that the mapping is onto
  for (i in seq(along=x)) {
    item <- x[[i]]
    from <- item$from
    to   <- item$to[1]
    if (length(item$to)>1) warning("Code mapping is invalid")
    from.txt <- paste(outputwords(from), collapse=" ")
    to.txt <- paste(to)
    txt[i] <- paste(indent, paste(from.txt, to.txt, sep=" = "), sep="")
    if (!is.null(co <- comment(item))) {
      txt[i] <- paste(txt[i], co, sep=" # ")
    }
  }
  return(txt)
}
ag_output_scale <- function (x, ...) {
  txt <- rep(NA, length(x))
  name <- "scale"
  for (i in seq(along=x)) {
    item <- x[[i]]
    a.txt <- paste(item$score)
    b.txt <- paste(item$label)
    txt[i] <- paste(name, " ", paste(a.txt, b.txt, sep=" "), sep="")
    if (!is.null(co <- comment(item))) {
      txt[i] <- paste(txt[i], co, sep=" # ")
    }
  }
  return(txt)
}
ag_output_group <- function (x, ...) {
  txt <- rep(NA, length(x))
  name <- "group"
  for (i in seq(along=x)) {
    item <- x[[i]]
    txt[i] <- paste(name, " ", paste(outputwords(item), collapse=" "), sep="")
    if (!is.null(co <- comment(item))) {
      txt[i] <- paste(txt[i], co, sep=" # ")
    }
  }
  return(txt)
}
ag_output.graphbook_item <- function (x)
{
  header <- "data"
  da <- (names(x)==header)
  hdr <- NULL
  if (!any(da)) {
    warning("Bad graphbook format: no 'data' item found")
  } else {
    y <- x[da]
    hdr <- ag_output_header(unlist(y), header)
    x <- x[-which(da)]
  }
  na <- names(x)
  txt <- sapply(seq(along=x),
    function (i) {
      name <- na[i]
      value <- x[[i]]
      f <- switch(name,
        ".blank"=ag_output_blank,
        "scale"=ag_output_scale,
        "group"=ag_output_group,
        "na"=ag_output_singleliners,
        "napp"=ag_output_singleliners,
        "code"=ag_output_code,
        ag_output_default
      )
      f(value, name=name)
    })
  c(hdr, txt, "")
}
summary.graphbook <- function (x, numbers=FALSE, ...)
{ 
  txtlist <- lapply(x, ag_output)
  if (numbers) {
    txt <- unlist(lapply(seq(along=txtlist), function (i) {
      c(paste("#", i), txtlist[[i]])
    }))
  } else {
    txt <- unlist(txtlist)
  }
  return(txt)
}
print.graphbook_item <- function (x, ...)
{
  cat("graphbook_item:\n")
  a <- ag_output(x)
  cat(unlist(ag_output(x)), sep="\n")
}
print.graphbook <- function (x, ...)
{ 
  if (length(x)<1) {
    txt <- "Empty Graphbook"
  } else {
    txt <- summary(x, ...)
    cat("Graphbook:\n")
  }
  cat(txt, sep="\n")
}
write.graphbook <- function (x, file="", numbers=FALSE, ...)
{ 
  txt <- summary(x, numbers=numbers, ...)
  cat(txt, sep="\n", file=file)
}
