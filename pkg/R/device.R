# ================================================================================
# 
# ================================================================================
#
# new.screen
#
ag_device <- function (device="screen", paper.size="letter", filename=NULL)
{
  if (device=="screen") {
    device <- getOption("device")
  }
  device.abbrev <- list("ps"="postscript")
  if (!is.null(actual.device.name <- device.abbrev[[device]])) {
    device <- actual.device.name
  }
  papersize <- list("letter"=c(8.5,11), "a4"=c(8.27,11.69))
  if (is.character(paper.size)) {
    papersize.name <- paper.size
    size.inches <- papersize[[tolower(papersize.name)]]
    if (is.null(size.inches)) stop("Cannot set paper size to", paper.size)
  } else if (is.numeric(paper.size)) {
    size.inches <- paper.size
    papersize.name <- "custom paper size"
  } else {
    stop("Cannot understand given paper size: ", paper.size)
  }
  agdevice <- list(device=device,
    papersize.inches=size.inches,
    papersize.name=papersize.name,
    filename=filename,
    save.file=FALSE,
    screen=FALSE
  )
  nonscreen <- c("postscript", "pdf")
  agdevice$screen <- (!(device %in% nonscreen))
  if (!agdevice$screen) {
      if (is.null(filename)) filename <- "ag"
      if (device=="postscript") {
        if (!.has.extension(filename, "ps")) filename <- paste(filename, "ps", sep=".")
        postscript(filename, horizontal=FALSE, onefile=TRUE)
      } else if (device=="pdf") {
        if (!.has.extension(filename, "pdf")) filename <- paste(filename, "pdf", sep=".")
        pdf(file=filename, width=size.inches[1], height=size.inches[2],
            onefile=TRUE)
      }
      cat("Saving", device,"file",filename,"... ")
      agdevice$dev.cur <- dev.cur()
  }
  class(agdevice) <- "agdevice"
  agdevice  
}
ag_new_page <- function (agdevice) 
{
  UseMethod("ag_new_page")
}
ag_new_page.agdevice <- function (agdevice)
{
  device <- agdevice$device
  size   <- agdevice$papersize.inches
  if (agdevice$screen) {
    do.call(device, list(width=size[1], height=size[2]))
  }
}
ag_close_device <- function (agdevice) 
{
  UseMethod("ag_close_device")
}
ag_close_device.agdevice <- function (agdevice)
{
  if (!is.null(agdevice$dev.cur)) {
    dev.off(agdevice$dev.cur)
    cat("done.\n")
  }
}
#
# gridxy : 
#
gridxy <- function (n) {
  ymax <- ceiling(sqrt(trunc(n)))
  xmax <- ceiling(n/ymax)
  c(xmax, ymax)
}
#
# ag_graphable_item
#
autograph.graphable_item <- function(x, data=.GlobalEnv, name=deparse(substitute(x)), ...)
{
  X <- ag_graphable_item(x, data=data)
  if (!is.null(X)) {
    ag_plot(X, name=name)
  }
  invisible(NULL)
}
autograph.graphbook <- function(x, data=.GlobalEnv, name=deparse(substitute(x)), device="screen", file.out=NULL, paper.size="letter", debug=FALSE, grid=gridxy(min(length(x),9)), pause=FALSE)
{
  Gbk <- x
  graph_index <- seq(along=Gbk)
  # if (debug) cat("Starting autograph.data.frame\n")
  datafile <- attr(Gbk, "datafile")
  codefile <- attr(Gbk, "graphbook.name")
  #
  filename <- if (is.null(file.out)) gsub("[^[[:alnum:]]]", "", name) else file.out
  agdevice <- ag_device(filename=filename,
    paper.size=paper.size,
    device=device
  )
  screen <- agdevice$screen
  plot.array  <-  grid
  n.per.page  <-  prod(plot.array)
  if (pause) {
    old.par <- par(ask=TRUE)
    on.exit(par(old.par))
  }
  graphed <- 0
  data.available <- names(x)
  for (i in graph_index) {
    X <- ag_graphable_item(Gbk[[i]], data=data)
    if (is.null(X)) next
    if (ignore_item(X)) next
    if ( graphed %% n.per.page == 0 ) {
      ag_new_page(agdevice)
      par(mfrow=plot.array)
      ag_defaultmargins()
    }
    ag_plot(X)
    graphed <- graphed + 1
    if ( graphed %% n.per.page == 1 ) {
      my.title = "Data:"
      if (is.character(datafile))
        my.title <- paste(my.title, " file '", datafile, "'", sep="")
      else
        my.title <- paste(my.title, " internal object '", name, "'", sep="")
      if (is.null(codefile))
        my.title <- paste(my.title, ", no graphbook used", sep="")
       else
        my.title <- paste(my.title, ", graphbook file: '", codefile, "'", sep="")
      mtext(my.title, line=3, outer=TRUE)
      mtext(date(), line=4, outer=TRUE)
      ### mtext("Page ", line=4,outer=TRUE)
    }
  }  
  ag_close_device(agdevice)
  invisible(NULL)
}
ag_extract_data <- function(data, obj.names=NULL)
{
  if (is.null(obj.names)) return(NULL)
  column.names <- make.names(obj.names)
  if (is.list(data)) {
    data.names <- names(data)
    which.exact <- (obj.names %in% data.names)
    which.nonexact <- (column.names %in% data.names)
    X <- list()
    for (i in seq(along=obj.names)) {
      on <- obj.names[i]
      cn <- column.names[i]
      X[[on]] <- if (which.exact[i]) data[[on]] else if (which.nonexact[i]) data[[cn]] else list(NULL)
    }
  } else if (is.environment(data)) {
    notfound <- structure(rep(list(NULL), length(column.names)), names=column.names)
    X <- mget(column.names, envir=data, ifnotfound=notfound)
    names(X) <- obj.names
  } else {
    stop("Given data is not a data frame, list or an environment. Columns given were:", paste(column.names))
  }
  if (any(which.null <- sapply(X, is.null))) {
    stop("Following data vectors were not found:", paste(obj.names[which.null]))
  } 
  return(X)
}
ag_graphable_item <- function(x, data)
{
  obj.names <- x$data
  if (is.null(obj.names)) return(NULL)
  X <- ag_extract_data(data, obj.names)
  #
  # Here 'X' is _always_ a list with the names matching exactly the 
  # vector names in 'data' (not artificial make.names names)
  #
  graphattr(X) <- x
  return(X)
}
#---end ag.R---
