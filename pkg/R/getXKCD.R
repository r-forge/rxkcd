read.xkcd <- function(file = NULL)
{
  if(!is.null(file) && file.exists(file)) {
    xkcd <- file
  } else {
    path <- system.file("xkcd", package = "RXKCD")
    datafiles <- list.files(path)
    if(!is.null(file) && file.exists(file.path(path, file))) {
      xkcd <- file.path(path, file)
    } else {
      if(!is.null(file)) stop("sorry, ", sQuote(file), " not found")
      file <- datafiles
      xkcd <- file.path(path, file)
    }
  }
  out <-read.csv(xkcd)
  return(out)
}

xkcd.env <- new.env()

.onLoad <- function(lib, pkg) {
  assign("xkcd.data", read.xkcd(), envir = xkcd.env)
}

#' Search your favorite XKCD comic strip by title/trascript
#'
#' This function use grep to inspect the title and trascript for all the occurrences of a specified string and return a data.frame with both the number and the title of the XKCD comic strips.
#'
#' @param which string.
#' @param xkcd.data A character string giving a xkcd file in csv format. By default the csv file in the data directory of the xkcd package are used.
#'
#' @return a data.frame containing the following fields: \itemize{
#' \item num The num of the XKCD comic strip
#' \item title The title of the XKCD comic strip
#' }
#'
#' @references http://xkcd.com/license.html
#'
#' @export
#'
#' @examples
#'
#' library("RXKCD")
#' searchXKCD(which="significant") 
#' searchXKCD(which="someone is wrong")
#'
searchXKCD<- function(which="significant", xkcd.data = NULL){
	if(is.null(xkcd.data))
		xkcd.data <- get("xkcd.data", envir = xkcd.env)
		if(is.character(which)) {
		  if(length(which) > 1) which <- sample(which)
		which.tt <- grep(which, xkcd.data["title"][[1]], ignore.case = TRUE, useBytes = TRUE)
		which.tr <- grep(which, xkcd.data["transcript"][[1]], ignore.case =TRUE, useBytes = TRUE)
		which.all <- unique(c(which.tr, which.tt))
		} 
  out <- data.frame(num=xkcd.data[which.all, "num"], title=xkcd.data[which.all, "title"])
	return(out)	
}
#'
#' Display your favourite XKCD comic in R
#'
#' This function fetches a XKCD comic strip (randomly or by number) and displays it on screen.
#'
#' @param which string: either "current" or "random"; or a number indicating the specific strip.
#' @param display  logical; TRUE (default) if you like to display the strip on the screen
#' @param html logical; TRUE if you like to open the XKCD web page for the selected comic in your browser: if TRUE it sets display and saveImg arguments to FALSE. Default FALSE
#' @param saveImg logical; TRUE if you want to save image in the current directory. Default FALSE
#'
#' @return a list containing the following fields: \itemize{
#' \item imgURL of the XKCD comic strip image (png)
#' \item title Title of the XKCD comic strip
#' \item month
#' \item numNumber of the XKCD comic strip
#' \item link
#' \item year Year of publication
#' \item safe_title
#' \item transcript
#' \item alt
#' \item day
#' }
#'
#' @references http://xkcd.com/license.html
#'
#' @export
#'
#' @examples
#'
#' library("RXKCD")
#' significant <- getXKCD(882, display=FALSE)
#'
getXKCD <- function(which = "current", display = TRUE, html = FALSE, saveImg = FALSE) {
	if (which=="current") xkcd <- fromJSON("http://xkcd.com/info.0.json")
	else if(which=="random"|which=="") {
		current <- fromJSON("http://xkcd.com/info.0.json")
		num <- sample(1:current["num"][[1]], 1)
		xkcd <- fromJSON(paste("http://xkcd.com/",num,"/info.0.json",sep=""))
	} 
	else xkcd <- fromJSON(paste("http://xkcd.com/",which,"/info.0.json",sep=""))
	
	if(html) browseURL( paste("http://xkcd.com/", as.numeric(xkcd["num"][[1]]),sep="") ) 
	else {
		if(grepl(".png",xkcd["img"][[1]])){
			download.file(url=xkcd["img"][[1]], quiet=TRUE, mode="wb", destfile=paste(tempdir(),"xkcd.png",sep="/"))
			xkcd.img <- readPNG( paste(tempdir(),"xkcd.png",sep="/") )
		}
		else if(grepl(".jpg",xkcd["img"][[1]])){
			download.file(url=xkcd["img"][[1]], quiet=TRUE, mode="wb", destfile=paste(tempdir(),"xkcd.jpg",sep="/"))
			xkcd.img <- read.jpeg( paste(tempdir(),"xkcd.jpg",sep="/") )
		} else stop("Unsupported image format! Try html = TRUE")
		if(display) {
			max.dim = max(dim(xkcd.img))
			plot(1:max.dim, type="n", axes=F, xaxt="n",yaxt="n",xlab="",ylab="")
			rasterImage(xkcd.img, xleft=0, ybottom=0, xright=dim(xkcd.img)[[2]], ytop=dim(xkcd.img)[[1]])
		}
		if(saveImg) writePNG( image=xkcd.img, target=paste(xkcd$title,".png",sep="") )
	}
	return(xkcd)
}
