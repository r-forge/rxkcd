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

# getXKCD <- function(which="current", display=TRUE, useGTK=FALSE, saveImg=FALSE){
# 	require(EBImage)
# 	if (which=="current") xkcd <- fromJSON("http://xkcd.com/info.0.json")
# 	else if(which=="random"|which=="") {
# 		current <- fromJSON("http://xkcd.com/info.0.json")
# 		num = sample(1:current$num, 1)
# 		xkcd <- fromJSON(paste("http://xkcd.com/",num,"/info.0.json",sep=""))
# 	} 
# 	else xkcd <- fromJSON(paste("http://xkcd.com/",which,"/info.0.json",sep=""))
# 	xkcd.img <-readImage(xkcd$img[[1]])
# 	if(display) display(xkcd.img,useGTK=useGTK,title=xkcd$title)
# 	if(saveImg) writeImage(xkcd.img,paste(xkcd$title,".png",sep=""))
# 	return(xkcd)
# }

searchXKCD<- function(which="significant", xkcd.data=NULL){
	if(is.null(xkcd.data))
		xkcd.data <- get("xkcd.data", envir = xkcd.env)
		if(is.character(which)) {
		  if(length(which) > 1) which <- sample(which)
		which.tt <- grep(which, xkcd.data$title, ignore.case = TRUE, useBytes = TRUE)
		which.tr <- grep(which, xkcd.data$transcript, ignore.case =TRUE, useBytes = TRUE)
		which.all <- unique(c(which.tr, which.tt))
		} 
  out <- data.frame(num=xkcd.data[which.all, "num"], title=xkcd.data[which.all, "title"])
	return(out)	
}

getXKCD <- function(which="current", display=TRUE, saveImg=FALSE){
	if (which=="current") xkcd <- fromJSON("http://xkcd.com/info.0.json")
	else if(which=="random"|which=="") {
		current <- fromJSON("http://xkcd.com/info.0.json")
		num = sample(1:current$num, 1)
		xkcd <- fromJSON(paste("http://xkcd.com/",num,"/info.0.json",sep=""))
	} 
	else xkcd <- fromJSON(paste("http://xkcd.com/",which,"/info.0.json",sep=""))
	if(grepl(".png",xkcd$img)){
		download.file(url=xkcd$img, destfile=paste(tempdir(),"xkcd.png",sep="/"))
		xkcd.img <- readPNG( paste(tempdir(),"xkcd.png",sep="/") )
	}
	else if(grepl(".jpg",xkcd$img)){
		download.file(url=xkcd$img, destfile=paste(tempdir(),"xkcd.jpg",sep="/"))
		xkcd.img <- read.jpeg( paste(tempdir(),"xkcd.jpg",sep="/") )
	} else stop("Unsupported image format!")
	max.dim = max(dim(xkcd.img))
	plot(1:max.dim, type="n", axes=F, xaxt="n",yaxt="n",xlab="",ylab="")
	if(display) rasterImage(xkcd.img, xleft=0, ybottom=0, xright=dim(xkcd.img)[[2]], ytop=dim(xkcd.img)[[1]])
	if(saveImg) writePNG( image=xkcd.img, target=paste(xkcd$title,".png",sep="") )
	return(xkcd)
}