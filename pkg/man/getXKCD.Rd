\name{getXKCD}
\alias{getXKCD}
\title{Display your favourite XKCD comic in R}
\usage{
  getXKCD(which = "current", display = TRUE, html = FALSE,
    saveImg = FALSE)
}
\arguments{
  \item{which}{string: either "current" or "random"; or a
  number indicating the specific strip.}

  \item{display}{logical; TRUE (default) if you like to
  display the strip on the screen}

  \item{html}{logical; TRUE if you like to open the XKCD
  web page for the selected comic in your browser: if TRUE
  it sets display and saveImg arguments to FALSE. Default
  FALSE}

  \item{saveImg}{logical; TRUE if you want to save image in
  the current directory. Default FALSE}
}
\value{
  a list containing the following fields: \itemize{ \item
  imgURL of the XKCD comic strip image (png) \item title
  Title of the XKCD comic strip \item month \item numNumber
  of the XKCD comic strip \item link \item year Year of
  publication \item safe_title \item transcript \item alt
  \item day }
}
\description{
  This function fetches a XKCD comic strip (randomly or by
  number) and displays it on screen.
}
\examples{
library("RXKCD")
significant <- getXKCD(882, display=FALSE)
}
\references{
  http://xkcd.com/license.html
}

