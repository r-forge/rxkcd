\name{getXKCD}
\alias{getXKCD}
\title{Display your favourite XKCD comic in R}
\description{
This function fetches a XKCD comic strip (randomly or by number) and displays it on screen.
}
\usage{
getXKCD(which = "current", display = TRUE, saveImg = FALSE)
}
\arguments{
  \item{which}{ string: either "current" or "random"; or a number indicating the specific strip.}
  \item{display}{ logical; TRUE (default) if you like to display the strip on the screen}
  \item{saveImg}{logical; TRUE if you want to save image in the current directory. Default FALSE}
}
\details{
}
\value{
a list containing the following fields:
\item{img}{The URL of the XKCD comic strip image (png)}
\item{title}{The title of the XKCD comic strip}
\item{month}{}
\item{num}{The number of the XKCD comic strip}
\item{link}{}
\item{year}{}
\item{safe_title}{}
\item{transcript}{}
\item{alt}{}
\item{day}{}
}
\references{
http://xkcd.com/license.html
}
\author{Paolo Sonego <paolo.sonego@gmail.com>}
\note{
}

\seealso{
}
\examples{
current <- getXKCD("current", display=FALSE)
}