\name{searchXKCD}
\alias{searchXKCD}
\title{Search your favorite XKCD comic strip by title/trascript}
\usage{
  searchXKCD(which = "significant", xkcd.data = NULL)
}
\arguments{
  \item{which}{string.}

  \item{xkcd.data}{A character string giving a xkcd file in
  csv format. By default the csv file in the data directory
  of the xkcd package are used.}
}
\value{
  a data.frame containing the following fields: \itemize{
  \item num The num of the XKCD comic strip \item title The
  title of the XKCD comic strip }
}
\description{
  This function use grep to inspect the title and trascript
  for all the occurrences of a specified string and return
  a data.frame with both the number and the title of the
  XKCD comic strips.
}
\examples{
library("RXKCD")
searchXKCD(which="significant")
searchXKCD(which="someone is wrong")
}
\references{
  http://xkcd.com/license.html
}

