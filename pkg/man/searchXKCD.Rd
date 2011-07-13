\name{searchXKCD}
\alias{searchXKCD}
\title{Search your favorite XKCD comic strip by title/trascript}
\description{
This function use grep to inspenct the title and trascript for all the occurrences of a specified string and return a data.frame with both the number and the title of the XKCD comic strips .
}
\usage{
searchXKCD(which = "significant", xkcd.data=NULL)
}
\arguments{
  \item{which}{ string }
  \item{xkcd.data}{ A character string giving a xkcd file in csv format.  
                    By default the csv file in the data directory of the xkcd 
                    package are used.}

}
\details{
}
\value{
a data.frame containing the following fields:
\item{num}{The num of the XKCD comic strip}
\item{title}{The title of the XKCD comic strip}

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
searchXKCD(which="significant") 
searchXKCD(which="someone is wrong")
}
