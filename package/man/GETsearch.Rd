\name{GETsearch}
\alias{GETsearch}
\title{Brickset search GET request}
\usage{
GETsearch(url = "http://brickset.com/webServices/brickset.asmx/",
  apiKey = "", userHash = "", query = "", theme = "", subtheme = "",
  setNumber = "", year = "", Owned = "", Wanted = "")
}
\arguments{
  \item{url}{brickset webservices url}

  \item{apiKey}{brickset apiKey (s. details)}

  \item{userHash}{brickset userHash (s. details)}

  \item{query}{A search string.}

  \item{theme}{Valid name of a theme.}

  \item{substheme}{Valid name of a subtheme.}

  \item{setNumber}{string or integer (s. details)}

  \item{year}{string or integer}

  \item{Owned}{Pass a '1' to get a list of set(s) the
  specified user owns.}

  \item{Wanted}{Pass a '1' to get a list of set(s) the
  specified user wants.}
}
\value{
brickset XML response set
}
\description{
Perform brickset search request via http GET
}
\details{
If \code{setNumber} does not include a '\code{-#}' after
the number (e.g. 70006-1), the suffix \code{-1} is added
automatically

Without providing a valid \code{apiKey} a maximum of 20
results will be returned.

If \code{userHash} is provided, the returned data will
contain flags indicating whether the specified user owns
and/or wants the set.
}

