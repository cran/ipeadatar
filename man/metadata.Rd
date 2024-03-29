% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ipeadata_pkg.R
\name{metadata}
\alias{metadata}
\title{Returns a metadata about the requested series}
\usage{
metadata(code, language = c("en", "br"), quiet = FALSE)
}
\arguments{
\item{code}{A character vector with Ipeadata code.}

\item{language}{String specifying the selected language. Language options are
English (\code{"en"}, default) and Brazilian Portuguese (\code{"br"}).}

\item{quiet}{Logical. If \code{FALSE} (default), a progress bar is shown.}
}
\value{
A data frame containing Ipeadata code, name, short comment, last update, theme name,
 source's name and full name, source's URL, frequency, unity, multiplier factor, status,
 subject code and the country or territorial code of requested series.
}
\description{
Returns a list with metadata information about the requested series.
}
\note{
The original language of the available series' names and the comments were preserved.
The Ipeadata codes may be required by \code{available_series()}.
}
\examples{
\dontrun{
# Metadata from
# "PRECOS12_IPCA12": Extended National Consumer Price Index (IPCA), Brazil
meta <- metadata(code = "PRECOS12_IPCA12")
}

}
\references{
This R package uses the Ipeadata API.
For more information go to \url{http://www.ipeadata.gov.br/}.
}
\seealso{
\code{\link{available_series}}, \code{\link{available_subjects}},
\code{\link{available_territories}}
}
