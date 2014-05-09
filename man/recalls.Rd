\name{recalls}
\alias{recalls}
%\alias{print.recall}
\title{Get recall data}
\description{Retrieve recall data from the DigitalGov.gov Recall API}
\usage{
recalls(query = NULL, organization = NULL,
        start_date = NULL, end_date = NULL,
        food_type = NULL, upc = NULL,
        make = NULL, model = NULL, year = NULL, code = NULL,
        page = 1, per_page = 50, sort = NULL, ...)
}
\arguments{
  \item{query}{A character string that will be used in search recall data. The API executes approximate matching (e.g., so \dQuote{choke} will also return \dQuote{choking}.}
  \item{organization}{A character vector naming one or more federal agencies, by abbreviation. One of \dQuote{CPSC}, \dQuote{FDA}, \dQuote{NHTSA}, or \dQuote{USDA}.}
  \item{start_date}{A character string containing the start date of a recall in YYYY-MM-DD format.}
  \item{end_date}{A character string containing the end date of a recall in YYYY-MM-DD format.}
  \item{food_type}{An optional character string containing either \dQuote{food} or \dQuote{drug} to narrow results only to food or drug recalls.}
  \item{upc}{An optional character string containing a product UPC code.}
  \item{make}{For vehicle recalls, a character string naming a vehicle or equipment make.}
  \item{model}{For vehicle recalls, a character string naming a vehicle or equipment model.}
  \item{year}{For vehicle recalls, a character string naming a vehicle or equipment year.}
  \item{code}{For vehicle recalls, a character string naming an NHTSA code. One of \dQuote{E}, \dQuote{V}, \dQuote{I}, \dQuote{T}, \dQuote{T}, \dQuote{C}, or \dQuote{X}.}
  \item{page}{A numeric value or character string specifying a specific page of results to return (between 1 and 20). Setting \code{page=NULL} will force a return of all available pages (i.e., the first 1000 results). Attempting to request more than 1000 results will produce a message.}
  \item{per_page}{A numeric value or character string specifying a page size for results (between 1 and 50). This is overridden to 50 if \code{page=NULL}.}
  \item{sort}{A character string specifying whether to sort result by relevance (\dQuote{rel}; the default) or date (\dQuote{date}).}
  \item{...}{Other arguments. Currently ignored.}
}
\details{The particular objects returned will vary in structure depending on type of recall. As a result, attempting to build a rectangular dataset from different types of recall data may prove complicated. It is recommended to retrieve recalls of only one type if the goal is to \code{\link{rbind}} the list of results into a matrix or data.frame.}
\value{A list containing one or more objects of class \dQuote{recall}.}
%\references{}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
\examples{
\dontrun{
# by organization
recalls(organization = 'CPSC')

# food
recalls(query='peanut butter', food_type='food')

# product
recalls(query='burn')

# vehicle
recalls(make='ford', year='2010')
recalls(make='ford', year='2011')

}
}
\keyword{ IO }
