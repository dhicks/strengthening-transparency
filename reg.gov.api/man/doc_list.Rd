% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/docket.R
\name{doc_list}
\alias{doc_list}
\title{Retrieve the documents associated with a docket}
\usage{
doc_list(docket_id, verbose = FALSE)
}
\arguments{
\item{docket_id}{Docket identifier, eg, `'EPA-HQ-OA-2018-0259'`}

\item{verbose}{Display the API query}
}
\value{
Data frame with document metadata
}
\description{
Retrieve the documents associated with a docket
}
\note{
No paging implemented here
}
