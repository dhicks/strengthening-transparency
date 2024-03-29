% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/docket.R
\name{get_block}
\alias{get_block}
\title{Get metadata for a block of 5000 comments}
\usage{
get_block(
  doc_id,
  lastdate = NULL,
  block_size = 5000,
  max_pages = 20,
  verbose = FALSE
)
}
\arguments{
\item{doc_id}{API document identifier, eg, `'090000648320bc9e'`}

\item{lastdate}{If not `NULL`, only comments posted on or after this date-time will be included in search results. Used for paging through blocks of 5000 comments.}

\item{block_size}{Number of comments in the block.  Used to determine how many pages need to be retrieved.}

\item{verbose}{Passed down to `get_comment_page`}

\item{max_page}{Maximum number of pages to be retrieved; here mainly for debugging purposes}
}
\value{
A data frame containing the comment metadata
}
\description{
Get metadata for a block of 5000 comments
}
