% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BigDataPE.R
\name{bdpe_fetch_chunks}
\alias{bdpe_fetch_chunks}
\title{Fetch data from the BigDataPE API in chunks}
\usage{
bdpe_fetch_chunks(
  base_name,
  total_limit = Inf,
  chunk_size = 500000L,
  query = list(),
  verbosity = 0L,
  endpoint = "https://www.bigdata.pe.gov.br/api/buscar"
)
}
\arguments{
\item{base_name}{A string specifying the name of the dataset associated with the token.}

\item{total_limit}{An integer specifying the maximum number of records to fetch. Default is Inf (all available data).}

\item{chunk_size}{An integer specifying the number of records to fetch per chunk. Default is 50000}

\item{query}{A named list of additional query parameters to filter the API results. Default is an empty list.}

\item{verbosity}{An integer specifying the verbosity level for the API requests.
Values are:
- \code{0}: No verbosity (default).
- \code{1}: Minimal verbosity, showing request status.
- \code{2}: Detailed verbosity, including request and response details.}

\item{endpoint}{A string specifying the API endpoint URL. Default is "https://www.bigdata.pe.gov.br/api/buscar".}
}
\value{
A tibble containing all the data retrieved from the API.
}
\description{
This function retrieves data from the BigDataPE API iteratively in chunks.
It uses \code{bdpe_fetch_data} as the base function and supports limits for the
total number of records to fetch and the size of each chunk.
}
\examples{
\dontrun{
# Store a token for the dataset
bdpe_store_token("dengue_dataset", "token")

# Fetch up to 500 records in chunks of 100
data <- bdpe_fetch_chunks("dengue_dataset", total_limit = 500, chunk_size = 100)

# Fetch all available data in chunks of 200
data <- bdpe_fetch_chunks("dengue_dataset", chunk_size = 200)
}
}
