# BigDataPE is now a thin wrapper around the generic 'apifetch' package.
# Every bdpe_* function delegates to its af_* counterpart, supplying the
# conventions specific to the Big Data PE service. The public API and the
# environment-variable token names are unchanged, so existing code and stored
# tokens keep working.

# Internal: the Big Data PE API profile -------------------------------------
# Captures everything specific to this service: the token is sent verbatim in
# the Authorization header (`af_auth_raw()`); `limit`/`offset` travel as HTTP
# headers (`af_paginate_offset("header")`); the API returns a "Mensagem" status
# column that is dropped when combining chunks; and the service is only
# reachable from the PE Conectado network or VPN.
.bdpe_api <- function(endpoint = "https://www.bigdata.pe.gov.br/api/buscar") {
  apifetch::af_api(
    endpoint     = endpoint,
    service      = "BigDataPE",
    auth         = apifetch::af_auth_raw(),
    pagination   = apifetch::af_paginate_offset("header"),
    drop_cols    = "Mensagem",
    connect_hint = "Check that you are on the PE Conectado network or VPN."
  )
}

#' Constructs a URL with query parameters
#'
#' This function appends a list of query parameters to a base URL. It is a thin
#' re-export of [apifetch::parse_queries()].
#'
#' @param url The base URL to which query parameters will be added.
#' @param query_list A list of query parameters to be added to the URL.
#' @return The complete URL with the query parameters appended.
#' @examples
#' parse_queries("https://www.example.com", list(param1 = "value1", param2 = "value2"))
#' @export
parse_queries <- function(url, query_list) {
  apifetch::parse_queries(url, query_list)
}

#' Store a token in an environment variable for a specific dataset
#'
#' This function stores an authentication token for a specific dataset
#' in a system environment variable. The environment variable name is
#' constructed by converting the dataset name to ASCII (removing accents),
#' replacing spaces with underscores, and prefixing it with "BigDataPE_".
#'
#' If a variable with that name already exists (and is non-empty), the function
#' will not overwrite it.
#'
#' @param base_name The name of the dataset (character).
#' @param token The authentication token for the dataset (character).
#'
#' @return No return value, called for side effects.
#' @examples
#' bdpe_store_token("education_dataset", "your-token-here")
#'
#' @export
bdpe_store_token <- function(base_name, token) {
  apifetch::af_store_token(base_name, token, service = "BigDataPE")
}

#' Retrieve the token associated with a specific dataset
#'
#' This function retrieves the authentication token stored
#' in an environment variable for a specific dataset. If the token is not found,
#' it returns `NULL` and prints a message instead of throwing an error.
#'
#' @param base_name The name of the dataset (character).
#'
#' @return A string containing the authentication token, or `NULL` if the token is not found.
#' @examples
#' token <- bdpe_get_token("education_dataset")
#'
#' @export
bdpe_get_token <- function(base_name) {
  apifetch::af_get_token(base_name, service = "BigDataPE")
}

#' Remove the token associated with a specific dataset
#'
#' This function removes the authentication token stored
#' in an environment variable for a specific dataset. If the token is not found,
#' it prints a message and does not throw an error.
#'
#' @param base_name The name of the dataset (character).
#'
#' @return No return value. If the token is found, it is removed. If not, a message is displayed.
#' @examples
#' bdpe_remove_token("education_dataset")
#'
#' @export
bdpe_remove_token <- function(base_name) {
  apifetch::af_remove_token(base_name, service = "BigDataPE")
}

#' List all datasets that have stored tokens in environment variables
#'
#' This function returns a character vector of dataset names
#' that have their tokens stored in environment variables.
#' Specifically, it looks for variables that begin with the prefix
#' `"BigDataPE_"`.
#'
#' @return A character vector of dataset names with stored tokens.
#'         If no tokens are found, an empty vector is returned and
#'         a message is printed.
#'
#' @examples
#' bdpe_list_tokens()
#'
#' @export
bdpe_list_tokens <- function() {
  apifetch::af_list_tokens(service = "BigDataPE")
}

#' Fetch data from the BigDataPE API
#'
#' This function retrieves data from the BigDataPE API using securely stored tokens associated with datasets.
#' Users can specify pagination parameters (`limit` and `offset`) and additional query filters to customize the data retrieval.
#'
#' @param base_name A string specifying the name of the dataset associated with the token.
#' @param limit An integer specifying the maximum number of records to retrieve per request. Default is Inf (all records).
#'              If set to a non-positive value or `Inf`, no limit will be applied.
#' @param offset An integer specifying the starting record for the query. Default is 0.
#'               If set to a non-positive value or `Inf`, no offset will be applied.
#' @param query A named list of additional query parameters to filter the API results. Default is an empty list.
#' @param endpoint A string specifying the API endpoint URL. Default is "https://www.bigdata.pe.gov.br/api/buscar".
#' @param verbosity An integer specifying the verbosity level.
#'                  Values are:
#'                  - `0`: No output (default).
#'                  - `1`: Show progress messages (records fetched, totals).
#'                  - `2`: Show progress messages **and** full HTTP request/response details.
#'
#' @return A tibble containing the data returned by the API.
#' @examples
#' \dontrun{
#' # Store a token for the dataset
#' bdpe_store_token("dengue_dataset", "token")
#'
#' # Fetch 50 records from the beginning
#' data <- bdpe_fetch_data("dengue_dataset", limit = 50)
#'
#' # Fetch records with additional query parameters
#' data <- bdpe_fetch_data("dengue_dataset", query = list(field = "value"))
#'
#' # Fetch all data without limits
#' data <- bdpe_fetch_data("dengue_dataset", limit = Inf)
#' }
#' @export
bdpe_fetch_data <- function(
    base_name,
    limit = Inf,
    offset = 0L,
    query = list(),
    verbosity = 0L,
    endpoint = "https://www.bigdata.pe.gov.br/api/buscar") {
  apifetch::af_fetch(
    .bdpe_api(endpoint),
    base_name,
    limit = limit,
    offset = offset,
    query = query,
    verbosity = verbosity
  )
}

#' Fetch data from the BigDataPE API in chunks
#'
#' This function retrieves data from the BigDataPE API iteratively in chunks.
#' It uses `bdpe_fetch_data` as the base function and supports limits for the
#' total number of records to fetch and the size of each chunk.
#'
#' @param base_name A string specifying the name of the dataset associated with the token.
#' @param total_limit An integer specifying the maximum number of records to fetch. Default is Inf (all available data).
#' @param chunk_size An integer specifying the number of records to fetch per chunk. Default is 500000
#' @param query A named list of additional query parameters to filter the API results. Default is an empty list.
#' @param endpoint A string specifying the API endpoint URL. Default is "https://www.bigdata.pe.gov.br/api/buscar".
#' @param verbosity An integer specifying the verbosity level.
#'                  Values are:
#'                  - `0`: No output (default).
#'                  - `1`: Show progress messages (records fetched, totals).
#'                  - `2`: Show progress messages **and** full HTTP request/response details.
#' @return A tibble containing all the data retrieved from the API.
#' @examples
#' \dontrun{
#' # Store a token for the dataset
#' bdpe_store_token("dengue_dataset", "token")
#'
#' # Fetch up to 500 records in chunks of 100
#' data <- bdpe_fetch_chunks("dengue_dataset", total_limit = 500, chunk_size = 100)
#'
#' # Fetch all available data in chunks of 200
#' data <- bdpe_fetch_chunks("dengue_dataset", chunk_size = 200)
#' }
#' @export
bdpe_fetch_chunks <- function(
    base_name,
    total_limit = Inf,
    chunk_size = 500000L,
    query = list(),
    verbosity = 0L,
    endpoint = "https://www.bigdata.pe.gov.br/api/buscar") {
  apifetch::af_fetch_all(
    .bdpe_api(endpoint),
    base_name,
    total_limit = total_limit,
    chunk_size = chunk_size,
    query = query,
    verbosity = verbosity
  )
}
