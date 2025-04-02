#' Constructs a URL with query parameters
#'
#' This function appends a list of query parameters to a base URL.
#'
#' @param url The base URL to which query parameters will be added.
#' @param query_list A list of query parameters to be added to the URL.
#' @return The complete URL with the query parameters appended.
#' @examples
#' parse_queries("https://www.example.com", list(param1 = "value1", param2 = "value2"))
#' @export
parse_queries <- function(url, query_list) {
  if (length(query_list) == 0) {
    return(url) # Return the base URL if no queries are provided
  }

  # Filter out parameters with empty values
  query_list <- query_list[query_list != ""]

  # If no valid parameters remain, return the base URL
  if (length(query_list) == 0) {
    return(url)
  }

  # Encode parameters and construct the query string
  query_string <- paste(
    sapply(names(query_list), function(name) {
      paste0(
        utils::URLencode(name, reserved = TRUE), "=", utils::URLencode(as.character(query_list[[name]]), reserved = TRUE)
      )
    }),
    collapse = "&"
  )

  # Append the query string to the URL
  paste0(url, "?", query_string)
}



#' Store a token in an environment variable for a specific dataset
#'
#' This function stores an authentication token for a specific dataset
#' in a system environment variable. The environment variable name is
#' constructed by converting the dataset name to ASCII (removing accents),
#' replacing spaces with underscores, and prefixing it with "BigDataPE_".
#'
#' If a variable with that name already exists (and is non-empty), the function
#' will stop and notify you to avoid overwriting. Adjust this behavior as needed.
#'
#' @param base_name The name of the dataset (character).
#' @param token The authentication token for the dataset (character).
#'
#' @return No return value, called for side effects.
#' @examples
#' bdpe_store_token("educação dataset", "your-token-here")
#'
#' @export
bdpe_store_token <- function(base_name, token) {
  # 1. Validate inputs
  if (!is.character(base_name) || !nzchar(base_name)) {
    stop("Dataset name must be a valid string.")
  }
  if (!is.character(token) || !nzchar(token)) {
    stop("Token must be a valid string.")
  }

  # 2. Convert the dataset name to ASCII (removing accents), then replace spaces with underscores
  base_name_ascii <- iconv(base_name, from = "", to = "ASCII//TRANSLIT")
  base_name_sanitized <- gsub(" ", "_", base_name_ascii)

  # 3. Construct the environment variable name
  env_var_name <- paste0("BigDataPE_", base_name_sanitized)

  # 4. Check if the environment variable is already in use
  if (nzchar(Sys.getenv(env_var_name, unset = ""))) {
    # Stop to avoid overwriting an existing token; change to warning or message as needed
    message(
      "The environment variable '", env_var_name,
      "' is already defined. Not overwriting to avoid data loss."
    )
    return(invisible())
  }

  # 5. Store the token in the environment variable
  env_list <- list(token)
  names(env_list) <- env_var_name
  do.call(Sys.setenv, env_list)

  message("Token stored in environment variable: ", env_var_name)
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
  # 1. Validate the base_name
  if (!is.character(base_name) || !nzchar(base_name)) {
    stop("Dataset name must be a valid string.")
  }

  # 2. Convert the dataset name to ASCII (removing accents), then replace spaces with underscores
  base_name_ascii <- iconv(base_name, from = "", to = "ASCII//TRANSLIT")
  base_name_sanitized <- gsub(" ", "_", base_name_ascii)

  # 3. Construct the environment variable name
  env_var_name <- paste0("BigDataPE_", base_name_sanitized)

  # 4. Retrieve the token from the environment variable
  token <- Sys.getenv(env_var_name, unset = "")

  # 5. If the token is empty, return NULL with a message
  if (token == "") {
    message("No token found for dataset: ", base_name)
    return(NULL)
  }

  return(token)
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
  # 1. Validate the base_name
  if (!is.character(base_name) || !nzchar(base_name)) {
    stop("Dataset name must be a valid string.")
  }

  # 2. Convert the dataset name to ASCII (removing accents), then replace spaces with underscores
  base_name_ascii <- iconv(base_name, from = "", to = "ASCII//TRANSLIT")
  base_name_sanitized <- gsub(" ", "_", base_name_ascii)

  # 3. Construct the environment variable name
  env_var_name <- paste0("BigDataPE_", base_name_sanitized)

  # 4. Check if the variable exists (non-empty)
  if (nzchar(Sys.getenv(env_var_name, unset = ""))) {
    # If it exists, unset (remove) the variable
    Sys.unsetenv(env_var_name)
    message("Token successfully removed for dataset: ", base_name)
  } else {
    message("No token found for dataset: ", base_name)
  }
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
  # 1. Retrieve all environment variables
  all_envs <- Sys.getenv()

  # 2. Filter for those that start with 'BigDataPE_'
  stored_tokens <- grep("^BigDataPE_", names(all_envs), value = TRUE)

  # 3. If no tokens found, return empty vector with a message
  if (length(stored_tokens) == 0) {
    message("No tokens found in environment variables.")
    return(character(0))
  }

  # 4. Remove prefix from variable names to extract the "sanitized" dataset name
  dataset_names <- sub("^BigDataPE_", "", stored_tokens)

  # 5. Return the dataset names
  return(dataset_names)
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
#' @param verbosity An integer specifying the verbosity level for the API requests.
#'                  Values are:
#'                  - `0`: No verbosity (default).
#'                  - `1`: Minimal verbosity, showing request status.
#'                  - `2`: Detailed verbosity, including request and response details.
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

  # Retrieve the token for the specified dataset
  token <- bdpe_get_token(base_name)

  # Input validation
  stopifnot(is.list(query))
  stopifnot(is.integer(offset))
  stopifnot(is.integer(limit) || is.infinite(limit))

  # Adjust limit and offset for the request
  if (is.infinite(limit) || limit <= 0) limit <- ""
  if (is.infinite(offset) || offset <= 0) offset <- ""

  # Build and send the API request
  req <- endpoint |>
    parse_queries(query_list = query) |>
    httr2::request() |>
    httr2::req_headers(
      Authorization = token,
      limit = limit,
      offset = offset
    )

  # Perform the request and parse the response
  res <- req |>
    httr2::req_perform(verbosity = verbosity) |>
    httr2::resp_body_json(simplifyVector = TRUE) |>
    tibble::as_tibble()

  return(res)
}


#' Fetch data from the BigDataPE API in chunks
#'
#' This function retrieves data from the BigDataPE API iteratively in chunks.
#' It uses `bdpe_fetch_data` as the base function and supports limits for the
#' total number of records to fetch and the size of each chunk.
#'
#' @param base_name A string specifying the name of the dataset associated with the token.
#' @param total_limit An integer specifying the maximum number of records to fetch. Default is Inf (all available data).
#' @param chunk_size An integer specifying the number of records to fetch per chunk. Default is 50000
#' @param query A named list of additional query parameters to filter the API results. Default is an empty list.
#' @param endpoint A string specifying the API endpoint URL. Default is "https://www.bigdata.pe.gov.br/api/buscar".
#' @param verbosity An integer specifying the verbosity level for the API requests.
#'                  Values are:
#'                  - `0`: No verbosity (default).
#'                  - `1`: Minimal verbosity, showing request status.
#'                  - `2`: Detailed verbosity, including request and response details.
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

  # Input validation
  stopifnot(is.integer(total_limit) || is.infinite(total_limit))
  stopifnot(is.integer(chunk_size) && chunk_size > 0)
  stopifnot(is.list(query))

  # Initialize variables
  offset <- 0L
  total_fetched <- 0L
  all_data <- list()

  # Fetch data in chunks
  repeat {
    # Calculate the limit for the current chunk
    current_limit <- as.integer(min(chunk_size, total_limit - total_fetched))

    #if(verbosity > 0) message("Current limit: ", current_limit)

    # Break if no more records are needed
    if (current_limit <= 0) break

    # Fetch the current chunk
    chunk <- bdpe_fetch_data(
      base_name = base_name,
      limit = current_limit,
      offset = offset,
      query = query,
      verbosity = verbosity,
      endpoint = endpoint
    )
    nms <- names(chunk)

    #if(verbosity > 0)
    #  chunk |> glimpse()

    if ("Mensagem" %in% nms)
      chunk <- dplyr::select(chunk, -"Mensagem")

    # Stop if the API returns no data
    if (nrow(chunk) == 0L) break

    # Append the chunk to the results
    all_data <- append(all_data, list(chunk))
    total_fetched <- as.integer(total_fetched) + nrow(chunk)

    # Update the offset for the next chunk
    offset <- as.integer(offset) + nrow(chunk)

    # Break if the total limit is reached
    if (total_fetched >= total_limit) break
  }

  # Combine all chunks into a single tibble
  combined_data <- dplyr::bind_rows(all_data)
  return(combined_data)
}
