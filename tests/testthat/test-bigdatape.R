# These tests pin down the Big Data PE service contract that BigDataPE adds on
# top of 'apifetch': token names, the verbatim Authorization header, limit and
# offset sent as HTTP headers, and the "Mensagem" column dropped from chunks.
# The HTTP layer is mocked, so no request leaves the machine.

# A fake Big Data PE API serving `n_rows` records, paged by the limit/offset
# headers. Every request is recorded in `env$requests`.
mock_bdpe <- function(env, n_rows = 7L) {
  env$requests <- list()
  function(req) {
    env$requests[[length(env$requests) + 1L]] <- req
    offset <- as.integer(req$headers$offset %||% 0L)
    limit <- as.integer(req$headers$limit %||% n_rows)
    n <- max(0L, min(limit, n_rows - offset))
    body <- if (n == 0L) {
      "[]"
    } else {
      rows <- sprintf('{"id":%d,"Mensagem":"ok"}', offset + seq_len(n))
      paste0("[", paste(rows, collapse = ","), "]")
    }
    httr2::response(
      200L,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw(body)
    )
  }
}

`%||%` <- function(x, y) if (is.null(x)) y else x

test_that("tokens use the BigDataPE_<dataset> environment variable", {
  withr::local_envvar(c(BigDataPE_Saude_publica = NA))
  bdpe_store_token("Saúde pública", "tok")
  expect_equal(Sys.getenv("BigDataPE_Saude_publica"), "tok")
  expect_equal(bdpe_get_token("Saúde pública"), "tok")
  expect_true("Saude_publica" %in% bdpe_list_tokens())
  bdpe_remove_token("Saúde pública")
  expect_null(bdpe_get_token("Saúde pública"))
})

test_that("bdpe_store_token() only replaces a token with overwrite = TRUE", {
  withr::local_envvar(c(BigDataPE_ds = NA))
  bdpe_store_token("ds", "old")
  bdpe_store_token("ds", "new")
  expect_equal(bdpe_get_token("ds"), "old")
  bdpe_store_token("ds", "new", overwrite = TRUE)
  expect_equal(bdpe_get_token("ds"), "new")
})

test_that("bdpe_fetch_data() sends the raw token and paging headers", {
  withr::local_envvar(c(BigDataPE_ds = "secret"))
  env <- new.env()
  out <- httr2::with_mocked_responses(
    mock_bdpe(env),
    bdpe_fetch_data("ds", limit = 2, offset = 3, query = list(ano = 2020))
  )
  req <- env$requests[[1]]
  expect_equal(req$url, "https://www.bigdata.pe.gov.br/api/buscar?ano=2020")
  expect_equal(httr2::req_get_headers(req, "reveal")$Authorization, "secret")
  expect_equal(req$headers$limit, 2L)
  expect_equal(req$headers$offset, 3L)
  expect_equal(out$id, 4:5)
  expect_false("Mensagem" %in% names(out))
})

test_that("bdpe_fetch_chunks() requests 50000 records per chunk by default", {
  withr::local_envvar(c(BigDataPE_ds = "secret"))
  env <- new.env()
  httr2::with_mocked_responses(mock_bdpe(env), bdpe_fetch_chunks("ds"))
  expect_equal(env$requests[[1]]$headers$limit, 50000L)
})

test_that("bdpe_fetch_chunks() pages through the data and drops Mensagem", {
  withr::local_envvar(c(BigDataPE_ds = "secret"))
  env <- new.env()
  out <- httr2::with_mocked_responses(
    mock_bdpe(env, n_rows = 7L),
    bdpe_fetch_chunks("ds", chunk_size = 3)
  )
  expect_equal(out$id, 1:7)
  expect_false("Mensagem" %in% names(out))
  offsets <- vapply(env$requests, function(r) r$headers$offset %||% 0L, integer(1))
  expect_equal(offsets, c(0L, 3L, 6L, 7L))
})

test_that("bdpe_fetch_chunks() respects total_limit, also with chunk_size = Inf", {
  withr::local_envvar(c(BigDataPE_ds = "secret"))
  env <- new.env()
  out <- httr2::with_mocked_responses(
    mock_bdpe(env),
    bdpe_fetch_chunks("ds", total_limit = 5, chunk_size = Inf)
  )
  expect_equal(out$id, 1:5)
  expect_length(env$requests, 1L)
})

test_that("the endpoint can be overridden", {
  withr::local_envvar(c(BigDataPE_ds = "secret"))
  env <- new.env()
  httr2::with_mocked_responses(
    mock_bdpe(env),
    bdpe_fetch_data("ds", endpoint = "https://example.org/api?x=1",
                    query = list(y = 2))
  )
  expect_equal(env$requests[[1]]$url, "https://example.org/api?x=1&y=2")
})
