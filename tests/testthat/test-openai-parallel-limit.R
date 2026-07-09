# Unit tests for query_api_parallel_limit(): the best-effort probe of a
# LiteLLM-style `/key/info` endpoint for a key's max_parallel_requests. The HTTP
# layer is mocked so no network is touched; the function must never throw and
# must return NA whenever the limit cannot be determined.

query_api_parallel_limit <- mutator:::query_api_parallel_limit

test_that("returns NA when no base_url is configured", {
  expect_identical(query_api_parallel_limit(list(api_key = "k")), NA_integer_)
  expect_identical(
    query_api_parallel_limit(list(base_url = "", api_key = "k")),
    NA_integer_
  )
})

test_that("returns the limit reported by a 200 response", {
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    status_code = function(resp) 200L,
    content = function(resp, ...) list(info = list(max_parallel_requests = 8)),
    .package = "httr"
  )
  expect_identical(
    query_api_parallel_limit(list(base_url = "http://proxy/v1", api_key = "k")),
    8L
  )
})

test_that("returns NA on a non-200 response", {
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    status_code = function(resp) 403L,
    content = function(resp, ...) stop("should not be reached"),
    .package = "httr"
  )
  expect_identical(
    query_api_parallel_limit(list(base_url = "http://proxy/v1", api_key = "k")),
    NA_integer_
  )
})

test_that("returns NA when the response omits max_parallel_requests", {
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    status_code = function(resp) 200L,
    content = function(resp, ...) list(info = list()),
    .package = "httr"
  )
  expect_identical(
    query_api_parallel_limit(list(base_url = "http://proxy/v1", api_key = "k")),
    NA_integer_
  )
})

test_that("returns NA when the reported limit is not a positive integer", {
  testthat::local_mocked_bindings(
    GET = function(...) structure(list(), class = "response"),
    status_code = function(resp) 200L,
    content = function(resp, ...) list(info = list(max_parallel_requests = 0)),
    .package = "httr"
  )
  expect_identical(
    query_api_parallel_limit(list(base_url = "http://proxy/v1", api_key = "k")),
    NA_integer_
  )
})

test_that("never throws: a transport error becomes NA", {
  testthat::local_mocked_bindings(
    GET = function(...) stop("connection refused"),
    .package = "httr"
  )
  expect_identical(
    query_api_parallel_limit(list(base_url = "http://proxy/v1", api_key = "k")),
    NA_integer_
  )
})
