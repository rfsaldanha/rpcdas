test_that("get sinasc with mun residence works", {
  res <- get_sinasc(agg = "mun_res", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 4000)
})

test_that("get sinasc with mun occurence works", {
  res <- get_sinasc(agg = "mun_ocor", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 4000)
})

test_that("get sinasc with uf residence works", {
  res <- get_sinasc(agg = "uf_res", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("get sinasc with uf occurence works", {
  res <- get_sinasc(agg = "uf_ocor", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("get sinasc with sex filter works", {
  res <- get_sinasc(agg = "mun_res", ano = 2010, sexo = "Masculino")

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 4000)
})

test_that("get sinasc with uf residence and month aggregation works", {
  res <- get_sinasc(agg = "uf_res", agg_time = "month", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 300)
})

test_that("get sinasc with uf residence and week aggregation works", {
  res <- get_sinasc(agg = "uf_res", agg_time = "week", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 1400)
})

test_that("get sinasc with uf residence and multiple years works", {
  res <- get_sinasc(agg = "uf_res", agg_time = "week", ano = c(2010, 2011))

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 2800)
})
