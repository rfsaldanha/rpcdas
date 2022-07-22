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

test_that("get sinasc with sex filterworks", {
  res <- get_sinasc(agg = "mun_res", ano = 2010, sexo = "Masculino")

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 4000)
})
