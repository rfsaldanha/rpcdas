test_that("get sih with mun residence works", {
  res <- get_sih(agg = "mun_res", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 3000)
})

test_that("get sih with mun occurence works", {
  res <- get_sih(agg = "mun_ocor", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 3000)
})

test_that("get sih with uf residence works", {
  res <- get_sih(agg = "uf_res", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("get sih with uf occurence works", {
  res <- get_sih(agg = "uf_ocor", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("get sih with sex filterworks", {
  res <- get_sih(agg = "mun_res", ano = 2010, sexo = "Masculino")

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 4000)
})

test_that("get sih with uf and age less than works", {
  res <- get_sih(agg = "uf_res", ano = 2010, idade_a = 10)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("get sih with uf and age greater than works", {
  res <- get_sih(agg = "uf_res", ano = 2010, idade_b = 10)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("get sih with uf and age interval than works", {
  res <- get_sih(agg = "uf_res", ano = 2010, idade_a = 10, idade_b = 30)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("get sih with mun residence and sex and age interval than works", {
  res <- get_sih(agg = "mun_res", ano = 2010, sexo = "Feminino", idade_a = 10, idade_b = 30)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 20)
})

test_that("get sih with uf residence by month works", {
  res <- get_sih(agg = "uf_res", agg_time = "month", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 300)
})

test_that("get sih with uf residence by week works", {
  res <- get_sih(agg = "uf_res", agg_time = "week", ano = 2010)

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 1400)
})

test_that("get sih with uf residence with multiple years works", {
  res <- get_sih(agg = "uf_res", ano = c(2010, 2011))

  expect_equal("tbl_df", class(res)[1])
  expect_gt(nrow(res), 40)
})
