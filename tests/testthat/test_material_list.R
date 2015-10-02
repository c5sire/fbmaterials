context("Material lists")

tbl_new <- new_materials_table()
tbl_get <- get_materials_table("potato", 2000, "TEST050")

test_that("Dummy table is created.", {
  expect_true(is.data.frame(tbl_new), TRUE)
  expect_true(is.data.frame(tbl_get), TRUE)
})
