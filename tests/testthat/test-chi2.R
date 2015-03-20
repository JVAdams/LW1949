
test_that("chi2() returns the correct values", {
  norm <- chi2(c(10, 8, 3), c(7, 7, 7))
  nanorm <- sapply(norm, . %>% is.na %>% sum)
  expect_that(nanorm, is_equivalent_to(c(0, 0)))
  expect_that(norm$chi["df"], is_equivalent_to(1))

  norm2 <- chi2(c(10, 8), c(7, 7))
  nanorm2 <- sapply(norm2, . %>% is.na %>% sum)
  expect_that(nanorm2, is_equivalent_to(c(1, 0)))
  expect_that(norm2$chi["df"], is_equivalent_to(0))
  expect_that(is.na(norm2$chi["pval"]), is_true())

  norm3 <- chi2(10, 7)
  expect_that(is.na(norm3$chi["df"]), is_true())
  expect_that(is.na(norm3$chi["pval"]), is_true())
  expect_that(norm3$chi["chistat"], is_equivalent_to(norm3$contrib))

  expect_that(chi2(letters[1:3], c(7, 7, 7)), throws_error())
  expect_that(chi2(c(10, 8, 3), letters[1:3]), throws_error())
  expect_that(chi2(c(10, -1, 3), c(7, 7, 7)), throws_error())
  expect_that(chi2(c(10, 8, 3), c(7, 0, 7)), throws_error())
  expect_that(chi2(c(NA, 8, NA), c(7, NA, 7)), throws_error())
})
