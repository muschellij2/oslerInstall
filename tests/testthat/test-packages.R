test_that("packages not in OSLER", {

  expect_error(osler_install("worstPackageEver"))

})