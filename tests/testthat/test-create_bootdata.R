context("create_bootdata")

test_that("Outputs match expectations", {
  ## Toy example: create data frame, set seed, create object to test
  my_df <- data.frame(
    id = sample(1:10, size = 50, replace = TRUE),
    x = rnorm(n = 50),
    y = rbinom(n = 50, size = 1, prob = 0.2)
  )

  my_seed <- 25

  testobj <- create_bootdata(
    my_df,
    cluster_var = "id",
    nboot = 5,
    seed = my_seed
  )

  ## Second test object to test reproducibility of function given a seed
  testobj2 <- create_bootdata(
    my_df,
    cluster_var = "id",
    nboot = 5,
    seed = my_seed
  )

  ## Length of output = list of length nboot
  expect_is(testobj, "list", info = "testobj is not a list.")
  expect_equal(
    5,
    length(testobj),
    info = "testobj does not return a list of length specified."
  )

  ## Each element of df_list is a data.frame
  expect_true(
    all(purrr::map_lgl(testobj, is.data.frame)),
    info = "Not all elements of testobj are data.frames."
  )

  ## If seed specified, make sure list is same every time
  expect_true(
    all.equal(testobj, testobj2),
    info = "Some element(s) of the two test lists of data.frames aren't equal."
  )

})