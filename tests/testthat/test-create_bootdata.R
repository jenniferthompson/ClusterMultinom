context("create_bootdata")

test_that("Outputs match expectations (complete case)", {
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

  ## Each element of testobj is different
  expect_true(
    purrr::map_lgl(
      testobj[2:length(testobj)],
      ~ is.character(all.equal(target = testobj[[1]], current = .))
    ) %>%
      all(),
    info = "Some element(s) of testobj are the same."
  )

  ## If seed specified, make sure list is same every time
  expect_true(
    all.equal(testobj, testobj2),
    info = "Some element(s) of the two test lists of data.frames aren't equal."
  )

})


test_that("Outputs match expectations (mice)", {
  if(requireNamespace("mice", quietly = TRUE)){
    ## Toy example: create data frame, set seed, create object to test
    my_df <- data.frame(
      id = sample(1:10, size = 50, replace = TRUE),
      x = rnorm(n = 50),
      y = rbinom(n = 50, size = 1, prob = 0.2)
    )

    ## Set some values to MCAR
    set.seed(56)
    my_df$x[sample(1:50, size = 10)] <- NA
    my_df$y[sample(1:50, size = 10)] <- NA

    my_seed <- 25

    testobj <- create_bootdata(
      my_df,
      cluster_var = "id",
      nboot = 5,
      seed = my_seed,
      impute = TRUE
    )

    ## Second test object to test reproducibility of function given a seed
    testobj2 <- create_bootdata(
      my_df,
      cluster_var = "id",
      nboot = 5,
      seed = my_seed,
      impute = TRUE
    )

    ## Length of output = list of length nboot
    expect_is(testobj, "list", info = "testobj is not a list.")
    expect_equal(
      5,
      length(testobj),
      info = "testobj does not return a list of length specified."
    )

    ## Each element of df_list is a mids object
    expect_true(
      all(purrr::map_lgl(testobj, is.mids)),
      info = "Not all elements of testobj are mids objects."
    )

    ## Each element of testobj is different
    expect_true(
      purrr::map_lgl(
        testobj[2:length(testobj)],
        ~ is.character(all.equal(target = testobj[[1]], current = .))
      ) %>%
        all(),
      info = "Some element(s) of testobj are the same."
    )

    ## If seed specified, make sure list is same every time
    expect_true(
      all.equal(testobj, testobj2),
      info = "Some element(s) of the two test lists of mids objects aren't equal."
    )

    ## Does passing arguments to mice() work?
    number_imps <- 10
    impmethod <- "mean"

    testobj_miceargs <- create_bootdata(
      my_df,
      cluster_var = "id",
      nboot = 5,
      seed = my_seed,
      impute = TRUE,
      m = number_imps,
      method = impmethod
    )

    expect_true(
      testobj_miceargs[[1]]$m == number_imps,
      info = "Number of imputations in mids object is not what was specified."
    )

    expect_true(
      if(length(impmethod) == 1){
        all.equal(
          as.character(testobj_miceargs[[1]]$method),
          rep(impmethod, ncol(my_df))
        )
      } else{
        all.equal(as.character(testobj_miceargs[[1]]$method), impmethod)
      },
      info = "Imputation methods for mids object are not what was specified."
    )
  }
})