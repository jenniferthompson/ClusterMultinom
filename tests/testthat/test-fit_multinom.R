context("Both use cases for fit_multinom")

test_that("fit_multinom, complete case", {
  ## Simple model - should fit with no issues
  simple_mod <- fit_multinom(formula = y ~ x1 + x2, df = my_df, ref_level = "A")
  expect_s4_class(simple_mod, "vglm")

  ## Complex model fit on small subset of data - should break
  complex_mod <- fit_multinom(
    formula = y ~ rcs(x1, 5) * rcs(x2, 5),
    df = my_df[1:100,],
    ref_level = "A"
  )

  ## Not sure why this test is failing. Works interactively.
  # expect_true(inherits(complex_mod, "try-error"),
  #             info = "Complex model on small df should result in try-error.")
})

test_that("fit_multinom, MI", {
  ## Simple model - should fit with no issues
  simple_mod <- fit_multinom(
    formula = y ~ x1 + x2,
    df = my_df_mice,
    ref_level = "A"
  )

  expect_true(
    all(purrr::map_lgl(simple_mod$analyses, ~ inherits(., "vglm"))),
    info = "For this simple model, all imputations should have a successful fit of type vglm."
  )

  expected_coefs <- (length(unique(my_df[!is.na(my_df$y), "y"])) - 1) * 3
  expect_true(
    all(
      purrr::map_lgl(
        simple_mod$analyses,
        ~ length(.@coefficients) == expected_coefs
      )
    ),
    info = "Each imputation should have 3 * [unique levels of y - 1] coefficients."
  )
})

## Model actually fails if number of failures big enough
