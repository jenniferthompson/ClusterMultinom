ClusterMultinom
================

Current Status: UNDER CONSTRUCTION
----------------------------------

Purpose
-------

ClusterMultinom provides a suite of functions to make the process of fitting multinomial logistic regression models with clustered data, as well as extracting and visualizing meaningful results from those models, as painless as possible.

Current Capabilities
--------------------

The package assumes your multinomial logistic regression model will be fit using `VGAM::vglm(..., family = multinomial(...))`, and that you want to calculate variance of beta coefficients using the bootstrap. It also allows for multiple imputation, using the [`mice`](https://CRAN.R-project.org/package=mice) package and following the "boot MI" procedure recommended by [Schomaker & Heumann](https://arxiv.org/abs/1602.07933).

The functions included mirror the following steps:

1.  `create_bootdata()`: Create `B` datasets.
    -   Optional: Multiply impute each dataset `M` times.
2.  `summarize_multinom_list()`: Fit the same model to each bootstrapped dataset (or set of imputations), saving relevant information (especially coefficient estimates).

\*\* NOTE: FUNCTIONALITY CURRENTLY ENDS HERE \*\*

1.  Hypothesis testing for individual coefficients or for groups of coefficients.
2.  Calculate odds ratios and confidence limits for a coefficient.
3.  Calculate predicted probabilities and confidence limits for each outcome level, given a matrix of **X** values.

The functions are written and exported so that the user can access each step of the process independently if needed; however, wrappers are also provided to perform common tasks which fall completely within the context of the package.

Dependencies
------------

Currently, `purrr` and `tibble` are **Imported** and all other package dependencies are **Suggested**. Those suggested packages include, in order of importance:

1.  [`VGAM`](https://cran.r-project.org/package=VGAM): We chose `VGAM::vglm()` for our multinomial logistic regression fits because it produces the most conservative warning messages of the options we tried, which are important when fitting a potentially unstable model to many bootstrapped datasets.
2.  [`mice`](https://cran.r-project.org/package=mice): `mice` is a popular, flexible package for multiple imputation.
3.  [`gapminder`](https://cran.r-project.org/package=gapminder): Required only to run examples.
4.  [`testthat`](https://cran.r-project.org/package=testthat): Required only for testing.
5.  [`rms`](https://cran.r-project.org/package=rms): Required only for testing, but useful for fitting spline terms with `rms::rcs()`.

Contributors
------------

This is joint work with [Rameela Chandrasekhar](http://biostat.mc.vanderbilt.edu/wiki/Main/RameelaChandrasekhar).

We are grateful to [Cole Beck](http://biostat.mc.vanderbilt.edu/wiki/Main/ColeBeck) for his technical guidance and excellent debugging.

Future Directions/Collaboration Opportunities
---------------------------------------------

1.  Including sandwich estimation in addition to bootstrap methods to calculate coefficient variances.
2.  Adding flexibility such that `purrr` need not be installed to use this package.
3.  Adding methods for `mlogit` or `nnet::multinom()` model fits, in addition to `VGAM::vglm()`.

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
