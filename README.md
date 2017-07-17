The ClusterMultinom Package
================
Jennifer Thompson

Current Status: UNDER CONSTRUCTION
----------------------------------

Purpose
-------

`ClusterMultinom` provides a suite of functions to make the process of fitting multinomial logistic regression models with clustered data, and extracting meaningful results from those models, as painless as possible.

Current Capabilities
--------------------

Currently, this package assumes your multinomial logistic regression model is fit using `VGAM::vglm(..., family = multinomial(...))`, and that you want to calculate variance of beta coefficients using the bootstrap.

The functions included mirror the following steps in the process:

1.  Create datasets to fit these multinomial models using the cluster bootstrap.
2.  Fit the same model to each bootstrapped dataset, saving errors/warnings of failed model fits and saving coefficients of successful model fits.
3.  Create a vector of point estimates, and a corresponding variance-covariance matrix, for all model coefficients.
4.  Calculate p-values for individual coefficients or for groups of coefficients.
5.  Calculate odds ratios and confidence limits for a coefficient.
6.  Calculate predicted probabilities and confidence limits for each outcome level, given a matrix of **X** values.

Dependencies
------------

Currently, all package dependencies are `Suggested`.

1.  `VGAM`: We chose `VGAM::vglm()` for our multinomial logistic regression fits because it produces the most conservative warning messages of the options we tried, which are important when fitting a potentially unstable model to many bootstrapped datasets. Therefore, the only methods available for some functions are for `vglm` objects. PRs to add methods for `mlogit` or `nnet::multinom()` model fits are welcome.
2.  `purrr`: Compared to the best base R approaches I could write, `purrr` gets the same results faster and requires less code. PRs to write the package more flexibly, so that users could use this package whether or not they choose to install `purrr`, are welcome.
3.  `gapminder`: Required only to run examples.
4.  `testthat`: For testing only.

Future Directions
-----------------

1.  Including sandwich estimation in addition to bootstrap methods to calculate coefficient variances.
2.  Adding flexibility such that `purrr` need not be installed to use this package.
3.  Adding methods for `mlogit` or `nnet::multinom()` model fits, in addition to `VGAM::vglm()`.
