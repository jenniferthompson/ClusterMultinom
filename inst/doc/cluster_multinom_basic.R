## ------------------------------------------------------------------------
library(gapminder)
head(gapminder)

## ----fit_model_nocluster-------------------------------------------------
library(VGAM, quietly = TRUE)

## Combine Australia/NZ with Asia to help model stability
gapminder$continent <- with(gapminder, {
  ifelse(continent == "Oceania", "Asia", as.character(continent))
})

## Fit model with outcome = continent,
##  exposures = life expectancy, population, GDP
our_mod <- vglm(
  continent ~ lifeExp + pop + gdpPercap,
  data = gapminder,
  family = multinomial(refLevel = "Africa")
)

# ## Print model summary (commented out for length)
# summary(our_mod)


## ----create_gapminder_bootdata-------------------------------------------
library(ClusterMultinom)
library(purrr)
library(tibble)

gap_bootdfs <- create_bootdata(
  df = gapminder,
  cluster_var = "year",
  nboot = 10,
  seed = 1234
)

length(gap_bootdfs)

## ----run_gapminder_models------------------------------------------------
## Fit model with outcome = continent, exposures = [life expectancy, population,
##   GDP] to each data.frame in our list
## For demo purposes, we ask for 7 successful model fits
our_mod_summary <- summarize_multinom_list(
  continent ~ lifeExp + pop + gdpPercap,
  df_list = gap_bootdfs,
  ref_level = "Africa",
  testdf = gapminder,
  nsuccfits = 7
)


## ----print_gapminder_summary---------------------------------------------
our_mod_summary


