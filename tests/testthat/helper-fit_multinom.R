## Toy example: create data frame, set seed, create object to test
my_df <- data.frame(
  id = sample(1:50, size = 500, replace = TRUE),
  x1 = sample(c(NA, 1:100), size = 500, replace = TRUE),
  x2 = sample(c(NA, 1:100), size = 500, replace = TRUE),
  y = sample(c(NA, LETTERS[1:3]), size = 500, replace = TRUE)
)

my_df_mice <- mice::mice(my_df)
my_df_mice_small <- mice::mice(my_df[1:50,])
