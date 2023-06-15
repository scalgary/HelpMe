## code to prepare `Rhone` dataset goes here

# Create the dataframe
Rhone <- data.frame(
  word = c("money", "future", "unemployment", "circumstances",
           "hard", "economic", "egoism", "employment", "finances", "war"),
  unqualified = c(51, 53, 71, 1, 7, 7, 21, 12, 10, 4),
  cep = c(64, 90, 111, 7, 11, 13, 37, 35, 7, 7),
  bepc = c(32, 78, 50, 5, 4, 12, 14, 19, 7, 7),
  high_school_diploma = c(29, 75, 40, 5, 3, 11, 26, 6, 3, 6),
  university = c(17, 22, 11, 4, 2, 11, 9, 7, 1, 2),
  thirty = c(59, 115, 79, 9, 2, 18, 14, 21, 8, 7),
  fifty = c(66, 117, 88, 8, 17, 19, 34, 30, 12, 6),
  more_fifty = c(70, 86, 177, 5, 18, 17, 61, 28, 8, 13)
)

# Set the word column as row names
row.names(Rhone) <- Rhone$word

# Now, you can drop the word column as it's not needed anymore
Rhone$word <- NULL

usethis::use_data(Rhone, overwrite = TRUE)
