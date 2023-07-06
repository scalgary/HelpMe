df <- readr::read_csv("/Users/passion/junk/rawdat.csv")



HelpMe:::onecor.wtd(df$Q9,df$Q10)
HelpMe:::wtd.cor(df$Q9,df$Q10,df$weight)


library(dplyr)
library(weights)


# Define a custom function
weighted_correlation <- function(df, group_vars, var1, var2, weight_var) {
  df %>%
    group_by(across(all_of(group_vars))) %>%
    dplyr::summarize(
      Variable1 = var1,  # Adding Variable1 column
      Variable2 = var2,  # Adding Variable2 column
      correlation = HelpMe:::wtd.cor(.data[[var1]], .data[[var2]], .data[[weight_var]])[1],
      p_value = HelpMe:::wtd.cor(.data[[var1]], .data[[var2]], .data[[weight_var]])[4],
      n_size = HelpMe:::wtd.cor(.data[[var1]], .data[[var2]], .data[[weight_var]])[5])
  
}

# List of all variable combinations
vars <- c("Q2","Q3","Q5","Q9","Q10")
couples <- combn(vars, 2, simplify = FALSE)
group_vars <- c("brand", "ANNUAL_DIGITAL_AD_SPEND_Marker")  # specify the group vars here

# Empty list to store results
result_list <- list()

# Iterate over each pair of variables
for (i in seq_along(couples)) {
  var1 <- couples[[i]][1]
  var2 <- couples[[i]][2]
  
  # Calculate the weighted correlation for this pair of variables
  res <- weighted_correlation(df, group_vars, var1, var2, "weight")
  
  # Add the result to the list
  result_list[[i]] <- res
}

# Combine all results into a single data frame
results_df <- bind_rows(result_list)

results_df 
