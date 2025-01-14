# Function to calculate agreement between categories, within each pair of columns
library(tidyverse)

calculate_agreement <- function(data) {
  # Get column names of items (excluding the QRP column)
  item_names <- colnames(data)[-1]
  
  # Calculate agreement for each pair of items
  agreement <- combn(item_names, 2, simplify = FALSE) %>%
    map_df(~{

      # Extract columns for the pair of items
      pair_data <- data[, c(.x[1], .x[2])]

      # Return results
      tibble(
        item1 = .x[1],
        item2 = .x[2],
        both_present =sum(pair_data[[.x[1]]] == 1 & pair_data[[.x[2]]] == 1),
        both_absent = sum(pair_data[[.x[1]]] == 0 & pair_data[[.x[2]]] == 0),
        one_present = sum(pair_data[[.x[1]]] == 1 & pair_data[[.x[2]]] == 0),
        other_present = sum(pair_data[[.x[1]]] == 0 & pair_data[[.x[2]]] == 1),
        agreement_n = sum(both_present, both_absent),
        disagreement_n = sum(one_present, other_present),
        agreement_prop = agreement_n/nrow(data),
        disagreement_prop = disagreement_n/nrow(data)
      )
    })
  
  return(agreement)
}

