#set up quadmap
#read file - has to exists and to be .csv
#need 1 colonne character and other columns numeric
#nb colonne has to be impair
# read and create a liste of data map


#' Split quadmap file
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
split_data_frame <- function(df){

# Vérifie si toutes les valeurs de la dataframe non 1st column sont numériques
  if (!all(sapply(df[,-1], is.numeric))) {
  stop("Non numeric values detected")
}

# Vérifie si nb column is impair

  if (ncol(df) %% 2 == 0){
  stop("Nb columns is pair")
}

  if (ncol(df) ==1){
     stop("There is just 1 column")
}

   # Vérifie s'il y a des valeurs manquantes
  if (any(is.na(df))) {
     check <- FALSE
     stop("Missing values detected")
  }

   df_list <- list()

   # Calculate the number of pairs of columns
   nb_pairs <- (ncol(df) - 1) / 2

   # For each pair of columns, create a new dataframe and add it to the list
   for (i in 1:nb_pairs) {
     df_list[[i]] <- df[, c(1, 1 + i, 1 + i + nb_pairs)]
   }

   # Return the list of dataframes
   return(df_list)
}


