#' Title
#'
#' @param file_csv
#' @param folder
#'
#' @return df
#' @export
#'
#' @examples
read.file <- function(file_csv, folder =".") {
  # Vérifier si le fichier existe
  file_path <- file.path(folder, file_csv)
  if (!file.exists(file_path)) {
    stop("file doesn't exist : ", file_path)
  }

  # Déterminer le type de fichier en fonction de l'extension
  ext <- sub(".*\\.(.*)$", "\\1", file_path)

  # Lire le fichier en fonction de son type
  if (!(ext == "csv")) {
    stop("file not csv: ", file_path)

  }

  df <- read.csv(file_path, header = TRUE,
           stringsAsFactors = FALSE)
  if (!is.character(df[[1]])) {
    stop("First column doesn't contain characters.")
  }


  return(df)
}

