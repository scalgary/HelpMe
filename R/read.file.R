#' Read file with some checking
#' @noRd
#' @param file_csv nom fichier
#' @param folder folder could be omitted
#'
#' @return df
#'
#'
#' @examples
read.file <- function(file_csv, folder = NULL) {
  # Vérifier si le fichier existe
  if (!(is.null(folder))) {file_path <- file.path(folder, file_csv)
  } else {
    file_path <- file_csv
  }
  if (!file.exists(file_path)) {
    stop("file doesn't exist : ", file_path)
  }

  # Déterminer le type de fichier en fonction de l'extension
  ext <- sub(".*\\.(.*)$", "\\1", file_path)

  # Lire le fichier en fonction de son type
  if (!(ext == "csv")) {
    stop("file not csv: ", file_path)

  }

  df <- utils::read.csv(file_path, header = TRUE,
           stringsAsFactors = FALSE)
  if (!is.character(df[[1]])) {
    stop("First column doesn't contain characters.")
  }


  return(df)
}

