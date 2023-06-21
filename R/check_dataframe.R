#' Check if df appropriate for CA
#' @noRd
#' @param df that we hope to be appropriate for CA
#'
#' @return Boolean
#'
#'
#' @examples check_dataframe(Rhone)
check_dataframe <- function(df) {
  check <- TRUE
  # Vérifie si l'argument est une dataframe
  if (!is.data.frame(df)) {
    check <- FALSE
    stop("Argument needs to be a dataframe")
  }

  # Vérifie si la dataframe a au moins 3 lignes
  if (nrow(df) < 3) {
    check <- FALSE
    stop("dataframe needs to have at least 3 rows")
  }

  # Vérifie si la dataframe a au moins 3 colonnes
  if (ncol(df) < 3) {
    check <- FALSE
    stop("dataframe needs to have at least 3 columns")
  }

  # Vérifie si toutes les lignes ont des noms
  if (any(is.na(rownames(df)))) {
    check <- FALSE
    stop("Missing Attributes names")
  }

  # Vérifie si toutes les valeurs de la dataframe sont numériques
  if (!all(sapply(df, is.numeric))) {
    check <- FALSE
    stop("Non numeric values detected")
  }

  # Vérifie s'il y a des valeurs manquantes
  if (any(is.na(df))) {
    check <- FALSE
    stop("Missing values detected")
  }
  return(check)
}
