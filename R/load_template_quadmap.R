
#' Title
#'
#' @return
#' @export
#'
#' @examples
load_template_perceptualmap<- function() {
  # Obtenir le chemin d'accès au fichier dans le package
  file_in_package <- system.file(package = "HelpMe", "Rcode/How_to_run_perceptual_map.R")
  # Vérifier que le fichier existe
  if (!file.exists(file_in_package)) {
    stop("Le fichier 'How_to_run_perceptual_map.R' n'a pas été trouvé dans le package.")
  }

  # Définir le chemin d'accès où le fichier sera copié
  file_in_wd <- file.path(getwd(), "How_to_run_perceptual_map.R")

  # Copier le fichier
  file.copy(from = file_in_package, to = file_in_wd)

  message("Fichier 'How_to_run_perceptual_map.R' copié dans le répertoire de travail courant.")
  # Ouvrir le fichier dans RStudio
  if (rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(file_in_wd)
  } else {
    message("RStudio API n'est pas disponible. Le fichier 'template.R' a été copié dans le répertoire de travail courant.")
  }
}


load_template_quadmap<- function() {
  # Obtenir le chemin d'accès au fichier dans le package
  file_in_package <- system.file(package = "HelpMe", "Rcode/How_to_run_perceptual_map.R")
  # Vérifier que le fichier existe
  if (!file.exists(file_in_package)) {
    stop("Le fichier 'How_to_run_perceptual_map.R' n'a pas été trouvé dans le package.")
  }

  # Définir le chemin d'accès où le fichier sera copié
  file_in_wd <- file.path(getwd(), "How_to_run_quadmap.R")

  # Copier le fichier
  file.copy(from = file_in_package, to = file_in_wd)

  message("Fichier 'How_to_run_perceptual_map.R' copié dans le répertoire de travail courant.")
  # Ouvrir le fichier dans RStudio
  if (rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(file_in_wd)
  } else {
    message("RStudio API n'est pas disponible. Le fichier 'template.R' a été copié dans le répertoire de travail courant.")
  }
}
