
#' Title
#' @noRd
#' @return
#'
#'
#' @examples
load_template_perceptualmap<- function() {
  # Get path
  file_in_package <- system.file(package = "HelpMe", "Rcode/How_to_run_perceptual_map.R")
  # check exist
  if (!file.exists(file_in_package)) {
    stop("No file 'How_to_run_perceptual_map.R' found.")
  }

  # where to copy
  file_in_wd <- file.path(getwd(), "Run_perceptual_map.R")

  # Copy
  file.copy(from = file_in_package, to = file_in_wd)

  message("File 'How_to_run_perceptual_map.R' copied in working directory.")
  # open
  if (rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(file_in_wd)
  } else {
    message("No RStudio API. file 'template.R' has been saved in working directory.")
  }
}


load_template_quadmap<- function() {
  # Get path file
  file_in_package <- system.file(package = "HelpMe", "Rcode/How_to_run_quadmap.R")
  # Check if it exists
  if (!file.exists(file_in_package)) {
    stop("No file 'How_to_run_perceptual_map.R' found.")
  }

  # Where to copy
  file_in_wd <- file.path(getwd(), "Run_quadmap.R")

  # Copy
  file.copy(from = file_in_package, to = file_in_wd)

  message("File 'How_to_run_perceptual_map.R' copied in working directory")
  # Ouvrir le fichier dans RStudio
  if (rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(file_in_wd)
  } else {
    message("No RStudio API. file 'template.R' has been saved in working directory.")
  }
}
