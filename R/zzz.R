#' Title
#'
#' @param libname
#' @param pkgname
#'
#' @return
#' @export
#'
#' @examples
.onLoad <- function(libname, pkgname) {
  # Poser la question
  response <- readline("Veuillez entrer 1 : (Quadmap template) ou 2 : (Perceptual Map template) ")

  # Assurez-vous que la réponse est soit "1" soit "2"
  while (response != "1" && response != "2") {
    response <- readline("Réponse invalide. Veuillez entrer 1 ou 2 : ")
  }
  # Utilisez la réponse dans le reste de la fonction
  if (response == "1") {
    # Code à exécuter si la réponse est "1"
    load_template_quadmap()
  } else {
    # Code à exécuter si la réponse est "2"
    load_template_perceptualmap()
  }

}
