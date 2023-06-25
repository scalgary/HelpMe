
#' Get template automatically
#' @noRd
#' @return
#'
#'
#' @examples
get_template <- function() {
  # ask question
  response <- readline("Veuillez entrer 1 : (Quadmap template) ou 2 : (Perceptual Map template) ")

  # check 1 ir 2
  while (response != "1" && response != "2") {
    response <- readline("invalid answer. Select 1 ou 2 : ")
  }
  # use answer
  if (response == "1") {
    # 1 answer
    load_template_quadmap()
  } else {
    # 2 answer
    load_template_perceptualmap()
  }
}
