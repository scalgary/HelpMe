plots <- HelpMe::run_quadmap("inst/extdata/dataquad.csv")


init_object <- officer::read_pptx()   # Votre objet initial
A <- function(pres, i) {

  pres %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(names(plots)[i], location= officer::ph_location_type(type = "title")) %>%
    officer::ph_with(rvg::dml(ggobj = plots[[i]]),
                     location=officer::ph_location_type(type="body"))
}



result <- Reduce(A, seq_along(plots), init = init_object)

print(result, target ="new.pptx")


path <- system.file(package = "HelpMe", "template/dataquad.csv")
