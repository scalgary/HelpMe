
library(HelpMe)

###############Template QuadMap###############

file_csv <- "dataquad.csv" #your file 2n +1 columns
#1st column label
# 2 to n+1 Performance
#n+2 to 2n+1 impact
#columns impact give name quadmap

plots <- HelpMe::run_quadmap(file_csv)

#visualisation
plots

###export to pptx needs officer and rvg
library(officer)
library(rvg)
##helper function to get 1 pptx with all the quadmaps

add_slide_pptx <- function(doc_pptx, i) {

    doc_pptx %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(names(plots)[i], location= officer::ph_location_type(type = "title")) %>%
    officer::ph_with(rvg::dml(ggobj = plots[[i]]),
                     location=officer::ph_location_type(type="body"))
}
#######


init_pptx <- officer::read_pptx()  # Votre objet initial
result <- Reduce(add_slide_pptx, seq_along(plots), init = init_pptx)
print(result, target ="new.pptx")






