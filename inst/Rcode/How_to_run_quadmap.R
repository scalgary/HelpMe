
library(HelpMe)

###############Template QuadMap###############

file_csv <- "dataquad.csv"
file_csv <- system.file(package = "HelpMe", "extdata/dataquad.csv")

#your file 2n +1 columns
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
#helper functions not in package as depend of ooficer and rbg

source(system.file(package = "HelpMe", "Rcode/helper_functions.R"))



print(custom_pptx(plots),"with_template.pptx")



init_pptx <- officer::read_pptx(system.file(package = "HelpMe", "template/templateGG.pptx"))

# Votre objet initial
result <- Reduce(add_slide_pptx_template_v2, plots, init = init_pptx)
result <- result %>%
  officer::add_slide(layout = "OnlyTitle", master = "Custom Design") %>%
  officer::ph_with("Appendix", location= ph_location_label(ph_label = "Title 4")) %>%
  officer::add_slide(layout = "OnlyTitle", master = "Custom Design") %>%
  officer::ph_with("Appendix", location= ph_location_label(ph_label = "Title 4"))

print(result,"newsad.pptx")



init_pptx <- officer::read_pptx(system.file(package = "HelpMe", "template/templateGG.pptx"))
plots <- HelpMe::run_quadmap(file_csv)
add_slide_pptx_template <- function(doc_pptx, i, plots = plots) {


  doc_pptx %>%
    officer::add_slide(layout = "OnlyTitle", master = "Custom Design") %>%
    officer::ph_with(names(plots)[i], location= ph_location_label(ph_label = "Title 4")) %>%
    officer::add_slide(layout = "OnlyTitle", master = "Custom Design")  %>%
    officer::ph_with(names(plots)[i], location= ph_location_label(ph_label = "Title 4")) %>%
    officer::add_slide(layout = "TitleContent", master = "Custom Design") %>%
    officer::ph_with(paste0(names(plots)[i]," - Leveraging  Strengths and Weaknesses")
                     , location= ph_location_label(ph_label = "Title 4")) %>%
    officer::ph_with(rvg::dml(ggobj = plots[[i]]),
                     location=ph_location_label(ph_label = "Content Placeholder 2"))



}
# Votre objet initial
result <- Reduce(add_slide_pptx_template, seq_along(plots), init = init_pptx)
result <- result %>%
  officer::add_slide(layout = "OnlyTitle", master = "Custom Design") %>%
  officer::ph_with("Appendix", location= ph_location_label(ph_label = "Title 4")) %>%
  officer::add_slide(layout = "OnlyTitle", master = "Custom Design") %>%
  officer::ph_with("Appendix", location= ph_location_label(ph_label = "Title 4"))




plots <- HelpMe::run_quadmap(file_csv)

init_pptx <- officer::read_pptx(system.file(package = "HelpMe", "template/templateGG.pptx"))

add_slide_pptx_template <- function(doc_pptx, i, myplots = plots) {


  doc_pptx %>%
    officer::add_slide(layout = "OnlyTitle", master = "Custom Design") %>%
    officer::ph_with(names(myplots)[i], location= ph_location_label(ph_label = "Title 4")) %>%
    officer::add_slide(layout = "OnlyTitle", master = "Custom Design")  %>%
    officer::ph_with(names(myplots)[i], location= ph_location_label(ph_label = "Title 4")) %>%
    officer::add_slide(layout = "TitleContent", master = "Custom Design") %>%
    officer::ph_with(paste0(names(myplots)[i]," - Leveraging  Strengths and Weaknesses")
                     , location= ph_location_label(ph_label = "Title 4")) %>%
    officer::ph_with(rvg::dml(ggobj = myplots[[i]]),
                     location=ph_location_label(ph_label = "Content Placeholder 2"))



}
# Votre objet initial
result <- Reduce(add_slide_pptx_template, seq_along(plots), init = init_pptx)
result <- result %>%
  officer::add_slide(layout = "OnlyTitle", master = "Custom Design") %>%
  officer::ph_with("Appendix", location= ph_location_label(ph_label = "Title 4")) %>%
  officer::add_slide(layout = "OnlyTitle", master = "Custom Design") %>%
  officer::ph_with("Appendix", location= ph_location_label(ph_label = "Title 4"))
