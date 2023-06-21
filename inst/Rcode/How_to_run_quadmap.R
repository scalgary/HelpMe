
library(HelpMe)

###############Template QuadMap###############

file_csv <- "dataquad.csv"
file_csv <- system.file(package = "HelpMe", "extdata/dataquad.csv")

#your file 2n +1 columns
#1st column label
# 2 to n+1 Performance
#n+2 to 2n+1 impact
#columns impact give name quadmap

plots <- HelpMe::run_quadmap(file_csv, TBdetail = "T2B")

#visualisation
plots

###export to pptx needs officer and rvg
##helper function to get 1 pptx with all the quadmaps
#helper functions not in package as depend of ooficer and rbg
source(system.file(package = "HelpMe", "Rcode/helper_functions.R"))
##helper function to get 1 pptx with all the quadmaps
#helper functions not in package as depend of ooficer and rbg

plots <- HelpMe::run_quadmap(file_csv, TBdetail = "T2B")
print(custom_pptx(plots),"with_template_T2B.pptx")

plots <- HelpMe::run_quadmap(file_csv, TBdetail = "TB")
print(custom_pptx(plots),"with_template_TB.pptx")

