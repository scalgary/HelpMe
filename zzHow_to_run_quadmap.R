
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

source(system.file(package = "HelpMe", "Rcode/helper_functions.R")))


#print(custom_reduce(seq_along(plots),FALSE),"test_1.pptx")
print(custom_reduce(seq_along(plots),TRUE),"test_2.pptx")

