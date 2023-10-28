library("devtools")




load_all()
document()
devtools::build_rmd("vignettes/how_to_Perceptual_Maps.Rmd")
devtools::build_rmd("vignettes/How_to_Quadmap.Rmd")

check()
build()
remove.packages("HelpMe")
install.packages("~/SAS_MS/HelpMe_2.1.6.tar.gz", repos = NULL, type = "source")


use_package("haven")
