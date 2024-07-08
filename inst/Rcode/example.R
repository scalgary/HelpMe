setwd("~/HelpMe")
devtools::load_all()
devtools::document()
devtools::check()
devtools::build()


options(ggrepel.max.overlaps = Inf)


setwd("J:\\sas_ms\\24-BrandPulse-AR123\\ID")
file <- list.files(pattern="quadmap")
file
myplotsBG <- run_quadmap_BG(file,TBdetail="T2B", max.overlaps = Inf)
myplotsBG[["Love"]]


if (HelpMe::check_pptx()) {save_quadmap_pptx(myplotsBG,"IDBGx.pptx")}
