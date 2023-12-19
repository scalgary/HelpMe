
where <-"J:\\sas_ms\\23-083352-01\\sat\\sat"
library(here)
library(tidyverse)

read.csv("J:\\sas_ms\\23-083352-01\\sat\\data_satall.csv") %>%
  group_by(brand) %>%
  summarise(count=n()) %>% write.csv("countsat.csv")

ll <- list.files(where,"modelfit",recursive = TRUE, full.names = TRUE)

myfunction <- function(file) {
  df_summary <- readr::read_csv(file) %>% mutate(from =dirname(file)) %>%
    mutate(brandindex=paste0("Brand",index)) %>%
    mutate(brand = case_when(
      brandindex =="Brand0" ~ 'Category',
      .default =as.character(str_to_title(brand)))) %>%
    select(-valid.n)


  return(df_summary)

}

all <- purrr::map_dfr(ll,myfunction)




purrr::map_dfr(ll,myfunction) %>% write_csv(file.path(where,"summary_fitsat.csv"))

summary_fit <- readr::read_csv(file.path(where,"summary_fitsat.csv"))
summary_fit

read_and_transform <-function(from,brandindex,brand){
  df <- readr::read_csv(list.files(here(from,brandindex),"driver",full.names=TRUE)) %>%
    select(Name,Label,Driver) %>%
    mutate(from=brand)
  return(df)
}


list_of_dataframes <- pmap(list(summary_fit$from,summary_fit$brandindex,summary_fit$brand),read_and_transform)




result <- dplyr::bind_rows(list_of_dataframes)

write.csv(result,"resultsat.csv")
result %>% pivot_wider(id_cols = c("Name","Label"), names_from= from, values_from = Driver) %>% write.csv("driverssat.csv")



myplots <- HelpMe::run_quadmap("J:\\sas_ms\\23-083352-01\\sat\\quadmapsat.csv",max.overlaps = Inf)
save_quadmap_pptx(myplots,"SAT.pptx")

