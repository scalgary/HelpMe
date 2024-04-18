

library(here)
library(tidyverse)

where <-"J:\\sas_ms\\22-071458-02\\H2\\AR1_2\\AUTO"
data_ibn <- "J:\\sas_ms\\22-071458-02\\H2\\AR1_2\\data_SAT_ANDROID_AUTO.csv"
wheresaved <- "C:\\Users\\Sandrine.Lebon01\\MAIN\\usatibn\\AR1_2"

get_count <- function(by="brand"){
read.csv(data_ibn) %>%
  group_by(!!! syms(by) ) %>%
  summarise(count=n()) %>% write.csv(file.path(wheresaved ,"countsat.csv"))}


get_count("country")



myfunction <- function(file) {
  df_summary <- readr::read_csv(file) %>% mutate(from =dirname(file)) %>%
    mutate(brandindex=paste0("Brand",index)) %>%
    mutate(brand = case_when(
      brandindex =="Brand0" ~ 'Category',
      .default =as.character(str_to_title(brand)))) %>%
    select(-valid.n)


  return(df_summary)

}

ll <- list.files(where,"modelfit",recursive = TRUE, full.names = TRUE)
all <- purrr::map_dfr(ll,myfunction)




summary_fit <- purrr::map_dfr(ll,myfunction)
summary_fit %>% write_csv(file.path(wheresaved,"summary_fitsat.csv"))


read_and_transform <-function(from,brandindex,brand){
  df <- readr::read_csv(list.files(here(from,brandindex),"driver",full.names=TRUE)) %>%
    select(Name,Label,Driver) %>%
    mutate(from=brand)
  return(df)
}


list_of_dataframes <- pmap(list(summary_fit$from,summary_fit$brandindex,summary_fit$brand),read_and_transform)




result <- dplyr::bind_rows(list_of_dataframes)

write.csv(result,file.path(wheresaved,"resultsat.csv"))
result %>% pivot_wider(id_cols = c("Name","Label"), names_from= from, values_from = Driver) %>%
  write.csv(file.path(wheresaved,"driverssat.csv"))



myplots <- run_quadmap("J:\\sas_ms\\22-071458-02\\H2\\AR1_2\\quadmap.csv",max.overlaps = Inf)
save_quadmap_pptx(myplots,file.path("C:\\Users\\Sandrine.Lebon01\\MAIN\\usatibn\\AR1_2","SAT.pptx"))
