library(bigrquery)


library(dplyr)


# Store the project ID
projectid = 'mgcp-1192365-ips-wm-md-srf818'

# Set your query
sql <- "SELECT * FROM `mgcp-1192365-ips-wm-md-srf818.WM_FT_Dataset.tblMetricMap` LIMIT 10;"

# Run the query; this returns a bq_table object that you can query further
tb <- bq_project_query(projectid, sql)

# Store the first 10 rows of the data in a tibble
sample <-bq_table_download(tb, n_max = 10)

# Print the 10 rows of data
sample


