source("scrape_grgeko.R")

# Daten Scrapen
geschafte_list <- scrape_grgeko(legislatur = 2024)

# Daten aufbereiten für OGD 
prepare_ogd_vorstoesse(geschafte_list)


saveRDS(Sys.time(),"last_run.rds")

