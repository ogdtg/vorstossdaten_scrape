source("scrape_grgeko.R")

# Daten Scrapen
geschafte_list <- scrape_grgeko(legislatur = 2024)

# Daten aufbereiten für OGD 
data_ogd <- prepare_ogd_vorstoesse(geschafte_list)


write.table(data_ogd, file = "vorstoesse.csv", quote = T, sep = ",", dec = ".", 
            row.names = F, na="",fileEncoding = "utf-8")


saveRDS(Sys.time(),"last_run.rds")

