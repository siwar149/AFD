rm(list = ls())
gc()


bucket1 <- "projet-esteem"
set_wd1 <- "Gloria/matrices"

bucket2 <- "siwar"
set_wd2 <- "data/Gloria"
set_wd3 <- "data/bio/rds"


star <- s3read_using(FUN = data.table::fread,
                      object = paste(set_wd3,"/star_satellites.rds",sep=""),
                      bucket = bucket2, opts = list("region" = ""))
