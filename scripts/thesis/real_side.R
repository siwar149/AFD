#### Starting with the Leontief matrix

## setting bucket and all

bucket <- "projet-esteem"
set_wd <- "Gloria/matrices"

L <- s3read_using(FUN = readRDS,
             object = paste(set_wd,"/L_2019.rds",sep=""),
             bucket = bucket, opts = list("region" = ""))