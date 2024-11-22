rm(list = ls())
gc()


library(Matrix)
library(tidyverse)

# useful function
agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x) }


bucket1 <- "projet-esteem"
set_wd1 <- "Gloria/matrices"

bucket2 <- "siwar"
set_wd2 <- "data/Gloria"
set_wd3 <- "data/bio/rds"

# read labels
io <- as.data.table(s3read_using(FUN = readRDS,
                          object = paste(set_wd2,"/label_IO.rds",sep=""),
                          bucket = bucket2, opts = list("region" = "")))

fd <- s3read_using(FUN = data.table::fread,
                         object = paste("Gloria/labels/label_FD.rds",sep=""),
                         bucket = bucket1, opts = list("region" = ""))

colnames(io) <- c("iso", "country", "sector")
colnames(fd) <- c("iso", "country", "category")



# read data
L <- s3read_using(FUN = readRDS,
                  object = paste(set_wd2,"/L_2019.rds",sep=""),
                  bucket = bucket2, opts = list("region" = ""))

X <- s3read_using(FUN = readRDS,
                  object = paste(set_wd2,"/X_2019.rds",sep=""),
                  bucket = bucket2, opts = list("region" = ""))

Y <- s3read_using(FUN = readRDS,
                  object = paste(set_wd2,"/Y_2019.rds",sep=""),
                  bucket = bucket2, opts = list("region" = ""))

ext <- s3read_using(FUN = readRDS,
                   object = paste(set_wd2,"/score_pays-v3.rds",sep=""),
                   bucket = bucket2, opts = list("region" = ""))




# handling final demand and ext
colnames(Y) <- fd$country
Yc <- agg(Y)

X <- X + 0.0001
ext <- ext / X


#settings
countries <- unique(io$country)



