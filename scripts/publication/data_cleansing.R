#### ECPLORING Magacho's approach

bucket1 <- "projet-esteem"
set_wd1 <- "Gloria/matrices"

bucket2 <- "siwar"
set_wd2 <- "data/Gloria"
set_wd3 <- "data/bio/rds"

library("dplyr")

# intensity of nSTAR
e <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd2,"/e_2019.rds",sep=""),
                  bucket = bucket2, opts = list("region" = ""))

# Output
x <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd1,"/x_2019.rds",sep=""),
                  bucket = bucket1, opts = list("region" = ""))

x <- x + 0.0001 # No data on Yemen's output

# Labels of final demand, output, and satelite accounts
label_IO <- as.data.table(s3read_using(FUN = readRDS,
                  object = paste(set_wd2,"/label_IO.rds",sep=""),
                  bucket = bucket2, opts = list("region" = "")))

f <- as.data.table(s3read_using(FUN = data.table::fread,
                  object = paste(set_wd2,"/f_2019.rds",sep=""),
                  bucket = bucket2, opts = list("region" = "")))

Z <- as.data.table(s3read_using(FUN = readRDS,
                                object = paste(set_wd2,"/IO_2019.rds",sep=""),
                                bucket = bucket2, opts = list("region" = "")))

x <- as.numeric(as.matrix(x))

B <- solve(diag(x)) %*% Z
