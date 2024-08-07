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

# The B matrix
B <- solve(diag(x)) %*% as.matrix(Z)

# First step in getting the Gosh multiplier
G <- diag(dim(B)[1]) - B

# The Gosh multiplier
G <- solve(G)

# Saving my accomplishments of the day
s3write_using(x = as.data.table(B), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd2,"/B_2019.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))

s3write_using(x = as.data.table(G), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd2,"/G_2019.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))

