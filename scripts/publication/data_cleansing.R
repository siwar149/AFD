#### ECPLORING Magacho's approach

bucket1 <- "projet-esteem"
set_wd1 <- "Gloria/matrices"

bucket2 <- "siwar"
set_wd2 <- "data/Gloria"
set_wd3 <- "data/bio/rds"

library("tidyr")

# intensity of nSTAR
e <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd2,"/e3_2019.rds",sep=""),
                  bucket = bucket2, opts = list("region" = ""))

# Output
x <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd1,"/x_2019.rds",sep=""),
                  bucket = bucket1, opts = list("region" = ""))

x <- x + 0.0001 # No data on Yemen's output
x <- as.data.table(x)

# Labels of final demand, output, and satelite accounts
label_IO <- as.data.table(s3read_using(FUN = readRDS,
                                       object = paste(set_wd2,"/label_IO.rds",sep=""),
                                       bucket = bucket2, opts = list("region" = "")))

colnames(label_IO) <- c("iso", "country", "sector")

nace <- read_excel("data/NACE-Gloria.xlsx", sheet = "Feuil1")

label_IO <- label_IO %>%
  left_join(nace, by = c("sector"="Gloria"))

rm(nace)

f <- as.data.table(s3read_using(FUN = data.table::fread,
                                object = paste(set_wd2,"/f_2019.rds",sep=""),
                                bucket = bucket2, opts = list("region" = "")))

Z <- s3read_using(FUN = data.table::fread,
                        object = paste(set_wd1,"/IO_2019.rds",sep=""),
                        bucket = bucket1, opts = list("region" = ""))

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

eu1 <- c("AUT", "BEL", "DEU", "ESP", "FRA", "HRV", "HUN", "ITA",
         "LUX", "POL", "PRT", "SVK")

## Getting backward and forward induced extinction-risk
# backward

L <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd1,"/L_2019.rds",sep=""),
                  bucket = bucket1, opts = list("region" = ""))

G <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd1,"/G_2019.rds",sep=""),
                  bucket = bucket1, opts = list("region" = ""))

bk <- t(as.matrix(e)) %*% (as.matrix(L) - diag(dim(L)[1]))

bk <- as.data.table(t(bk))

# need to multiply by final demand to get the absolute footprint
bk <- bk * f

# weird results here
bk_eu <- cbind(label_IO[which(label_IO$iso %in% eu1),],
               bk[which(label_IO$iso %in% eu1),])

# forward (I am really not interested on the forward footprint of Europe)
fr <- (as.matrix(G) - diag(dim(G)[1])) %*% as.matrix(e)

fr <- as.data.table(fr)

fr <- fr * v

# Found the mistake here. Now everything fine
fr_eu <- cbind(label_IO[which(label_IO$iso %in% eu1),],
               fr[which(label_IO$iso %in% eu1),])

fr_latam6 <- cbind(label_IO[which(label_IO$iso %in% latam6),],
                   fr[which(label_IO$iso %in% latam6),])


# Interesting but not useful because I know what sectors I am going to shock
rm("bk", "fr", "fr_eu", "bk_eu", "G")




