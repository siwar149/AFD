
bucket1 <- "projet-esteem"
bucket2 <- "siwar"

set_wd1 <- "Gloria/matrices"
set_wd2 <- "data/Gloria"
set_wd3 <- "data/bio/rds"

# Get the nSTAR intensity vector
score <- s3read_using(FUN = readRDS,
                      object = paste(set_wd3,"/score_pays.rds",sep=""),
                      bucket = bucket2, opts = list("region" = ""))

x <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd1,"/x_2019.rds",sep=""),
                  bucket = bucket1, opts = list("region" = ""))

x <- x + 0.0001 # No data on Yemen's output

label_IO <- as.data.table(s3read_using(FUN = readRDS,
                                       object = paste(set_wd2,"/label_IO.rds",sep=""),
                                       bucket = bucket2, opts = list("region" = "")))

colnames(label_IO) <- c("iso", "country", "sector")

e <- label_IO %>%
  left_join(score, by = c("iso", "sector")) %>%
  mutate(score= if_else(is.na(score), 0.0001, score))

e <- e$score / x$x

s3write_using(x = as.data.table(e), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd2,"/e_2019.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))


# Global demand vector
f <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd1,"/FD_2019.rds",sep=""),
                  bucket = bucket1, opts = list("region" = ""))

f <- rowSums(f)


s3write_using(x = as.data.table(f), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd2,"/f_2019.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))


L <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd1,"/L_2019.rds",sep=""),
                  bucket = bucket1, opts = list("region" = ""))

# compute vector with multipliers of consumption footprint
mcf <- t(as.matrix(L)) %*% e

s3write_using(x = as.data.table(mcf), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd2,"/mcf.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))


### Flow matrix for world map
T <- diag(e) %*% as.matrix(L) %*% diag(f)


s3write_using(x = as.data.table(T[,in_eu]), FUN = data.table::fwrite, na = "", 
              object = paste("data/Gloria/Teu.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))





# A curiosity
## Get the set of final demands where there are negative values
#index1 <- as.numeric(sub("V", "", names(which(apply(f1, 2, function(col) sum(col < 0) > 0)))))
#label_f[index1,] # one can see that changes in inventories have negative values
#View(f[which(apply(f1, 1, function(row) any(row < 0))), index1])
# Then we exclude changes in inventories from the footprint calculation
#f1 <- as.matrix(f1[,-which(apply(f1, 2, function(col) sum(col < 0) > 0))])



# comes in handy
#eu1 <- c("AUT", "BEL", "DEU", "ESP", "FRA", "HRV", "HUN", "ITA",
#         "LUX", "POL", "PRT", "SVK")