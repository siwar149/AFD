#### Real side analysis

library("readr")
library("Rcpp")
library("RcppEigen")
library("RcppArmadillo")
library("microbenchmark")

gc()


## setting bucket and all

bucket1 = "projet-esteem"
set_wd1 <- "Gloria/matrices"

bucket2 = "siwar"
set_wd2 <- "data/Gloria"
set_wd3 <- "data/bio/rds"

## First find the most impactful sectors per country

score <- s3read_using(FUN = readRDS,
                      object = paste(set_wd3,"/score_pays.rds",sep=""),
                      bucket = bucket2, opts = list("region" = ""))


## we get the consumption of EU 27

eu <-  c('AUT', 'BEL', 'BGR', 'HRV', 'CYP', 'CZE', 'DNK', 'EST',
         'FIN', 'FRA', 'DEU', 'GRC', 'HUN', 'IRL', 'ITA', 'LVA', 
         'LTU', 'LUX', 'MLT', 'NLD', 'POL', 'PRT', 'ROU', 'SVK', 
         'SVN', 'ESP', 'SWE', 'XEU')

score_ext <- score[which(!score$iso %in% eu),]


cSectors <- score_ext %>%
  group_by(iso) %>%
  arrange(desc(score)) %>%
  slice_head(n = 1)

length(unique(cSectors$sector))


pSectors <- unique(cSectors$sector)

pSectors

f <- as.data.frame(s3read_using(FUN = readRDS,
                      object = paste(set_wd2,"/FD_2019.rds",sep=""),
                      bucket = bucket2, opts = list("region" = "")))

label_f <- as.data.frame(s3read_using(FUN = readRDS,
                                object = paste(set_wd2,"/label_FD.rds",sep=""),
                                bucket = bucket2, opts = list("region" = "")))

### Just keeping the final demand from EU

f <- f[, which(label_f$V1 %in% eu)]

f <- as.matrix(rowSums(f))

## We get only the household consumption from final demand and EU

label_IO <- as.data.frame(s3read_using(FUN = readRDS,
                      object = paste(set_wd2,"/label_IO.rds",sep=""),
                      bucket = bucket2, opts = list("region" = "")))


f[which(label_IO$V1 %in% eu),] <- 0

f[which(!label_IO$V3 %in% pSectors),] <- 0

s <- f

rm("f")




### check final output

x <- as.matrix(s3read_using(FUN = readRDS,
                    object = paste(set_wd2,"/x_2019.rds",sep=""),
                    bucket = bucket2, opts = list("region" = "")))

colnames(x) <- "output"



##### Now simulate demand #####

L <- as.matrix(s3read_using(FUN = readRDS,
             object = paste(set_wd2,"/L_2019.rds",sep=""),
             bucket = bucket2, opts = list("region" = "")))

s <- as.matrix(s)

## reduction of 10% final demand on specific sectors of EU
s <- -(s * 0.1)

k <- L %*% s

## putting some labels to the shock and calculating impact on output

k <- cbind(label_IO, k)

k <- k[which(k$V1 %in% eu),]

x <- x[which(k$V1 %in% eu),]

k <- cbind(k, x)

colnames(k)[4] <- "loss"

k <- k %>%
  mutate(share= abs(loss) / x * 100)


nace <- read_excel("data/NACE-Gloria.xlsx", sheet = "Feuil1")

k <- k %>%
  left_join(nace, by = c("V3"="Gloria"))

k <- k[, -3]

k <- k %>%
  group_by(V1, V2, NACE) %>%
  summarise(
    loss = sum(loss),
    x = sum(x),
    share = sum(share)
  )

rm("L")

s3write_using(x = as.data.frame(k), FUN = data.table::fwrite, na = "", 
              object = paste("data/Gloria/k_2019.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))


# Checking bach data




### recalculating the leontief

label_IO <- label_IO %>%
  mutate(id = paste(V1, V3, sep = " "))

score <- score %>%
  mutate(id = paste(iso, sector, sep = " "))

index3 <- which(label_IO$id %in% score$id)


Z <- as.matrix(s3read_using(FUN = readRDS,
                  object = paste(set_wd2,"/IO_2019.rds",sep=""),
                  bucket = bucket2, opts = list("region" = "")))

Z <- Z[index3, index3]

x <- x[index3,]

x <- x + 0.0001

A <- Z %*% solve(diag(x))    # Technical coefficients matrix

# recalculated reduced version of matrix

library("leontief")

L <- leontief_inverse(A)

L <- solve(diag(dim(A)[1])-A)


s3write_using(x = as.data.frame(L), FUN = data.table::fwrite, na = "", 
              object = paste("data/Gloria/L1_2019.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))

s3write_using(x = as.data.frame(A), FUN = data.table::fwrite, na = "", 
              object = paste("data/Gloria/A1_2019.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))




L <- as.matrix(s3read_using(FUN = data.table::fread,
                object = paste(set_wd2,"/L1_2019.rds",sep=""),
                bucket = bucket2, opts = list("region" = "")))

rm("A", "Z")

## calculate all value chain impacts on biodiversity

b <- score$score

b <- b / x

tL <- as.matrix(t(L))

db <- diag(b)

sourceCpp("C/test.cpp")

E <- eigenMapMatMult(tL, db)

E <- tL %*% db


s3write_using(x = as.data.frame(E), FUN = data.table::fwrite, na = "", 
              object = paste("data/Gloria/E1_2019.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))