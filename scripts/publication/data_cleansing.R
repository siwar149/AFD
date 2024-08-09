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

colnames(label_IO) <- c("iso", "country", "sector")

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

## Getting backward and forward induced extinction-risk
# backward

L <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd1,"/L_2019.rds",sep=""),
                  bucket = bucket1, opts = list("region" = ""))

bk <- diag(as.integer(as.matrix(e))) %*% as.matrix(L) - diag(as.integer(as.matrix(e)))

bk <- as.data.table(colSums(bk))

# forward
fr <- as.matrix(G) %*% diag(as.integer(as.matrix(e))) - diag(as.integer(as.matrix(e)))

fr <- as.data.table(rowSums(fr))

eu1 <- c("AUT", "BEL", "DEU", "ESP", "FRA", "HRV", "HUN", "ITA",
         "LUX", "POL", "PRT", "SVK")


# weird results here
bk_eu <- cbind(label_IO[which(label_IO$iso %in% eu1),],
               bk[which(label_IO$iso %in% eu1),])

# I got negative values in this one (def not doing something right)
fr_eu <- cbind(label_IO[which(label_IO$iso %in% eu1),],
               fr[which(label_IO$iso %in% eu1),])
# Interesting but not useful because I know what sectors I am goint to shock
rm("bk", "fr", "fr_eu", "bk_eu", "L", "G")

#### Using the Gosh model to measure the impact of halting exports of 
# agricultural products to Europe

B <- as.data.table(s3read_using(FUN = data.table::fread,
                  object = paste(set_wd2,"/B_2019.rds",sep=""),
                  bucket = bucket2, opts = list("region" = "")))

# countries where the shock comes from
latam6 <- c("HND", "COL", "BRA", "GTM", "PER", "ECU")

nace <- read_excel("data/NACE-Gloria.xlsx", sheet = "Feuil1")

label_IO <- label_IO %>%
  left_join(nace, by = c("sector"="Gloria"))

in_eu <- which(label_IO$iso %in% eu1)

latam6A <- which(label_IO$iso %in% latam6 & label_IO$NACE == "A")

# Shocking only the exports to EU countries
B[latam6A, in_eu] <- 0

v <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd1,"/VA_2019.rds",sep=""),
                  bucket = bucket1, opts = list("region" = ""))

v <- as.data.table(colSums(v))

label_VA <- s3read_using(FUN = data.table::fread,
                  object = paste("Gloria/labels/label_VA.rds",sep=""),
                  bucket = bucket1, opts = list("region" = ""))

Grj <- diag(dim(B)[1]) - B
Grj <- solve(Grj)
Grj <- as.data.table(Grj)

x1 <- t(as.matrix(v)) %*% as.matrix(Grj)

x1 <- as.data.table(t(x1))
x <- as.data.table(x)

dx <- (x1 - x)/x

dx <- cbind(label_IO[in_eu,], dx[in_eu])


# Starting to look at exposure

exp <- as.data.table(colSums(Z[latam6A, ..in_eu]))

exp <- exp / x[in_eu,] * 100

exp <- cbind(label_IO[in_eu,], exp)

exp1 <- exp %>%
  group_by(iso, country, NACE) %>%
  summarise(V1 = sum(V1)) %>%
  arrange(desc(V1))

head(exp1, 28) %>% print(n = 28)

