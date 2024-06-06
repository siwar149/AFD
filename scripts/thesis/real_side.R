#### Real side analysis

library("tidyr")
#library("MASS")

##### STAR metric

### setting bucket and all

bucket1 <- "projet-esteem"
bucket2 <- "siwar"

set_wd1 <- "Gloria/matrices"
set_wd2 <- "data/Gloria"
set_wd3 <- "data/bio/rds"


### Calculate the nSTAR footprint of EU

# european countries
eu <-  c('AUT', 'BEL', 'BGR', 'HRV', 'CYP', 'CZE', 'DNK', 'EST',
         'FIN', 'FRA', 'DEU', 'GRC', 'HUN', 'IRL', 'ITA', 'LVA', 
         'LTU', 'LUX', 'MLT', 'NLD', 'POL', 'PRT', 'ROU', 'SVK', 
         'SVN', 'ESP', 'SWE', 'XEU')

eu1 <- c("AUT", "BEL", "DEU", "ESP", "FRA", "HRV", "HUN", "ITA",
         "LUX", "POL", "PRT", "SVK")

# load the relevant matrices and vectors
label_IO <- as.data.table(s3read_using(FUN = readRDS,
                                       object = paste(set_wd2,"/label_IO.rds",sep=""),
                                       bucket = bucket2, opts = list("region" = "")))

colnames(label_IO) <- c("iso", "country", "sector")

mcf <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd2,"/mcf.rds",sep=""),
                  bucket = bucket2, opts = list("region" = ""))

f <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd2,"/f_2019.rds",sep=""),
                  bucket = bucket2, opts = list("region" = ""))


# Actual nSTAR consumption based footprint
t <- mcf * f

in_eu1 <- which(label_IO$iso %in% eu1)
not_eu <- which(!label_IO$iso %in% eu1)

summary(mcf[in_eu1,])



tis <- t

tis[not_eu] <- 0
tis[tis$V1 < 0,] <- 0
t[t$V1 < 0,] <- 0

# decomposing the shock
dt <- -0.01 * sum(tis) * (tis / sum(tis)) # the mistake was here
                                          # it was all about the weights

View(tis / sum(tis))

# Calculate variation in demand
df <- dt / mcf
summary(abs(df[in_eu1]) / f[in_eu1])

# Calculate variation in output
L <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd1,"/L_2019.rds",sep=""),
                  bucket = bucket1, opts = list("region" = ""))

df <- as.matrix(df)
df <- as.numeric(df)

dx <- as.matrix(L) %*% df

x <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd1,"/x_2019.rds",sep=""),
                  bucket = bucket1, opts = list("region" = ""))

x <- x + 0.0001 # No data on Yemen's output

summary(abs(dx[in_eu1]) / x[in_eu1])

#s3write_using(x = as.data.table(dx), FUN = data.table::fwrite, na = "", 
#              object = paste(set_wd2,"/dx.rds",sep=""),
#              bucket = bucket2, opts = list("region" = ""))

f <- as.data.table(f)

g <- cbind(dx, x, df, f)

colnames(g)[c(1,3:4)] <- c("dx", "df", "f")

g <- cbind(label_IO, g)

g <- g[in_eu,]

s3write_using(x = as.data.table(g), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd2,"/g_2019.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))


### Add a column with the NACE sectors
nace <- read_excel("data/NACE-Gloria.xlsx", sheet = "Feuil1")

g <- g %>%
  left_join(nace, by = c("sector"="Gloria"))


g <- g[, -3]

g <- g %>%
  group_by(iso, country, NACE) %>%
  summarise(
    dx = sum(dx),
    x = sum(x),
    df = sum(df),
    f = sum(f)
  )

g <- g %>%
  mutate(abvarx= abs(dx) / x * 100,
         abvarf= abs(df) / f * 100)


s3write_using(x = as.data.table(g), FUN = data.table::fwrite, na = "", 
              object = paste("data/Gloria/g_1_2019.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))


# Keeping only the countries in the BACH data

bach <- read.csv("data/export-bach-2019.csv", sep = ";", header = T)
iso <- read_excel("data/iso.xlsx", sheet = "Sheet1")

g <- g %>%
  left_join(iso, by = c("iso"="iso"))

sample <- unique(bach$country)

g <- g[which(g$eu %in% sample), ]


s3write_using(x = as.data.table(g), FUN = data.table::fwrite, na = "", 
              object = paste("data/Gloria/g1_2_2019.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))
