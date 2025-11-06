#### Real side analysis

library("tidyr")
#library("MASS")

##### STAR metric

### setting bucket and all

# bucket1 <- "projet-esteem"
# bucket2 <- "siwar"
# 
# set_wd1 <- "Gloria/matrices"
# set_wd2 <- "data/Gloria"
# set_wd3 <- "data/bio/rds"

path <- '/mnt/nfs_fineprint/tmp/gloria/v059-compiled/'

### Calculate the nSTAR footprint of EU

# european countries
eu <-  c('AUT', 'BEL', 'BGR', 'HRV', 'CYP', 'CZE', 'DNK', 'EST',
         'FIN', 'FRA', 'DEU', 'GRC', 'HUN', 'IRL', 'ITA', 'LVA', 
         'LTU', 'LUX', 'MLT', 'NLD', 'POL', 'PRT', 'ROU', 'SVK', 
         'SVN', 'ESP', 'SWE', 'XEU')

eu1 <- c("AUT", "BEL", "DEU", "ESP", "FRA", "HRV", "HUN", "ITA",
         "LUX", "POL", "PRT", "SVK")

# load the relevant matrices and vectors
label_IO <- as.data.frame(readRDS("rds/label_IO.rds"))

colnames(label_IO) <- c("iso", "country", "sector")

mcf3 <- readRDS("rds/mcf3.rds")

f <- readRDS(paste0(path, 'Y/Y_2019.rds'))

f <- rowSums(f)

# Actual nSTAR consumption based footprint
t <- mcf3 * f

in_eu1 <- which(label_IO$iso %in% eu1)
not_eu <- which(!label_IO$iso %in% eu1)

summary(mcf3[in_eu1,])



tis <- t

tis[not_eu] <- 0
tis[tis < 0] <- 0

# decomposing the shock
dt <- -0.01 * sum(tis) * (tis / sum(tis)) # the mistake was here
                                          # it was all about the weights

View(tis / sum(tis))

# Calculate variation in demand
df <- dt / mcf3
summary(abs(df[in_eu1]) / f[in_eu1])

# Calculate variation in output
L <- readRDS(paste0(path, "L/L_2019.rds"))

df <- as.numeric(df)

dx <- L %*% df

x <- readRDS(paste0(path, "X/X_2019.rds"))

x <- x + 0.0001 # No data on Yemen's output

summary(abs(dx[in_eu1]) / x[in_eu1])

saveRDS(dx,"matrices/dx3.rds")

f <- as.data.table(f)

g <- cbind(dx, x, df, f)

colnames(g)[c(1,3:4)] <- c("dx", "df", "f")

g <- cbind(label_IO, g)

g <- g[in_eu1,]

saveRDS(g,"matrices/g3_2019.rds")

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
    f = sum(f), .groups = 'drop'
  )

g <- g %>%
  mutate(abvarx= abs(dx) / x * 100,
         abvarf= abs(df) / f * 100)


saveRDS(g, "matrices/g3_1_2019.rds")


# Keeping only the countries in the BACH data

bach <- read.csv("data/export-bach-2019.csv", sep = ";", header = T)
iso <- read_excel("data/iso.xlsx", sheet = "Sheet1")

g <- g %>%
  left_join(iso, by = c("iso"="iso"))

sample <- unique(bach$country)

g <- g[which(g$eu %in% sample), ]


saveRDS(g,"matrices/g3_2_2019.rds")

rm(list = ls())
gc()
