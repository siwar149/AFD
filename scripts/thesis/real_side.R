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

### First find the most impactful sectors per country


rs_pressure <- s3read_using(FUN = readRDS,
             object = paste(set_wd3,"/redlist_score_per_pressure.rds",sep=""),
             bucket = bucket2, opts = list("region" = ""))

### Calculate the nSTAR footprint of EUs final demand
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


# european countries
eu <-  c('AUT', 'BEL', 'BGR', 'HRV', 'CYP', 'CZE', 'DNK', 'EST',
         'FIN', 'FRA', 'DEU', 'GRC', 'HUN', 'IRL', 'ITA', 'LVA', 
         'LTU', 'LUX', 'MLT', 'NLD', 'POL', 'PRT', 'ROU', 'SVK', 
         'SVN', 'ESP', 'SWE', 'XEU')

#eu1 <- c("AUT", "BEL", "DEU", "ESP", "FRA", "HRV", "HUN", "ITA",
#         "LUX", "POL", "PRT", "SVK")

f <- s3read_using(FUN = data.table::fread,
                        object = paste(set_wd1,"/FD_2019.rds",sep=""),
                        bucket = bucket1, opts = list("region" = ""))

label_f <- as.data.table(s3read_using(FUN = readRDS,
                              object = paste(set_wd2,"/label_FD.rds",sep=""),
                              bucket = bucket2, opts = list("region" = "")))
f1 <- rowSums(f)

#f1 <- f[, which(label_f$V1 %in% eu)]
#f2 <- f[, which(label_f$V1 %in% eu1)]


## Get the set of final demands where there are negative values
index1 <- as.numeric(sub("V", "", names(which(apply(f1, 2, function(col) sum(col < 0) > 0)))))
#index2 <- as.numeric(sub("V", "", names(which(apply(f2, 2, function(col) sum(col < 0) > 0)))))

label_f[index1,] # one can see that changes in inventories have negative values
#label_f[index2,]

View(f[which(apply(f1, 1, function(row) any(row < 0))), index1])

# Then we exclude changes in inventories from the footprint calculation
f1 <- as.matrix(f1[,-which(apply(f1, 2, function(col) sum(col < 0) > 0))])


L <- s3read_using(FUN = data.table::fread,
                      object = paste(set_wd1,"/L_2019.rds",sep=""),
                      bucket = bucket1, opts = list("region" = ""))

# compute vector with multipliers of consumption footprint
mcf <- t(as.matrix(L)) %*% e

s3write_using(x = as.data.table(mcf), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd2,"/mcf.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))

# Actual nSTAR consumption based footprint
t <- mcf * f1

s3write_using(x = as.data.table(t), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd2,"/t.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))

tis <- t

in_eu <- which(label_IO$iso %in% eu)
not_eu <- which(!label_IO$iso %in% eu)

tis[not_eu] <- 0

dt <- -0.01 * sum(t[in_eu]) * (tis / sum(t))

# Calculate variation in demand
dF <- dt / mcf
abs(dF) / F1

# Calculate variation in output
dx <- as.matrix(L) %*% dF
abs(dx) / x

#s3write_using(x = as.data.table(dx), FUN = data.table::fwrite, na = "", 
#              object = paste(set_wd2,"/dx.rds",sep=""),
#              bucket = bucket2, opts = list("region" = ""))

f1 <- as.data.table(f1)

g <- cbind(dx, x, dF, f1)

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
              object = paste("data/Gloria/g_2_2019.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))


### Flow matrix for world map
T <- diag(e) %*% as.matrix(L) %*% diag(f1)


s3write_using(x = as.data.table(T[,in_eu]), FUN = data.table::fwrite, na = "", 
              object = paste("data/Gloria/Teu.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))





# Convert to data.table
plcy <- as.data.table(plcy)


# Define groups of 5 contiguous columns

# Define group size
group_size <- 5

# Calculate number of groups
num_groups <- ncol(plcy) %/% group_size

# Initialize empty list to store results
plcy_sum <- list()

# Loop through each group and calculate row sums
for (i in 0:(num_groups - 1)) {
  plcy_sum[[i + 1]] <- plcy[, rowSums(.SD, na.rm = TRUE), 
                            .SDcols = ((i * group_size + 1):((i + 1) * group_size))]
}

# Combine results into a single data.table
plcy_sum <- as.data.table(do.call(cbind, plcy_sum))

# Assign the country labels to each final demand footprint
colnames(plcy_sum) <- label_f[index1,]$V1

# Start the analysis for EU and non-EU countries
plcy_sum_eu <- cbind(label_IO[which(label_IO$iso %in% eu),], plcy_sum[which(label_IO$iso %in% eu),])
plcy_sum_ext <- cbind(label_IO[which(!label_IO$iso %in% eu),], plcy_sum[which(!label_IO$iso %in% eu),])


# EU
plcy_sum_eu <- plcy_sum_eu %>%
  group_by(sector) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))

# Reshape the data to long format
plcy_sum_eu_long <- plcy_sum_eu %>%
  pivot_longer(cols = -sector, names_to = "variable", values_to = "value")

# Get the top 12 values for each numeric variable
top_12_eu <- plcy_sum_eu_long %>%
  group_by(variable) %>%
  top_n(12, value) %>%
  mutate(sum_value = sum(value),
         share = value / sum_value,
         shock = 0.01)


# EXT
plcy_sum_ext <- plcy_sum_ext %>%
  group_by(sector) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))

# Reshape the data to long format
plcy_sum_ext_long <- plcy_sum_ext %>%
  pivot_longer(cols = -sector, names_to = "variable", values_to = "value")

# Get the top 12 values for each numeric variable
top_12_ext <- plcy_sum_ext_long %>%
  group_by(variable) %>%
  top_n(12, value) %>%
  mutate(sum_value = sum(value),
         share = value / sum_value,
         shock = 0.01)

rm(plcy_sum_ext_long, plcy_sum_eu_long, plcy_sum_ext, plcy_sum_eu)


top_12_eu <- top_12_eu %>%
  mutate(region = "eu")

top_12_ext <- top_12_ext %>%
  mutate(region = "ext")


top_12 <- rbind(top_12_eu, top_12_ext)
rm(top_12_eu, top_12_ext)


# create a "shock" matrix
is_eu <- label_IO$iso %in% eu


dplcy <- label_IO %>%
  mutate(region = if_else(is_eu, "eu", "ext"),
         id = paste(sector, region)) %>%
  select(-sector, -region)

# changing shock
top_12_merge <- top_12 %>%
  mutate(id = paste(sector, region)) %>%
  select(variable, id, shock)

for (i in names(plcy_sum)) {
  dplcy <- dplcy %>%
    left_join(top_12_merge[top_12_merge$variable == i,], by = "id") %>%
    select(-variable) %>%
    rename(!!i := shock)
}

dplcy <- dplcy %>%
  select(-c(iso, country, id))

dplcy[is.na(dplcy)] <- 0
  
dplcy <- dplcy * -1


s3write_using(x = as.data.table(dplcy), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd2,"/dplcy.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))

s3write_using(x = as.data.table(plcy_sum), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd2,"/plcy_sum.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))

s3write_using(x = as.data.table(E), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd2,"/E.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))

### Calculate the final demand that corresponds to the reduction in biodiversity footprint
dplcy <- as.matrix(dplcy)
plcy_sum <- as.matrix(plcy_sum)

dp <- dplcy * plcy_sum

rm(dplcy, plcy_sum)


s3write_using(x = as.data.table(dp), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd2,"/dp.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))

# solve singularity
Einv <- ginv(diag(e)%*%L)

df <- matrix(data = NA, nrow = nrow(dp), ncol = ncol(dp))

colnames(df) <- colnames(dp)

for (i in colnames(df)) {
  df[, i] <- dp[, i] / e1
}

df[is.na(df)] <- 0


s3write_using(x = as.data.table(df), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd2,"/df1.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))

 
# Calculate variation in output
dx <- L %*% df

s3write_using(x = as.data.table(dx), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd2,"/dx1.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))


dx <- s3read_using(FUN = data.table::fread,
                   object = paste(set_wd2,"/dx.rds",sep=""),
                   bucket = bucket2, opts = list("region" = ""))

dx <- as.data.table(rowSums(dx))





