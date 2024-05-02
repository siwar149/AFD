#### Real side analysis

library("tidyr")
install.packages("Matrix")

##### STAR metric

### setting bucket and all

bucket1 = "projet-esteem"
bucket2 = "siwar"

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

label_IO <- as.data.frame(s3read_using(FUN = readRDS,
                  object = paste(set_wd2,"/label_IO.rds",sep=""),
                  bucket = bucket2, opts = list("region" = "")))

colnames(label_IO) <- c("iso", "country", "sector")

e <- label_IO %>%
  left_join(score, by = c("iso", "sector")) %>%
  mutate(score= if_else(is.na(score), 0, score)) %>%
  select(score)


e <- e$score / x$x
e[is.na(e)] <- 0 ## handle na's bc yemen doesn't have data on output
E <- diag(e)

# now we get the final demand for european countries
eu <-  c('AUT', 'BEL', 'BGR', 'HRV', 'CYP', 'CZE', 'DNK', 'EST',
         'FIN', 'FRA', 'DEU', 'GRC', 'HUN', 'IRL', 'ITA', 'LVA', 
         'LTU', 'LUX', 'MLT', 'NLD', 'POL', 'PRT', 'ROU', 'SVK', 
         'SVN', 'ESP', 'SWE', 'XEU')

#eu1 <- c("AUT", "BEL", "DEU", "ESP", "FRA", "HRV", "HUN", "ITA",
#         "LUX", "POL", "PRT", "SVK")

f <- as.data.frame(s3read_using(FUN = data.table::fread,
                                object = paste(set_wd1,"/FD_2019.rds",sep=""),
                                bucket = bucket1, opts = list("region" = "")))

label_f <- as.data.frame(s3read_using(FUN = readRDS,
                                      object = paste(set_wd2,"/label_FD.rds",sep=""),
                                      bucket = bucket2, opts = list("region" = "")))

f1 <- f[, which(label_f$V1 %in% eu)]
#f2 <- f[, which(label_f$V1 %in% eu1)]


## Get the set of final demands where there are negative values
index1 <- as.numeric(sub("V", "", names(which(apply(f1, 2, function(col) sum(col < 0) > 0)))))
#index2 <- as.numeric(sub("V", "", names(which(apply(f2, 2, function(col) sum(col < 0) > 0)))))

label_f[index1,] # one can see that changes in inventories have negative values
#label_f[index2,]

View(f[which(apply(f1, 1, function(row) any(row < 0))), index1])

# Then we exclude changes in inventories from the footprint calculation
f1 <- as.matrix(f1[,-which(apply(f1, 2, function(col) sum(col < 0) > 0))])


L <- as.matrix(s3read_using(FUN = data.table::fread,
                            object = paste(set_wd1,"/L_2019.rds",sep=""),
                            bucket = bucket1, opts = list("region" = "")))


# calculate the footprint matrix
plcy <- E %*% L %*% f1

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
  mutate(sum_value = sum(value)) %>%
  mutate(share = value / sum_value)


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
  mutate(sum_value = sum(value)) %>%
  mutate(share = value / sum_value)

rm(plcy_sum_ext_long, plcy_sum_eu_long, plcy_sum_ext, plcy_sum_eu, k, g)


top_12_eu <- top_12_eu %>%
  mutate(region = "eu")

top_12_ext <- top_12_ext %>%
  mutate(region = "ext")


top_12 <- rbind(top_12_eu, top_12_ext)
rm(top_12_eu, top_12_ext)



is_eu <- label_IO$iso %in% eu


dplcy <- label_IO %>%
  mutate(region = if_else(is_eu, "eu", "ext"),
         id = paste(sector, region))

top_12_merge <- top_12 %>%
  mutate(id = paste(sector, region)) %>%
  select(variable, id, share)

for (i in names(plcy_sum)) {
  dplcy <- dplcy %>%
    left_join(top_12[top_12$variable == i,], by = "id") %>%
    select(-variable) %>%
    rename(!!i := share)
}


dplcy[, 6:33] <- dplcy[, 6:33] * -1

dplcy <- replace(dplcy, is.na(dplcy), 0)

dplcy <- dplcy[, 6:33]
  

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

# solve singularity
e <- as.matrix(e)
e1 <- t(L) %*% e

df <- dp 

for (i in colnames(df)) {
  df[, i] <- dp[, i] / e1
}


df <- replace(df, is.na(df), 0)
 
# Calculate variation in output
dx <- L %*% df

s3write_using(x = as.data.table(dx), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd2,"/dx.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))


###############################################
score_ext <- score[which(!score$iso %in% eu),]
score_eu <- score[which(score$iso %in% eu),]

non_eu <- unique(score_ext$iso)


nace <- read_excel("data/NACE-Gloria.xlsx", sheet = "Feuil1")

rs_pressure <- rs_pressure %>%
  left_join(nace, by = c("sector"="Gloria"))

score1_ext <- rs_pressure[which(!rs_pressure$iso %in% eu),]
score1_eu <- rs_pressure[which(rs_pressure$iso %in% eu),]

### At pressure level

cSectors1_ext <- score1_ext %>%
  group_by(iso, country, sector, Lfd_Nr) %>%
  summarise(star = sum(score_sum)) %>%
  group_by(iso, country) %>%
  top_n(2, wt = star) %>%
  arrange(desc(star))

country_sum <- score1_ext %>%
  group_by(iso, country) %>%
  summarise(total_star = sum(score_sum))

cSectors1_ext <- cSectors1_ext %>%
  left_join(country_sum, by = c("iso", "country")) %>%
  mutate(share = star / total_star)

s3write_using(x = as.data.frame(cSectors1_ext), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd2,"/cSectors_ext.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))

### at sector level of NACE
country_sum <- score1_ext %>%
  group_by(iso, country, NACE) %>%
  summarise(total_star = sum(score_sum)) %>%
  arrange(desc(total_star)) %>%
  slice_head(n = 1)



cSectors1_eu <- score1_eu %>%
  group_by(iso) %>%
  arrange(desc(score_sum)) %>%
  slice_head(n = 12)


### At sector level
cSectors_ext <- score_ext %>%
  group_by(iso) %>%
  arrange(desc(score)) %>%
  slice_head(n = 1)

cSectors_eu <- score_eu %>%
  group_by(iso) %>%
  arrange(desc(score)) %>%
  slice_head(n = 1)

pSectors_ext <- unique(cSectors_ext$sector)
pSectors_eu <- unique(cSectors_eu$sector)

rm("score")



### Just keeping the final demand from EU and certain types of demand

td <- c("Household final consumption P.3h", "Government final consumption P.3g")



### We get only the household consumption from final demand and EU




#f[which(label_IO$V1 %in% eu),] <- 0

f1[which(label_IO$V1 %in% non_eu & !label_IO$V3 %in% pSectors_ext), ] <- 0
f1[which(label_IO$V1 %in% eu & !label_IO$V3 %in% pSectors_eu), ] <- 0

s <- f1

rm("f1")


s3write_using(x = as.data.frame(s), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd2,"/s1_2019.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))


### check final output

x <- s3read_using(FUN = data.table::fread,
                    object = paste(set_wd1,"/x_2019.rds",sep=""),
                    bucket = bucket1, opts = list("region" = ""))

colnames(x) <- "output"


##### Now simulate demand #####

L <- as.matrix(s3read_using(FUN = data.table::fread,
             object = paste(set_wd1,"/L_2019.rds",sep=""),
             bucket = bucket1, opts = list("region" = "")))


s <- as.matrix(s)


### reduction of 1% final demand on specific sectors of EU and EXT
s <- -(s * 0.01)

k <- L %*% s

### Keeping the results for the relevant countries

k <- cbind(k, x)

k <- cbind(label_IO, k)

k <- k[which(k$V1 %in% eu),]

colnames(k)[4] <- "loss"

s3write_using(x = as.data.frame(k), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd2,"/k_2019.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))


### Add a column with the NACE sectors
nace <- read_excel("data/NACE-Gloria.xlsx", sheet = "Feuil1")

k <- k %>%
  left_join(nace, by = c("V3"="Gloria"))

### Drop anomalous values

k <- k %>%
  filter(abs(loss) <= output)

k <- k[, -3]

k <- k %>%
  group_by(V1, V2, NACE) %>%
  summarise(
    loss = sum(loss),
    output = sum(output)
  )

k <- k %>%
  mutate(rshare= abs(loss) / output * 100)

rm("L")

s3write_using(x = as.data.frame(k), FUN = data.table::fwrite, na = "", 
              object = paste("data/Gloria/k5_2019.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))


k <- as.data.frame(s3read_using(FUN = data.table::fread,
              object = paste(set_wd2,"/k1_2019.rds",sep=""),
              bucket = bucket2, opts = list("region" = "")))


# Checking bach data

bach <- read.csv("data/export-bach-2019.csv", sep = ";", header = T)
iso <- read_excel("data/iso.xlsx", sheet = "Sheet1")

k <- k %>%
  left_join(iso, by = c("V1"="iso"))

sample <- unique(bach$country)

k <- k[which(k$eu %in% sample), ]


s3write_using(x = as.data.frame(k), FUN = data.table::fwrite, na = "", 
              object = paste("data/Gloria/k_2019.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))



##################################################################
################## Other paramters ###############################
##################################################################

QT <- s3read_using(FUN = data.table::fread,
             object = "Gloria/matrices/QT_2019.rds",
             bucket = bucket1, opts = list("region" = ""))


label_Q <- s3read_using(FUN = data.table::fread,
                          object = "Gloria/labels/label_Q.rds",
                          bucket = bucket1, opts = list("region" = ""))




