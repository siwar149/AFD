
# bucket1 <- "projet-esteem"
# bucket2 <- "siwar"
# 
# set_wd1 <- "Gloria/matrices"
# set_wd2 <- "data/Gloria"
# set_wd3 <- "data/bio/rds"

## Get the nSTAR intensity vector
#score <- s3read_using(FUN = readRDS,
#                      object = paste(set_wd3,"/score_pays.rds",sep=""),
#                      bucket = bucket2, opts = list("region" = ""))

library(Matrix)

path <- '/mnt/nfs_fineprint/tmp/gloria/v059-compiled/'

score3 <- readRDS("rds/score_pays.rds")


x <- readRDS(paste0(path, "X/X_2019.rds"))

x <- x + 0.000001 # No data on Yemen's output   

label_IO <- as.data.frame(readRDS("rds/label_IO.rds"))


colnames(label_IO) <- c("iso", "country", "sector")

#e <- label_IO %>%
#  left_join(score, by = c("iso", "sector")) %>%
#  mutate(score= if_else(is.na(score), 0.0001, score))

e3 <- label_IO %>%
  left_join(score3, by = c("iso", "sector")) %>%
  mutate(score= if_else(is.na(score), 0, score))

e3 <- e3$score / x

saveRDS(e3,"rds/e3_2019.rds")


# Global demand vector
f <- readRDS(paste0(path, "Y/Y_2019.rds"))


f <- rowSums(f)


#s3write_using(x = as.data.table(f), FUN = data.table::fwrite, na = "", 
#              object = paste(set_wd2,"/f_2019.rds",sep=""),
#              bucket = bucket2, opts = list("region" = ""))


L <- readRDS(paste0(path, "L/L_2019.rds"))

# compute vector with multipliers of consumption footprint
mcf3 <- t(L) %*% e3$score

MP <- e3 * L

saveRDS(mcf3, "rds/mcf3.rds")
saveRDS(MP, 'rds/MP.rds')

### Flow matrix for world map
T <- diag(e3$score) %*% L %*% diag(f)

eu1 <- c("AUT", "BEL", "DEU", "ESP", "FRA", "HRV", "HUN", "ITA",
         "LUX", "POL", "PRT", "SVK")

in_eu1 <- which(label_IO$iso %in% eu1)


saveRDS(as(T[,in_eu1], 'dgeMatrix'), "rds/Teu3.rds")


rm(list = ls())
gc()

# VARIATION IN INVENTORIES IS NEGATIVE FOR ALL COUNTRIES
## Get the set of final demands where there are negative values
#index1 <- as.numeric(sub("V", "", names(which(apply(f1, 2, function(col) sum(col < 0) > 0)))))
#label_f[index1,] # one can see that changes in inventories have negative values
#View(f[which(apply(f1, 1, function(row) any(row < 0))), index1])
# Then we exclude changes in inventories from the footprint calculation
#f1 <- as.matrix(f1[,-which(apply(f1, 2, function(col) sum(col < 0) > 0))])



# DO THIS ANALYSIS BUT WITH THE Teu MATRIX
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










# comes in handy
