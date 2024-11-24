rm(list = ls())
gc()


library(Matrix)
library(tidyverse)

# useful function
agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x) }


bucket1 <- "projet-esteem"
set_wd1 <- "Gloria/matrices"

bucket2 <- "siwar"
set_wd2 <- "data/Gloria"
set_wd3 <- "data/bio/rds"

# read labels
io <- as.data.table(s3read_using(FUN = readRDS,
                          object = paste(set_wd2,"/label_IO.rds",sep=""),
                          bucket = bucket2, opts = list("region" = "")))

fd <- s3read_using(FUN = data.table::fread,
                         object = paste("Gloria/labels/label_FD.rds",sep=""),
                         bucket = bucket1, opts = list("region" = ""))

colnames(io) <- c("iso", "country", "sector")
colnames(fd) <- c("iso", "country", "category")



# read data
L <- s3read_using(FUN = readRDS,
                  object = paste(set_wd2,"/L_2019.rds",sep=""),
                  bucket = bucket2, opts = list("region" = ""))

X <- s3read_using(FUN = readRDS,
                  object = paste(set_wd2,"/X_2019.rds",sep=""),
                  bucket = bucket2, opts = list("region" = ""))

Y <- s3read_using(FUN = readRDS,
                  object = paste(set_wd2,"/Y_2019.rds",sep=""),
                  bucket = bucket2, opts = list("region" = ""))

ext <- s3read_using(FUN = readRDS,
                   object = paste(set_wd2,"/score_pays-v3.rds",sep=""),
                   bucket = bucket2, opts = list("region" = ""))



# handling final demand and ext
colnames(Y) <- fd$country
Yc <- agg(Y)

X <- X + 0.0001
ext <- ext / X


#settings
countries <- unique(io$country)



# footprinting
MP <- ext * L

results_all <- list()

for (country in countries) {
  print(country)
  
  com <- sum(Yc[, country] * t(MP[which(io$country == country),]))
  prd <- sum(rowSums(Yc) * t(MP[which(io$country == country),]))
  exp <- sum(rowSums(Yc[, colnames(Yc) != country]) * t(MP[which(io$country == country),]))
  imp <- sum(Yc[, country] * t(MP[which(io$country != country),]))
  
  results_all[[length(results_all) + 1]] <- data.table(
    country = country,
    com = as.numeric(com),
    prd = as.numeric(prd),
    exp = as.numeric(exp),
    imp = as.numeric(imp))
  
}

results <- rbindlist(results_all)

# nfp = prod - exp + imp
results <- results %>%
  mutate(nfp = prd - exp + imp)

results <- results %>%
  mutate(type = case_when(
    imp > prd ~ "Net Importer",
    exp > com ~ "Net Exporter",
    com > exp ~ "Net Domestic Consumer"
  ))

s3write_using(x = as.data.table(results), FUN = saveRDS, 
              object = paste(set_wd3,"/results-v2.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))


# footprinting of specific pressures
star <- s3read_using(FUN = readRDS,
                     object = paste(set_wd3,"/star_satellites.rds",sep=""),
                     bucket = bucket2, opts = list("region" = ""))


pa <- c("2572", "2426", "2280", "72", "1915", "1842")

colnames(star)

pressure_results <- list()
for (var in pa) {
  print(var)
  MP <- star[,var] * L
  
  for (country in countries) {
    fp <- sum(MP %*% Yc[,country])
    
    pressure_results[[length(pressure_results) + 1]] <- data.table(
      country = country,
      var = var,
      value = fp)
    
  }
}


p_results <- rbindlist(pressure_results)

p_results <- p_results %>%
  pivot_wider(names_from = var, values_from = value)
