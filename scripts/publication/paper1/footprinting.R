rm(list = ls())
gc()

install.packages("ggrepel")
library(ggrepel) 


# bucket1 <- "projet-esteem"
# set_wd1 <- "Gloria/matrices"
# 
# bucket2 <- "siwar"
# set_wd2 <- "data/Gloria"
# set_wd3 <- "data/bio/rds"

# upload the star value of all pressures
star <- readRDS("rds/star_satellites.rds")

label_IO <- as.data.frame(readRDS("rds/label_IO.rds"))

colnames(label_IO) <- c("iso", "country", "sector")

nace <- read_excel("data/NACE-Gloria.xlsx", sheet = "Feuil1")

label_IO <- label_IO %>%
  left_join(nace, by = c("sector"="Gloria"))

rm(nace)

star <- label_IO %>%
  left_join(star, by = c("iso", "country", "sector"))

star[is.na(star)] <- 0

#summary(star[,5:108])

## get the pressure name
biotope <- readRDS("matrices/biotope_threats.rds")

press <- as.numeric(colnames(star)[c(5:108)])

biotope <- biotope[which(biotope$Lfd_Nr %in% press),]

# fixing some values
biotope[Lfd_Nr %in% c(68:72), pressure := paste0(pressure, " (Land use)")]
biotope[Lfd_Nr %in% c(80:84), pressure := paste0(pressure, " (PDF)")]

pressures <- biotope %>%
  select(-threat) %>%
  unique()



#loading output data
x <- readRDS(paste0(path,"X/X_2019.rds"))

x <- x + 0.0001 # No data on Yemen's output

star <- star[, 5:108]

### Find the top 10 pressures
t10 <- names(sort(colSums(star), decreasing = T)[1:10])
pressures[pressures$Lfd_Nr %in% t10,]

e <- t(t(star) / x)


path <- '/mnt/nfs_fineprint/tmp/gloria/v059-compiled/'
# loading final demand and leontief matrix
FD <- readRDS(paste0(path,"Y/Y_2019.rds"))

label_FD <- readRDS(paste0("rds/label_FD.rds"))

colnames(label_FD) <- c("iso", "country", "sector")

L <- readRDS(paste0(path,"L/L_2019.rds"))

f <- rowSums(FD)

sr <- readRDS("rds/score_pays.rds")

sr <- label_IO %>%
  left_join(sr, by = c("iso", "sector")) %>%
  mutate(score= if_else(is.na(score), 0.0001, score)) %>%
  select(score)

e1 <- sr / x


##### Starting with the footprinting analysis

# finding the pressure that exerts most impact
countries <- unique(label_FD$country)

FD <- readRDS(paste0(path, 'Y/Y_2019.rds'))

agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x) }

eu <-  c('AUT', 'BEL', 'BGR', 'HRV', 'CYP', 'CZE', 'DNK', 'EST',
         'FIN', 'FRA', 'DEU', 'GRC', 'HUN', 'IRL', 'ITA', 'LVA', 
         'LTU', 'LUX', 'MLT', 'NLD', 'POL', 'PRT', 'ROU', 'SVK', 
         'SVN', 'ESP', 'SWE', 'XEU')

l <- read_excel('~/projects/bio-carbon/Data/fd-labels.xlsx')
cn <- regmatches(l$labels, gregexpr("\\([^()]+\\)", l$labels))
cn <- sapply(cn, function(x) if(length(x) >= 2) x[2] else x[1])
cn <- gsub("[()]", "", cn)

eu <-  c('AUT', 'BEL', 'BGR', 'HRV', 'CYP', 'CZE', 'DNK', 'EST',
         'FIN', 'FRA', 'DEU', 'GRC', 'HUN', 'IRL', 'ITA', 'LVA', 
         'LTU', 'LUX', 'MLT', 'NLD', 'POL', 'PRT', 'ROU', 'SVK', 
         'SVN', 'ESP', 'SWE', 'XEU')

f <- rowSums(FD)
colnames(FD) <- cn
FD <- agg(FD)
f_eu <- rowSums(FD[,colnames(FD) %in% eu])


results1 <- list()

for (var in t10) {
  
  E <- e[, var] * L
  
  for (country in countries) {
    
    # Indices of rows/columns for the current country in label_IO and label_FD
    con <- which(label_IO$country == country)
    
    # Indices of rows/columns for other countries in label_IO and label_FD
    ncon <- which(label_IO$country != country)
    
    
    # Calculate fdom, fexp, and fimp
    fdom <- sum(sr[con,])
    fexp <- sum(rowSums(E[con,ncon]))
    fimp <- sum(colSums(E[ncon,con]))
    
    # Store the results in a data.table row
    results1[[length(results1) + 1]] <- data.table(
      country = country,
      psr =  var,
      fdom = as.numeric(fdom),
      fexp = as.numeric(fexp),
      fimp = as.numeric(fimp)
    )
    
    print(paste("for ", var, ": ", country," done!"))
  }
}

results1 <- rbindlist(results1)

s3write_using(x = as.data.table(results1), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd3,"/results1.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))

gs <- global_scores %>%
  filter(!pressure %in% c(80:84))

top10 <- gs %>%
  arrange(desc(score)) %>%
  head(10) %>%
  select(pressure)

pressures <- biotope %>%
  select(-threat) %>%
  unique()

lookup <- pressures[which(pressures$Lfd_Nr %in% top10$pressure),]


class(top10$pressure)

countries <- unique(label_FD$country)
# calculate disaggregated footprints for the top 10

results2 <- list()

for (var in top10$pressure) {
  
  e_n <- as.matrix(e[,..var])
  
  for (country in countries) {
  # Indices of rows/columns for the current country in label_IO and label_FD
  cnt1 <- which(label_IO$country == country)
  cnt2 <- which(label_FD$country == country)
  
  # Indices of rows/columns for other countries in label_IO and label_FD
  ncnt1 <- which(label_IO$country != country)
  ncnt2 <- which(label_FD$country != country)
  
  
  # Extract the column of "e" corresponding to the current variable
  e1_dom <- as.matrix(e_n[cnt1,])
  e1_ext <- as.matrix(e_n[ncnt1,])
  
  # Calculate fdom, fexp, and fimp
  fdom <- sum(sr[cnt1,])
  fexp <- t(e1_dom) %*% L[cnt1,cnt1] %*% rowSums(FD[cnt1, ncnt2, with = FALSE])
  fimp <- t(e1_ext) %*% L[ncnt1,ncnt1] %*% rowSums(FD[ncnt1, cnt2, with = FALSE])
  
  
  # Store the results in a data.table row
  results2[[length(results2) + 1]] <- data.table(
    country = country,
    fdom = as.numeric(fdom),
    fexp = as.numeric(fexp),
    fimp = as.numeric(fimp))
 
  }
  
}


class(results2)

results2 <- cbind(label_IO, results2)

results2_countries <- results2[, lapply(.SD, sum, na.rm = TRUE), by = .(iso, country), .SDcols = is.numeric]

# changing column names
target_colnames <- colnames(results2_countries)[3:12]

# Create a named vector of new names where names are the old column names
# This ensures we map the new names correctly to the old names, regardless of order
name_mapping <- setNames(lookup$pressure, lookup$Lfd_Nr)

# Ensure that we only rename columns that exist in both target_colnames and lookup$Lfd_Nr
common_columns <- intersect(target_colnames, lookup$Lfd_Nr)

# Rename the columns that match
setnames(results2_countries, old = common_columns, new = name_mapping[common_columns])



s3write_using(x = as.data.table(results2_countries), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd3,"/pressures_countries.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))


# calculate consumptions based relevance of each pressure
pressures$Lfd_Nr <- as.character(pressures$Lfd_Nr)

global_scores <- global_scores %>%
  mutate(shr = score / sum(score) * 100) %>%
  left_join(pressures, by = c("pressure"="Lfd_Nr"))


### MODIFYING THIS ###
# calculate the environmental impact matrix (NO)
path <- "/mnt/nfs_fineprint/tmp/gloria/v059-compiled/"
FD <- readRDS(paste0(path, 'Y/Y_2019.rds'))

agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x) }

eu <-  c('AUT', 'BEL', 'BGR', 'HRV', 'CYP', 'CZE', 'DNK', 'EST',
         'FIN', 'FRA', 'DEU', 'GRC', 'HUN', 'IRL', 'ITA', 'LVA', 
         'LTU', 'LUX', 'MLT', 'NLD', 'POL', 'PRT', 'ROU', 'SVK', 
         'SVN', 'ESP', 'SWE', 'XEU')

cn <- regmatches(l$labels, gregexpr("\\([^()]+\\)", l$labels))
cn <- sapply(cn, function(x) if(length(x) >= 2) x[2] else x[1])
cn <- gsub("[()]", "", cn)

colnames

# calculate net footprint of each sector
sectors <- unique(label_IO$NACE)

pr <- data.table()

# Loop over each country
for (var in t10) {
  mp <- as.numeric(t(e[,var]) %*% L)
  
  fp <- mp * f
  
  pr <- cbind(pr,fp)
}

colnames(pr) <- t10

pr <- cbind(label_IO, pr)

spc <- pr %>% group_by(NACE) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups =  'drop') %>%
  pivot_longer(-NACE) %>%
  group_by(name) %>%
  mutate(value = value / sum(value) * 100) %>%
  ungroup()

# general STAR
mp <- as.numeric(t(e1$score) %*% L)
fp <- mp * f
w <- cbind(label_IO, fp) %>%
  group_by(NACE) %>%
  summarise(fp = sum(fp), .groups = 'drop') %>%
  pivot_longer(-NACE) %>%
  mutate(value = value / sum(value) * 100)


#### check specifics of food and beverage ###
w1 <- cbind(label_IO, fp) %>% group_by(sector, NACE) %>% summarise(fp = sum(fp), .groups = 'drop') %>%
  pivot_longer(-c(sector,NACE)) %>% mutate(value = value / sum(value) * 100)
###

w <- w %>% mutate(name = 'Total STAR score')

spc <- rbind(spc,w)

saveRDS(spc, "rds/sector_pressures-1.rds")


# Pressure scores

results2_pressures <- results2[, lapply(.SD, sum, na.rm = TRUE), by = .(NACE), .SDcols = is.numeric]

# changing column names
target_colnames <- colnames(results2_pressures)[2:11]

name_mapping <- setNames(lookup$pressure, lookup$Lfd_Nr)

common_columns <- intersect(target_colnames, lookup$Lfd_Nr)

setnames(results2_pressures, old = common_columns, new = name_mapping[common_columns])


s3write_using(x = as.data.table(results2_pressures), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd3,"/pressures_per_sector.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))





# Aggregate the final demand vectors for each country
agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x) }


l <- read_excel('~/projects/bio-carbon/Data/fd-labels.xlsx')
cn <- regmatches(l$labels, gregexpr("\\([^()]+\\)", l$labels))
cn <- sapply(cn, function(x) if(length(x) >= 2) x[2] else x[1])
cn <- gsub("[()]", "", cn)

colnames(FD) <- cn
FD <- agg(FD)


## Calculating net footprint of nSTAR for specific countries


cns <- c("USA", "CHN", "JPN", "DEU", "FRA", "GBR", "MDG",
        "TZA", "LKA", "PNG", "CRI", "CIV", "COL", "BRA",
        "IDN", "ECU", "PER", "MEX")

countries <- colnames(FD)

MP <- readRDS('rds/MP.rds')

# getting the footprints
s <- MP %*% FD

# Initialize an empty list to store results
results3 <- list()

# Loop over each country
for (country in countries) {
  
  # Indices of rows/columns for the current country in label_IO and label_FD
  cnt1 <- which(label_IO$iso == country)
  
  # Indices of rows/columns for other countries in label_IO and label_FD
  ncnt1 <- which(label_IO$iso != country)
    
  # Calculate fdom, fexp, and fimp
  fdom <- sum(s[cnt1, colnames(s) == country])
  fexp <- sum(s[cnt1, colnames(s) != country])
  fimp <- sum(s[ncnt1, colnames(s) == country])
    
  # Store the results in a data.table row
  results3[[length(results3) + 1]] <- data.table(
    country = country,
    fdom = as.numeric(fdom),
    fexp = as.numeric(fexp),
    fimp = as.numeric(fimp)
  )
  
}

# Combine all the results into one data.table
results3f <- rbindlist(results3)

# View the final data table
print(results3f)

results3f <- results3f %>%
  mutate(nfp = fdom + fimp)

results3f1 <- results3f %>%
  mutate(type = case_when(
    fimp > fexp & fimp > fdom ~ "Importer",
    fexp > fdom ~ "Exporter",
    fdom > fimp ~ "Domestic"
  ))

unique(results3f1$type)

saveRDS(results3f1, "rds/net-footprint-countries-v2.rds")



E <- diag(e1$score) %*% L %*% diag(f)

country_fp <- list()
## Net footprint of countries
for (country in countries) {
  # Indices of rows/columns for the current country in label_IO and label_FD
  con <- which(label_IO$country == country)
  
  # Indices of rows/columns for other countries in label_IO and label_FD
  ncon <- which(label_IO$country != country)
  
  
  # Calculate fdom, fexp, and fimp
  fdom <- sum(sr[con,])
  fexp <- sum(rowSums(E[con,ncon]))
  fimp <- sum(colSums(E[ncon,con]))
  
  # Store the results in a data.table row
  country_fp[[length(country_fp) + 1]] <- data.table(
    country = country,
    fdom = as.numeric(fdom),
    fexp = as.numeric(fexp),
    fimp = as.numeric(fimp)
  )
  
  print(paste(country," done!"))
}

country_fp <- rbindlist(country_fp)

country_fp <- country_fp %>%
  mutate(nfp = fdom - fexp + fimp)

country_fp <- country_fp %>%
  mutate(type = case_when(
    fimp > fdom - fexp ~ "Net Importer",
    fexp > fdom ~ "Net Exporter",
    fdom > fimp - fexp ~ "Net Domestic Consumer"
  ))

s3write_using(x = as.data.table(country_fp), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd3,"/net-footprint-countries-v3.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))
