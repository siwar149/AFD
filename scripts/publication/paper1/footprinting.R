rm(list = ls())
gc()

install.packages("ggrepel")
library(ggrepel) 


bucket1 <- "projet-esteem"
set_wd1 <- "Gloria/matrices"

bucket2 <- "siwar"
set_wd2 <- "data/Gloria"
set_wd3 <- "data/bio/rds"


star <- s3read_using(FUN = data.table::fread,
                      object = paste(set_wd3,"/star_satellites.rds",sep=""),
                      bucket = bucket2, opts = list("region" = ""))

setnames(star, as.character(star[1, ]))
star <- star[-1, ]

label_IO <- as.data.table(s3read_using(FUN = readRDS,
                      object = paste(set_wd2,"/label_IO.rds",sep=""),
                      bucket = bucket2, opts = list("region" = "")))

colnames(label_IO) <- c("iso", "country", "sector")

nace <- read_excel("data/NACE-Gloria.xlsx", sheet = "Feuil1")

label_IO <- label_IO %>%
  left_join(nace, by = c("sector"="Gloria"))

rm(nace)

star <- label_IO %>%
  left_join(star, by = c("iso", "country", "sector"))

star[is.na(star)] <- 0

summary(star[,5:108])

## get the pressure name
biotope <- s3read_using(FUN = data.table::fread,
                        object = paste(set_wd2,"/biotope_threats.rds",sep=""),
                        bucket = bucket2, opts = list("region" = ""))



press <- as.numeric(colnames(star)[c(5:108)])

biotope <- biotope[which(biotope$Lfd_Nr %in% press),]

# fixing some values
biotope[Lfd_Nr %in% c(68:72), pressure := paste0(pressure, " (Land use)")]
biotope[Lfd_Nr %in% c(80:84), pressure := paste0(pressure, " (PDF)")]

#loading output data
x <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd1,"/x_2019.rds",sep=""),
                  bucket = bucket1, opts = list("region" = ""))

x <- x + 0.0001 # No data on Yemen's output

star <- star[, 5:108]

e <- star[, lapply(.SD, function(col) col / x[[1]])]

# loading final demand and leontief matrix
FD <- s3read_using(FUN = data.table::fread,
                   object = paste(set_wd1,"/FD_2019.rds",sep=""),
                   bucket = bucket1, opts = list("region" = ""))

label_FD <- s3read_using(FUN = data.table::fread,
                   object = paste("Gloria/labels/label_FD.rds",sep=""),
                   bucket = bucket1, opts = list("region" = ""))

colnames(label_FD) <- c("iso", "country", "sector")

L <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd1,"/L_2019.rds",sep=""),
                  bucket = bucket1, opts = list("region" = ""))

### Starting with the footprinting analysis


ncol(e)


L <- as.matrix(L)

f <- rowSums(FD)

# finding the pressure that exerts most impact
results1 <- list()

for (var in colnames(e)) {
  
  e_n <- as.matrix(e[,..var])
  
  s <- t(e_n) %*% L %*% f
  
  results1[[length(results1) + 1]] <- data.table(
    pressure = var,
    score = as.numeric(s)
  )
  
}

global_scores <- rbindlist(results1)

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


# calculate disaggregated footprints for the top 10

results2 <- data.table()

for (var in top10$pressure) {
  
  e_n <- as.matrix(e[,..var])
  
  fp <- (t(L) %*% e_n) * as.matrix(f)
  
  print(dim(fp))
  
  # Store the results in the data table
  results2 <- cbind(results2,fp)
  
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



# calculate the footprint matrix
E <- diag(as.numeric(as.matrix(e1))) %*% L %*% diag(f) 


# calculate net footprint of each sector
sectors <- unique(label_IO$NACE)

sectoral_fp <- list()

# Loop over each country
for (sector in sectors) {
  
  # Indices of rows/columns for the current country in label_IO and label_FD
  sec <- which(label_IO$NACE == sector)
  
  # Indices of rows/columns for other countries in label_IO and label_FD
  nsec <- which(label_IO$NACE != sector)
  
  
  # Calculate fdom, fexp, and fimp
  fint <- sum(sr[sec,])
  fout <- sum(rowSums(E[sec,nsec]))
  finp <- sum(colSums(E[nsec,sec]))
  
  # Store the results in a data.table row
  sectoral_fp[[length(sectoral_fp) + 1]] <- data.table(
    sector = sector,
    fint = as.numeric(fint),
    fout = as.numeric(fout),
    finp = as.numeric(finp)
  )
  
}

sfp <- rbindlist(sectoral_fp)

sfp <- sfp %>%
  mutate(nfp = fint + finp - fout)


s3write_using(x = as.data.table(sfp), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd3,"/sector_pressures-1.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))


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


rp <- results2_pressures %>%
  mutate(across(where(is.numeric), ~ . / sum(.) * 100))




# Calculating net footprint of nSTAR for specific countries
sr <- s3read_using(FUN = data.table::fread,
                     object = paste(set_wd3,"/score_pays-v3.rds",sep=""),
                     bucket = bucket2, opts = list("region" = ""))

sr <- label_IO %>%
  left_join(sr, by = c("iso", "sector")) %>%
  mutate(score= if_else(is.na(score), 0.0001, score)) %>%
  select(score)
  
e1 <- sr / x


cns <- c("USA", "CHN", "JPN", "DEU", "FRA", "GBR", "MDG",
        "TZA", "LKA", "PNG", "CRI", "CIV", "COL", "BRA",
        "IDN", "ECU", "PER", "MEX")

countries <- unique(label_FD$country)

# Initialize an empty list to store results
results3 <- list()

# Loop over each country
for (country in countries) {
  
  # Indices of rows/columns for the current country in label_IO and label_FD
  cnt1 <- which(label_IO$country == country)
  cnt2 <- which(label_FD$country == country)
  
  # Indices of rows/columns for other countries in label_IO and label_FD
  ncnt1 <- which(label_IO$country != country)
  ncnt2 <- which(label_FD$country != country)
  
    
  # Extract the column of "e" corresponding to the current variable
  e1_dom <- as.matrix(e1[cnt1,])
  e1_ext <- as.matrix(e1[ncnt1,])
    
  # Calculate fdom, fexp, and fimp
  fdom <- sum(sr[cnt1,])
  fexp <- t(e1_dom) %*% L[cnt1,cnt1] %*% rowSums(FD[cnt1, ncnt2, with = FALSE])
  fimp <- t(e1_ext) %*% L[ncnt1,ncnt1] %*% rowSums(FD[ncnt1, cnt2, with = FALSE])
    
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
  select(-var) %>%
  mutate(nfp = fdom - fexp + fimp)

results3f1 <- results3f %>%
  mutate(type = case_when(
    fimp > fdom - fexp ~ "Net Importer",
    fexp > fdom ~ "Net Exporter",
    fdom > fimp - fexp ~ "Net Domestic Consumer"
  ))

s3write_using(x = as.data.table(results3f1), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd3,"/net-footprint-countries.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))



# Loop over each country
for (country in countries) {
  
  # Indices of rows/columns for the current country in label_IO and label_FD
  cnt1 <- which(label_IO$country == country)
  cnt2 <- which(label_FD$country == country)
  
  # Indices of rows/columns for other countries in label_IO and label_FD
  ncnt1 <- which(label_IO$country != country)
  ncnt2 <- which(label_FD$country != country)
  
  # Loop over each variable in the columns of "e"
  for (var in colnames(e)) {
    
    # Extract the column of "e" corresponding to the current variable
    e_cnt1_var <- as.matrix(e[,..var])
    e_cnt1_var[ncnt1,] <- 0
    e_ncnt1_var <- as.matrix(e[,..var])
    e_ncnt1_var[cnt1,] <- 0
    
    # Calculate fdom, fexp, and fimp
    fdom <- t(e_cnt1_var) %*% L %*% rowSums(FD[, cnt2, with = FALSE])
    fexp <- t(e_cnt1_var) %*% L %*% rowSums(FD[, ncnt2, with = FALSE])
    fimp <- t(e_ncnt1_var) %*% L %*% rowSums(FD[, cnt2, with = FALSE])
    
    # Store the results in a data.table row
    results[[length(results) + 1]] <- data.table(
      country = country,
      var = var,
      fdom = as.numeric(fdom),
      fexp = as.numeric(fexp),
      fimp = as.numeric(fimp)
    )
  }
}
