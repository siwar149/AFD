rm(list = ls())
gc()


bucket1 <- "projet-esteem"
set_wd1 <- "Gloria/matrices"

bucket2 <- "siwar"
set_wd2 <- "data/Gloria"
set_wd3 <- "data/bio/rds"


Q <- s3read_using(FUN = data.table::fread,
                     object = paste(set_wd1,"/QT_2020.rds",sep=""),
                     bucket = bucket1, opts = list("region" = ""))

# remember to always check the dimensions!!!
label_QT <- s3read_using(FUN = read.csv,
                         object = "data/label_TQ_57.csv",
                         bucket = bucket2, opts = list("region" = ""))


m <- c("Iron ores", "Bauxite and other aluminium ores - gross ore",
       "Copper ores", "Manganese ores")

metals <- Q[which(label_QT$Sat_indicator %in% m),]

metals1 <- as.data.table(t(metals))

colnames(metals1) <- m

summary(metals1)

metals1 <- cbind(label_IO, metals1)


# Let the footprinting start

label_IO <- as.data.table(s3read_using(FUN = readRDS,
                                       object = paste(set_wd2,"/label_IO.rds",sep=""),
                                       bucket = bucket2, opts = list("region" = "")))

colnames(label_IO) <- c("iso", "country", "sector")

x <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd1,"/x_2019.rds",sep=""),
                  bucket = bucket1, opts = list("region" = ""))

x <- x + 0.0001 # No data on Yemen's output

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

L <- as.matrix(L)

eu <-  c('AUT', 'BEL', 'BGR', 'HRV', 'CYP', 'CZE', 'DNK', 'EST',
         'FIN', 'FRA', 'DEU', 'GRC', 'HUN', 'IRL', 'ITA', 'LVA', 
         'LTU', 'LUX', 'MLT', 'NLD', 'POL', 'PRT', 'ROU', 'SVK', 
         'SVN', 'ESP', 'SWE', 'XEU')


index <- which(label_IO$iso %in% eu)

f <- rowSums(FD)

e <- metals1[, lapply(.SD, function(col) col / x[[1]])]


fp <- data.table()

for (var in colnames(e)) {
 
  E = diag(e[[var]]) %*% L %*% diag(f)
  
  v <- rowSums(E[,index])
  
  fp <- cbind(fp, v)
  
  print(paste(var," done", sep = " "))
}

rm(E)
gc()
fp <- cbind(label_IO, fp)

colnames(fp)[4:7] <- m

fp_c <- fp[, lapply(.SD, sum, na.rm = TRUE), by = .(iso, country), .SDcols = is.numeric]


fp_n <- fp_c[!iso %in% eu]

fp_n <- select(fp_n, -country)
fp_n <- cbind(fp_n[,2:5], fp_n[,1])

fp_eu <- fp[iso %in% eu, 
            lapply(.SD, sum, na.rm = TRUE), 
            .SDcols = is.numeric][, iso := "EU"]

fpt <- rbind(fp_eu, fp_n)

fpt <- fpt %>%
  left_join(unique(label_IO[,1:2]), by = "iso")


s3write_using(x = as.data.table(fpt), FUN = write.csv, na = "", 
              object = "data/European_metal_footprints.rds",
              bucket = bucket2, opts = list("region" = ""))
