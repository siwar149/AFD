##### The financial analysis #####

bucket1 = "projet-esteem"
set_wd1 <- "Gloria/matrices"

bucket2 = "siwar"
set_wd2 <- "data/Gloria"
set_wd3 <- "data/bio/rds"


### Merge the Gloria simulation results with the Bach data
k <- as.data.frame(s3read_using(FUN = data.table::fread,
                                object = paste(set_wd2,"/k_2019.rds",sep=""),
                                bucket = bucket2, opts = list("region" = "")))

bach <- read.csv("data/export-bach-2019.csv", sep = ";", header = T)

### Eliminate the observations that comprise small and medium enterprises
bach <- bach[-which(bach$size == "1"),]

#bachT <- bach[which(bach$size == "0"),]
#bachD <- bach[-which(bach$size == "0"),]

colnames(k)[c(3,7)] <- c("sector", "country")

### Perform a raw merge
k1 <- k %>%
  left_join(bach, by = c("country", "sector"))

### select only relevant variables
sltd <- grep("^I1|^I83|^I10|^It1|^It3|^Ic1|^A1|^A51|^A6|^A7|^A|^E1|^E2|^E|^L1|^L2|
     ^L61|^L|^R2|^R31|^R32|^R33", names(k1), value = TRUE)

### Keep only the relevant variables
k2 <- cbind(k1[,7], k1[, c(3:6,9,11:17)], k1[sltd])

colnames(k2)[1] <- "country"

### Check correspondence between Gloria's Output and Bach's Turnover
k2 <- k2 %>%
  mutate(fit= if_else(size == "0", turnover / output, NA))

View(cbind(k2[,c(1,2,334)]))


k2 <- k2 %>%
  mutate(int = if_else(size == "0", I83_WM * I83_NBQ + I10_WM * I10_NBQ, NA))

k2 <- k2 %>%
  mutate(dit = if_else(size == "0", - 1 / turnover^2 * int * (loss * fit), NA))


k2 <- k2 %>%
  mutate(it1 = if_else(size == "0", int / turnover, NA))


k2 <- k2 %>%
  mutate(it2 = if_else(size == "0", int / turnover + dit, NA))


View(cbind(k2[,c(1,2,334:338)]))

