##### The financial analysis #####

bucket1 = "projet-esteem"
set_wd1 <- "Gloria/matrices"

bucket2 = "siwar"
set_wd2 <- "data/Gloria"
set_wd3 <- "data/bio/rds"



k <- as.data.frame(s3read_using(FUN = data.table::fread,
                                object = paste(set_wd2,"/k_2019.rds",sep=""),
                                bucket = bucket2, opts = list("region" = "")))

bach <- read.csv("data/export-bach-2019.csv", sep = ";", header = T)

bach <- bach[-which(bach$size == "1"),]

#bachT <- bach[which(bach$size == "0"),]

#bachD <- bach[-which(bach$size == "0"),]

colnames(k)[c(3,7)] <- c("sector", "country")

k1 <- k %>%
  left_join(bach, by = c("country", "sector"))


sltd <- grep("^I1|^I10|^It1|^It3|^Ic1|^A1|^A51|^A6|^A7|^A|^E1|^E2|^E|^L1|^L2|
     ^L61|^L|^R2|^R31|^R32|^R33", names(k1), value = TRUE)

k2 <- k1[sltd]

k3 <- cbind(k1[,7], k1[, c(3:6,9,11:17)], k2)

colnames(k3)[1] <- "country"




