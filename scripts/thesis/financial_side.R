##### The financial analysis #####

bucket1 <- "projet-esteem"
set_wd1 <- "Gloria/matrices"

bucket2 <- "siwar"
set_wd2 <- "data/Gloria"
set_wd3 <- "data/bio/rds"


### Merge the Gloria simulation results with the Bach data
g <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd2,"/g_2_2019.rds",sep=""),
                  bucket = bucket2, opts = list("region" = ""))

bach <- read.csv("data/export-bach-2019.csv", sep = ";", header = T)

### Eliminate the observations that comprise small and medium enterprises
bach <- bach[which(bach$size == "0"),]

g1 <- g %>%
  select(eu, NACE, abvarx) %>%
  rename(country = eu,
         sector = NACE) %>%
  mutate(abvarx = abvarx * (-1/100))

### Perform a raw merge
g1 <- g1 %>%
  left_join(bach, by = c("country", "sector"))

### select only relevant variables
sltd <- grep("^I1|^I83|^I10|^It1|^It3|^Ic1|^A1|^A51|^A6|^A7|^A|^E1|^E2|^E|^L1|^L2|
     |^L3|^L61|^L|^R2|^R31|^R32|^R33", names(g1), value = TRUE)

g1[is.na(g1)] <- 0
### create variable of interest payments over net turnover
g1 <- g1 %>%
  mutate(ipt1 = (I83_WM+I10_WM)/(I1_WM+abvarx*I1_WM),
         vaript = ((ipt1 - R24_WM*10^-2) / (R24_WM*10^-2))*100)

s3write_using(x = as.data.table(g1), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd2,"/g_3_2019.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))


### Data for exposure graph

g1[is.na(g1)] <- 0


g1 <- g1 %>%
  mutate(liab = L1_WM + L2_WM + L31_WM) %>%
  select(country, sector, liab) %>%
  group_by(country) %>%
  mutate(sumliab = sum(liab))


