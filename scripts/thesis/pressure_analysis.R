Teu <- s3read_using(FUN = data.table::fread,
                    object = paste(set_wd2,"/Teu.rds",sep=""),
                    bucket = bucket2, opts = list("region" = ""))

label_IO <- s3read_using(FUN = data.table::fread,
                         object = "Gloria/labels/label_IO.rds",
                         bucket = bucket1, opts = list("region" = ""))

colnames(label_IO) <- c("iso", "country", "sector")

nace <- read_excel("data/NACE-Gloria.xlsx", sheet = "Feuil1")

label_IO <- label_IO %>%
  left_join(nace, by = c("sector"="Gloria"))

eu <-  c('AUT', 'BEL', 'BGR', 'HRV', 'CYP', 'CZE', 'DNK', 'EST',
         'FIN', 'FRA', 'DEU', 'GRC', 'HUN', 'IRL', 'ITA', 'LVA', 
         'LTU', 'LUX', 'MLT', 'NLD', 'POL', 'PRT', 'ROU', 'SVK', 
         'SVN', 'ESP', 'SWE', 'XEU')

eu1 <- c("AUT", "BEL", "DEU", "ESP", "FRA", "HRV", "HUN", "ITA",
         "LUX", "POL", "PRT", "SVK")

cs <- c("C", "F", "G", "H")

in_eu <- which(label_IO$iso %in% eu)

not_eu <- which(!label_IO$iso %in% eu)

label_Teu <- label_IO[which(label_IO$iso %in% eu1),]

cs_eu1 <- which(label_Teu$NACE %in% cs)

CS <- Teu[, ..cs_eu1]

CS_eu <- cbind(label_IO[in_eu,], CS[in_eu,])
CS_ext <- cbind(label_IO[not_eu,], CS[not_eu,])

rm(CS)

# get specific sectors
mcf <- s3read_using(FUN = data.table::fread,
                    object = paste(set_wd2,"/mcf.rds",sep=""),
                    bucket = bucket2, opts = list("region" = ""))

f <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd2,"/f_2019.rds",sep=""),
                  bucket = bucket2, opts = list("region" = ""))

t <- mcf * f

tis <- t

tis[not_eu] <- 0

weights <- tis / sum(t)


t <- cbind(label_IO, t, weights)

colnames(t)[c(5,6)] <- c("score", "weight")

t <- t[which(t$iso %in% eu1 & t$NACE %in% cs),]

# 10 sectors with the most footprint
sec_analyse <- head(t[t$score > 2,] %>% arrange(desc(score)), 10)[, 1:5] # just change 10 if I want to see more sectors

label_Teu <- label_Teu[cs_eu1,]

label_Teu <- label_Teu %>%
  mutate(id = paste(iso, sector))

sec_analyse <- sec_analyse %>%
  mutate(id = paste(iso, sector))

ten <- which(label_Teu$id %in% sec_analyse$id)
ten1 <- ten + 4
ten1 <- c(c(1:4), ten1)

CS_eu <- CS_eu[, ..ten1]
CS_ext <- CS_ext[, ..ten1]

colnames(CS_eu)[c(5:14)] <- label_Teu$id[ten]
colnames(CS_ext)[c(5:14)] <- label_Teu$id[ten]


#s3write_using(x = as.data.table(CS_eu), FUN = data.table::fwrite, na = "", 
#              object = paste(set_wd2,"/CS_eu_2019.rds",sep=""),
#              bucket = bucket2, opts = list("region" = ""))

#s3write_using(x = as.data.table(CS_ext), FUN = data.table::fwrite, na = "", 
#              object = paste(set_wd2,"/CS_ext_2019.rds",sep=""),
#              bucket = bucket2, opts = list("region" = ""))

CS_eu <- s3read_using(FUN = data.table::fread,
                      object = paste(set_wd2,"/CS_eu_2019.rds",sep=""),
                      bucket = bucket2, opts = list("region" = ""))

CS_ext <- s3read_using(FUN = data.table::fread,
                       object = paste(set_wd2,"/CS_ext_2019.rds",sep=""),
                       bucket = bucket2, opts = list("region" = ""))

colSums(CS_ext[, c(5:14)]) # we take Germany as an example






deu_alc_eu <- CS_eu[, c(1:4, 7)]
deu_alc_ext <- CS_ext[, c(1:4, 7)]

deu_alc_eu <- deu_alc_eu %>%
  arrange(desc(`DEU Alcoholic and other  beverages`)) %>%
  slice_head(n = 10)

deu_alc_ext <- deu_alc_ext %>%
  arrange(desc(`DEU Alcoholic and other  beverages`)) %>%
  slice_head(n = 10)

pressures <- s3read_using(FUN = readRDS,
                          object = paste(set_wd3,"/redlist_score_per_pressure.rds",sep=""),
                          bucket = bucket2, opts = list("region" = ""))


pressures <- pressures %>%
  mutate(id = paste(iso, sector))

deu_alc_eu <- deu_alc_eu %>%
  mutate(id=paste(iso, sector))

deu_alc_ext <- deu_alc_ext %>%
  mutate(id=paste(iso, sector))


pressures <- pressures %>%
  select(id, Lfd_Nr, score_sum)

deu_alc_eu <- deu_alc_eu %>%
  left_join(pressures, by = "id")


press <- s3read_using(FUN = data.table::fread,
                      object = paste(set_wd3,"/press.rds",sep=""),
                      bucket = bucket2, opts = list("region" = ""))

press <- press %>%
  mutate(id = paste(iso, sector)) %>%
  select(id, Lfd_Nr, Sat_head_indicator, Sat_unit, pressure)


deu_alc_eu <- deu_alc_eu %>%
  left_join(press, by = c("id", "Lfd_Nr"))

biotope <- s3read_using(FUN = data.table::fread,
                        object = paste(set_wd2,"/biotope_threats.rds",sep=""),
                        bucket = bucket2, opts = list("region" = ""))

colnames(biotope)[1] <- "sat"

deu_alc_eu <- deu_alc_eu %>%
  left_join(biotope, by = "Lfd_Nr")


deu_alc_ext <- deu_alc_ext %>%
  left_join(pressures, by = "id")

deu_alc_ext <- deu_alc_ext %>%
  left_join(press, by = c("id", "Lfd_Nr"))

deu_alc_ext <- deu_alc_ext %>%
  left_join(biotope, by = "Lfd_Nr")


deu_alc_eu <- deu_alc_eu %>%
  select(iso, country, sector, NACE, `DEU Alcoholic and other  beverages`, Sat_head_indicator,
         sat, Sat_unit, pressure, score_sum, threat)

deu_alc_ext <- deu_alc_ext %>%
  select(iso, country, sector, NACE, `DEU Alcoholic and other  beverages`, Sat_head_indicator,
         sat, Sat_unit, pressure, score_sum, threat)

View(deu_alc_ext[deu_alc_ext$iso == "PER",])