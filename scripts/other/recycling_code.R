###############DONT KNOW WHAT TO DO WITH THIS######################
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