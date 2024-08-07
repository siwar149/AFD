### DOING the sankey diagram

bucket1 <- "projet-esteem"
set_wd1 <- "Gloria/matrices"

bucket2 <- "siwar"
set_wd2 <- "data/Gloria"
set_wd3 <- "data/bio/rds"

install.packages("networkD3")
library("networkD3")
library("dplyr")


bach <- read.csv("data/export-bach-2019.csv", sep = ";", header = T)

### Eliminate the observations that comprise small and medium enterprises
bach <- bach[which(bach$size == "0"),]

bach <- bach %>%
  select(country, year, sector, total_assets, turnover, gross_value_added,
         nb_firms, employees, L1_WM, L2_WM, L31_WM)

# Replace NAs with 0s
bach[is.na(bach)] <- 0

bach <- bach %>%
  mutate(bonds = (L1_WM * 10^-2) * total_assets,
         loans = (L2_WM * 10^-2) * total_assets,
         other = (L31_WM * 10^-2) * total_assets) %>%
  select(country, sector, total_assets, bonds, loans, other) %>%
  filter(!sector %in% c("Mc", "Z0", "Zc")) %>%  # remove rows with sector values "Mc", "Z0", and "Zc"
  group_by(sector) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(rbonds = bonds / sum(bonds) * 100,
         rloans = loans / sum(loans) * 100,
         rother = other / sum(other) * 100)

bondsR <- sum(bach$bonds)/sum(bach$bonds, bach$loans, bach$other)
loansR <- sum(bach$loans)/sum(bach$bonds, bach$loans, bach$other)
otherR <- sum(bach$other)/sum(bach$bonds, bach$loans, bach$other)

bach <- bach %>%
  select(sector, rbonds, rloans, rother) %>%
  group_by(sector) %>%
  arrange(desc(rloans)) %>%
  filter(sector %in% c("M", "C", "L", "D", "G", "H", "J"))

bach$sector[7] <- "X"
bach$rbonds[7] <- 100 - sum(bach$rbonds[-7])
bach$rloans[7] <- 100 - sum(bach$rloans[-7])
bach$rother[7] <- 100 - sum(bach$rother[-7])

bach$rbonds <- bach$rbonds * bondsR
bach$rloans <- bach$rloans * loansR
bach$rother <- bach$rother * otherR

bach <- as.data.table(bach)
colnames(bach)[c(2:4)] <- c("Bonds", "Loans", "Other")

links1 <- melt(bach, variable.name = "source", value.name =  "value")





# Connection to LAC countries
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

eu1 <- c("AUT", "BEL", "DEU", "ESP", "FRA", "HRV", "HUN", "ITA",
         "LUX", "POL", "PRT", "SVK")

# Get the index of industrial sectors in EU12

eu_C <- which(label_IO[label_IO$iso %in% eu1,]$NACE == "C")

# Getting the nSTAR requirements of the industry sector of EU12
eu_fp <- as.data.table(rowSums(Teu[,..eu_C]))

# Defining the shock source countries
latam <- c("HND", "COL", "BRA", "GTM", "PER", "ECU")

latam_A <- which(label_IO$iso %in% latam & label_IO$NACE == "A")
latam_X <- which(label_IO$iso %in% latam & label_IO$NACE != "A")

# Defining all sectors in EU12
eu_all <- which(label_IO$iso %in% eu1)

# Defning RoW
row <- which(!label_IO$iso %in% eu1 & !label_IO$iso %in% latam)

total <- sum(eu_fp[latam_A,]) + sum(eu_fp[latam_X,]) + sum(eu_fp[eu_all,]) +
  sum(eu_fp[row,])

rowSums(bach[, 2:4])[3]

sum(eu_fp[latam_A,]) / total * 19.67636
sum(eu_fp[latam_X,]) / total * 19.67636
sum(eu_fp[eu_all,]) / total * 19.67636
sum(eu_fp[row,]) / total * 19.67636


bach$t <- rowSums(bach[, -1])


#### FIRST SANKEY
# Doing the first steps of the Sankey network graph
nodes = data.frame("name" = 
                     c("Bonds", # Node 0
                       "Loans", # Node 1
                       "Other", # Node 2
                       "M", # Node 3
                       "C", # Node 4
                       "L", # Node 5
                       "D", # Node 6
                       "G", # Node 7
                       "H", # Node 8
                       "X",
                       "LAC6 A",
                       "LAC6 X",
                       "EU12 all",
                       "RoW",
                       "Inputs")) 


links = as.data.frame(matrix(c(
  0, 3, 4.4042112,
  0, 4, 0.8853608,
  0, 5, 0.8507802,
  0, 6, 1.4256298,
  0, 7, 0.2802680,
  0, 8, 0.9053796,
  0, 9, 2.0775448,
  1, 3, 4.2369592,
  1, 4, 6.7186903,
  1, 5, 6.7182508,
  1, 6, 1.9548538,
  1, 7, 4.0969518,
  1, 8, 2.7824099,
  1, 9, 7.2992439,
  2, 3, 16.655313,
  2, 4, 12.0723040,
  2, 5, 2.5385339,
  2, 6, 6.0582585,
  2, 7, 4.5660117,
  2, 8, 3.6118211,
  2, 9, 9.8612232,
  4, 10, 3.923613,
  4, 11, 1.146471,
  4, 12, 5.429341,
  4, 13, 9.176935,
  3, 14, 25.29648,
  5, 14, 10.10756,
  6, 14, 9.438742,
  7, 14, 8.943231,
  8, 14, 7.299611,
  9, 14, 19.23801),
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")


sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30)


#### Second Sankey


score <- s3read_using(FUN = readRDS,
                      object = paste(set_wd3,"/score_pays.rds",sep=""),
                      bucket = bucket2, opts = list("region" = ""))

e <- label_IO %>%
  left_join(score, by = c("iso", "sector")) %>%
  mutate(score= if_else(is.na(score), 0.0001, score))


lac6A <- cbind(label_IO[latam_A,], eu_fp[latam_A], e$score[latam_A])
colnames(lac6A)[c(5,6)] <- c("com", "prod")


pressures <- as.data.table(s3read_using(FUN = readRDS,
                                        object = paste(set_wd3,"/redlist_score_per_pressure.rds",sep=""),
                                        bucket = bucket2, opts = list("region" = "")))

press <- s3read_using(FUN = data.table::fread,
                      object = paste(set_wd3,"/press.rds",sep=""),
                      bucket = bucket2, opts = list("region" = ""))


lac6A <- lac6A %>%
  mutate(r = com / prod) %>%
  mutate(id = paste(iso, sector))

pressures <- pressures %>%
  mutate(id = paste(iso, sector)) %>%
  select(-sector, -country, -iso)


lac6A <- lac6A %>%
  left_join(pressures, by = "id")

biotope <- s3read_using(FUN = data.table::fread,
                        object = paste(set_wd2,"/biotope_threats.rds",sep=""),
                        bucket = bucket2, opts = list("region" = ""))

biotope1 <- biotope %>%
  select(pressure, Lfd_Nr) %>%
  unique()


lac6A <- lac6A %>%
  left_join(biotope1, by = "Lfd_Nr")

lac6p <- lac6A %>%
  mutate(fp = r * score_sum) %>%
  select(iso, sector, pressure, fp)

lac6p1 <- lac6p %>%
  group_by(pressure) %>%
  summarise(fp = sum(fp)) %>%
  mutate(rfp = fp / sum(fp) * 100) %>%
  arrange(desc(rfp)) %>%
  slice_head(n = 11)

lac6p1$pressure[11] <- "Other"
lac6p1$rfp[11] <- 100 - sum(lac6p1$rfp[-11])


nodes1 = data.frame("name" = 
                     c("LAC6 Agriculture",
                       "NH3",
                       "Permanent crops",
                       "PM2.5 bio",
                       "CO2 sc",
                       "Extensive forestry",
                       "N2O",
                       "CO",
                       "Agriculture water stress",
                       "Straw",
                       "Tobacco",
                       "Other")) 


links1 = as.data.frame(matrix(c(
  0, 1, 30.9,
  0, 2, 19.6,
  0, 3, 5.44,
  0, 4, 5.18,
  0, 5, 4.40,
  0, 6, 4.21,
  0, 7, 3.53,
  0, 8, 3.25,
  0, 9, 2.51,
  0, 10, 2.32,
  0, 11, 18.6),
  byrow = TRUE, ncol = 3))
names(links1) = c("source", "target", "value")


sankeyNetwork(Links = links1, Nodes = nodes1,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30)
