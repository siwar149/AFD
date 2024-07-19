### DOING the sankey diagram

bucket1 <- "projet-esteem"
set_wd1 <- "Gloria/matrices"

bucket2 <- "siwar"
set_wd2 <- "data/Gloria"
set_wd3 <- "data/bio/rds"

#install.packages("networkD3")
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
  mutate(bonds = bonds / sum(bonds),
         loans = loans / sum(loans),
         other = other / sum(other))

bach <- bach %>%
  select(sector, bonds, loans, other) %>%
  mutate(bonds = bonds * 100,
         loans = loans * 100,
         other = other * 100) %>%
  group_by(sector) %>%
  arrange(desc(loans)) %>%
  filter(sector %in% c("M", "C", "L", "D", "G", "H", "J"))

bach$sector[7] <- "X"
bach$bonds[7] <- 100 - sum(bach$bonds[-7])
bach$loans[7] <- 100 - sum(bach$loans[-7])
bach$other[7] <- 100 - sum(bach$other[-7])


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
eu_C <- which(label_IO$iso %in% eu1 & label_IO$NACE == "C")

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

sum(eu_fp[latam_A,]) / total * 49.8547
sum(eu_fp[latam_X,]) / total * 49.8547
sum(eu_fp[eu_all,]) / total * 49.8547
sum(eu_fp[row,]) / total * 49.8547


# Doing the first steps of the network graph
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
                       "RoW")) # Node 9


links = as.data.frame(matrix(c(
  0, 3, 40.669871,
  0, 4, 8.175700,
  0, 5, 7.856371,
  0, 6, 13.164714,
  0, 7, 2.588083,
  0, 8, 8.360560,
  0, 9, 19.184701,
  1, 3, 12.532653,
  1, 4, 19.873455,
  1, 5, 19.872155,
  1, 6, 5.782332,
  1, 7, 12.118520,
  1, 8, 8.230190,
  1, 9, 21.590695,
  2, 3, 30.083582,
  2, 4, 21.805542,
  2, 5, 4.585215,
  2, 6, 10.942701,
  2, 7, 8.247337,
  2, 8, 6.523835,
  2, 9, 17.811788,
  4, 10, 9.941399,
  4, 11, 2.904854,
  4, 12, 13.75652,
  4, 13, 23.25193,
  3, 10, 0,
  5, 10, 0,
  6, 10, 0,
  7, 10, 0,
  8, 10, 0,
  9, 10, 0),
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")


sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30)

