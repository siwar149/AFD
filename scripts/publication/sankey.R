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
i <- label_IO[label_IO$iso %in% eu1,]

eu_C <- which(i$NACE == "C")
eu_M <- which(i$NACE == "M")
eu_L <- which(i$NACE == "L")
eu_G <- which(i$NACE == "G")
eu_H <- which(i$NACE == "H")
eu_D <- which(i$NACE == "D")
eu_X <- which(i$NACE != "C" | i$NACE != "M" | i$NACE != "L" | i$NACE != "G" | i$NACE != "H" | i$NACE != "D")

# Getting the nSTAR requirements of the industry sector of EU12
eu_fpC <- as.data.table(rowSums(Teu[,..eu_C]))
eu_fpM <- as.data.table(rowSums(Teu[,..eu_M]))
eu_fpL <- as.data.table(rowSums(Teu[,..eu_L]))
eu_fpG <- as.data.table(rowSums(Teu[,..eu_G]))
eu_fpH <- as.data.table(rowSums(Teu[,..eu_H]))
eu_fpD <- as.data.table(rowSums(Teu[,..eu_D]))
eu_fpX <- as.data.table(rowSums(Teu[,..eu_X]))

# Defining the shock source countries
latam <- c("HND", "COL", "BRA", "GTM", "PER", "ECU")

latam_A <- which(label_IO$iso %in% latam & label_IO$NACE == "A")
latam_X <- which(label_IO$iso %in% latam & label_IO$NACE != "A")

# Defining all sectors in EU12
eu_A <- which(label_IO$iso %in% eu1 & label_IO$NACE == "A")
eu_X <- which(label_IO$iso %in% eu1 & label_IO$NACE != "A")

# Defning RoW
row_A <- which((!label_IO$iso %in% eu1 & !label_IO$iso %in% latam) & label_IO$NACE == "A")
row_X <- which((!label_IO$iso %in% eu1 & !label_IO$iso %in% latam) & label_IO$NACE != "A")


total_A <- sum(eu_fpC[latam_A,]) + sum(eu_fpC[eu_A,]) + sum(eu_fpC[row_A,])
total_X <- sum(eu_fpC[latam_X,]) + sum(eu_fpC[eu_X,]) + sum(eu_fpC[row_X,])

total <- sum(eu_fpC) + sum(eu_fpM) + sum(eu_fpL) + sum(eu_fpG) + sum(eu_fpH) +
  sum(eu_fpD) + sum(eu_fpX)

sum(rowSums(bach[, 2:4]))

# C
sum(eu_fpC[latam_A,]) / total * 100
sum(eu_fpC[latam_X,]) / total * 100
sum(eu_fpC[eu_A,]) / total * 100
sum(eu_fpC[eu_X,]) / total * 100
sum(eu_fpC[row_A,]) / total * 100
sum(eu_fpC[row_X,]) / total * 100

# M
sum(eu_fpM[latam_A,]) / total * 100
sum(eu_fpM[latam_X,]) / total * 100
sum(eu_fpM[eu_A,]) / total * 100
sum(eu_fpM[eu_X,]) / total * 100
sum(eu_fpM[row_A,]) / total * 100
sum(eu_fpM[row_X,]) / total * 100

# L
sum(eu_fpL[latam_A,]) / total * 100
sum(eu_fpL[latam_X,]) / total * 100
sum(eu_fpL[eu_A,]) / total * 100
sum(eu_fpL[eu_X,]) / total * 100
sum(eu_fpL[row_A,]) / total * 100
sum(eu_fpL[row_X,]) / total * 100

# G
sum(eu_fpG[latam_A,]) / total * 100
sum(eu_fpG[latam_X,]) / total * 100
sum(eu_fpG[eu_A,]) / total * 100
sum(eu_fpG[eu_X,]) / total * 100
sum(eu_fpG[row_A,]) / total * 100
sum(eu_fpG[row_X,]) / total * 100

# H
sum(eu_fpH[latam_A,]) / total * 100
sum(eu_fpH[latam_X,]) / total * 100
sum(eu_fpH[eu_A,]) / total * 100
sum(eu_fpH[eu_X,]) / total * 100
sum(eu_fpH[row_A,]) / total * 100
sum(eu_fpH[row_X,]) / total * 100

# D
sum(eu_fpD[latam_A,]) / total * 100
sum(eu_fpD[latam_X,]) / total * 100
sum(eu_fpD[eu_A,]) / total * 100
sum(eu_fpD[eu_X,]) / total * 100
sum(eu_fpD[row_A,]) / total * 100
sum(eu_fpD[row_X,]) / total * 100

# X
sum(eu_fpX[latam_A,]) / total * 100
sum(eu_fpX[latam_X,]) / total * 100
sum(eu_fpX[eu_A,]) / total * 100
sum(eu_fpX[eu_X,]) / total * 100
sum(eu_fpX[row_A,]) / total * 100
sum(eu_fpX[row_X,]) / total * 100


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
                       "X", # Node 9
                       "LAC6 A", # Node 10
                       "LAC6 X", # Node 11
                       "EU12 A", # Node 12
                       "EU12 X", # Node 13
                       "RoW A", # Node 14
                       "RoW X")) # Node 15
                    


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
  4, 10, 5.704144,
  4, 11, 1.666738,
  4, 12, 3.072818,
  4, 13, 4.820351,
  4, 14, 7.005716,
  4, 15, 6.335701,
  3, 10, 0.009875841,
  3, 11, 0.01138527,
  3, 12, 0.005645337,
  3, 13, 0.06073412,
  3, 14, 0.02874348,
  3, 15, 0.06173004,
  5, 10, 0.03750007,
  5, 11, 0.03316329,
  5, 12, 0.01748482,
  5, 13, 0.2313803,
  5, 14, 0.1007694,
  5, 15, 0.1654907,
  6, 10, 0.0198984,
  6, 11, 0.1167234,
  6, 12, 0.01216057,
  6, 13, 2.813662,
  6, 14, 0.06920122,
  6, 15, 0.4569857,
  7, 10, 0.0737928,
  7, 11, 0.08803161,
  7, 12, 0.05560915,
  7, 13, 0.5349017,
  7, 14, 0.2176024,
  7, 15, 0.4790053,
  8, 10, 0.04639282,
  8, 11, 0.06412195,
  8, 12, 0.02059896,
  8, 13, 0.8736235,
  8, 14, 0.1212371,
  8, 15, 0.4643975,
  9, 10, 0,
  9, 11, 0,
  9, 12, 0,
  9, 13, 0,
  9, 14, 0,
  9, 15, 0),
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")


# Keep the dark color scale for nodes
dark_color_scale <- 'd3.scaleOrdinal()
  .domain(["Other", "Loans", "Bonds", "X", "C", "D", "M", "H", "G", "L", 
           "LAC6 A", "LAC6 X", "EU12 A", "EU12 X", "RoW A", "RoW X"])
  .range(["#2E8B57", "#1C1C1C", "#483D8B", "#800000", "#2F4F4F", "#8B4513", 
          "#5F9EA0", "#4B0082", "#4B0082", "#8B0000", "#556B2F", "#2F4F4F", 
          "#1C1C1C", "#2E8B57", "#5F9EA0", "#8B4513"])'

# Highlighting links connected to node "C" (node 4)
links$LinkGroup <- ifelse(links$source == 4 | links$target == 4, "highlight", "normal")


link_color_scale <- 'd3.scaleOrdinal()
  .domain(["highlight", "normal"])
  .range(["#FFA500", "#A9A9A9"])'

sankeyNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target", Value = "value",
              NodeID = "name", LinkGroup = "LinkGroup", colourScale = dark_color_scale, 
              nodeWidth = 30, nodePadding = 10, fontSize = 14, 
              sinksRight = FALSE)


#### Second Sankey


score <- s3read_using(FUN = readRDS,
                      object = paste(set_wd3,"/score_pays.rds",sep=""),
                      bucket = bucket2, opts = list("region" = ""))

e <- label_IO %>%
  left_join(score, by = c("iso", "sector")) %>%
  mutate(score= if_else(is.na(score), 0.0001, score))


lac6A <- cbind(label_IO[latam_A,], eu_fpC[latam_A], e$score[latam_A])
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
