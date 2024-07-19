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
                       "X")) # Node 9


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
  2, 9, 17.811788),
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")


sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30)
