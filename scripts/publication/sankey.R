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
  mutate(finLiab = L1_WM + L2_WM + L31_WM,
         absVal = (finLiab * 10^-2) * total_assets) %>%
  select(country, sector, total_assets, absVal) %>%
  filter(!sector %in% c("Mc", "Z0", "Zc")) %>%  # remove rows with sector values "Mc", "Z0", and "Zc"
  group_by(sector) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(aloc = absVal / sum(absVal))

bach %>%
  group_by(sector) %>%
  arrange(desc(aloc)) %>%
  mutate(aloc = aloc * 100)


bach %>%
  group_by(sector) %>%
  mutate(aloc = aloc * 100) %>%
  arrange(desc(aloc)) %>%
  mutate(
    cumsum_aloc = cumsum(aloc),
    to_keep = cumsum_aloc <= 70,
    final_sector = if_else(row_number() == n() & any(!to_keep), "X", as.character(sector)),
    to_keep = if_else(final_sector == "X", TRUE, to_keep)
  ) %>%
  ungroup() %>%
  filter(to_keep) %>%
  select(-cumsum_aloc, -to_keep)
