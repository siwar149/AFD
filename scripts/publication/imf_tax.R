imf_tax <- s3read_using(FUN = read.csv,
                  header = T,
                  object = "data/taxes_imf.csv",
                  bucket = bucket2, opts = list("region" = ""))

eur <- c("Austria", "Belgium", "Germany", "Spain", "France", "Croatia, Rep. of",
         "Hungary", "Italy", "Luxembourg", "Poland, Rep. of", "Portugal", "Slovak Rep.")

tx <- which(imf_tax$Country.Name %in% eur &
             (imf_tax$Classification.Name == "Taxes on payroll & workforce" |
              imf_tax$Classification.Name == "Taxes on income, profits, & capital gains") &
              imf_tax$Sector.Name == "General government" &
              imf_tax$Unit.Name == "Domestic currency" & imf_tax$Attribute == "Value")

imf_tax <- imf_tax[tx,]

imf_tax$X2019 <- as.numeric(imf_tax$X2019)

imf_tax$X2019 <- imf_tax$X2019 * 1.1201

imf_tax <- imf_tax %>%
  select(Country.Name, Classification.Name, X2019)

imf_tax <- imf_tax %>%
  pivot_wider(id_cols = "Country.Name", names_from = "Classification.Name", values_from = "X2019") %>%
  rename(tps = "Taxes on income, profits, & capital gains",
         twg = "Taxes on payroll & workforce",
         country = Country.Name)

imf_tax[c(3, 9, 11), 1] <- c("Croatia", "Poland", "Slovakia")

par <- var_tax[, c(2,4,5)]

imf_tax <- imf_tax %>%
  left_join(par, by = "country")

imf_tax <- imf_tax %>%
  mutate(tp = tps / ps,
         tw = twg / wg) %>%
  select(country, tp, tw)


