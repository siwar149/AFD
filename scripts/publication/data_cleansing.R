#### ECPLORING Magacho's approach

bucket1 <- "projet-esteem"
set_wd1 <- "Gloria/matrices"

bucket2 <- "siwar"
set_wd2 <- "data/Gloria"
set_wd3 <- "data/bio/rds"

library("tidyr")

# intensity of nSTAR
e <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd2,"/e_2019.rds",sep=""),
                  bucket = bucket2, opts = list("region" = ""))

# Output
x <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd1,"/x_2019.rds",sep=""),
                  bucket = bucket1, opts = list("region" = ""))

x <- x + 0.0001 # No data on Yemen's output
x <- as.data.table(x)

# Labels of final demand, output, and satelite accounts
label_IO <- as.data.table(s3read_using(FUN = readRDS,
                                       object = paste(set_wd2,"/label_IO.rds",sep=""),
                                       bucket = bucket2, opts = list("region" = "")))

colnames(label_IO) <- c("iso", "country", "sector")

f <- as.data.table(s3read_using(FUN = data.table::fread,
                                object = paste(set_wd2,"/f_2019.rds",sep=""),
                                bucket = bucket2, opts = list("region" = "")))

Z <- as.data.table(s3read_using(FUN = readRDS,
                                object = paste(set_wd2,"/IO_2019.rds",sep=""),
                                bucket = bucket2, opts = list("region" = "")))

x <- as.numeric(as.matrix(x))

# The B matrix
B <- solve(diag(x)) %*% as.matrix(Z)

# First step in getting the Gosh multiplier
G <- diag(dim(B)[1]) - B

# The Gosh multiplier
G <- solve(G)

# Saving my accomplishments of the day
s3write_using(x = as.data.table(B), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd2,"/B_2019.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))

s3write_using(x = as.data.table(G), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd2,"/G_2019.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))

eu1 <- c("AUT", "BEL", "DEU", "ESP", "FRA", "HRV", "HUN", "ITA",
         "LUX", "POL", "PRT", "SVK")

## Getting backward and forward induced extinction-risk
# backward

L <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd1,"/L_2019.rds",sep=""),
                  bucket = bucket1, opts = list("region" = ""))

G <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd1,"/G_2019.rds",sep=""),
                  bucket = bucket1, opts = list("region" = ""))

bk <- t(as.matrix(e)) %*% (as.matrix(L) - diag(dim(L)[1]))

bk <- as.data.table(t(bk))

# need to multiply by final demand to get the absolute footprint
bk <- bk * f

# weird results here
bk_eu <- cbind(label_IO[which(label_IO$iso %in% eu1),],
               bk[which(label_IO$iso %in% eu1),])

# forward (I am really not interested on the forward footprint of Europe)
fr <- (as.matrix(G) - diag(dim(G)[1])) %*% as.matrix(e)

fr <- as.data.table(fr)

fr <- fr * v

# Found the mistake here. Now everything fine
fr_eu <- cbind(label_IO[which(label_IO$iso %in% eu1),],
               fr[which(label_IO$iso %in% eu1),])

fr_latam6 <- cbind(label_IO[which(label_IO$iso %in% latam6),],
                   fr[which(label_IO$iso %in% latam6),])


# Interesting but not useful because I know what sectors I am going to shock
rm("bk", "fr", "fr_eu", "bk_eu", "G")



####### ESTEEM ########
# Starting to look at exposure

exp <- as.data.table(colSums(Z[latam6A, ..in_eu]))

exp <- exp / x[in_eu,] * 100

exp <- cbind(label_IO[in_eu,], exp)

exp1 <- exp %>%
  group_by(iso, country, NACE) %>%
  summarise(V1 = sum(V1)) %>%
  arrange(desc(V1))

head(exp1, 28) %>% print(n = 28)


# loss in output

#### Using the Gosh model to measure the impact of halting exports of 
# agricultural products to Europe

A <- as.data.table(s3read_using(FUN = data.table::fread,
                                object = paste(set_wd1,"/A_2019.rds",sep=""),
                                bucket = bucket1, opts = list("region" = "")))

# countries where the shock comes from
latam6 <- c("HND", "COL", "BRA", "GTM", "PER", "ECU")

nace <- read_excel("data/NACE-Gloria.xlsx", sheet = "Feuil1")

label_IO <- label_IO %>%
  left_join(nace, by = c("sector"="Gloria"))

in_eu <- which(label_IO$iso %in% eu1)

latam6A <- which(label_IO$iso %in% latam6 & label_IO$NACE == "A")

# Shocking only the exports to EU countries
A[latam6A, in_eu] <- 0
A[in_eu, latam6A] <- 0

# Shocking final demand
FD <- s3read_using(FUN = data.table::fread,
                   object = paste(set_wd1,"/FD_2019.rds",sep=""),
                   bucket = bucket1, opts = list("region" = ""))

label_FD <- s3read_using(FUN = data.table::fread,
                         object = paste("Gloria/labels/label_FD.rds",sep=""),
                         bucket = bucket1, opts = list("region" = ""))

fd_eu <- which(lavel_FD$V1 %in% eu1)

FD[latam6A, fd_eu] <- 0

f <- as.data.table(rowSums(FD))

rm(label_FD, FD)

v <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd1,"/VA_2019.rds",sep=""),
                  bucket = bucket1, opts = list("region" = ""))

txp <- as.data.table(t(v[2,]))



#v <- as.data.table(colSums(v))

#label_VA <- s3read_using(FUN = data.table::fread,
#                         object = paste("Gloria/labels/label_VA.rds",sep=""),
#                         bucket = bucket1, opts = list("region" = ""))

Lcj <- diag(dim(A)[1]) - A
Lcj <- solve(Lcj)
Lcj <- as.data.table(Lcj)

x1 <- as.matrix(Lcj) %*% as.matrix(f)

x1 <- as.data.table(x1)

#s3write_using(x = as.data.table(x1), FUN = data.table::fwrite, na = "", 
#              object = paste(set_wd2,"/x1_la6.rds",sep=""),
#              bucket = bucket2, opts = list("region" = ""))

#dx <- (x1 - x)/x * 100

#dx <- cbind(label_IO[in_eu,], dx[in_eu])


x1 <- s3read_using(FUN = data.table::fread,
                   object = paste(set_wd2,"/x1_la6.rds",sep=""),
                   bucket = bucket2, opts = list("region" = ""))

# Looking at taxes on production, profits and payroll
wg <- as.data.table(t(v[1,]))
ps <- as.data.table(t(v[4,]))

var <- cbind(label_IO[in_eu,], x1[in_eu,], x[in_eu,], txp[in_eu,], ps[in_eu,], wg[in_eu,])
colnames(var)[5] <- "x1"
colnames(var)[7] <- "tax"
colnames(var)[8] <- "ps"
colnames(var)[9] <- "wg"

var <- var %>%
  group_by(iso, country, NACE) %>%
  summarise(x1 = sum(x1),
            x = sum(x),
            tax = sum(tax),
            ps = sum(ps),
            wg = sum(wg)) %>%
  mutate(rate = tax / x)


var_tax <- var %>%
  mutate(x1t = x1 * rate,
         xt = x * rate) %>%
  group_by(iso, country) %>%
  summarise(fr = (1 - sum(x1t) / sum(xt))*100,
            ps = sum(ps),
            wg = sum(wg))


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




# Looking at the wage share generated by each sector
wg <- as.data.table(t(v[1,]))

wg <- cbind(label_IO[in_eu,], wg[in_eu,])

wg <- wg %>%
  group_by(iso, country, NACE) %>%
  summarise(V1 = sum(V1))

var <- cbind(var, wg[,4]) %>%
  rename(wg = V1) %>%
  mutate(wgr = wg / x)

var_wg <- var %>%
  mutate(x1w = x1 * wgr,
         xw = x * wgr) %>%
  group_by(iso, country) %>%
  summarise(w = (1- sum(x1w) / sum(xw))*100)

# Looking at the amount of employment generated by each sector
Q <- s3read_using(FUN = data.table::fread,
                  object = paste(set_wd1,"/QT_2019.rds",sep=""),
                  bucket = bucket1, opts = list("region" = ""))

label_Q <- s3read_using(FUN = data.table::fread,
                        object = paste("Gloria/labels/label_Q.rds",sep=""),
                        bucket = bucket1, opts = list("region" = ""))

emp <- as.data.table(colSums(Q[label_Q$Sat_head_indicator == "Employment", ]))

emp <- cbind(label_IO[in_eu,], emp[in_eu,])

emp <- emp %>%
  group_by(iso, country, NACE) %>%
  summarise(V1 = sum(V1))

var <- cbind(var, wg[,4]) %>%
  rename(emp = V1) %>%
  mutate(empr = emp / x)

var_emp <- var %>%
  mutate(x1n = x1 * empr,
         xn = x * empr) %>%
  group_by(iso, country) %>%
  summarise(n = (1 - sum(x1n) / sum(xn))*100)
