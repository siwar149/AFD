setwd("~/thesis")
d <- read.csv("shs-data.csv", header = T)

d1 <- read.csv("shs-data-dis.csv", header = T)
colnames(d1)

d2 <- read.csv("sec.csv", header = T)

d3 <- read.csv("export-bach-2019.csv", header = T, sep = ";")
View(d2)

# EXIOBASE #
d4 <- read.delim('A.txt', header = T, sep = "\t")
