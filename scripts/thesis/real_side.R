#### Starting with the Leontief matrix

## configuring access key for personal storage

Sys.setenv("AWS_ACCESS_KEY_ID" = "JABHEOJOWQXHXVALIL78",
           "AWS_SECRET_ACCESS_KEY" = "ShqKC8gzUWmEB8xdULHMD9kSmR5HFP3ByqIUWvOi",
           "AWS_DEFAULT_REGION" = "us-east-1",
           "AWS_SESSION_TOKEN" = "eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJhY2Nlc3NLZXkiOiJKQUJIRU9KT1dRWEhYVkFMSUw3OCIsImFsbG93ZWQtb3JpZ2lucyI6WyIqIl0sImF1ZCI6WyJtaW5pby1kYXRhbm9kZSIsIm9ueXhpYSIsImFjY291bnQiXSwiYXV0aF90aW1lIjoxNzEwODUzNjgyLCJhenAiOiJvbnl4aWEiLCJlbWFpbCI6Im9ydGl6Z3V6bWFuc2FAYWZkLmZyIiwiZW1haWxfdmVyaWZpZWQiOnRydWUsImV4cCI6MTcxMjY1OTczMiwiZmFtaWx5X25hbWUiOiJPcnRpeiIsImdpdmVuX25hbWUiOiJTaXdhciIsImdyb3VwcyI6WyJVU0VSX09OWVhJQSIsImVzdGVlbSJdLCJpYXQiOjE3MTIwNTQ5MzEsImlzcyI6Imh0dHBzOi8vYXV0aC5sYWIuc3NwY2xvdWQuZnIvYXV0aC9yZWFsbXMvc3NwY2xvdWQiLCJqdGkiOiI0ODEyYzFmMi0zZWMwLTQ4ZjYtOGZiOC1mOGU0MjQzZGI4MmEiLCJuYW1lIjoiU2l3YXIgT3J0aXoiLCJwb2xpY3kiOiJzdHNvbmx5IiwicHJlZmVycmVkX3VzZXJuYW1lIjoic2l3YXIiLCJyZWFsbV9hY2Nlc3MiOnsicm9sZXMiOlsib2ZmbGluZV9hY2Nlc3MiLCJ1bWFfYXV0aG9yaXphdGlvbiIsImRlZmF1bHQtcm9sZXMtc3NwY2xvdWQiXX0sInJlc291cmNlX2FjY2VzcyI6eyJhY2NvdW50Ijp7InJvbGVzIjpbIm1hbmFnZS1hY2NvdW50IiwibWFuYWdlLWFjY291bnQtbGlua3MiLCJ2aWV3LXByb2ZpbGUiXX19LCJzY29wZSI6Im9wZW5pZCBwcm9maWxlIGdyb3VwcyBlbWFpbCIsInNlc3Npb25fc3RhdGUiOiIzZDg5OTJkNS0zNmU2LTRlZDYtOWVlZC1jNGQ4ZThiYmY5ZjUiLCJzaWQiOiIzZDg5OTJkNS0zNmU2LTRlZDYtOWVlZC1jNGQ4ZThiYmY5ZjUiLCJzdWIiOiIzY2IzYTMzZS0yMzA1LTRkMDEtYjZhYy05ZWFkNzQxYjMwNzEiLCJ0eXAiOiJCZWFyZXIifQ.60ycZNlIH-_C-xfKzOWOXnEci2LMNyIn91_7sPI9xK-GYkNjpNAuYa6Q4yG04z31FZG4ID2yuV0_C9agVibdFw",
           "AWS_S3_ENDPOINT"= "minio.lab.sspcloud.fr")

library("paws")
minio <- paws::s3(config = list(
  credentials = list(
    creds = list(
      access_key_id = Sys.getenv("AWS_ACCESS_KEY_ID"),
      secret_access_key = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
      session_token = Sys.getenv("AWS_SESSION_TOKEN")
    )),
  endpoint = paste0("https://", Sys.getenv("AWS_S3_ENDPOINT")),
  region = Sys.getenv("AWS_DEFAULT_REGION")))

minio$list_buckets()


## setting bucket and all

bucket1 = "projet-esteem"
set_wd1 <- "Gloria/matrices"

bucket2 = "siwar"
set_wd2 <- "data/Gloria"
set_wd3 <- "data/bio/rds"

## First find the most impactful sectors per country

score <- s3read_using(FUN = readRDS,
                      object = paste(set_wd3,"/score_pays.rds",sep=""),
                      bucket = bucket2, opts = list("region" = ""))


cSectors1 <- score %>%
  group_by(iso) %>%
  # Arrange entries within each group by score in descending order
  arrange(desc(score)) %>%
  # Select the top three entries within each group
  slice_head(n = 3)

cSectors2 <- score %>%
  group_by(iso) %>%
  arrange(desc(score)) %>%
  slice_head(n = 2)

cSectors3 <- score %>%
  group_by(iso) %>%
  arrange(desc(score)) %>%
  slice_head(n = 1)

length(unique(cSectors1$sector))
length(unique(cSectors2$sector))
length(unique(cSectors3$sector))

pSectors <- unique(cSectors3$sector)

pSectors

f <- as.data.frame(s3read_using(FUN = readRDS,
                      object = paste(set_wd2,"/FD_2019.rds",sep=""),
                      bucket = bucket2, opts = list("region" = "")))

label_f <- as.data.frame(s3read_using(FUN = readRDS,
                                object = paste(set_wd2,"/label_FD.rds",sep=""),
                                bucket = bucket2, opts = list("region" = "")))

## we get the consumption of EU 27

eu <-  c('AUT', 'BEL', 'BGR', 'HRV', 'CYP', 'CZE', 'DNK', 'EST',
         'FIN', 'FRA', 'DEU', 'GRC', 'HUN', 'IRL', 'ITA', 'LVA', 
         'LTU', 'LUX', 'MLT', 'NLD', 'POL', 'PRT', 'ROU', 'SVK', 
         'SVN', 'ESP', 'SWE', 'XEU')

## We get only the household consumption from final demand and EU

index <- which(label_f$V3 == label_f$V3[1] & label_f$V1 %in% eu)

feu <- f[, index]

feu <- as.data.frame(rowSums(f))

colnames(feu) <- "V1"

label_IO <- as.data.frame(s3read_using(FUN = readRDS,
                      object = paste(set_wd2,"/label_IO.rds",sep=""),
                      bucket = bucket2, opts = list("region" = "")))

index2 <- which(label_IO$V3 %in% pSectors)

feu[-index2,] <- 0




##############################################################

L <- as.data.frame(s3read_using(FUN = readRDS,
             object = paste(set_wd2,"/L_2019.rds",sep=""),
             bucket = bucket2, opts = list("region" = "")))