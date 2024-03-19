# Setting the list of packages to be installed and attached
packages <- c('dplyr', 'ggplot2', 'data.table', 'aws.s3', 'ARTofR', 'readxl')
to_install <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)
lapply(packages, require, character.only=TRUE)

# Set Sys.setenv (go to Mon Compte > Connexion au storage > Aws.S3 and copy and paste here)

install.packages("paws", repos = "https://cloud.R-project.org")

Sys.setenv("AWS_ACCESS_KEY_ID" = "W70KJO71REOHA9GV7NQJ",
           "AWS_SECRET_ACCESS_KEY" = "tne9cd8nUAOrICS6zS3nIiJF+2JGU4VBPO3ulpFY",
           "AWS_DEFAULT_REGION" = "us-east-1",
           "AWS_SESSION_TOKEN" = "eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJhY2Nlc3NLZXkiOiJXNzBLSk83MVJFT0hBOUdWN05RSiIsImFsbG93ZWQtb3JpZ2lucyI6WyIqIl0sImF1ZCI6WyJtaW5pby1kYXRhbm9kZSIsIm9ueXhpYSIsImFjY291bnQiXSwiYXV0aF90aW1lIjoxNzEwODUzNjgyLCJhenAiOiJvbnl4aWEiLCJlbWFpbCI6Im9ydGl6Z3V6bWFuc2FAYWZkLmZyIiwiZW1haWxfdmVyaWZpZWQiOnRydWUsImV4cCI6MTcxMTQ3ODM2OCwiZmFtaWx5X25hbWUiOiJPcnRpeiIsImdpdmVuX25hbWUiOiJTaXdhciIsImdyb3VwcyI6WyJVU0VSX09OWVhJQSIsImVzdGVlbSJdLCJpYXQiOjE3MTA4NzM1NjYsImlzcyI6Imh0dHBzOi8vYXV0aC5sYWIuc3NwY2xvdWQuZnIvYXV0aC9yZWFsbXMvc3NwY2xvdWQiLCJqdGkiOiJhZDU0NDQ2Yy1jMzA0LTQ3Y2MtOThkOC1mNGFlZTEzNTZkYWYiLCJuYW1lIjoiU2l3YXIgT3J0aXoiLCJwb2xpY3kiOiJzdHNvbmx5IiwicHJlZmVycmVkX3VzZXJuYW1lIjoic2l3YXIiLCJyZWFsbV9hY2Nlc3MiOnsicm9sZXMiOlsib2ZmbGluZV9hY2Nlc3MiLCJ1bWFfYXV0aG9yaXphdGlvbiIsImRlZmF1bHQtcm9sZXMtc3NwY2xvdWQiXX0sInJlc291cmNlX2FjY2VzcyI6eyJhY2NvdW50Ijp7InJvbGVzIjpbIm1hbmFnZS1hY2NvdW50IiwibWFuYWdlLWFjY291bnQtbGlua3MiLCJ2aWV3LXByb2ZpbGUiXX19LCJzY29wZSI6Im9wZW5pZCBwcm9maWxlIGdyb3VwcyBlbWFpbCIsInNlc3Npb25fc3RhdGUiOiIzZDg5OTJkNS0zNmU2LTRlZDYtOWVlZC1jNGQ4ZThiYmY5ZjUiLCJzaWQiOiIzZDg5OTJkNS0zNmU2LTRlZDYtOWVlZC1jNGQ4ZThiYmY5ZjUiLCJzdWIiOiIzY2IzYTMzZS0yMzA1LTRkMDEtYjZhYy05ZWFkNzQxYjMwNzEiLCJ0eXAiOiJCZWFyZXIifQ.bWjNUEUUbl0IuzmdxWMTKLVH42wmzb20LTLy9Hu4DYEdqRqBf605CSnes4-zUS8bcHYZTLg14NrEaa8UpWTr1w",
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


bucketlist(region="") # Check if it's alright

# 0) Loading files 

bucket <- "projet-esteem"
set_wd <- "Gloria/data/057"

# alternative
bucket <- "siwar"
set_wd <- "data"

# Loading matrices (need to upload the .zio files and check dates first)
OBJECTNAME <- s3read_using(FUN = data.table::fread,encoding = "UTF-8",
                           object = paste(set_wd,"/20230320_120secMother_AllCountries_002_V-Results_2021_057_Markup001(full).csv",sep=""),
                           bucket = bucket, opts = list("region" = ""))

# Loading matrices (need to upload the .zio files and check dates first)
d <- s3read_using(FUN = data.table::fread,encoding = "UTF-8",
                           object = paste(set_wd,"/export-bach-2019.csv",sep=""),
                           bucket = bucket, opts = list("region" = ""))

# Saving files
s3write_using(x = as.data.frame(OBJECTNAME), FUN = data.table::fwrite, na = "", 
              object = paste("Gloria/matrices/x_2021.rds",sep=""),
              bucket = bucket, opts = list("region" = ""))
