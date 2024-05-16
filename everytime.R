# Setting the list of packages to be installed and attached
packages <- c('dplyr', 'ggplot2', 'data.table', 'aws.s3', 'ARTofR', 'readxl')
to_install <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)
lapply(packages, require, character.only=TRUE)

# Set Sys.setenv (go to Mon Compte > Connexion au storage > Aws.S3 and copy and paste here)

install.packages("paws", repos = "https://cloud.R-project.org")

Sys.setenv("AWS_ACCESS_KEY_ID" = "I70TOWLN85Q06IS1VALB",
           "AWS_SECRET_ACCESS_KEY" = "1ilh44cK3BqqVjcGSkHi+nmxo2EX+nECVR2Ktl5V",
           "AWS_DEFAULT_REGION" = "us-east-1",
           "AWS_SESSION_TOKEN" = "eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJhY2Nlc3NLZXkiOiJJNzBUT1dMTjg1UTA2SVMxVkFMQiIsImFsbG93ZWQtb3JpZ2lucyI6WyIqIl0sImF1ZCI6WyJtaW5pby1kYXRhbm9kZSIsIm9ueXhpYSIsImFjY291bnQiXSwiYXV0aF90aW1lIjoxNzE1ODUwODEyLCJhenAiOiJvbnl4aWEiLCJlbWFpbCI6Im9ydGl6Z3V6bWFuc2FAYWZkLmZyIiwiZW1haWxfdmVyaWZpZWQiOnRydWUsImV4cCI6MTcxNjQ1NTYxOSwiZmFtaWx5X25hbWUiOiJPcnRpeiIsImdpdmVuX25hbWUiOiJTaXdhciIsImdyb3VwcyI6WyJVU0VSX09OWVhJQSIsImVzdGVlbSJdLCJpYXQiOjE3MTU4NTA4MTgsImlzcyI6Imh0dHBzOi8vYXV0aC5sYWIuc3NwY2xvdWQuZnIvYXV0aC9yZWFsbXMvc3NwY2xvdWQiLCJqdGkiOiJjZDAzOWEwNy04NGM4LTQ5MWUtYjA5OC01NmE4NTEyMDZhZjQiLCJuYW1lIjoiU2l3YXIgT3J0aXoiLCJwb2xpY3kiOiJzdHNvbmx5IiwicHJlZmVycmVkX3VzZXJuYW1lIjoic2l3YXIiLCJyZWFsbV9hY2Nlc3MiOnsicm9sZXMiOlsib2ZmbGluZV9hY2Nlc3MiLCJ1bWFfYXV0aG9yaXphdGlvbiIsImRlZmF1bHQtcm9sZXMtc3NwY2xvdWQiXX0sInJlc291cmNlX2FjY2VzcyI6eyJhY2NvdW50Ijp7InJvbGVzIjpbIm1hbmFnZS1hY2NvdW50IiwibWFuYWdlLWFjY291bnQtbGlua3MiLCJ2aWV3LXByb2ZpbGUiXX19LCJzY29wZSI6Im9wZW5pZCBwcm9maWxlIGdyb3VwcyBlbWFpbCIsInNlc3Npb25fc3RhdGUiOiIyN2FlMDUxYi1lMjdkLTQ2ZDktYTViMy00YTI1ZDMyNDhkNmUiLCJzaWQiOiIyN2FlMDUxYi1lMjdkLTQ2ZDktYTViMy00YTI1ZDMyNDhkNmUiLCJzdWIiOiIzY2IzYTMzZS0yMzA1LTRkMDEtYjZhYy05ZWFkNzQxYjMwNzEiLCJ0eXAiOiJCZWFyZXIifQ.g3rdzh74wxRT1GbhQ92mOlmTrLm_WUd-jLv-PmLXkPF-vEBCM23zLk_-gOsgmTFsYLqF57C3RZYGk1FCHXptMg",
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

rm(list = ls())

bucketlist(region="") # Check if it's alright

# 0) Loading files 

#bucket <- "projet-esteem"
#set_wd <- "Gloria/data/057"

# alternative
#bucket <- "siwar"
#set_wd <- "data"

# Loading matrices (need to upload the .zio files and check dates first)
#OBJECTNAME <- s3read_using(FUN = data.table::fread,encoding = "UTF-8",
#                           object = paste(set_wd,"/20230320_120secMother_AllCountries_002_V-Results_2021_057_Markup001(full).csv",sep=""),
#                           bucket = bucket, opts = list("region" = ""))

# Loading matrices (need to upload the .zio files and check dates first)
#d <- s3read_using(FUN = data.table::fread,encoding = "UTF-8",
#                           object = paste(set_wd,"/export-bach-2019.csv",sep=""),
#                           bucket = bucket, opts = list("region" = ""))

# Saving files
#s3write_using(x = as.data.frame(OBJECTNAME), FUN = data.table::fwrite, na = "", 
#              object = paste("Gloria/matrices/x_2021.rds",sep=""),
#              bucket = bucket, opts = list("region" = ""))
