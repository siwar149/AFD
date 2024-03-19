# Setting the list of packages to be installed and attached
packages <- c('dplyr', 'ggplot2', 'data.table', 'aws.s3', 'ARTofR', 'readxl')
to_install <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)
lapply(packages, require, character.only=TRUE)

# Set Sys.setenv (go to Mon Compte > Connexion au storage > Aws.S3 and copy and paste here)

bucketlist(region="") # Check if it's alright

# 0) Loading files 

bucket <- "projet-esteem"
set_wd <- "Gloria/data/057"

# Loading matrices (need to upload the .zio files and check dates first)
OBJECTNAME <- s3read_using(FUN = data.table::fread,encoding = "UTF-8",
                           object = paste(set_wd,"/20230320_120secMother_AllCountries_002_V-Results_2021_057_Markup001(full).csv",sep=""),
                           bucket = bucket, opts = list("region" = ""))

# Saving files
s3write_using(x = as.data.frame(OBJECTNAME), FUN = data.table::fwrite, na = "", 
              object = paste("Gloria/matrices/x_2021.rds",sep=""),
              bucket = bucket, opts = list("region" = ""))
