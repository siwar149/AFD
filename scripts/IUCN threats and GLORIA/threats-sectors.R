
threats_sectors <- read_excel("data/threats_to_sectors.xlsx", sheet = "Feuil1")
gloria_threats <- read_excel("data/gloria_to_threats.xlsx", sheet = "Feuil1")

threats_sectors <- threats_sectors %>%
  left_join(gloria_threats, by = "b_sec", relationship = "many-to-many")

threats_sectors <- threats_sectors %>%
  filter(!is.na(b_sec)) %>%
  filter(b_sec != "-")

gloria_threats$b_sec <- "Z"

threats_sectors <- threats_sectors %>%
  left_join(gloria_threats, by = "b_sec", relationship = "many-to-many")

threats_sectors$gloria_sec.x[614:length(threats_sectors$gloria_sec.x)] <-
  threats_sectors$gloria_sec.y[614:length(threats_sectors$gloria_sec.x)]

threats_sectors <- threats_sectors %>%
  rename(sector = gloria_sec.x) %>%
  select(-b_sec, -gloria_sec.y)

s3write_using(x = as.data.table(threats_sectors), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd,"/threats-sectors.rds",sep=""),
              bucket = bucket, opts = list("region" = ""))