# Packages
library(tidyverse)
library(rvest) 
library(lubridate)

# Links
General.format <- "http://portalsivigila.ins.gov.co/Microdatos/datos_"
Code <- 452  
Years <- 2007:2018
Files.names <- paste0(Years, "_", Code, ".csv")
Files.location <- paste0("RawData/",Files.names)
Links <- paste0(General.format, Files.names)


# Download and read
Injury <- data.frame()

if (!dir.exists("RawData")){
  dir.create("RawData")
}

for (i in 1:length(Links)) 
{
  if (!file.exists(Files.location[i]))
  {
    download.file(Links[i], destfile = Files.location[i], mode = "wb")
  }
 
  Injury.year <- read.csv(Files.location[i]) %>% 
    select(UNI_MED, EDAD, FEC_CON)
  
  Injury <- rbind(Injury, Injury.year)
}

# Wrangling
Injury <- Injury %>%
  mutate(FEC_CON = ymd(FEC_CON))  

# Remove unnecessary variables
rm(list = setdiff(ls(), "Injury"))

# Save data frame   
if(!file.exists("Rdata"))
{
  dir.create("Rdata")
}

save(Injury, file = "Rdata/Injury.rda")


