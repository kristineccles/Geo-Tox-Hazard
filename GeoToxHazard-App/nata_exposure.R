######################################################
# Prepare the NATA and Tox21 data for Shiny
# By: Kristin Eccles
# Date: May7th, 2021
# Written in R Version 4.0.2
######################################################
# Libraries
library(ggplot2)
library(viridis)
library(dplyr)
library(tigris)
library(rgdal)
library(sf)
library(purrr)
library(readr)
library(reshape2)
library(stringr)
library(spdep)
library(maps)
library(vroom)

#####################################################################################
#### Import data ####

ice_data <- get(load("GeoToxHazard-App/data/210105_ICE_cHTS_invitrodbv33.Rdata"))
ice_data <- subset(ice_data, new_hitc == 1)

df_chem_prop <- read.csv("GeoToxHazard-App/data/INVITRODBv3_20181017.csv")

# NATA ambient environmental concentrations
nata_df<- vroom("GeoToxHazard-App/data/2014_NATA_CONCS.csv")

# Add webnames
nata_chemicals <- vroom("GeoToxHazard-App/data/NATA_pollutant_names_casrn.csv")
#simplify names
nata_chemicals$web_name <- str_to_title(nata_chemicals$web_name, locale = "en")
nata_chemicals$web_name <- str_replace(nata_chemicals$web_name , " \\s*\\([^\\)]+\\)", "")

#Spatial Data
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
mapStates = map("state", fill = TRUE, plot = FALSE)

county_2014 <-st_read("GeoToxHazard-App/data/cb_2014_us_county_5m/cb_2014_us_county_5m.shp")
county_2014$GEOID <- as.numeric(county_2014$GEOID)

#limit to continental USA
county_2014<- subset(county_2014,  STATEFP != "02" )
county_2014<- subset(county_2014,  STATEFP != "15" )
county_2014<- subset(county_2014,  STATEFP != "60" )
county_2014<- subset(county_2014,  STATEFP != "66" )
county_2014<- subset(county_2014,  STATEFP != "69" )
county_2014<- subset(county_2014,  STATEFP != "72" )
county_2014<- subset(county_2014,  STATEFP != "78" )
county_2014$countyid <-as.numeric(paste0(county_2014$STATEFP, county_2014$COUNTYFP))

#####################################################################################
#### Prepare data ####
# Determine what the overlap between NATA chemicals and tox21 chemicals
# Join NATA and TOX21
nata_tox21 <- merge(ice_data[,c("casn", "aenm", "AC50", "ACC")], nata_chemicals,  by.x = "casn", by.y = "casrn") # n = 3533

# aggregate to county
nata_county <- aggregate(nata_df[,7:ncol(nata_df)] , by=list(nata_df$STCOFIPS) , FUN=mean) 

#melt
nata_county_stack <- melt(nata_county, id=1)
colnames(nata_county_stack) <- cbind("FIPS", "chemical", "concentration")
nata_county_stack$FIPS <- as.numeric (nata_county_stack$FIPS)

# add casrn
nata_county_stack <- left_join(nata_county_stack, nata_chemicals, by=c("chemical" = "smoke_name"), keep= FALSE)

#remove any missing data
nata_county_stack <- na.omit(nata_county_stack)

# add molecular weight
nata_county_stack <- left_join(nata_county_stack,df_chem_prop[,c("CASRN", "AVERAGE_MASS")], by=c("casrn" = "CASRN"), keep=FALSE)

# Join with Tox21
nata_tox21 <- left_join (nata_county_stack, nata_tox21, keep= FALSE, by= c("casrn" = "casn"))
#remove rows that dont have both NATA and tox21
nata_tox21<- nata_tox21[!is.na(nata_tox21$aenm), ]

#convert to from g/mol to ug/mol
nata_tox21$AVERAGE_MASS <- as.numeric(nata_tox21$AVERAGE_MASS)
nata_tox21$conc_uM <- nata_tox21$concentration * 10^-9 / nata_tox21$AVERAGE_MASS *10^6
nata_tox21$weight <- (nata_tox21$conc_uM*24*365*70)/(nata_tox21$ACC/1000)

##############################################################################################3
#### Assay Heat Map ####

heatmap_df<- as.data.frame(nata_tox21) %>%
  group_by(aenm, web_name.x) %>%
  summarise(mean=mean(ACC))
heatmap_df <- as.data.frame(heatmap_df)
colnames(heatmap_df) <- c("assay_name", "chemical_name", "ACC")
write.csv(heatmap_df, "GeoToxHazard-App/heatmap_df.csv")


#### RQ MAP ####
# Make dataframe for RQ Sum Map
nata_tox21_df<- as.data.frame(nata_tox21) %>%
  group_by(FIPS, aenm) %>%
  summarise(sum=sum(weight))
nata_tox21_df <- as.data.frame(nata_tox21_df)
colnames(nata_tox21_df) <-c("FIPS", "assay_name", "sum_HQ")

nata_tox21_tbl <-as.data.frame(dcast(nata_tox21_df, FIPS ~ assay_name, mean))
nata_tox21_tbl <- nata_tox21_tbl[, colSums(nata_tox21_tbl != 0) > 0]
nata_tox21_tbl <-na.omit(nata_tox21_tbl)
# add geography 
nata_tox21_sp <- left_join(county_2014, nata_tox21_tbl, by=c("countyid" = "FIPS"), keep=FALSE)
#save df for Shiny App
saveRDS(nata_tox21_sp, "GeoToxHazard-App/nata_tox21_sp.rds")

# Output list of assay names
assay_list <- as.data.frame(unique(nata_tox21_df$assay_name))
colnames(assay_list) <- "assay"
write.csv(assay_list, "GeoToxHazard-App/assay_list.csv", row.names = FALSE)


#### Count Map ####
nata_tox21$count <- 1
count_df<- as.data.frame(nata_tox21) %>%
  group_by(FIPS, aenm, web_name.x) %>%
  summarise(sum=sum(count))
count_df <- as.data.frame(count_df)
colnames(count_df) <- c("FIPS", "assay_name", "chem_count")


count_tbl <-as.data.frame(dcast(count_df, FIPS ~ assay_name, sum))


# add geography 
chem_count_sp <- left_join(county_2014, count_tbl, by=c("countyid" = "FIPS"), keep=FALSE)
#save df for Shiny App
saveRDS(chem_count_sp, "GeoToxHazard-App/chem_count_sp.rds")


#### GI Plot ####

#Gi* Cluster analysis
# find the neighbours - queens case
neighbours <- poly2nb(nata_tox21_sp, queen=TRUE) 
weighted_neighbours <- nb2listw(include.self(neighbours), zero.policy = TRUE, style="W")

gi_function <- function(x) localG(x, listw=weighted_neighbours, zero.policy=TRUE )
gi_matrix <- as.list(st_drop_geometry(na.omit(nata_tox21_sp[,11:653])))
local_g_unlist<- t(do.call(rbind,lapply(gi_matrix, gi_function)))
local_g<- as.data.frame(cbind(nata_tox21_sp$countyid, local_g_unlist))
colnames(local_g)[1] <- c("FIPS")

# add geography back
localG_sp <- left_join(county_2014, local_g, by=c("countyid" = "FIPS"), keep=FALSE)
#save df for Shiny App
saveRDS(localG_sp, "GeoToxHazard-App/localG_sp.rds")


a =subset(localG_df, NCCT_TPO_AUR_dn >= 3.886 |  NCCT_TPO_AUR_dn <= -3.886)
a$NCCT_TPO_AUR_dn
b= as.data.frame(localG_df$NCCT_TPO_AUR_dn)
