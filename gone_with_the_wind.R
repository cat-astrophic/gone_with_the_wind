# This project looks at population change due to renewable energy facilities

# Making sure requisite libraries are installed

list.of.packages <- c('modelsummary', 'ttidycensus', 'panelView', 'stargazer', 'sandwich', 'viridis',
                      'leaflet', 'ggplot2', 'tigris', 'lmtest', 'dplyr', 'DRDID', 'did', 'sf')

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,'Package'])]

if (length(new.packages) > 0) {
  
  install.packages(new.packages)
  
}

# Loading libraries

library(modelsummary)
library(tidycensus)
library(panelView)
library(stargazer)
library(sandwich)
library(viridis)
library(leaflet)
library(ggplot2)
library(tigris)
library(lmtest)
library(dplyr)
library(DRDID)
library(did)
library(sf)

# Project directory

direc <- 'D:/gone_with_the_wind/'

# Reading in the facilities and RUC code data

wind <- read_sf(paste0(direc, 'data/uswtdbSHP/uswtdb_V8_1_20250522.shp'))
sol <- read_sf(paste0(direc, 'data/uspvdbSHP/uspvdb_v3_0_20250430.shp'))
ruc <- read.csv(paste0(direc, 'data/Ruralurbancontinuumcodes2023.csv'))

# Remove observations where year == -9999 and where year > 2023

wind <- wind %>% filter(p_year > 0) %>% filter(p_year <= 2023)
sol <- sol %>% filter(p_year <= 2023)

# Get counties shapefile with FIPS codes for the US

cons <- counties()

# Making all shapefile share the same projection

cons <- st_transform(cons, st_crs(sol))
wind <- st_transform(wind, st_crs(sol))

# Creating a dataframe of counties and facility openings

county.list <- sort(unique(cons$GEOID))

wind.year <- c()
sol.year <- c()

for (c in county.list) {
  
  print(c)
  
  cons.tmp <- cons %>% filter(GEOID == c)
  
  wind.insides <- st_within(wind, cons.tmp)
  sol.insides <- st_within(sol, cons.tmp)
  
  wind.ids <- c()
  sol.ids <- c()
  
  for (i in 1:length(wind.insides)) {
    
    if (length(wind.insides[i][[1]]) > 0) {
      
      wind.ids <- c(wind.ids, i)
      
    }
    
  }
  
  for (i in 1:length(sol.insides)) {
    
    if (length(sol.insides[i][[1]]) > 0) {
      
      sol.ids <- c(sol.ids, i)
      
    }
    
  }
  
  if (length(wind.ids) > 0) {
    
    wind.year <- c(wind.year, wind$p_year[wind.ids[1]])
    
  } else {
    
    wind.year <- c(wind.year, NA)
    
  }
  
  if (length(sol.ids) > 0) {
    
    sol.year <- c(sol.year, sol$p_year[sol.ids[1]])
    
  } else {
    
    sol.year <- c(sol.year, NA)
    
  }
  
}

cdf <- as.data.frame(cbind(county.list, wind.year, sol.year))
colnames(cdf) <- c('FIPS', 'Wind_Year', 'Solar_Year')

# Bringing in ACS data

acs.pop <- as.data.frame(NULL)

for (y in 2009:2023) {
  
  tmp <- get_acs(geography = 'county', year = y, variables = 'DP05_0001')
  tmp$Year <- rep(y, nrow(tmp))
  acs.pop <- rbind(acs.pop, tmp)
  
}

acs.data <- acs.pop[,c(1,4,6)]
colnames(acs.data)[2] <- 'Population'

# Getting covariates

v09 <- c('DP03_0063', 'DP03_0009P', 'DP02_0061P', 'DP02_0067P')
v18 <- c('DP03_0062', 'DP03_0009P', 'DP02_0061P', 'DP02_0067P')
v23 <- c('DP03_0062', 'DP03_0009P', 'DP02_0062P', 'DP02_0068P')

acs.x <- as.data.frame(get_acs(geography = 'county', year = 2009, variables = v09))
acs.x$Year <- rep(2009, nrow(acs.x))

v <- c()

for (i in 1:nrow(acs.x)) {
  
  print(i)
  v <- c(v, which(v09 == acs.x$variable[i]))
  
}

acs.x$V <- v

for (y in 2010:2018) {
  
  tmp <- get_acs(geography = 'county', year = y, variables = v18)
  tmp$Year <- rep(y, nrow(tmp))
  
  v <- c()
  
  for (i in 1:nrow(tmp)) {
    
    print(i)
    v <- c(v, which(v18 == tmp$variable[i]))
    
  }
  
  tmp$V <- v
  acs.x <- rbind(acs.x, tmp)
  
}

for (y in 2019:2023) {
  
  tmp <- get_acs(geography = 'county', year = y, variables = v23)
  tmp$Year <- rep(y, nrow(tmp))
  
  v <- c()
  
  for (i in 1:nrow(tmp)) {
    
    print(i)
    v <- c(v, which(v23 == tmp$variable[i]))
    
  }
  
  tmp$V <- v
  acs.x <- rbind(acs.x, tmp)
  
}

acs.inc <- c()
acs.unemp <- c()
acs.hs <- c()
acs.bs <- c()

for (i in 1:nrow(acs.pop)) {
  
  print(i)
  tmp <- acs.x %>% filter(GEOID == acs.pop$GEOID[i]) %>% filter(Year == acs.pop$Year[i])
  
  acs.inc <- c(acs.inc, tmp[which(tmp$V == 1),]$estimate[1])
  acs.unemp <- c(acs.unemp, tmp[which(tmp$V == 2),]$estimate[1])
  acs.hs <- c(acs.hs, tmp[which(tmp$V == 3),]$estimate[1])
  acs.bs <- c(acs.bs, tmp[which(tmp$V == 4),]$estimate[1])
  
}

acs.data$Income <- log(acs.inc)
acs.data$Unemployment <- acs.unemp
acs.data$HS <- acs.hs
acs.data$BS <- acs.bs

# Subsetting for the contiguous US

acs.data$State <- substr(acs.data$GEOID, 1, 2)
acs.data <- acs.data %>% filter(! State %in% c('02', '15', '72'))

# Creating lagged population data

lag.pop <- c()

for (i in 1:nrow(acs.data)) {
  
  print(i)
  tmp <- acs.data %>% filter(GEOID == acs.data$GEOID[i]) %>% filter(Year == (acs.data$Year[i]-1))
  
  if (nrow(tmp) > 0) {
    
    lag.pop <- c(lag.pop, tmp$Population[1])
    
  } else {
    
    lag.pop <- c(lag.pop, NA)
    
  }
  
}

acs.data$Lagged <- lag.pop

# Dropping 2009 from the data since no lagged data exists for 2009

acs.data <- acs.data %>% filter(Year > 2009)

# Adding SDiD data

sol.treats <- c()
wind.treats <- c()

sol.posts <- c()
wind.posts <- c()

sol.y <- c()
wind.y <- c()

for (i in 1:nrow(acs.data)) {
  
  print(i)
  
  if (nrow(cdf[which(cdf$FIPS == acs.data$GEOID[i]),]) == 0) {
    
    sol.treats <- c(sol.treats, 0)
    sol.posts <- c(sol.posts, 0)
    
    wind.treats <- c(wind.treats, 0)
    wind.posts <- c(wind.posts, 0)
    
  } else {
    
    if (is.na(cdf[which(cdf$FIPS == acs.data$GEOID[i]),]$Solar_Year)) {
      
      sol.treats <- c(sol.treats, 0)
      sol.posts <- c(sol.posts, 0)
      
    } else {
      
      sol.treats <- c(sol.treats, 1)
      
      if (cdf[which(cdf$FIPS == acs.data$GEOID[i]),]$Solar_Year[1] <= acs.data$Year[i]) {
        
        sol.posts <- c(sol.posts, 1)
        
      } else {
        
        sol.posts <- c(sol.posts, 0)
        
      }
      
    }
    
    if (is.na(cdf[which(cdf$FIPS == acs.data$GEOID[i]),]$Wind_Year)) {
      
      wind.treats <- c(wind.treats, 0)
      wind.posts <- c(wind.posts, 0)
      
    } else {
      
      wind.treats <- c(wind.treats, 1)
      
      if (cdf[which(cdf$FIPS == acs.data$GEOID[i]),]$Wind_Year[1] <= acs.data$Year[i]) {
        
        wind.posts <- c(wind.posts, 1)
        
      } else {
        
        wind.posts <- c(wind.posts, 0)
        
      }
      
    }
    
  }
  
}

acs.data <- cbind(acs.data, sol.treats, wind.treats, sol.posts, wind.posts)
colnames(acs.data)[10:13] <- c('Treated_Solar', 'Treated_Wind', 'Post_Solar', 'Post_Wind')

acs.data$DiD_Solar <- acs.data$Treated_Solar * acs.data$Post_Solar
acs.data$DiD_Wind <- acs.data$Treated_Wind * acs.data$Post_Wind

# Log population data

acs.data$LogPop <- log(acs.data$Population)
acs.data$LogLag <- log(acs.data$Lagged)

# Creating variables for the callaway and sant'anna sdid model

acs.data$Time <- acs.data$Year - 2009

sol.xxx <- c()
wind.xxx <- c()

for (i in 1:nrow(acs.data)) {
  
  print(i)
  tmp <- acs.data %>% filter(GEOID == acs.data$GEOID[i])
  
  if (max(tmp$Treated_Solar) == 0) {
    
    sol.xxx <- c(sol.xxx, 0)
    
  } else {
    
    sol.xxx <- c(sol.xxx, min(which(tmp$Post_Solar == 1)))
    
  }
  
  if (max(tmp$Treated_Wind) == 0) {
    
    wind.xxx <- c(wind.xxx, 0)
    
  } else {
    
    wind.xxx <- c(wind.xxx, min(which(tmp$Post_Wind == 1)))
    
  }
  
}

acs.data$Sol_Treat_Time <- sol.xxx
acs.data$Wind_Treat_Time <- wind.xxx

# Make ids numeric for att_gt

uniq <- sort(unique(acs.data$GEOID))

id.vals <- c()

for (i in 1:nrow(acs.data)) {
  
  print(i)
  id.vals <- c(id.vals, which(uniq == acs.data$GEOID[i]))
  
}

acs.data$ID <- id.vals

# Remove units treated in first period (or prior)

sol.data <- acs.data %>% filter(Sol_Treat_Time != 1)
wind.data <- acs.data %>% filter(Wind_Treat_Time != 1)

# Running the staggered diff-in-diff for solar without additional controls

sol_sdid1 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', xformla = ~ LogLag, clustervars = 'ID', data = sol.data)
sol_sdid2 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', xformla = ~ LogLag, control_group = 'notyettreated', clustervars = 'ID', data = sol.data)
sol_sdid3 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', anticipation = 1, xformla = ~ LogLag, clustervars = 'ID', data = sol.data)
sol_sdid4 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', anticipation = 1, xformla = ~ LogLag, control_group = 'notyettreated', clustervars = 'ID', data = sol.data)

sol_sdid1_cs <- aggte(sol_sdid1, type = 'dynamic')
sol_sdid2_cs <- aggte(sol_sdid2, type = 'dynamic')
sol_sdid3_cs <- aggte(sol_sdid3, type = 'dynamic')
sol_sdid4_cs <- aggte(sol_sdid4, type = 'dynamic')

# Viewing results

summary(sol_sdid1_cs)
summary(sol_sdid2_cs)
summary(sol_sdid3_cs)
summary(sol_sdid4_cs)

ggdid(sol_sdid1_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(sol_sdid2_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(sol_sdid3_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(sol_sdid4_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities -') + theme(plot.title = element_text(hjust = 0.5))

# Running the staggered diff-in-diff for wind without additional controls

wind_sdid1 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', xformla = ~ LogLag, clustervars = 'ID', data = wind.data)
wind_sdid2 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', xformla = ~ LogLag, control_group = 'notyettreated', clustervars = 'ID', data = wind.data)
wind_sdid3 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', anticipation = 1, xformla = ~ LogLag, clustervars = 'ID', data = wind.data)
wind_sdid4 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', anticipation = 1, xformla = ~ LogLag, control_group = 'notyettreated', clustervars = 'ID', data = wind.data)

wind_sdid1_cs <- aggte(wind_sdid1, type = 'dynamic')
wind_sdid2_cs <- aggte(wind_sdid2, type = 'dynamic')
wind_sdid3_cs <- aggte(wind_sdid3, type = 'dynamic')
wind_sdid4_cs <- aggte(wind_sdid4, type = 'dynamic')

# Viewing results

summary(wind_sdid1_cs)
summary(wind_sdid2_cs)
summary(wind_sdid3_cs)
summary(wind_sdid4_cs)

ggdid(wind_sdid1_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(wind_sdid2_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(wind_sdid3_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(wind_sdid4_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines -') + theme(plot.title = element_text(hjust = 0.5))

# Running the staggered diff-in-diff for solar with additional controls

sol_sdid11 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = sol.data)
sol_sdid22 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = sol.data)
sol_sdid33 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = sol.data)
sol_sdid44 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = sol.data)

sol_sdid11_cs <- aggte(sol_sdid11, type = 'dynamic')
sol_sdid22_cs <- aggte(sol_sdid22, type = 'dynamic')
sol_sdid33_cs <- aggte(sol_sdid33, type = 'dynamic')
sol_sdid44_cs <- aggte(sol_sdid44, type = 'dynamic')

# Viewing results

summary(sol_sdid11_cs)
summary(sol_sdid22_cs)
summary(sol_sdid33_cs)
summary(sol_sdid44_cs)

ggdid(sol_sdid11_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(sol_sdid22_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(sol_sdid33_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(sol_sdid44_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities -') + theme(plot.title = element_text(hjust = 0.5))

# Running the staggered diff-in-diff for wind with additional controls

wind_sdid11 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = wind.data)
wind_sdid22 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = wind.data)
wind_sdid33 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = wind.data)
wind_sdid44 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = wind.data)

wind_sdid11_cs <- aggte(wind_sdid11, type = 'dynamic')
wind_sdid22_cs <- aggte(wind_sdid22, type = 'dynamic')
wind_sdid33_cs <- aggte(wind_sdid33, type = 'dynamic')
wind_sdid44_cs <- aggte(wind_sdid44, type = 'dynamic')

# Viewing results

summary(wind_sdid11_cs)
summary(wind_sdid22_cs)
summary(wind_sdid33_cs)
summary(wind_sdid44_cs)

ggdid(wind_sdid11_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(wind_sdid22_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(wind_sdid33_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(wind_sdid44_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines -') + theme(plot.title = element_text(hjust = 0.5))

# Creating a leaflet of the locations of wind facilities

wind <- st_transform(wind, 4269)

nombres <- c('United States Virgin Islands', 'Commonwealth of the Northern Mariana Islands', 'Puerto Rico', 'Guam', 'Hawaii', 'Alaska', 'American Samoa')
nat <- states()
nat <- nat %>% filter(! NAME %in% nombres)

windsides <- st_within(wind, nat)

wf <- c()

for (i in 1:length(windsides)) {
  
  print(i)
  wf <- c(wf, as.integer(length(windsides[[i]]) > 0))
  
}

wind$Fig <- wf

wind2 <- wind %>% filter(Fig == 1)

leaflet(wind2) %>% addCircleMarkers(data = wind2$geometry, opacity = 1, radius = .1, color = 'purple') %>% addProviderTiles(providers$CartoDB.Positron)

# Creating a leaflet of the locations of solar facilities

sol2 <- sol %>% st_set_geometry(NULL)
sol2 <- st_as_sf(sol2, coords = c('xlong', 'ylat'))
sol2 <- sol2 %>% st_set_crs(4269)

sunsides <- st_within(sol2, nat)

has.solar <- c()

for (i in 1:length(sunsides)) {
  
  print(i)
  
  if (length(sunsides[[i]]) > 0) {
    
    has.solar <- c(has.solar, as.integer(length(sunsides[[i]]) > 0))
    
  } else {
    
    has.solar <- c(has.solar, 0)
    
  }
  
}

sol2$Fig <- has.solar

sol2 <- sol2 %>% filter(Fig == 1)

leaflet(sol2) %>% addCircleMarkers(data = sol2$geometry, opacity = 1, radius = .1, color = 'orange') %>% addProviderTiles(providers$CartoDB.Positron)

# Prep for heterogeneity analyses across the urban-rural divide

ruc <- ruc %>% filter(Attribute == 'RUCC_2023')
ruc$Value <- as.integer(ruc$Value)
ruc$Urban <- as.integer(ruc$Value <= 3)

acs.data$FIPS <- as.integer(acs.data$GEOID)

hurb <- c()

for (i in 1:nrow(acs.data)) {
  
  print(i)
  tmp <- ruc %>% filter(FIPS == acs.data$FIPS[i])
  hurb <- c(hurb, max(tmp$Urban[1], 0, na.rm = TRUE))
  
}

acs.data$Urban <- hurb

sol.data <- acs.data %>% filter(Sol_Treat_Time != 1)
wind.data <- acs.data %>% filter(Wind_Treat_Time != 1)

# Running heterogeneity analyses across the urban-rural divide

# Urban counties

# Running the staggered diff-in-diff for solar

usol_sdid1 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = sol.data[which(sol.data$Urban == 1),])
usol_sdid2 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = sol.data[which(sol.data$Urban == 1),])
usol_sdid3 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = sol.data[which(sol.data$Urban == 1),])
usol_sdid4 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = sol.data[which(sol.data$Urban == 1),])

usol_sdid1_cs <- aggte(usol_sdid1, type = 'dynamic')
usol_sdid2_cs <- aggte(usol_sdid2, type = 'dynamic')
usol_sdid3_cs <- aggte(usol_sdid3, type = 'dynamic')
usol_sdid4_cs <- aggte(usol_sdid4, type = 'dynamic')

# Viewing results

summary(usol_sdid1_cs)
summary(usol_sdid2_cs)
summary(usol_sdid3_cs)
summary(usol_sdid4_cs)

ggdid(usol_sdid1_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities in Urban Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(usol_sdid2_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities in Urban Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(usol_sdid3_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities in Urban Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(usol_sdid4_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities in Urban Counties -') + theme(plot.title = element_text(hjust = 0.5))

# Running the staggered diff-in-diff for wind

uwind_sdid1 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = wind.data[which(wind.data$Urban == 1),])
uwind_sdid2 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = wind.data[which(wind.data$Urban == 1),])
uwind_sdid3 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = wind.data[which(wind.data$Urban == 1),])
uwind_sdid4 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = wind.data[which(wind.data$Urban == 1),])

uwind_sdid1_cs <- aggte(uwind_sdid1, type = 'dynamic')
uwind_sdid2_cs <- aggte(uwind_sdid2, type = 'dynamic')
uwind_sdid3_cs <- aggte(uwind_sdid3, type = 'dynamic')
uwind_sdid4_cs <- aggte(uwind_sdid4, type = 'dynamic')

# Viewing results

summary(uwind_sdid1_cs)
summary(uwind_sdid2_cs)
summary(uwind_sdid3_cs)
summary(uwind_sdid4_cs)

ggdid(uwind_sdid1_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines in Urban Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(uwind_sdid2_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines in Urban Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(uwind_sdid3_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines in Urban Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(uwind_sdid4_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines in Urban Counties -') + theme(plot.title = element_text(hjust = 0.5))

# Rural counties

# Running the staggered diff-in-diff for solar

rsol_sdid1 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = sol.data[which(sol.data$Urban == 0),])
rsol_sdid2 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = sol.data[which(sol.data$Urban == 0),])
rsol_sdid3 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = sol.data[which(sol.data$Urban == 0),])
rsol_sdid4 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = sol.data[which(sol.data$Urban == 0),])

rsol_sdid1_cs <- aggte(rsol_sdid1, type = 'dynamic')
rsol_sdid2_cs <- aggte(rsol_sdid2, type = 'dynamic')
rsol_sdid3_cs <- aggte(rsol_sdid3, type = 'dynamic')
rsol_sdid4_cs <- aggte(rsol_sdid4, type = 'dynamic')

# Viewing results

summary(rsol_sdid1_cs)
summary(rsol_sdid2_cs)
summary(rsol_sdid3_cs)
summary(rsol_sdid4_cs)

ggdid(rsol_sdid1_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities in Rural Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(rsol_sdid2_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities in Rural Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(rsol_sdid3_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities in Rural Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(rsol_sdid4_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities in Rural Counties -') + theme(plot.title = element_text(hjust = 0.5))

# Running the staggered diff-in-diff for wind

rwind_sdid1 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = wind.data[which(wind.data$Urban == 0),])
rwind_sdid2 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = wind.data[which(wind.data$Urban == 0),])
rwind_sdid3 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = wind.data[which(wind.data$Urban == 0),])
rwind_sdid4 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = wind.data[which(wind.data$Urban == 0),])

rwind_sdid1_cs <- aggte(rwind_sdid1, type = 'dynamic')
rwind_sdid2_cs <- aggte(rwind_sdid2, type = 'dynamic')
rwind_sdid3_cs <- aggte(rwind_sdid3, type = 'dynamic')
rwind_sdid4_cs <- aggte(rwind_sdid4, type = 'dynamic')

# Viewing results

summary(rwind_sdid1_cs)
summary(rwind_sdid2_cs)
summary(rwind_sdid3_cs)
summary(rwind_sdid4_cs)

ggdid(rwind_sdid1_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines in Rural Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(rwind_sdid2_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines in Rural Counties -') + theme(plot.title = element_text(hjust = 0.5)) + ylim(c(-.1, .1))
ggdid(rwind_sdid3_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines in Rural Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(rwind_sdid4_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines in Rural Counties -') + theme(plot.title = element_text(hjust = 0.5))

# Creating a regression results table

atandt <- rbind(c(wind_sdid1_cs$overall.att, wind_sdid2_cs$overall.att, wind_sdid3_cs$overall.att, wind_sdid4_cs$overall.att),
                c(sol_sdid1_cs$overall.att, sol_sdid2_cs$overall.att, sol_sdid3_cs$overall.att, sol_sdid4_cs$overall.att),
                c(wind_sdid11_cs$overall.att, wind_sdid22_cs$overall.att, wind_sdid33_cs$overall.att, wind_sdid44_cs$overall.att),
                c(sol_sdid11_cs$overall.att, sol_sdid22_cs$overall.att, sol_sdid33_cs$overall.att, sol_sdid44_cs$overall.att),
                c(rwind_sdid1_cs$overall.att, rwind_sdid2_cs$overall.att, rwind_sdid3_cs$overall.att, rwind_sdid4_cs$overall.att),
                c(rsol_sdid1_cs$overall.att, rsol_sdid2_cs$overall.att, rsol_sdid3_cs$overall.att, rsol_sdid4_cs$overall.att),
                c(uwind_sdid1_cs$overall.att, uwind_sdid2_cs$overall.att, uwind_sdid3_cs$overall.att, uwind_sdid4_cs$overall.att),
                c(usol_sdid1_cs$overall.att, usol_sdid2_cs$overall.att, usol_sdid3_cs$overall.att, usol_sdid4_cs$overall.att))

serrs <- rbind(c(wind_sdid1_cs$overall.se, wind_sdid2_cs$overall.se, wind_sdid3_cs$overall.se, wind_sdid4_cs$overall.se),
               c(sol_sdid1_cs$overall.se, sol_sdid2_cs$overall.se, sol_sdid3_cs$overall.se, sol_sdid4_cs$overall.se),
               c(wind_sdid11_cs$overall.se, wind_sdid22_cs$overall.se, wind_sdid33_cs$overall.se, wind_sdid44_cs$overall.se),
               c(sol_sdid11_cs$overall.se, sol_sdid22_cs$overall.se, sol_sdid33_cs$overall.se, sol_sdid44_cs$overall.se),
               c(rwind_sdid1_cs$overall.se, rwind_sdid2_cs$overall.se, rwind_sdid3_cs$overall.se, rwind_sdid4_cs$overall.se),
               c(rsol_sdid1_cs$overall.se, rsol_sdid2_cs$overall.se, rsol_sdid3_cs$overall.se, rsol_sdid4_cs$overall.se),
               c(uwind_sdid1_cs$overall.se, uwind_sdid2_cs$overall.se, uwind_sdid3_cs$overall.se, uwind_sdid4_cs$overall.se),
               c(usol_sdid1_cs$overall.se, usol_sdid2_cs$overall.se, usol_sdid3_cs$overall.se, usol_sdid4_cs$overall.se))

t.stats <- atandt / serrs

p.10 <- matrix(as.integer(abs(t.stats) > 1.645), 8, 4)
p.05 <- matrix(as.integer(abs(t.stats) > 1.960), 8, 4)
p.01 <- matrix(as.integer(abs(t.stats) > 2.576), 8, 4)

stars <- matrix(0, 8, 4)

for (i in 1:8) {
  
  for (j in 1:4) {
    
    if (p.01[i,j] == 1) {
      
      stars[i,j] <- 3
      
    } else if (p.05[i,j] == 1) {
      
      stars[i,j] <- 2
      
    } else if (p.10[i,j] == 1) {
      
      stars[i,j] <- 1
      
    }
    
  }
  
}

atandt <- round(atandt, 3)
serrs <- round(serrs, 3)

write.csv(atandt, paste0(direc, 'results/coefs.txt'), row.names = FALSE)
write.csv(serrs, paste0(direc, 'results/serrs.txt'), row.names = FALSE)
write.csv(stars, paste0(direc, 'results/stars.txt'), row.names = FALSE)

# Creating a summary statistics figure

sums <- acs.data[,c('LogPop', 'Treated_Solar', 'DiD_Solar', 'Treated_Wind', 'DiD_Wind', 'Urban', 'Income', 'Unemployment', 'HS', 'BS')]

colnames(sums) <- c('Log Population', 'Treated: Solar', 'Treated x Post: Solar', 'Treated: Wind', 'Treated x Post: Wind', 'Urban', 'Log Income', 'Unemployment Rate', 'High School Grad %', 'College Grad %')

datasummary_skim(sums, fmt = '%.3f')

# Placebo testing

# (1) randomize treatment and timing together

sol.data <- acs.data %>% filter(Sol_Treat_Time != 1)
wind.data <- acs.data %>% filter(Wind_Treat_Time != 1)

sol.data <- sol.data %>% filter(!is.na(Sol_Treat_Time)) %>% filter(!is.na(LogPop)) %>% filter(!is.na(LogLag)) %>% filter(!is.na(Income)) %>% filter(!is.na(Unemployment)) %>% filter(!is.na(HS)) %>% filter(!is.na(BS))
wind.data <- wind.data %>% filter(!is.na(Wind_Treat_Time)) %>% filter(!is.na(LogPop)) %>% filter(!is.na(LogLag)) %>% filter(!is.na(Income)) %>% filter(!is.na(Unemployment)) %>% filter(!is.na(HS)) %>% filter(!is.na(BS))

set.seed(420)

s_ids <- sample(nrow(sol.data))
w_ids <- sample(nrow(wind.data))

sol.data$Sol_Treat_Time_P <- sol.data$Sol_Treat_Time[s_ids]
wind.data$Wind_Treat_Time_P <- wind.data$Wind_Treat_Time[w_ids]

# Running the staggered diff-in-diff for solar with additional controls

sol_sdid1p1 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time_P', xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = sol.data)
sol_sdid2p1 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time_P', xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = sol.data)
sol_sdid3p1 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time_P', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = sol.data)
sol_sdid4p1 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time_P', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = sol.data)

sol_sdid1p1_cs <- aggte(sol_sdid1p1, type = 'dynamic')
sol_sdid2p1_cs <- aggte(sol_sdid2p1, type = 'dynamic')
sol_sdid3p1_cs <- aggte(sol_sdid3p1, type = 'dynamic')
sol_sdid4p1_cs <- aggte(sol_sdid4p1, type = 'dynamic')

# Viewing results

summary(sol_sdid1p1_cs)
summary(sol_sdid2p1_cs)
summary(sol_sdid3p1_cs)
summary(sol_sdid4p1_cs)

ggdid(sol_sdid1p1_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities -\n\n- Placebo Test -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(sol_sdid2p1_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities -\n\n- Placebo Test -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(sol_sdid3p1_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities -\n\n- Placebo Test -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(sol_sdid4p1_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities -\n\n- Placebo Test -') + theme(plot.title = element_text(hjust = 0.5))

# Running the staggered diff-in-diff for wind with additional controls

wind_sdid1p1 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time_P', xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = wind.data)
wind_sdid2p1 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time_P', xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = wind.data)
wind_sdid3p1 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time_P', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = wind.data)
wind_sdid4p1 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time_P', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = wind.data)

wind_sdid1p1_cs <- aggte(wind_sdid1p1, type = 'dynamic')
wind_sdid2p1_cs <- aggte(wind_sdid2p1, type = 'dynamic')
wind_sdid3p1_cs <- aggte(wind_sdid3p1, type = 'dynamic')
wind_sdid4p1_cs <- aggte(wind_sdid4p1, type = 'dynamic')

# Viewing results

summary(wind_sdid1p1_cs)
summary(wind_sdid2p1_cs)
summary(wind_sdid3p1_cs)
summary(wind_sdid4p1_cs)

ggdid(wind_sdid1p1_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines -\n\n- Placebo Test -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(wind_sdid2p1_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines -\n\n- Placebo Test -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(wind_sdid3p1_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines -\n\n- Placebo Test -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(wind_sdid4p1_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines -\n\n- Placebo Test -') + theme(plot.title = element_text(hjust = 0.5))

# (2) create artificial outcomes - show results and event study plots

sol.data$Placebo <- sol.data$LogPop[s_ids]
wind.data$Placebo <- wind.data$LogPop[w_ids]

sol.data$LogLag2 <- sol.data$LogLag[s_ids]
wind.data$LogLag2 <- wind.data$LogLag[w_ids]

sol.data$Income2 <- sol.data$Income[s_ids]
wind.data$Income2 <- wind.data$Income[w_ids]

sol.data$Unemployment2 <- sol.data$Unemployment[s_ids]
wind.data$Unemployment2 <- wind.data$Unemployment[w_ids]

sol.data$HS2 <- sol.data$HS[s_ids]
wind.data$HS2 <- wind.data$HS[w_ids]

sol.data$BS2 <- sol.data$BS[s_ids]
wind.data$BS2 <- wind.data$BS[w_ids]

# Running the staggered diff-in-diff for solar with additional controls

sol_sdid1p2 <- att_gt(yname = 'Placebo', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', xformla = ~ LogLag2 + Income2 + Unemployment2 + HS2 + BS2, clustervars = 'ID', data = sol.data)
sol_sdid2p2 <- att_gt(yname = 'Placebo', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', xformla = ~ LogLag2 + Income2 + Unemployment2 + HS2 + BS2, control_group = 'notyettreated', clustervars = 'ID', data = sol.data)
sol_sdid3p2 <- att_gt(yname = 'Placebo', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', anticipation = 1, xformla = ~ LogLag2 + Income2 + Unemployment2 + HS2 + BS2, clustervars = 'ID', data = sol.data)
sol_sdid4p2 <- att_gt(yname = 'Placebo', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', anticipation = 1, xformla = ~ LogLag2 + Income2 + Unemployment2 + HS2 + BS2, control_group = 'notyettreated', clustervars = 'ID', data = sol.data)

sol_sdid1p2_cs <- aggte(sol_sdid1p2, type = 'dynamic')
sol_sdid2p2_cs <- aggte(sol_sdid2p2, type = 'dynamic')
sol_sdid3p2_cs <- aggte(sol_sdid3p2, type = 'dynamic')
sol_sdid4p2_cs <- aggte(sol_sdid4p2, type = 'dynamic')

# Viewing results

summary(sol_sdid1p2_cs)
summary(sol_sdid2p2_cs)
summary(sol_sdid3p2_cs)
summary(sol_sdid4p2_cs)

ggdid(sol_sdid1p2_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities -\n\n- Placebo Test -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(sol_sdid2p2_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities -\n\n- Placebo Test -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(sol_sdid3p2_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities -\n\n- Placebo Test -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(sol_sdid4p2_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities -\n\n- Placebo Test -') + theme(plot.title = element_text(hjust = 0.5))

# Running the staggered diff-in-diff for wind with additional controls

wind_sdid1p2 <- att_gt(yname = 'Placebo', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', xformla = ~ LogLag2 + Income2 + Unemployment2 + HS2 + BS2, clustervars = 'ID', data = wind.data)
wind_sdid2p2 <- att_gt(yname = 'Placebo', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', xformla = ~ LogLag2 + Income2 + Unemployment2 + HS2 + BS2, control_group = 'notyettreated', clustervars = 'ID', data = wind.data)
wind_sdid3p2 <- att_gt(yname = 'Placebo', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', anticipation = 1, xformla = ~ LogLag2 + Income2 + Unemployment2 + HS2 + BS2, clustervars = 'ID', data = wind.data)
wind_sdid4p2 <- att_gt(yname = 'Placebo', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', anticipation = 1, xformla = ~ LogLag2 + Income2 + Unemployment2 + HS2 + BS2, control_group = 'notyettreated', clustervars = 'ID', data = wind.data)

wind_sdid1p2_cs <- aggte(wind_sdid1p2, type = 'dynamic')
wind_sdid2p2_cs <- aggte(wind_sdid2p2, type = 'dynamic')
wind_sdid3p2_cs <- aggte(wind_sdid3p2, type = 'dynamic')
wind_sdid4p2_cs <- aggte(wind_sdid4p2, type = 'dynamic')

# Viewing results

summary(wind_sdid1p2_cs)
summary(wind_sdid2p2_cs)
summary(wind_sdid3p2_cs)
summary(wind_sdid4p2_cs)

ggdid(wind_sdid1p2_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines -\n\n- Placebo Test -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(wind_sdid2p2_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines -\n\n- Placebo Test -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(wind_sdid3p2_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines -\n\n- Placebo Test -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(wind_sdid4p2_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines -\n\n- Placebo Test -') + theme(plot.title = element_text(hjust = 0.5))

# Saving placebo results

atandt <- rbind(c(wind_sdid1p1_cs$overall.att, wind_sdid2p1_cs$overall.att, wind_sdid3p1_cs$overall.att, wind_sdid4p1_cs$overall.att),
                c(sol_sdid1p1_cs$overall.att, sol_sdid2p1_cs$overall.att, sol_sdid3p1_cs$overall.att, sol_sdid4p1_cs$overall.att),
                c(wind_sdid1p2_cs$overall.att, wind_sdid2p2_cs$overall.att, wind_sdid3p2_cs$overall.att, wind_sdid4p2_cs$overall.att),
                c(sol_sdid1p2_cs$overall.att, sol_sdid2p2_cs$overall.att, sol_sdid3p2_cs$overall.att, sol_sdid4p2_cs$overall.att))

serrs <- rbind(c(wind_sdid1p1_cs$overall.se, wind_sdid2p1_cs$overall.se, wind_sdid3p1_cs$overall.se, wind_sdid4p1_cs$overall.se),
               c(sol_sdid1p1_cs$overall.se, sol_sdid2p1_cs$overall.se, sol_sdid3p1_cs$overall.se, sol_sdid4p1_cs$overall.se),
               c(wind_sdid1p2_cs$overall.se, wind_sdid2p2_cs$overall.se, wind_sdid3p2_cs$overall.se, wind_sdid4p2_cs$overall.se),
               c(sol_sdid1p2_cs$overall.se, sol_sdid2p2_cs$overall.se, sol_sdid3p2_cs$overall.se, sol_sdid4p2_cs$overall.se))

t.stats <- atandt / serrs

p.10 <- matrix(as.integer(abs(t.stats) > 1.645), 4, 4)
p.05 <- matrix(as.integer(abs(t.stats) > 1.960), 4, 4)
p.01 <- matrix(as.integer(abs(t.stats) > 2.576), 4, 4)

stars <- matrix(0, 4, 4)

for (i in 1:4) {
  
  for (j in 1:4) {
    
    if (p.01[i,j] == 1) {
      
      stars[i,j] <- 3
      
    } else if (p.05[i,j] == 1) {
      
      stars[i,j] <- 2
      
    } else if (p.10[i,j] == 1) {
      
      stars[i,j] <- 1
      
    }
    
  }
  
}

atandt <- round(atandt, 3)
serrs <- round(serrs, 3)

write.csv(atandt, paste0(direc, 'results/placebo_coefs.txt'), row.names = FALSE)
write.csv(serrs, paste0(direc, 'results/placebo_serrs.txt'), row.names = FALSE)
write.csv(stars, paste0(direc, 'results/placebo_stars.txt'), row.names = FALSE)

# Remove units receiving the other treatment and re-run main analysis

wind_only <- acs.data %>% filter(Treated_Solar == 0)
solar_only <- acs.data %>% filter(Treated_Wind == 0)

# Running the staggered diff-in-diff for solar with additional controls

sol_sdid111 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = solar_only)
sol_sdid222 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = solar_only)
sol_sdid333 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = solar_only)
sol_sdid444 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = solar_only)

sol_sdid111_cs <- aggte(sol_sdid111, type = 'dynamic')
sol_sdid222_cs <- aggte(sol_sdid222, type = 'dynamic')
sol_sdid333_cs <- aggte(sol_sdid333, type = 'dynamic')
sol_sdid444_cs <- aggte(sol_sdid444, type = 'dynamic')

# Viewing results

summary(sol_sdid111_cs)
summary(sol_sdid222_cs)
summary(sol_sdid333_cs)
summary(sol_sdid444_cs)

ggdid(sol_sdid111_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(sol_sdid222_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(sol_sdid333_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(sol_sdid444_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities -') + theme(plot.title = element_text(hjust = 0.5))

# Running the staggered diff-in-diff for wind with additional controls

wind_sdid111 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = wind_only)
wind_sdid222 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = wind_only)
wind_sdid333 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = wind_only)
wind_sdid444 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = wind_only)

wind_sdid111_cs <- aggte(wind_sdid111, type = 'dynamic')
wind_sdid222_cs <- aggte(wind_sdid222, type = 'dynamic')
wind_sdid333_cs <- aggte(wind_sdid333, type = 'dynamic')
wind_sdid444_cs <- aggte(wind_sdid444, type = 'dynamic')

# Viewing results

summary(wind_sdid111_cs)
summary(wind_sdid222_cs)
summary(wind_sdid333_cs)
summary(wind_sdid444_cs)

ggdid(wind_sdid111_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(wind_sdid222_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(wind_sdid333_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(wind_sdid444_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines -') + theme(plot.title = element_text(hjust = 0.5))

# Robustness tests where we match counties

w.treat.df <- wind_only %>% filter(Treated_Wind == 1)
w.con.df <- wind_only %>% filter(Treated_Wind == 0)

s.treat.df <- solar_only %>% filter(Treated_Solar == 1)
s.con.df <- solar_only %>% filter(Treated_Solar == 0)

w.treated.units <- unique(w.treat.df$ID)
w.control.units <- unique(w.con.df$ID)

s.treated.units <- unique(s.treat.df$ID)
s.control.units <- unique(s.con.df$ID)

w.treat.keep <- c()
w.con.keep <- c()

for (i in w.treated.units) {
  
  print(i)
  
  tmp <- w.treat.df %>% filter(ID == i)
  tmp <- tmp %>% filter(Time < tmp$Wind_Treat_Time[1])
  tmp2 <- w.con.df %>% filter(Time < tmp$Wind_Treat_Time[1])
  
  con_ids <- unique(tmp2$ID)
  con_pop <- c()
  con_inc <- c()
  con_une <- c()
  
  for (j in con_ids) {
    
    tmp3 <- tmp2 %>% filter(ID == j)
    con_pop <- c(con_pop, mean(tmp3$LogLag))
    con_inc <- c(con_inc, mean(tmp3$Income))
    con_une <- c(con_une, mean(tmp3$Unemployment))
    
  }
  
  ref_pop <- mean(tmp$LogLag)
  ref_inc <- mean(tmp$Income)
  ref_une <- mean(tmp$Unemployment)
  
  good_pop <- con_ids[which(abs(con_pop - ref_pop) < (.05 * ref_pop))]
  good_inc <- con_ids[which(abs(con_inc - ref_inc) < (.05 * ref_inc))]
  good_une <- con_ids[which(abs(con_une - ref_une) < 1)]
  
  keep_cons <- good_pop[which(good_pop %in% good_inc)]
  keep_cons <- keep_cons[which(keep_cons %in% good_une)]
  
  w.con.keep <- c(w.con.keep, keep_cons)
  w.con.keep <- unique(w.con.keep)
  
  if (length(keep_cons) > 0) {
    
    w.treat.keep <- c(w.treat.keep, i)
    
  }
  
}

s.treat.keep <- c()
s.con.keep <- c()

for (i in s.treated.units) {
  
  print(i)
  
  tmp <- s.treat.df %>% filter(ID == i)
  tmp <- tmp %>% filter(Time < tmp$Sol_Treat_Time[1])
  tmp2 <- s.con.df %>% filter(Time < tmp$Sol_Treat_Time[1])
  
  con_ids <- unique(tmp2$ID)
  con_pop <- c()
  con_inc <- c()
  con_une <- c()
  
  for (j in con_ids) {
    
    tmp3 <- tmp2 %>% filter(ID == j)
    con_pop <- c(con_pop, mean(tmp3$LogLag))
    con_inc <- c(con_inc, mean(tmp3$Income))
    con_une <- c(con_une, mean(tmp3$Unemployment))
    
  }
  
  ref_pop <- mean(tmp$LogLag)
  ref_inc <- mean(tmp$Income)
  ref_une <- mean(tmp$Unemployment)
  
  good_pop <- con_ids[which(abs(con_pop - ref_pop) < (.05 * ref_pop))]
  good_inc <- con_ids[which(abs(con_inc - ref_inc) < (.05 * ref_inc))]
  good_une <- con_ids[which(abs(con_une - ref_une) < 1)]
  
  keep_cons <- good_pop[which(good_pop %in% good_inc)]
  keep_cons <- keep_cons[which(keep_cons %in% good_une)]
  
  s.con.keep <- c(s.con.keep, keep_cons)
  s.con.keep <- unique(s.con.keep)
  
  if (length(keep_cons) > 0) {
    
    s.treat.keep <- c(s.treat.keep, i)
    
  }
  
}

w.treat.dfx <- wind_only %>% filter(ID %in% w.treat.keep)
w.con.dfx <- wind_only %>% filter(ID %in% w.con.keep)
wind_sub <- rbind(w.treat.dfx, w.con.dfx)

s.treat.dfx <- solar_only %>% filter(ID %in% s.treat.keep)
s.con.dfx <- solar_only %>% filter(ID %in% s.con.keep)
solar_sub <- rbind(s.treat.dfx, s.con.dfx)

# Running the staggered diff-in-diff for solar with additional controls

sol_sdid111m <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = solar_sub)
sol_sdid222m <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = solar_sub)
sol_sdid333m <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = solar_sub)
sol_sdid444m <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = solar_sub)

sol_sdid111m_cs <- aggte(sol_sdid111m, type = 'dynamic')
sol_sdid222m_cs <- aggte(sol_sdid222m, type = 'dynamic')
sol_sdid333m_cs <- aggte(sol_sdid333m, type = 'dynamic')
sol_sdid444m_cs <- aggte(sol_sdid444m, type = 'dynamic')

# Viewing results

summary(sol_sdid111m_cs)
summary(sol_sdid222m_cs)
summary(sol_sdid333m_cs)
summary(sol_sdid444m_cs)

ggdid(sol_sdid111m_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(sol_sdid222m_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(sol_sdid333m_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(sol_sdid444m_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities -') + theme(plot.title = element_text(hjust = 0.5))

# Running the staggered diff-in-diff for wind with additional controls

wind_sdid111m <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = wind_sub)
wind_sdid222m <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = wind_sub)
wind_sdid333m <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = wind_sub)
wind_sdid444m <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = wind_sub)

wind_sdid111m_cs <- aggte(wind_sdid111m, type = 'dynamic')
wind_sdid222m_cs <- aggte(wind_sdid222m, type = 'dynamic')
wind_sdid333m_cs <- aggte(wind_sdid333m, type = 'dynamic')
wind_sdid444m_cs <- aggte(wind_sdid444m, type = 'dynamic')

# Viewing results

summary(wind_sdid111m_cs)
summary(wind_sdid222m_cs)
summary(wind_sdid333m_cs)
summary(wind_sdid444m_cs)

ggdid(wind_sdid111m_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(wind_sdid222m_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(wind_sdid333m_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(wind_sdid444m_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines -') + theme(plot.title = element_text(hjust = 0.5))

# Repeating the urban/rural analyses with the truncated data set

# Urban counties

# Running the staggered diff-in-diff for solar

usol_sdid11 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = solar_only[which(solar_only$Urban == 1),])
usol_sdid22 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = solar_only[which(solar_only$Urban == 1),])
usol_sdid33 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = solar_only[which(solar_only$Urban == 1),])
usol_sdid44 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = solar_only[which(solar_only$Urban == 1),])

usol_sdid11_cs <- aggte(usol_sdid11, type = 'dynamic', na.rm = TRUE)
usol_sdid22_cs <- aggte(usol_sdid22, type = 'dynamic', na.rm = TRUE)
usol_sdid33_cs <- aggte(usol_sdid33, type = 'dynamic', na.rm = TRUE)
usol_sdid44_cs <- aggte(usol_sdid44, type = 'dynamic', na.rm = TRUE)

# Viewing results

summary(usol_sdid11_cs)
summary(usol_sdid22_cs)
summary(usol_sdid33_cs)
summary(usol_sdid44_cs)

ggdid(usol_sdid11_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities in Urban Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(usol_sdid22_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities in Urban Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(usol_sdid33_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities in Urban Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(usol_sdid44_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities in Urban Counties -') + theme(plot.title = element_text(hjust = 0.5))

# Running the staggered diff-in-diff for wind

uwind_sdid11 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = wind_only[which(wind_only$Urban == 1),])
uwind_sdid22 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = wind_only[which(wind_only$Urban == 1),])
uwind_sdid33 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = wind_only[which(wind_only$Urban == 1),])
uwind_sdid44 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = wind_only[which(wind_only$Urban == 1),])

uwind_sdid11_cs <- aggte(uwind_sdid11, type = 'dynamic', na.rm = TRUE)
uwind_sdid22_cs <- aggte(uwind_sdid22, type = 'dynamic', na.rm = TRUE)
uwind_sdid33_cs <- aggte(uwind_sdid33, type = 'dynamic', na.rm = TRUE)
uwind_sdid44_cs <- aggte(uwind_sdid44, type = 'dynamic', na.rm = TRUE)

# Viewing results

summary(uwind_sdid11_cs)
summary(uwind_sdid22_cs)
summary(uwind_sdid33_cs)
summary(uwind_sdid44_cs)

ggdid(uwind_sdid11_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines in Urban Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(uwind_sdid22_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines in Urban Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(uwind_sdid33_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines in Urban Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(uwind_sdid44_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines in Urban Counties -') + theme(plot.title = element_text(hjust = 0.5))

# Rural counties

# Running the staggered diff-in-diff for solar

rsol_sdid11 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = solar_only[which(solar_only$Urban == 0),])
rsol_sdid22 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = solar_only[which(solar_only$Urban == 0),])
rsol_sdid33 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = solar_only[which(solar_only$Urban == 0),])
rsol_sdid44 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = solar_only[which(solar_only$Urban == 0),])

rsol_sdid11_cs <- aggte(rsol_sdid11, type = 'dynamic', na.rm = TRUE)
rsol_sdid22_cs <- aggte(rsol_sdid22, type = 'dynamic', na.rm = TRUE)
rsol_sdid33_cs <- aggte(rsol_sdid33, type = 'dynamic', na.rm = TRUE)
rsol_sdid44_cs <- aggte(rsol_sdid44, type = 'dynamic', na.rm = TRUE)

# Viewing results

summary(rsol_sdid11_cs)
summary(rsol_sdid22_cs)
summary(rsol_sdid33_cs)
summary(rsol_sdid44_cs)

ggdid(rsol_sdid11_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities in Rural Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(rsol_sdid22_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities in Rural Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(rsol_sdid33_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities in Rural Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(rsol_sdid44_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities in Rural Counties -') + theme(plot.title = element_text(hjust = 0.5))

# Running the staggered diff-in-diff for wind

rwind_sdid11 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = wind_only[which(wind_only$Urban == 0),])
rwind_sdid22 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = wind_only[which(wind_only$Urban == 0),])
rwind_sdid33 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = wind_only[which(wind_only$Urban == 0),])
rwind_sdid44 <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = wind_only[which(wind_only$Urban == 0),])

rwind_sdid11_cs <- aggte(rwind_sdid11, type = 'dynamic', na.rm = TRUE)
rwind_sdid22_cs <- aggte(rwind_sdid22, type = 'dynamic', na.rm = TRUE)
rwind_sdid33_cs <- aggte(rwind_sdid33, type = 'dynamic', na.rm = TRUE)
rwind_sdid44_cs <- aggte(rwind_sdid44, type = 'dynamic', na.rm = TRUE)

# Viewing results

summary(rwind_sdid11_cs)
summary(rwind_sdid22_cs)
summary(rwind_sdid33_cs)
summary(rwind_sdid44_cs)

ggdid(rwind_sdid11_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines in Rural Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(rwind_sdid22_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines in Rural Counties -') + theme(plot.title = element_text(hjust = 0.5)) + ylim(c(-.1, .1))
ggdid(rwind_sdid33_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines in Rural Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(rwind_sdid44_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines in Rural Counties -') + theme(plot.title = element_text(hjust = 0.5))

# Repeating the urban/rural analyses with the matched data set

# Robustness tests where we match counties

# Urban only matching

uw.treat.df <- wind_only %>% filter(Treated_Wind == 1) %>% filter(Urban == 1)
uw.con.df <- wind_only %>% filter(Treated_Wind == 0) %>% filter(Urban == 1)

us.treat.df <- solar_only %>% filter(Treated_Solar == 1) %>% filter(Urban == 1)
us.con.df <- solar_only %>% filter(Treated_Solar == 0) %>% filter(Urban == 1)

uw.treated.units <- unique(uw.treat.df$ID)
uw.control.units <- unique(uw.con.df$ID)

us.treated.units <- unique(us.treat.df$ID)
us.control.units <- unique(us.con.df$ID)

uw.treat.keep <- c()
uw.con.keep <- c()

for (i in uw.treated.units) {
  
  print(i)
  
  tmp <- uw.treat.df %>% filter(ID == i)
  tmp <- tmp %>% filter(Time < tmp$Wind_Treat_Time[1])
  tmp2 <- uw.con.df %>% filter(Time < tmp$Wind_Treat_Time[1])
  
  con_ids <- unique(tmp2$ID)
  con_pop <- c()
  con_inc <- c()
  con_une <- c()
  
  for (j in con_ids) {
    
    tmp3 <- tmp2 %>% filter(ID == j)
    con_pop <- c(con_pop, mean(tmp3$LogLag))
    con_inc <- c(con_inc, mean(tmp3$Income))
    con_une <- c(con_une, mean(tmp3$Unemployment))
    
  }
  
  ref_pop <- mean(tmp$LogLag)
  ref_inc <- mean(tmp$Income)
  ref_une <- mean(tmp$Unemployment)
  
  good_pop <- con_ids[which(abs(con_pop - ref_pop) < (.05 * ref_pop))]
  good_inc <- con_ids[which(abs(con_inc - ref_inc) < (.05 * ref_inc))]
  good_une <- con_ids[which(abs(con_une - ref_une) < 1)]
  
  keep_cons <- good_pop[which(good_pop %in% good_inc)]
  keep_cons <- keep_cons[which(keep_cons %in% good_une)]
  
  uw.con.keep <- c(uw.con.keep, keep_cons)
  uw.con.keep <- unique(uw.con.keep)
  
  if (length(keep_cons) > 0) {
    
    uw.treat.keep <- c(uw.treat.keep, i)
    
  }
  
}

us.treat.keep <- c()
us.con.keep <- c()

for (i in us.treated.units) {
  
  print(i)
  
  tmp <- us.treat.df %>% filter(ID == i)
  tmp <- tmp %>% filter(Time < tmp$Sol_Treat_Time[1])
  tmp2 <- us.con.df %>% filter(Time < tmp$Sol_Treat_Time[1])
  
  con_ids <- unique(tmp2$ID)
  con_pop <- c()
  con_inc <- c()
  con_une <- c()
  
  for (j in con_ids) {
    
    tmp3 <- tmp2 %>% filter(ID == j)
    con_pop <- c(con_pop, mean(tmp3$LogLag))
    con_inc <- c(con_inc, mean(tmp3$Income))
    con_une <- c(con_une, mean(tmp3$Unemployment))
    
  }
  
  ref_pop <- mean(tmp$LogLag)
  ref_inc <- mean(tmp$Income)
  ref_une <- mean(tmp$Unemployment)
  
  good_pop <- con_ids[which(abs(con_pop - ref_pop) < (.05 * ref_pop))]
  good_inc <- con_ids[which(abs(con_inc - ref_inc) < (.05 * ref_inc))]
  good_une <- con_ids[which(abs(con_une - ref_une) < 1)]
  
  keep_cons <- good_pop[which(good_pop %in% good_inc)]
  keep_cons <- keep_cons[which(keep_cons %in% good_une)]
  
  us.con.keep <- c(us.con.keep, keep_cons)
  us.con.keep <- unique(us.con.keep)
  
  if (length(keep_cons) > 0) {
    
    us.treat.keep <- c(us.treat.keep, i)
    
  }
  
}

uw.treat.dfx <- wind_only %>% filter(ID %in% uw.treat.keep)
uw.con.dfx <- wind_only %>% filter(ID %in% uw.con.keep)
wind_sub.u <- rbind(uw.treat.dfx, uw.con.dfx)

us.treat.dfx <- solar_only %>% filter(ID %in% us.treat.keep)
us.con.dfx <- solar_only %>% filter(ID %in% us.con.keep)
solar_sub.u <- rbind(us.treat.dfx, us.con.dfx)

# Rural only matching

rw.treat.df <- wind_only %>% filter(Treated_Wind == 1) %>% filter(Urban == 0)
rw.con.df <- wind_only %>% filter(Treated_Wind == 0) %>% filter(Urban == 0)

rs.treat.df <- solar_only %>% filter(Treated_Solar == 1) %>% filter(Urban == 0)
rs.con.df <- solar_only %>% filter(Treated_Solar == 0) %>% filter(Urban == 0)

rw.treated.units <- unique(rw.treat.df$ID)
rw.control.units <- unique(rw.con.df$ID)

rs.treated.units <- unique(rs.treat.df$ID)
rs.control.units <- unique(rs.con.df$ID)

rw.treat.keep <- c()
rw.con.keep <- c()

for (i in rw.treated.units) {
  
  print(i)
  
  tmp <- rw.treat.df %>% filter(ID == i)
  tmp <- tmp %>% filter(Time < tmp$Wind_Treat_Time[1])
  tmp2 <- rw.con.df %>% filter(Time < tmp$Wind_Treat_Time[1])
  
  con_ids <- unique(tmp2$ID)
  con_pop <- c()
  con_inc <- c()
  con_une <- c()
  
  for (j in con_ids) {
    
    tmp3 <- tmp2 %>% filter(ID == j)
    con_pop <- c(con_pop, mean(tmp3$LogLag))
    con_inc <- c(con_inc, mean(tmp3$Income))
    con_une <- c(con_une, mean(tmp3$Unemployment))
    
  }
  
  ref_pop <- mean(tmp$LogLag)
  ref_inc <- mean(tmp$Income)
  ref_une <- mean(tmp$Unemployment)
  
  good_pop <- con_ids[which(abs(con_pop - ref_pop) < (.05 * ref_pop))]
  good_inc <- con_ids[which(abs(con_inc - ref_inc) < (.05 * ref_inc))]
  good_une <- con_ids[which(abs(con_une - ref_une) < 1)]
  
  keep_cons <- good_pop[which(good_pop %in% good_inc)]
  keep_cons <- keep_cons[which(keep_cons %in% good_une)]
  
  rw.con.keep <- c(rw.con.keep, keep_cons)
  rw.con.keep <- unique(rw.con.keep)
  
  if (length(keep_cons) > 0) {
    
    rw.treat.keep <- c(rw.treat.keep, i)
    
  }
  
}

rs.treat.keep <- c()
rs.con.keep <- c()

for (i in rs.treated.units) {
  
  print(i)
  
  tmp <- rs.treat.df %>% filter(ID == i)
  tmp <- tmp %>% filter(Time < tmp$Sol_Treat_Time[1])
  tmp2 <- rs.con.df %>% filter(Time < tmp$Sol_Treat_Time[1])
  
  con_ids <- unique(tmp2$ID)
  con_pop <- c()
  con_inc <- c()
  con_une <- c()
  
  for (j in con_ids) {
    
    tmp3 <- tmp2 %>% filter(ID == j)
    con_pop <- c(con_pop, mean(tmp3$LogLag))
    con_inc <- c(con_inc, mean(tmp3$Income))
    con_une <- c(con_une, mean(tmp3$Unemployment))
    
  }
  
  ref_pop <- mean(tmp$LogLag)
  ref_inc <- mean(tmp$Income)
  ref_une <- mean(tmp$Unemployment)
  
  good_pop <- con_ids[which(abs(con_pop - ref_pop) < (.05 * ref_pop))]
  good_inc <- con_ids[which(abs(con_inc - ref_inc) < (.05 * ref_inc))]
  good_une <- con_ids[which(abs(con_une - ref_une) < 1)]
  
  keep_cons <- good_pop[which(good_pop %in% good_inc)]
  keep_cons <- keep_cons[which(keep_cons %in% good_une)]
  
  rs.con.keep <- c(rs.con.keep, keep_cons)
  rs.con.keep <- unique(rs.con.keep)
  
  if (length(keep_cons) > 0) {
    
    rs.treat.keep <- c(rs.treat.keep, i)
    
  }
  
}

rw.treat.dfx <- wind_only %>% filter(ID %in% rw.treat.keep)
rw.con.dfx <- wind_only %>% filter(ID %in% rw.con.keep)
wind_sub.r <- rbind(rw.treat.dfx, rw.con.dfx)

rs.treat.dfx <- solar_only %>% filter(ID %in% rs.treat.keep)
rs.con.dfx <- solar_only %>% filter(ID %in% rs.con.keep)
solar_sub.r <- rbind(rs.treat.dfx, rs.con.dfx)

# Running the rura/urban staggered diff-in-diff for solar with matching

# Urban counties

# Running the staggered diff-in-diff for solar

usol_sdid11m <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = solar_sub.u[which(solar_sub.u$Urban == 1),])
usol_sdid22m <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = solar_sub.u[which(solar_sub.u$Urban == 1),])
usol_sdid33m <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = solar_sub.u[which(solar_sub.u$Urban == 1),])
usol_sdid44m <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = solar_sub.u[which(solar_sub.u$Urban == 1),])

usol_sdid11m_cs <- aggte(usol_sdid11m, type = 'dynamic', na.rm = TRUE)
usol_sdid22m_cs <- aggte(usol_sdid22m, type = 'dynamic', na.rm = TRUE)
usol_sdid33m_cs <- aggte(usol_sdid33m, type = 'dynamic', na.rm = TRUE)
usol_sdid44m_cs <- aggte(usol_sdid44m, type = 'dynamic', na.rm = TRUE)

# Viewing results

summary(usol_sdid11m_cs)
summary(usol_sdid22m_cs)
summary(usol_sdid33m_cs)
summary(usol_sdid44m_cs)

ggdid(usol_sdid11m_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities in Urban Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(usol_sdid22m_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities in Urban Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(usol_sdid33m_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities in Urban Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(usol_sdid44m_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities in Urban Counties -') + theme(plot.title = element_text(hjust = 0.5))

# Running the staggered diff-in-diff for wind

uwind_sdid11m <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = wind_sub.u[which(wind_sub.u$Urban == 1),])
uwind_sdid22m <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = wind_sub.u[which(wind_sub.u$Urban == 1),])
uwind_sdid33m <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = wind_sub.u[which(wind_sub.u$Urban == 1),])
uwind_sdid44m <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = wind_sub.u[which(wind_sub.u$Urban == 1),])

uwind_sdid11m_cs <- aggte(uwind_sdid11m, type = 'dynamic', na.rm = TRUE)
uwind_sdid22m_cs <- aggte(uwind_sdid22m, type = 'dynamic', na.rm = TRUE)
uwind_sdid33m_cs <- aggte(uwind_sdid33m, type = 'dynamic', na.rm = TRUE)
uwind_sdid44m_cs <- aggte(uwind_sdid44m, type = 'dynamic', na.rm = TRUE)

# Viewing results

summary(uwind_sdid11m_cs)
summary(uwind_sdid22m_cs)
summary(uwind_sdid33m_cs)
summary(uwind_sdid44m_cs)

ggdid(uwind_sdid11m_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines in Urban Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(uwind_sdid22m_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines in Urban Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(uwind_sdid33m_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines in Urban Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(uwind_sdid44m_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines in Urban Counties -') + theme(plot.title = element_text(hjust = 0.5))

# Rural counties

# Running the staggered diff-in-diff for solar

rsol_sdid11m <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = solar_sub.r[which(solar_sub.r$Urban == 0),])
rsol_sdid22m <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = solar_sub.r[which(solar_sub.r$Urban == 0),])
rsol_sdid33m <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = solar_only[which(solar_sub.r$Urban == 0),])
rsol_sdid44m <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Sol_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = solar_sub.r[which(solar_sub.r$Urban == 0),])

rsol_sdid11m_cs <- aggte(rsol_sdid11m, type = 'dynamic', na.rm = TRUE)
rsol_sdid22m_cs <- aggte(rsol_sdid22m, type = 'dynamic', na.rm = TRUE)
rsol_sdid33m_cs <- aggte(rsol_sdid33m, type = 'dynamic', na.rm = TRUE)
rsol_sdid44m_cs <- aggte(rsol_sdid44m, type = 'dynamic', na.rm = TRUE)

# Viewing results

summary(rsol_sdid11m_cs)
summary(rsol_sdid22m_cs)
summary(rsol_sdid33m_cs)
summary(rsol_sdid44m_cs)

ggdid(rsol_sdid11m_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities in Rural Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(rsol_sdid22m_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities in Rural Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(rsol_sdid33m_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities in Rural Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(rsol_sdid44m_cs, title = 'Average Effect by Length of Exposure\n\n- Solar Facilities in Rural Counties -') + theme(plot.title = element_text(hjust = 0.5))

# Running the staggered diff-in-diff for wind

rwind_sdid11m <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = wind_sub.r[which(wind_sub.r$Urban == 0),])
rwind_sdid22m <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = wind_sub.r[which(wind_sub.r$Urban == 0),])
rwind_sdid33m <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, clustervars = 'ID', data = wind_sub.r[which(wind_sub.r$Urban == 0),])
rwind_sdid44m <- att_gt(yname = 'LogPop', tname = 'Time', idname = 'ID', gname = 'Wind_Treat_Time', anticipation = 1, xformla = ~ LogLag + Income + Unemployment + HS + BS, control_group = 'notyettreated', clustervars = 'ID', data = wind_sub.r[which(wind_sub.r$Urban == 0),])

rwind_sdid11m_cs <- aggte(rwind_sdid11m, type = 'dynamic', na.rm = TRUE)
rwind_sdid22m_cs <- aggte(rwind_sdid22m, type = 'dynamic', na.rm = TRUE)
rwind_sdid33m_cs <- aggte(rwind_sdid33m, type = 'dynamic', na.rm = TRUE)
rwind_sdid44m_cs <- aggte(rwind_sdid44m, type = 'dynamic', na.rm = TRUE)

# Viewing results

summary(rwind_sdid11m_cs)
summary(rwind_sdid22m_cs)
summary(rwind_sdid33m_cs)
summary(rwind_sdid44m_cs)

ggdid(rwind_sdid11m_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines in Rural Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(rwind_sdid22m_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines in Rural Counties -') + theme(plot.title = element_text(hjust = 0.5)) + ylim(c(-.1, .1))
ggdid(rwind_sdid33m_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines in Rural Counties -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(rwind_sdid44m_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines in Rural Counties -') + theme(plot.title = element_text(hjust = 0.5))

# Saving truncated results

atandt <- rbind(c(wind_sdid111_cs$overall.att, wind_sdid222_cs$overall.att, wind_sdid333_cs$overall.att, wind_sdid444_cs$overall.att),
                c(sol_sdid111_cs$overall.att, sol_sdid222_cs$overall.att, sol_sdid333_cs$overall.att, sol_sdid444_cs$overall.att),
                c(rwind_sdid11_cs$overall.att, rwind_sdid22_cs$overall.att, rwind_sdid33_cs$overall.att, rwind_sdid44_cs$overall.att),
                c(rsol_sdid11_cs$overall.att, rsol_sdid22_cs$overall.att, rsol_sdid33_cs$overall.att, rsol_sdid44_cs$overall.att),
                c(uwind_sdid11_cs$overall.att, uwind_sdid22_cs$overall.att, uwind_sdid33_cs$overall.att, uwind_sdid44_cs$overall.att),
                c(usol_sdid11_cs$overall.att, usol_sdid22_cs$overall.att, usol_sdid33_cs$overall.att, usol_sdid44_cs$overall.att))

serrs <- rbind(c(wind_sdid111_cs$overall.se, wind_sdid222_cs$overall.se, wind_sdid333_cs$overall.se, wind_sdid444_cs$overall.se),
               c(sol_sdid111_cs$overall.se, sol_sdid222_cs$overall.se, sol_sdid333_cs$overall.se, sol_sdid444_cs$overall.se),
               c(rwind_sdid11_cs$overall.se, rwind_sdid22_cs$overall.se, rwind_sdid33_cs$overall.se, rwind_sdid44_cs$overall.se),
               c(rsol_sdid11_cs$overall.se, rsol_sdid22_cs$overall.se, rsol_sdid33_cs$overall.se, rsol_sdid44_cs$overall.se),
               c(uwind_sdid11_cs$overall.se, uwind_sdid22_cs$overall.se, uwind_sdid33_cs$overall.se, uwind_sdid44_cs$overall.se),
               c(usol_sdid11_cs$overall.se, usol_sdid22_cs$overall.se, usol_sdid33_cs$overall.se, usol_sdid44_cs$overall.se))

t.stats <- atandt / serrs

p.10 <- matrix(as.integer(abs(t.stats) > 1.645), 6, 4)
p.05 <- matrix(as.integer(abs(t.stats) > 1.960), 6, 4)
p.01 <- matrix(as.integer(abs(t.stats) > 2.576), 6, 4)

stars <- matrix(0, 6, 4)

for (i in 1:6) {
  
  for (j in 1:4) {
    
    if (p.01[i,j] == 1) {
      
      stars[i,j] <- 3
      
    } else if (p.05[i,j] == 1) {
      
      stars[i,j] <- 2
      
    } else if (p.10[i,j] == 1) {
      
      stars[i,j] <- 1
      
    }
    
  }
  
}

atandt <- round(atandt, 3)
serrs <- round(serrs, 3)

write.csv(atandt, paste0(direc, 'results/truncated_coefs.txt'), row.names = FALSE)
write.csv(serrs, paste0(direc, 'results/truncated_serrs.txt'), row.names = FALSE)
write.csv(stars, paste0(direc, 'results/truncated_stars.txt'), row.names = FALSE)

# Saving matched results

atandt <- rbind(c(wind_sdid111m_cs$overall.att, wind_sdid222m_cs$overall.att, wind_sdid333m_cs$overall.att, wind_sdid444m_cs$overall.att),
                c(sol_sdid111m_cs$overall.att, sol_sdid222m_cs$overall.att, sol_sdid333m_cs$overall.att, sol_sdid444m_cs$overall.att),
                c(rwind_sdid11m_cs$overall.att, rwind_sdid22m_cs$overall.att, rwind_sdid33m_cs$overall.att, rwind_sdid44m_cs$overall.att),
                c(rsol_sdid11m_cs$overall.att, rsol_sdid22m_cs$overall.att, rsol_sdid33m_cs$overall.att, rsol_sdid44m_cs$overall.att),
                c(uwind_sdid11m_cs$overall.att, uwind_sdid22m_cs$overall.att, uwind_sdid33m_cs$overall.att, uwind_sdid44m_cs$overall.att),
                c(usol_sdid11m_cs$overall.att, usol_sdid22m_cs$overall.att, usol_sdid33m_cs$overall.att, usol_sdid44m_cs$overall.att))

serrs <- rbind(c(wind_sdid111m_cs$overall.se, wind_sdid222m_cs$overall.se, wind_sdid333m_cs$overall.se, wind_sdid444m_cs$overall.se),
               c(sol_sdid111m_cs$overall.se, sol_sdid222m_cs$overall.se, sol_sdid333m_cs$overall.se, sol_sdid444m_cs$overall.se),
               c(rwind_sdid11m_cs$overall.se, rwind_sdid22m_cs$overall.se, rwind_sdid33m_cs$overall.se, rwind_sdid44m_cs$overall.se),
               c(rsol_sdid11m_cs$overall.se, rsol_sdid22m_cs$overall.se, rsol_sdid33m_cs$overall.se, rsol_sdid44m_cs$overall.se),
               c(uwind_sdid11m_cs$overall.se, uwind_sdid22m_cs$overall.se, uwind_sdid33m_cs$overall.se, uwind_sdid44m_cs$overall.se),
               c(usol_sdid11m_cs$overall.se, usol_sdid22m_cs$overall.se, usol_sdid33m_cs$overall.se, usol_sdid44m_cs$overall.se))

t.stats <- atandt / serrs

p.10 <- matrix(as.integer(abs(t.stats) > 1.645), 6, 4)
p.05 <- matrix(as.integer(abs(t.stats) > 1.960), 6, 4)
p.01 <- matrix(as.integer(abs(t.stats) > 2.576), 6, 4)

stars <- matrix(0, 6, 4)

for (i in 1:6) {
  
  for (j in 1:4) {
    
    if (p.01[i,j] == 1) {
      
      stars[i,j] <- 3
      
    } else if (p.05[i,j] == 1) {
      
      stars[i,j] <- 2
      
    } else if (p.10[i,j] == 1) {
      
      stars[i,j] <- 1
      
    }
    
  }
  
}

atandt <- round(atandt, 3)
serrs <- round(serrs, 3)

write.csv(atandt, paste0(direc, 'results/matched_coefs.txt'), row.names = FALSE)
write.csv(serrs, paste0(direc, 'results/matched_serrs.txt'), row.names = FALSE)
write.csv(stars, paste0(direc, 'results/matched_stars.txt'), row.names = FALSE)

# Population change figures

cons <- counties()

cons <- cons %>% filter(! STATEFP %in% c('02', '15', '72', '60', '66', '69', '78'))

change <- c()

for (f in unique(acs.data$FIPS)) {
  
  print(f)
  tmp <- acs.data %>% filter(FIPS == f)
  ling <- (tmp[which(tmp$Year == 2023),]$Population[1] - tmp[which(tmp$Year == 2010),]$Population[1]) / tmp[which(tmp$Year == 2010),]$Population[1]
  change <- c(change, ling)
  
}

fig.df <- as.data.frame(cbind(unique(acs.data$FIPS), change))
colnames(fig.df) <- c('FIPS', 'Change')
cons$FIPS <- as.integer(cons$GEOID)
cons <- left_join(cons, fig.df, by = 'FIPS')

national.mean <- mean(cons$Change, na.rm = TRUE)
cons$Relative <- as.integer(cons$Change < national.mean)

quail <- acs.data[which(acs.data$Year == 2023),c('FIPS', 'Treated_Solar', 'Treated_Wind')]

cons <- left_join(cons, quail, by = 'FIPS')
cons$Wind_Group <- (cons$Relative * cons$Treated_Wind) + cons$Treated_Wind
cons$Solar_Group <- (cons$Relative * cons$Treated_Wind) + cons$Treated_Solar
cons[is.na(cons)] <- 0
cons$Wind_Group <- cons$Wind_Group + 1
cons$Solar_Group <- cons$Solar_Group + 1

wvalues <- c('No Wind Turbines', 'Above US Average', 'Below US Average')
svalues <- c('No Solar Facilites', 'Above US Average', 'Below US Average')

cons$Wind_Group2 <- wvalues[cons$Wind_Group]
cons$Solar_Group2 <- svalues[cons$Solar_Group]

wpal <- colorFactor(palette = c('orange2', 'red4', 'white'), domain = sort(unique(cons$Wind_Group2)))
spal <- colorFactor(palette = c('orange2', 'red4', 'white'), domain = sort(unique(cons$Solar_Group2)))

leaflet(cons$geometry) %>% addTiles() %>% addPolygons(weight = 1.0, smoothFactor = 1.0, opacity = 1.0, fillOpacity = 1.0, color = 'gray', fillColor = wpal(cons$Wind_Group2)) %>% addLegend(position = 'bottomright', pal = wpal, values = cons$Wind_Group2, title = 'Legend')

leaflet(cons$geometry) %>% addTiles() %>% addPolygons(weight = 1.0, smoothFactor = 1.0, opacity = 1.0, fillOpacity = 1.0, color = 'gray', fillColor = spal(cons$Solar_Group2)) %>% addLegend(position = 'bottomright', pal = spal, values = cons$Solar_Group2, title = 'Legend')

