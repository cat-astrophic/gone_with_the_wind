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

# Remove wind observations where p_year == -9999

wind <- wind %>% filter(p_year > 0)

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
acs.x$Year <- rep(y, nrow(acs.x))

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

# Remove units treated in first period

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

# Dropping 2023 due to some missing controls

sol.data <- sol.data %>% filter(Year < 2023)
wind.data <- wind.data %>% filter(Year < 2023)

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

# Running the staggered diff-in-diff for wind without additional controls

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
ruc$Urban <- as.integer(ruc$Value <= 3)

acs.data$FIPS <- as.integer(acs.data$GEOID)

hurb <- c()

for (i in 1:nrow(acs.data)) {
  
  print(i)
  tmp <- ruc %>% filter(FIPS == acs.data$FIPS[i])
  hurb <- c(hurb, tmp$Urban[1])
  
}

acs.data$Urban <- hurb

sol.data <- acs.data %>% filter(Sol_Treat_Time != 1) %>% filter(Year < 2023)
wind.data <- acs.data %>% filter(Wind_Treat_Time != 1) %>% filter(Year < 2023)

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
ggdid(rwind_sdid2_cs, title = 'Average Effect by Length of Exposure\n\n- Wind Turbines in Rural Counties -') + theme(plot.title = element_text(hjust = 0.5)) + ylim(c(-.1, .05))
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
write.csv(serrs, paste0(direc, 'results/stars.txt'), row.names = FALSE)

# Creating a summary statistics figure

sums <- acs.data[,c('LogPop', 'Treated_Solar', 'DiD_Solar', 'Treated_Wind', 'DiD_Wind', 'Urban', 'Income', 'Unemployment', 'HS', 'BS')]

colnames(sums) <- c('Log Population', 'Treated: Solar', 'Treated x Post: Solar', 'Treated: Wind', 'Treated x Post: Wind', 'Urban', 'Log Income', 'Unemployment Rate', 'High School Grad %', 'College Grad %')

datasummary_skim(sums, fmt = '%.3f')

##################################### NOTES #####################################

# Callaway, Brantly and Pedro H.C. Sant'Anna.  "Difference-in-Differences with Multiple Time Periods." Journal of Econometrics, Vol. 225, No. 2, pp. 200-230, 2021. <https://doi.org/10.1016/j.jeconom.2020.12.001>, <https://arxiv.org/abs/1803.09015>
# https://www.sciencedirect.com/science/article/abs/pii/S030440762030378X

# https://www.journals.uchicago.edu/doi/abs/10.1086/230638?journalCode=ajs
# https://www.researchgate.net/profile/Paul-Mazerolle/publication/227622791_Using_the_Correct_Statistical_Test_for_Equality_of_Regression_Coefficients/links/0c9605322187ba6c70000000/Using-the-Correct-Statistical-Test-for-Equality-of-Regression-Coefficients.pdf

# https://scholar.google.com/scholar?start=10&q=do+people+actually+move+away+from+windmills&hl=en&as_sdt=0,47&as_ylo=2021
# https://scholar.google.com/citations?user=y6oSj2QAAAAJ&hl=en&oi=sra

