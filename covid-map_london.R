getwd()

library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
library(here)
library(janitor)
library(RColorBrewer)
library(sp)
library(spdep)

#read london borough shapefile
Londonborough <-
  st_read(
    here::here(
      'data',
      'statistical-gis-boundaries-london',
      'ESRI',
      'London_Borough_Excluding_MHW.shp'
    )
  ) %>%
  st_transform(., 27700)

#check whether shapefile is in the correct projection
st_crs(Londonborough)

#read csv file
covid_323_422 <-
  read_csv(
    '/Users/ziyichen/Desktop/SDSV/GIS/Data_for_CW/Code&data/data/new_cases-323-422.csv',
    na = c("NA", "n/a")
  ) %>%
  clean_names()

#rename csv file
names(covid_323_422)
names(Londonborough)

#join the shapefile and CSV file-for 323-422
Londonborough_merged <-
  Londonborough %>% left_join(covid_323_422, by = c('GSS_CODE' = 'area_code')) %>%
  distinct(GSS_CODE, NAME, new_cases,new_cases_per_10k_population) #choose which column to be used in the following analysis

st_crs(Londonborough_merged)

#read 523-622 data
covid_523_622 <-   read_csv(
  '/Users/ziyichen/Desktop/SDSV/GIS/Data_for_CW/Code&data/data/new_cases-523-622.csv',
  na = c("NA", "n/a")
) %>%
  clean_names()
names(covid_523_622)

#join the shapefile and CSV file for 523-622
Londonborough_merged_523 <-
  Londonborough %>% left_join(covid_523_622, by = c('GSS_CODE' = 'area_code')) %>%
  distinct(GSS_CODE, NAME, new_cases, new_cases_per_10k_523_622) #choose which column to be used in the following analysis

#making map
tmap_mode('plot')

#setting breaks
breaks_323 = c(10, 15, 20, 25, 30, 35) 
#map for 323-422
tm1 <- tm_shape(Londonborough_merged) +
  tm_polygons(
    'new_cases_per_10k_population',
    breaks = breaks_323,
    alpha = 0.5,
    palette = 'PuBu',
    colorNA = 'white',
    title = 'New cases rate'
  ) + tm_layout(
    legend.position = c('left', 'bottom'),
    legend.outside = FALSE,
    legend.title.size = 1.2,
    legend.text.size = 0.75,
    frame = FALSE
  ) + tm_credits('(A) New cases rate in 3.23-4.22', position = c('left', 'top'), size = 1.2) + tm_compass(type = "arrow", position = c("right", "bottom")) +tm_scale_bar(position =c('right', 'bottom'), text.size = 0.6)
tm1
#saving map
tmap_save(tm1,'/Users/ziyichen/Desktop/SDSV/GIS/Data_for_CW/Code&data/data/new_cases_rate(323-422).png')

#########################

#making map2
breaks_523=c(0,0.5,1,1.5,2,2.5,3)
tm2 <- tm_shape(Londonborough_merged_523) +
  tm_polygons(
    'new_cases_per_10k_523_622',
    breaks = breaks_523,
    alpha = 0.5,
    palette = 'PuBu',
    colorNA = 'white',
    title = 'New cases rate'
  ) + tm_layout(
    legend.position = c('left', 'bottom'),
    legend.outside = FALSE,
    legend.title.size = 1.2,
    legend.text.size = 0.75,
    frame = FALSE
  ) +
  tm_compass(type = "arrow", position = c("right", "bottom")) + tm_credits('(B) New cases rate in 5.23-6.22',
                                                                           position = c('left', 'top'),
                                                                           size =
                                                                             1.2) +
  tm_scale_bar(position = c('right', 'bottom'), text.size = 0.6)
tm2
tmap_save(tm2,'/Users/ziyichen/Desktop/SDSV/GIS/Data_for_CW/Code&data/data/new_cases_rate(523-622).png')

#combine two map
t=tmap_arrange(tm1,tm2,nrow=1)
tmap_save(t,'/Users/ziyichen/Desktop/SDSV/GIS/Data_for_CW/Code&data/data/New_cases_rate.png')
t


#spatial autocorrelation
london_exclude_city <-
  na.omit(Londonborough_merged) #exclude rows contain null data

#data cleaning for 5.23Londonborough dataset
london_exclude_city_523 <- na.omit(Londonborough_merged_523)

#creating polygon for 323 dataset
neibour_323 <- poly2nb(london_exclude_city, queen = TRUE)
neibour_323[[1]]

neibour_523 <- poly2nb(london_exclude_city_523,queen=TRUE)
neibour_523[[1]]

#assign weight matrix for each neighbouring polygon using row standardisation
lw_323 <- nb2listw(neibour_323, style = "W", zero.policy = TRUE)
lw_323$weights[1] #each neighbour is assigned a quarter of total weight

lw_523 <- nb2listw(neibour_523,style='W',zero.policy = TRUE)
lw_523$weights[1]

#computing global Moran's I for 3.23

moran.test(london_exclude_city$new_cases_per_10k_population, lw_323)

moran.test(london_exclude_city_523$new_cases_per_10k_523_622,lw_523)



#local Moran's I
local_moran_323 <- localmoran(london_exclude_city$new_cases_per_10k_population, lw_323)
summary(local_moran_323)

local_moran_523 <- localmoran(london_exclude_city_523$new_cases_per_10k_523_622,lw_523)
summary(local_moran_523)


#plot local moran map
#There are 5 columns of data.
#copy some of the columns (the I score (column 1) and the z-score standard deviation (column 4))
#the z-score (standardised value relating to whether high values or low values are clustering together)
#change local_moran type

local_moran_tibble_323 <- as_tibble(local_moran_323) #change to dataframe
local_moran_tibble_523 <- as_tibble(local_moran_523)

london_exclude_city <-
  london_exclude_city %>% mutate(local_moran_I = as.numeric(local_moran_tibble_323$Ii)) %>% mutate(local_moran_z =as.numeric(local_moran_tibble_323$Z.Ii))

london_exclude_city_523 <-
  london_exclude_city_523 %>% mutate(local_moran_I = as.numeric(local_moran_tibble_523$Ii)) %>% mutate(local_moran_z =
                                                                                                         as.numeric(local_moran_tibble_523$Z.Ii))


#setting color
MoranColours <- rev(brewer.pal(8, "RdBu"))

#plot a map
tmap_mode('plot')

tm_323 <- tm_shape(london_exclude_city) +
  tm_polygons(
    'local_moran_I',
    alpha = 0.5,
    palette = MoranColours,
    title = 'Local Moran I',midpoint=NA
  ) + tm_layout(
    legend.position = c('left', 'bottom'),
    legend.outside = FALSE,
    legend.title.size = 1.2,
    legend.text.size = 0.75,
    frame = FALSE
  ) + tm_compass(type = "arrow", position = c("right", "bottom")) +tm_scale_bar(position =
                                                                                  c('right', 'bottom'), text.size = 0.6)+tm_credits('(A) Local Moran in 3.23-4.22',position = c('left','top'),size = 1.2)

tm_323

tmap_save(tm_323,'/Users/ziyichen/Desktop/SDSV/GIS/Data_for_CW/Code&data/data/local_moran_323.png')

tm_523 <- tm_shape(london_exclude_city_523) +
  tm_polygons(
    'local_moran_I',
    alpha = 0.5,
    palette = MoranColours,
    title = 'Local Moran I',midpoint=NA
  ) + tm_layout(
    legend.position = c('left', 'bottom'),
    legend.outside = FALSE,
    legend.title.size = 1.2,
    legend.text.size = 0.75,
    frame = FALSE
  ) +tm_compass(type = "arrow", position = c("right", "bottom")) +tm_scale_bar(position =
                                                                                 c('right', 'bottom'), text.size = 0.6)+tm_credits('(B) Local Moran in 5.23-6.22',position = c('left','top'),size = 1.2)
tm_523
tmap_save(tm_523,'/Users/ziyichen/Desktop/SDSV/GIS/Data_for_CW/Code&data/data/local_moran_523.png')

#combine two map
t_c=tmap_arrange(tm_323,tm_523,nrow=1)
t_c
tmap_save(t_c,'/Users/ziyichen/Desktop/SDSV/GIS/Data_for_CW/Code&data/data/local_moran_combined.png')
#geary's test

geary.test(london_exclude_city$new_cases_per_10k_population, lw_323)

geary.test(london_exclude_city_523$new_cases_per_10k_523_622,lw_523)

#Getis Ord General G

globalG.test(london_exclude_city$new_cases_per_10k_population,lw_323)

globalG.test(london_exclude_city_523$new_cases_per_10k_523_622,lw_523)

#histogram and basic statistics
#only use rate of new cases to plot histrogram
covid_323_hist <- hist(covid_323_422$new_cases_per_10k_population,col='Dark blue',density=10,xlab='Rate of new cases',main='Rate of new cases during 3.23-4.22',angle=45,ylim=c(0,15))
mean_323 <- mean(covid_323_422$new_cases_per_10k_population)
var_323 <- var(covid_323_422$new_cases_per_10k_population)
std_323 <- sd(covid_323_422$new_cases_per_10k_population)
mean_323
var_323
std_323

covid_523_hist <- hist(covid_523_622$new_cases_per_10k_523_622,col='Dark blue',density=10,xlab='Rate of new cases',main='Rate of new cases during 5.23-6.22',angle=45,ylim=c(0,15))
mean_523 <- mean(covid_523_622$new_cases_per_10k_523_622)
var_523 <- var(covid_523_622$new_cases_per_10k_523_622)
std_523 <- sd(covid_523_622$new_cases_per_10k_523_622)
mean_523
var_523
std_523


