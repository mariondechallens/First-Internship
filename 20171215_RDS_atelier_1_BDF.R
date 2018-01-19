# ******************************************************************* ----
# RDS Atelier 1: Manipulation de données avec tidyverse ----
# ******************************************************************* ----

# ********************************* ----
# 0. Get and View data ----
# ********************************* ----
if(!"DBI" %in% installed.packages()){install.packages("DBI")}
library(DBI)

if(!"dplyr" %in% installed.packages()){install.packages("dplyr")}
if(!"magrittr" %in% installed.packages()){install.packages("magrittr")}
library(dplyr)
library(magrittr)

# * 0. Get the data ----
# Load the data set from rda file

# population from: https://tsb.intra.bdf.local:8443/tsb-web/spring/home?execution=e1s4
# see the file: 20171215_RDS_atelier_1_WITH_access_to_lxpr0142pv.unix.intra.bdf.local.R
# to know how it has been built

load("pop.rda")
str(pop)
View(pop)

# * 0. OLD way to mutate data ----

if(!"lubridate" %in% installed.packages()){install.packages("lubridate")}
library(lubridate)

pop$time  <- as.Date(pop$time)
pop$value <- as.numeric(pop$value)

# * 0. A better way to mutate data ----

pop %<>% dplyr::mutate(
  time  = as.Date(time),
  value = as.numeric(value))

str(pop)

# ********************************* ----
# 1. Compute some statistics ----
# ********************************* ----

# * 1.1. Statistics by countries ----

# * 1.1. OLD way ----
pop_by_country <- NULL

for (country in unique(pop$country)){
  pop_sub_country <- pop[pop$country == country,]
  mean_country    <- mean(pop_sub_country$value)
  
  pop_by_country <- rbind(pop_by_country, c(country, mean_country))
}

pop_by_country            <- as.data.frame(pop_by_country, stringsAsFactors = F)
names(pop_by_country)     <- c("country", "mean_value")
pop_by_country$mean_value <- as.numeric(pop_by_country$mean_value)

# * 1.1. A better way ----

pop %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(mean_value = mean(value)) %>%
  dplyr::ungroup()

# * 1.2. What if we wanted variance too? ----

# * 1.2. OLD way ----

# We had to change our previous code to add the variance !
# DO NOT RUN the following lines, just look at the changes necessary
pop_by_country <- NULL

for (country in unique(pop$country)){
  pop_sub_country  <- pop[pop$country == country,]
  mean_country     <- mean(pop_sub_country$value)
  # var_country    <- var(pop_sub_country$value)
  
  # pop_by_country <- rbind(pop_by_country, c(country, mean_country, var_country))
}

pop_by_country             <- as.data.frame(pop_by_country, stringsAsFactors = F)
# names(pop_by_country)    <- c("country", "mean_value", "var_value")
pop_by_country$mean_value  <- as.numeric(pop_by_country$mean_value)
# pop_by_country$var_value <- as.numeric(pop_by_country$var_value)

# * 1.2. Something better ? ----

# We use the previous dataset, build a new one for variance and merge them

var_pop_by_country <- NULL

for (country in unique(pop$country)){
  pop_sub_country    <- pop[pop$country == country,]
  var_country        <- var(pop_sub_country$value)
  
  var_pop_by_country <- rbind(var_pop_by_country, c(country, var_country))
}

var_pop_by_country           <- as.data.frame(var_pop_by_country, stringsAsFactors = F)
names(var_pop_by_country)    <- c("country", "var_value")
var_pop_by_country$var_value <- as.numeric(var_pop_by_country$var_value)

pop_by_country <- merge(pop_by_country, var_pop_by_country, by = "country")

# * 1.3. A better way ----

pop %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(mean_value = mean(value),
                   var_value  = var(value)) %>%
  dplyr::ungroup()

# ********************************* ----
# 2. Working on a sample ----
# ********************************* ----

# * 2.1. OLD way ----

# What if we want to work only with France ?

sub_pop <- pop[pop$country == "france", ]

# What if we want to work only with France AND Japan?

sub_pop <- pop[pop$country == "france" | pop$country == "japan", ]

# What if we want to work only with France AND Japan from 2000 to 2010 excluded ? 

sub_pop <- pop[(pop$country == "france" | pop$country == "japan") & (lubridate::year(pop$time) >= 2000 & lubridate::year(pop$time) < 2010), ]

# AND What if we want to order results by country and then by time

sub_pop <- pop[(pop$country == "france" | pop$country == "japan") & (lubridate::year(pop$time) >= 2000 & lubridate::year(pop$time) < 2010), ]
sub_pop <- sub_pop[order(sub_pop$country, sub_pop$time),]

# * 2.2. A better way ----

pop %>%
  dplyr::filter(country %in% c("france", "japan"),
                dplyr::between(lubridate::year(time), 2000, 2009)) %>%
  dplyr::arrange(country, time)

# * 2.3. [BONUS] ----

# What are the countries with data available for the year 2000 and 2010 ? ----

# OLD ----
unique(pop[lubridate::year(pop$time) == 2000 | lubridate::year(pop$time) == 2010,"country"])

# BETTER ----
pop %>%
  dplyr::filter(lubridate::year(pop$time) %in% c(2000, 2010)) %>%
  dplyr::select(country) %>%
  unique()

# What is the most recent observation for each country ? ----

# OLD ----
result <- NULL

for (country in unique(pop$country)){
  pop_sub_country    <- pop[pop$country == country,]
  pop_sub_country    <- pop_sub_country[order(pop_sub_country$time),]
  pop_sub_country    <- pop_sub_country[nrow(pop_sub_country),]
  
  result             <- rbind(result, pop_sub_country)
}
result

# BETTER ----
pop %>%
  dplyr::group_by(country) %>%
  dplyr::arrange(time) %>%
  dplyr::slice(n()) %>%
  ungroup()

# *********************************************** ----
# 3. Working with wide and long formats ----
# *********************************************** ----

if(!"tidyr" %in% installed.packages()){install.packages("tidyr")}

library(tidyr)

# * 0. Get the data ----

# see the file: 20171215_RDS_atelier_1_WITH_access_to_lxpr0142pv.unix.intra.bdf.local.R
# to know how it has been built

# just load the data set from rda file
load("wide_pop.rda")
View(wide_pop)
str(wide_pop)

# * 3.1. Different ways of selecting columns ----

# do the job
wide_pop[,1:6]

# flexible
wide_pop[,grepl("^time$|_population", names(wide_pop))]

# concise and piping
wide_pop %>% dplyr::select(time, contains("_population"))


# * 3.2. Convert from wide to long ----

# From

# time        australia_population  brazil_employment ...
# 1980-12-31  14.80176              41.26810          ...


# To

# time        type        country    value
# <date>      <chr>       <chr>      <dbl>
# 1980-12-31  population  australia  14.80176
# 2015-12-31  employment  france     24.967741
# ...

# * 3.2.1. OLD way ----

library(stringr)

s <- "C'est le premier atelier de la communauté RDS youhou!"
str_split(s, ' ')
s %<>% c("On apprend dplyr")
str_split(s, ' ')

long_pop <- NULL

for (j in 2:ncol(wide_pop)){
  temp_pop <- wide_pop[,c(1,j)]
  temp_pop$country <- str_split(names(temp_pop)[2], '_')[[1]][1]
  temp_pop$type    <- str_split(names(temp_pop)[2], '_')[[1]][2]
  
  names(temp_pop)[2] <- "value"
  
  long_pop %<>% rbind(temp_pop)
}

# * 3.2.1. A better way ----

wide_pop %>%
  tidyr::gather(type, value, -time) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(country = str_split(type, '_')[[1]][1],
                type    = str_split(type, '_')[[1]][2])

# *********** ----
# ADVICES ----
# *********** ----

# - DRY : Don't Repeat Yourself
# - PIPING operations is better to understand afterwards
# - Being familiar with DPLYR and it will be easier to use ggplot2
# - MAGRITTR with %<>% is convenient for DRY
# - Avoid creating temporary objects: compromise between code clarity and memory optimization

# List of tidyverse functions used:
#   group_by
#   mutate
#   filter
#   select
#   slice
#   summarise
#   arrange
#   between
#   n
#   rowwise
#   gather
#   spread
