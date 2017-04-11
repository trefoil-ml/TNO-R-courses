##################################################################################
# Project:      TNO time sereis analysis (petit example)
# Description:  Time series forcasting
# Data:         tsdata.csv
# By:           Hicham Zmarrou
# url:          www.Trefoil.ml
##################################################################################

# remember to change your working directory!!! (don't use mine)
# setwd("./tridata/TNO/data")


# rm(list =ls())
library(tidyverse)
library(xts)
library(highcharter)

tsdata       <- read_csv("./data/tsdata.csv")
tsdata       <- tsdata %>% mutate(datum = if_else(tsdata$date <= 31 , paste0(date, "-" ,"01", "-", "2017"), paste0(date%%31, "-" ,"02", "-", "2017")))
tsdata       <- select(tsdata, user.id, datum,-date,everything())

dup_waar     <-  tsdata  %>% group_by(user.id, datum) %>% summarize(dup_date = n()>1) %>%  filter(dup_date == TRUE) 
unqts        <-  tsdata %>% group_by(user.id, datum) %>% arrange(user.id) %>% filter(row_number() == 1)
ts_signals   <-  select(unqts, starts_with("signal")) %>% ungroup
ts_resources <-  select(unqts, contains("Resource")) %>% ungroup()

ts_signals_user1     <-  as_tibble(ts_signals %>% filter(user.id == "1"))
ts_signals_user1[,1] <- NULL

ts_signals_user1$datum <- as.Date(ts_signals_user1$datum, "%d-%m-%Y")
ts_signals_user1_xts <-  as.xts(ts_signals_user1[,-1], order.by=ts_signals_user1$datum)




# highchart(type = "stock") %>% 
#   hc_add_series(ts_signals_user1_xts[,1], id = "id1 Feeling", name = "Feeling")%>%
#   hc_add_series(ts_signals_user1_xts[,2], id = "id1 Fitness", name = "Fitness")%>%
#   hc_add_series(ts_signals_user1_xts[,3], id = "id1 Functioning", name = "Functioning")%>%
#   hc_add_series(ts_signals_user1_xts[,4], id = "id1 Motivation", name = "Motivation")%>%
#   hc_add_series(ts_signals_user1_xts[,5], id = "id1 Sleep", name = "Sleep")%>%
#   hc_add_series(ts_signals_user1_xts[,6], id = "id1 Stress", name = "Stress")

# We suppose study design is "balanced" over time, i.e. the  number and timing of the repeated measurements are the same
# for all individuals.



# When number and timing of the repeated measurements are the same 
# for all individuals, study design is said to be "balanced" over time.





