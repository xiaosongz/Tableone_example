library(tidyverse)
library(arsenal)
# load data
sample_data <- read_csv("Data/sample_data.csv")
# set up google sheet
# full URL is https://docs.google.com/spreadsheets/d/1zQ8ebWoBuGwo5bOhILh0929jB2O8hqoEo6Oa3SvtwIo/edit#gid=0
# unique SS id is string after "/d/" before "/edit#gid=0"
library(googlesheets4)
ss = "1zQ8ebWoBuGwo5bOhILh0929jB2O8hqoEo6Oa3SvtwIo"

# create tabulation -------------------------------------------------------

tab <- tableby(gtd_binary ~ distance + gdp + pop + region_name + income_group_name,
               data = sample_data,
               numeric.stats=c("meansd"),# you can specify numeric stats here
               # you can choose multiple like numeric.stats=c("N", "median", "q1q3")
               # cat.stats = c("countpct","countrowpct","countN") 
               # youcan add stratification, subset or other options for tabulation.
               numeric.test="kwt", # you can specify type of test for numerical variables here
               cat.test="chisq",
               cat.simplify = TRUE)# this will keep only 1 level for categorical variables that has only 2-levels
summary(tab)

# now rearrange the data for upload
cnt_pct = tab %>% as_tibble() %>%
  filter(term == "countpct") %>% # keep only categorical variables
  pivot_longer(cols = c(`0`,`1`,Total),names_to = "col_name") %>%  # create long data format so we only need to un-nest 1 column
  rowwise() %>% 
  mutate(value = list(set_names(value, c('cnt', 'pct')))) %>% # 
  unnest_wider(col = value) %>% 
  select(-c(group.term,group.label,strata.term)) %>% 
  pivot_wider(
    names_from = c('col_name'),
    values_from = c('cnt',"pct"),
    names_glue = "{col_name}_{.value}",
    names_vary = "slowest"
  ) %>% as.data.frame()

## now we can upload to googlesheet 
write_sheet(data = cnt_pct,sheet = "cnt_pct",ss = ss)
# you will see the following message if you run this the first time
# > write_sheet(data = cnt_pct,sheet = "cnt_pct",ss = ss)
# Is it OK to cache OAuth access credentials in the folder C:/Users/xxxxx/AppData/Local/gargle/gargle/Cache between R sessions?
#   
# 1: Yes
# 2: No
# Selection: Yes
# Waiting for authentication in browser...
# Press Esc/Ctrl + C to abort
# Authentication complete.
# v Writing to TableOne_example_Fealon.
# v Writing to sheet cnt_pct.

## same thing for the mean and sd

# now rearrange the data for upload
mean_sd = tab %>% as_tibble() %>%
  filter(term == "meansd") %>% # keep only categorical variables
  pivot_longer(cols = c(`0`,`1`,Total),names_to = "col_name") %>%  # create long data format so we only need to un-nest 1 column
  rowwise() %>% 
  mutate(value = list(set_names(value, c('mean', 'sd')))) %>% # you will need to modify this if you chose more/different numeric.stats or cat.stats
  unnest_wider(col = value) %>% 
  select(-c(group.term,group.label,strata.term)) %>% 
  pivot_wider(
    names_from = c('col_name'),
    values_from = c('mean',"sd"),
    names_glue = "{col_name}_{.value}",# this will control how the col names been created
    names_vary = "slowest"
  ) %>% as.data.frame()

## now we can upload to googlesheet 
write_sheet(data = mean_sd,sheet = "mean_sd",ss = ss)




