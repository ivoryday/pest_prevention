##########################################################################
###  Compilation of regression data, and regression models by table  #####
###   due to data restriction, postal codes are omitted, and         #####
###   variables of past neighborhood averages calculated based       #####
###   on postal codes directly provided (ds_avg)                     #####
###  Need to run ds_fn.R for functions, and ds_2018_RC.R for 2018 data ###
##########################################################################

library(dplyr)
library(tidyr)
library(corrplot)
library(car)
library(ggplot2)
library(mfx)
library(tidyverse)

rm(list=ls())

setwd('~/pathS')


## full public dataset from survey for grape growers
grape <- read.csv('grapedata.csv', stringsAsFactors = F)


# subset of data with postal code available (used to calculate neighborhood averages, postal codes omitted in puslished data)
grapepc <- read.csv("ds_grape_pcavailable.csv", stringsAsFactors = F)

# calculated past neighborhood averages
ds_avg <- read.csv("ds_past_avg.csv", stringsAsFactors = F)


# reshape data for relevant varieties and insect nets measures

varieties <- c("bburg", "chasselas" , "merlot", "riesling", "chardonnay", "gamaret", "grburg", "gamay", "sauvblanc")

nets_pre <- c("net_lat_insect_", "net_specific_row_", "net_multi_row_") # 



## restrict to farmers who appear at least twice

aa <- grapepc  %>% group_by(id_all) %>% filter(n()>1)


aa_1 <- aa %>% group_by(id_all) %>% arrange(year) %>% filter(row_number()==1) 
aa_n <- aa %>% group_by(id_all) %>% arrange(year) %>% filter(row_number()==n()) 



## get indicator of growing that variety
grow_n <- aa_n[, c(c("id_all", "year"), varieties)]
grow_long_n <- grow_n %>% gather(variety, grow, -c(id_all, year))
grow_long_n <- grow_long_n %>% ungroup() %>% arrange(id_all,variety, year)

## get df for year1 to extract year info
grow_1 <- aa_1[, c(c("id_all", "year"), varieties)]
grow_long_1 <- grow_1 %>% gather(variety, grow, -c(id_all, year))
grow_long_1 <- grow_long_1 %>% ungroup() %>% arrange(id_all,variety, year)

## add year1 info to year_n df
grow_long_n$year_1 <- grow_long_1$year


## prefix of vars to be reshaped - year 1
vars_1 <- c("ds", "prev", "mcol", "contr_infes", "insect", "eharv","satisf", "sort_time", nets_pre, "area") # "cl" is abort harvest
## vars to be reshaped - year n
vars_n <- c("ds", "prev", "mcol", "contr_infes", "insect", "eharv", nets_pre, "area")


df_1 <- vars_compile(grow_long_n, aa_1, vars_1)
df_1$nets <- 0
df_1$nets[df_1$net_lat_insect_==1 | df_1$net_multi_row_==1 | df_1$net_specific_row_==1] <- 1
df_1$invest <- 0
df_1$invest[df_1$nets==1] <- 1
df_1$labor <- 0
df_1$labor[df_1$eharv==1 | df_1$prev==1] <- 1
# redefine prevention as either invest or labor involved measures
df_1$prevention <- 0
df_1$prevention[df_1$invest==1 | df_1$labor==1] <- 1
colnames(df_1)[6:ncol(df_1)] <- paste0(colnames(df_1)[6:ncol(df_1)],"_1") # 


df_n <- vars_compile(grow_long_n, aa_n, vars_n)
df_n$nets <- 0
df_n$nets[df_n$net_lat_insect_==1 | df_n$net_multi_row_==1 | df_n$net_specific_row_==1] <- 1
df_n$invest <- 0
df_n$invest[df_n$nets==1] <- 1
df_n$labor <- 0
df_n$labor[df_n$eharv==1 | df_n$prev==1] <- 1
df_n$prevention <- 0
df_n$prevention[df_n$invest==1 | df_n$labor==1] <- 1
colnames(df_n)[6:ncol(df_n)] <- paste0(colnames(df_n)[6:ncol(df_n)],"_n")


df_1n <- cbind(df_n[, -which(names(df_n) %in% c("net_lat_insect__n", "net_specific_row__n", "net_multi_row__n"))], 
               df_1[, c("ds_1", "prev_1", "mcol_1", "contr_infes_1","insect_1","eharv_1", "nets_1" ,"invest_1", "labor_1", 
                        "satisf_1", "prevention_1", "sort_time_1", "area_1")])


# join other vars at the farm level - remove "post_code" from published data
df_1n_otvar <- left_join(df_1n, aa[, c("id_all",  "year","cantons", "process_direct_mg", "creat_finance_reserve", "clean_harvest", 
                                       "insurance", "diversification", "work_offarm", "investment_offarm",
                                       "agri_risk", "prod_risk", "market_risk","extern_finance_risk",
                                       "farm_earning_percent", "viti_earning_percent", "gender", "year_b", "successor",
                                       "man_pow", "surf_farm", "prod_orgf",
                                       "mg_grape", "mg_wine", "mg_grape_p_wm", "mg_grape_p_cop", "mg_grape_p_com", "mg_wine_dm", "mg_wine_com", 
                                       "mg_wine_distr", "mg_wine_gastro")], 
                         by=c("id_all"="id_all", "year"="year"))

# join first year expected yield loss
df_1n_otvar <- left_join(df_1n_otvar, aa[, c("id_all", "year", "yield_expected", "add_ds_cost")], 
                         by=c("id_all"="id_all", "year_1"="year"))

# recode vars start with "A"
df_1n_otvar <- df_1n_otvar %>% ungroup() %>%  mutate(across(c("ds_1", "ds_n",  "agri_risk", "prod_risk", "market_risk", "extern_finance_risk",
                                                              "farm_earning_percent", "viti_earning_percent", "yield_expected", "add_ds_cost"),
                                                            ~as.numeric(substring(.x, 2,))-1))

## recode marketing channels
df_1n_otvar <- df_1n_otvar %>% ungroup() %>%  mutate(across(c("mg_grape_p_wm", "mg_grape_p_cop", "mg_grape_p_com", "mg_wine_dm", "mg_wine_com", 
                                                            "mg_wine_distr", "mg_wine_gastro"), ~as.numeric(gsub(",", ".", .x))))
df_1n_otvar <- df_1n_otvar %>% ungroup() %>%  mutate(across(c("mg_grape_p_wm", "mg_grape_p_cop", "mg_grape_p_com", "mg_wine_dm", "mg_wine_com", 
                                                              "mg_wine_distr", "mg_wine_gastro"), ~replace_na(.x, 0)))

# recode other vars
df_1n_otvar$age <- as.numeric(df_1n_otvar$year) - as.numeric(df_1n_otvar$year_b)

df_1n_otvar$man_pow <- as.numeric(gsub(",", ".", df_1n_otvar$man_pow))
df_1n_otvar$surf_farm <- as.numeric(gsub(",", ".", df_1n_otvar$surf_farm))
df_1n_otvar$area_n <- as.numeric(gsub(",", ".", df_1n_otvar$area_n))
df_1n_otvar$mg_wine <- as.numeric(df_1n_otvar$mg_wine)

## add high susceptibility dummy
df_1n_otvar$suscept <- "_low"
df_1n_otvar$suscept[df_1n_otvar$variety %in% c('bburg', 'gamay')] <- 'High'
df_1n_otvar$suscept[df_1n_otvar$variety %in% c( 'gamaret', 'merlot', 'riesling', 'grburg')] <- 'Med-High'

## highly risk averse
df_1n_otvar$risk_av <- 0
df_1n_otvar$risk_av[df_1n_otvar$agri_risk < 3 | df_1n_otvar$prod_risk<3 | df_1n_otvar$market_risk<3 | df_1n_otvar$extern_finance_risk<3] <- 1

df_1n_otvar$risk_av_mktg <- 0
df_1n_otvar$risk_av_mktg[df_1n_otvar$market_risk<3 ] <- 1

df_1n_otvar$risk_av_prod <- 0
df_1n_otvar$risk_av_prod[df_1n_otvar$prod_risk<3 ] <- 1

df_1n_otvar$risk_av_ext_fin <- 0
df_1n_otvar$risk_av_ext_fin[df_1n_otvar$extern_finance_risk<3] <- 1


## only market as grape
df_1n_otvar$Marketing_grape <- 0
df_1n_otvar$Marketing_grape[df_1n_otvar$mg_grape==1 & df_1n_otvar$mg_wine==0] <- 1

## only market as wine
df_1n_otvar$Marketing_wine <- 0
df_1n_otvar$Marketing_wine[df_1n_otvar$mg_wine==1 & df_1n_otvar$mg_grape==0] <- 1


## recode "yield_expected", "add_ds_cost" as scales were different in 2016 and 2017-2018
# 2016: 0: 0; 1:1-25%; >2:>25%
# 2017: 0: 0; 1-5:1-25%; >5:>25%
df_1n_otvar$yield_expected_recode <- df_1n_otvar$yield_expected
df_1n_otvar$yield_expected_recode[df_1n_otvar$year_1==2016 & df_1n_otvar$yield_expected>=2] <- 2
df_1n_otvar$yield_expected_recode[df_1n_otvar$year_1==2017 & df_1n_otvar$yield_expected>=1 &  df_1n_otvar$yield_expected<=5] <- 1
df_1n_otvar$yield_expected_recode[df_1n_otvar$year_1==2017 & df_1n_otvar$yield_expected>=6] <- 2
#df_1n_otvar$yield_expected_recode[is.na(df_1n_otvar$yield_expected_recode)==T] <- 0

df_1n_otvar$add_ds_cost_recode <- df_1n_otvar$add_ds_cost
df_1n_otvar$add_ds_cost_recode[df_1n_otvar$year_1==2016 & df_1n_otvar$add_ds_cost>=2] <- 2
df_1n_otvar$add_ds_cost_recode[df_1n_otvar$year_1==2017 & df_1n_otvar$add_ds_cost>=1 &  df_1n_otvar$add_ds_cost<=5] <- 1
df_1n_otvar$add_ds_cost_recode[df_1n_otvar$year_1==2017 & df_1n_otvar$add_ds_cost>=6] <- 2
#df_1n_otvar$add_ds_cost_recode[is.na(df_1n_otvar$add_ds_cost_recode)==T] <- 0


# subset of those who grew
df_1n_otvar_grow <- df_1n_otvar[df_1n_otvar$grow==1, ] 

## add name of all measures 

df_1n_otvar_grow$measure_1 <- "_None"
df_1n_otvar_grow$measure_1[df_1n_otvar_grow$invest_1==1] <- "Invest"
df_1n_otvar_grow$measure_1[df_1n_otvar_grow$labor_1==1] <- "Labor"
df_1n_otvar_grow$measure_1[df_1n_otvar_grow$labor_1==1 & df_1n_otvar_grow$invest_1==1] <- "Invest" ## if not distinguish multiple
df_1n_otvar_grow$measure_1[df_1n_otvar_grow$insect_1==1 & df_1n_otvar_grow$prevention_1==0] <- "Pesticide"
df_1n_otvar_grow$measure_1[df_1n_otvar_grow$insect_1==1 & df_1n_otvar_grow$prevention_1==1] <- "Pesticide"



df_1n_otvar_grow$measure_n <- "_None"
df_1n_otvar_grow$measure_n[df_1n_otvar_grow$invest_n==1] <- "Invest"
df_1n_otvar_grow$measure_n[df_1n_otvar_grow$labor_n==1] <- "Labor"
df_1n_otvar_grow$measure_n[df_1n_otvar_grow$labor_n==1 & df_1n_otvar_grow$invest_n==1] <- "Invest" # and labor
df_1n_otvar_grow$measure_n[df_1n_otvar_grow$insect_n==1 & df_1n_otvar_grow$prevention_n==0] <- "Pesticide" # only
df_1n_otvar_grow$measure_n[df_1n_otvar_grow$insect_n==1 & df_1n_otvar_grow$prevention_n==1] <- "Pesticide" #and prevention


## add past averages

df_1n_otvar_grow_avg <- inner_join(df_1n_otvar_grow, ds_avg, by=c("id_all"="id_all", "year_1"="year", "variety"="variety"))
df_1n_otvar_grow_avg <- df_1n_otvar_grow_avg[!duplicated(df_1n_otvar_grow_avg),]

## if use conventional only

df_1n_otvar_grow_conv <- df_1n_otvar_grow_avg[df_1n_otvar_grow_avg$prod_orgf==0, ]

df_1n_otvar_grow_conv_impu <- mean_impute(df_1n_otvar_grow_conv, c("agri_risk", "prod_risk", "market_risk", "extern_finance_risk",
                                                                   "ds_1","ds_n","yield_expected_recode", "add_ds_cost_recode",
                                                                   "farm_earning_percent", "viti_earning_percent","age","man_pow", "surf_farm"))




regdf <- df_1n_otvar_grow_conv_impu[df_1n_otvar_grow_conv_impu$year==2018,] 

regdf2 <- rename(regdf, c("Average_perceived_infest"="avg_ds", "Average_labor_measure"="avg_labor",
                          "Average_invest_measure"="avg_invest", "Average_insecticide"="avg_insect",
                          "Susceptibility"="suscept", "Risk_willingness_market"="market_risk",
                          "Risk_willingness_production"="prod_risk","Risk_willingness_finance"="extern_finance_risk",
                          "Previous_exp_yield_loss"="yield_expected_recode", "Previous_exp_additional_cost"="add_ds_cost_recode",
                           "High_risk_aversion"="risk_av", "Hail_insurance"="insurance", "Ag_diversification"="diversification",
                           "Area"="area_n", "Earning_from_viticulture"="viti_earning_percent", "Processing_direct_marketing"="process_direct_mg",
                          "Investment_off_farm"="investment_offarm", "Work_off_farm"="work_offarm",
                          "Create_finance_reserve"="creat_finance_reserve",
                           "Gender"="gender", "Age"="age", "Successor"="successor"))  #, "investment_offarm"="Off-farm_invest", "work_offarm"="Off-farm_work"


regdf2$Male <- 0
regdf2$Male[regdf2$Gender=="M"] <- 1

regdf2$Successor <- as.numeric(regdf2$Successor)


### Table 3: pest prevention

m1 <- lm(prevention_n~ Average_perceived_infest+Susceptibility+
           Average_labor_measure+Average_invest_measure+Average_insecticide+
           Risk_willingness_market+
           Risk_willingness_production+
           Risk_willingness_finance+         
           Hail_insurance+
           Ag_diversification+Investment_off_farm+Work_off_farm+Create_finance_reserve+ 
           Earning_from_viticulture+
           Processing_direct_marketing+
           Previous_exp_yield_loss+
           Previous_exp_additional_cost+
           Male+
           Successor+Age+ Area+ 
           factor(year_1)+factor(cantons), data = regdf2)  
summary(m1)



###### risk pref one at a time

m11 <- lm(prevention_n~ Average_perceived_infest+Susceptibility+#
            Average_labor_measure+Average_invest_measure+Average_insecticide+
            Risk_willingness_market+
            Hail_insurance+
            Ag_diversification+Investment_off_farm+Work_off_farm+Create_finance_reserve+ 
            Earning_from_viticulture+
            Processing_direct_marketing+
            Previous_exp_yield_loss+
            Previous_exp_additional_cost+
            Male+
            Successor+Age+ Area+ 
           factor(year_1)+factor(cantons), data = regdf2)  
summary(m11)

m12 <- lm(prevention_n~ Average_perceived_infest+Susceptibility+#
            Average_labor_measure+Average_invest_measure+Average_insecticide+
            Risk_willingness_production+
            Hail_insurance+
            Ag_diversification+Investment_off_farm+Work_off_farm+Create_finance_reserve+ 
            Earning_from_viticulture+
            Processing_direct_marketing+
            Previous_exp_yield_loss+
            Previous_exp_additional_cost+
            Male+
            Successor+Age+ Area+ 
            factor(year_1)+factor(cantons), data = regdf2)  #
summary(m12)

m13 <- lm(prevention_n~ Average_perceived_infest+Susceptibility+#
            Average_labor_measure+Average_invest_measure+Average_insecticide+
            Risk_willingness_finance+         
            Hail_insurance+
            Ag_diversification+Investment_off_farm+Work_off_farm+Create_finance_reserve+ 
            Earning_from_viticulture+
            Processing_direct_marketing+
            Previous_exp_yield_loss+
            Previous_exp_additional_cost+
            Male+
            Successor+Age+ Area+ 
            factor(year_1)+factor(cantons), data = regdf2)  
summary(m13)



# for results in Table A2, in each model, comment out "Average_labor_measure+Average_invest_measure+Average_insecticide+"
# for results in Table A4, in each model, change from "Average_perceived_infest+Susceptibility" to "Average_perceived_infest*Susceptibility"


## Table A3, column (1), alternative risk measure
m14 <- lm(prevention_n~ Average_perceived_infest+Average_labor_measure+Average_invest_measure+Average_insecticide+
            Susceptibility+
            High_risk_aversion+
            Hail_insurance+
            Ag_diversification+Investment_off_farm+Work_off_farm+Create_finance_reserve+ 
            Earning_from_viticulture+
            Processing_direct_marketing+
            Previous_exp_yield_loss+
            Previous_exp_additional_cost+
            Earning_from_viticulture+
            Male+Successor+Age+ Area+ 
            factor(year_1)+factor(cantons), data = regdf2)  
summary(m14)


## Table A3, column (2), 2018 cross-section
m15 <- lm(prevention~ 
            Susceptibility+
            Risk_willingness_market+
            Risk_willingness_production+
            Risk_willingness_finance+
            Hail_insurance+
            Ag_diversification+Investment_off_farm+Work_off_farm+Create_finance_reserve+ 
            Earning_from_viticulture+
            Processing_direct_marketing+
            Earning_from_viticulture+
            Gender+Successor+Age+ Area+ 
            factor(cantons), data = regdf18_2)  
summary(m15)



## Table A5: invest vs labor
m2 <- lm(invest_n~ Average_perceived_infest+Susceptibility+
           Average_labor_measure+Average_invest_measure+Average_insecticide+
           Risk_willingness_market+
           Risk_willingness_production+
           Risk_willingness_finance+          
           Hail_insurance+
           Ag_diversification+Investment_off_farm+Work_off_farm+Create_finance_reserve+ 
           Earning_from_viticulture+
           Processing_direct_marketing+
           Previous_exp_yield_loss+
           Previous_exp_additional_cost+
           Male+Successor+Age+ Area+ 
           factor(year_1)+factor(cantons), data = regdf2[regdf2$prevention_n==1,])  
summary(m2)


m21 <- lm(invest_n~ Average_perceived_infest+Susceptibility+
            Average_labor_measure+Average_invest_measure+Average_insecticide+
            Risk_willingness_market+
            Hail_insurance+
            Ag_diversification+Investment_off_farm+Work_off_farm+Create_finance_reserve+ 
            Earning_from_viticulture+
            Processing_direct_marketing+
            Previous_exp_yield_loss+
            Previous_exp_additional_cost+
            Male+Successor+Age+ Area+ 
           factor(year_1)+factor(cantons), data = regdf2[regdf2$prevention_n==1,])  
summary(m21)

m22 <- lm(invest_n~ Average_perceived_infest+Susceptibility+
            Average_labor_measure+Average_invest_measure+Average_insecticide+
            Risk_willingness_production+
            Hail_insurance+
            Ag_diversification+Investment_off_farm+Work_off_farm+Create_finance_reserve+ 
            Earning_from_viticulture+
            Processing_direct_marketing+
            Previous_exp_yield_loss+
            Previous_exp_additional_cost+
            Male+Successor+Age+ Area+ 
           factor(year_1)+factor(cantons), data = regdf2[regdf2$prevention_n==1,])  
summary(m22)

m23 <- lm(invest_n~ Average_perceived_infest+Susceptibility+
            Average_labor_measure+Average_invest_measure+Average_insecticide+
            Risk_willingness_finance+ 
            Hail_insurance+
            Ag_diversification+Investment_off_farm+Work_off_farm+Create_finance_reserve+ 
            Earning_from_viticulture+
            Processing_direct_marketing+
            Previous_exp_yield_loss+
            Previous_exp_additional_cost+
            Male+Successor+Age+ Area+ 
           factor(year_1)+factor(cantons), data = regdf2[regdf2$prevention_n==1,])  
summary(m23)




## Table A6: probit model
pbt1 <- probitmfx(prevention_n~Average_perceived_infest+Susceptibility+
                    Average_labor_measure+Average_invest_measure+Average_insecticide+
                    Risk_willingness_market+
                    Risk_willingness_production+
                    Risk_willingness_finance+ Hail_insurance+
                    Ag_diversification+Investment_off_farm+Work_off_farm+Create_finance_reserve+ 
                    Earning_from_viticulture+
                    Processing_direct_marketing+
                    Previous_exp_yield_loss+
                    Previous_exp_additional_cost+
                    Gender+Successor+Age+ Area+ 
                    factor(year_1)#+factor(cantons)
                  , data = regdf2)

pbt1$mfxest

pbt1.1 <- probitmfx(prevention_n~Average_perceived_infest+Susceptibility+
                      Average_labor_measure+Average_invest_measure+Average_insecticide+
                      Risk_willingness_market+
                      Hail_insurance+
                      Ag_diversification+Investment_off_farm+Work_off_farm+Create_finance_reserve+ 
                      Earning_from_viticulture+
                      Processing_direct_marketing+
                      Previous_exp_yield_loss+
                      Previous_exp_additional_cost+
                      Gender+Successor+Age+ Area+ 
                      factor(year_1)#+factor(cantons)
                    , data = regdf2)

pbt1.1$mfxest

pbt1.2 <- probitmfx(prevention_n~Average_perceived_infest+Susceptibility+
                      Average_labor_measure+Average_invest_measure+Average_insecticide+
                      
                      Risk_willingness_production+
                      Hail_insurance+
                      Ag_diversification+Investment_off_farm+Work_off_farm+Create_finance_reserve+ 
                      Earning_from_viticulture+
                      Processing_direct_marketing+
                      Previous_exp_yield_loss+
                      Previous_exp_additional_cost+
                      Gender+Successor+Age+ Area+ 
                      factor(year_1)#+factor(cantons)
                    , data = regdf2)

pbt1.2$mfxest



pbt1.3 <- probitmfx(prevention_n~Average_perceived_infest+Susceptibility+
                      Average_labor_measure+Average_invest_measure+Average_insecticide+
                      
                      Risk_willingness_finance+ Hail_insurance+
                      Ag_diversification+Investment_off_farm+Work_off_farm+Create_finance_reserve+ 
                      Earning_from_viticulture+
                      Processing_direct_marketing+
                      Previous_exp_yield_loss+
                      Previous_exp_additional_cost+
                      Gender+Successor+Age+ Area+ 
                      factor(year_1)#+factor(cantons)
                    , data = regdf2)

pbt1.3$mfxest

