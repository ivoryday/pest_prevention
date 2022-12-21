## compile dataset for year 2018


grape2018Kt <- grape[grape$year==2018,] 


grow_2018 <- grape2018Kt[, c(c("id_all", "year"), varieties)]
grow_long_2018 <- grow_2018 %>% gather(variety, grow, -c(id_all, year))
grow_long_2018 <- grow_long_2018 %>% ungroup() %>% arrange(id_all,variety, year)



vars_18 <- c("ds", "prev", "mcol", "contr_infes", "insect", "eharv", nets_pre, "area")


df_2018 <- vars_compile(grow_long_2018, grape2018Kt, vars_n)
df_2018$nets <- 0
df_2018$nets[df_2018$net_lat_insect_==1 | df_2018$net_multi_row_==1 | df_2018$net_specific_row_==1] <- 1
df_2018$invest <- 0
df_2018$invest[df_2018$nets==1 ] <- 1
df_2018$labor <- 0
df_2018$labor[df_2018$eharv==1 | df_2018$prev==1] <- 1
df_2018$prevention <- 0
df_2018$prevention[df_2018$invest==1 | df_2018$labor==1] <- 1


df_2018_otvar <- left_join(df_2018, grape2018Kt[, c("id_all", "process_direct_mg", "creat_finance_reserve", "clean_harvest", 
                                              "insurance", "diversification", "work_offarm", "investment_offarm",
                                              "agri_risk", "prod_risk", "market_risk","extern_finance_risk",
                                              "farm_earning_percent", "viti_earning_percent", "gender", "year_b", "successor","cantons", #"bfs",
                                              "man_pow", "surf_farm", "prod_orgf",
                                              "mg_grape", "mg_wine")], 
                          by=c("id_all"="id_all"))

# recode vars start with "A"
df_2018_otvar <- df_2018_otvar %>% ungroup() %>%  mutate(across(c("ds",  "agri_risk", "prod_risk", "market_risk", "extern_finance_risk",
                                                                "farm_earning_percent", "viti_earning_percent"),
                                                              ~as.numeric(substring(.x, 2,))-1))

df_2018_otvar$age <- as.numeric(df_2018_otvar$year) - as.numeric(df_2018_otvar$year_b)
df_2018_otvar$man_pow <- as.numeric(gsub(",", ".", df_2018_otvar$man_pow))
df_2018_otvar$area <- as.numeric(gsub(",", ".", df_2018_otvar$area))
df_2018_otvar$surf_farm <- as.numeric(gsub(",", ".", df_2018_otvar$surf_farm))
df_2018_otvar$mg_wine <- as.numeric(df_2018_otvar$mg_wine)


## add high susceptibility dummy
df_2018_otvar$suscept <- "_low"
df_2018_otvar$suscept[df_2018_otvar$variety %in% c('bburg', 'gamay')] <- 'High'
df_2018_otvar$suscept[df_2018_otvar$variety %in% c( 'gamaret', 'merlot', 'riesling', 'grburg')] <- 'Med-High'

## highly risk averse
df_2018_otvar$risk_av <- 0
df_2018_otvar$risk_av[df_2018_otvar$agri_risk < 3 | df_2018_otvar$prod_risk<3 | df_2018_otvar$market_risk<3 | df_2018_otvar$extern_finance_risk<3] <- 1




df_2018_otvar_grow <- df_2018_otvar[df_2018_otvar$grow==1, ] 

df_2018_otvar_grow_conv <- df_2018_otvar_grow[df_2018_otvar_grow$prod_orgf==0, ]



df_2018_otvar_grow_conv_impu <- mean_impute(df_2018_otvar_grow_conv, c("agri_risk", "prod_risk", "market_risk", "extern_finance_risk",
                                                                     "farm_earning_percent", "viti_earning_percent","age","man_pow", "surf_farm"))

regdf18 <- df_2018_otvar_grow_conv_impu

regdf18_2 <- rename(regdf18, c("Susceptibility"="suscept", "Risk_willingness_market"="market_risk",
                          "Risk_willingness_production"="prod_risk","Risk_willingness_finance"="extern_finance_risk",
                         
                          "High_risk_aversion"="risk_av", "Hail_insurance"="insurance", "Ag_diversification"="diversification",
                          "Area"="area", "Earning_from_viticulture"="viti_earning_percent", "Processing_direct_marketing"="process_direct_mg",
                          "Investment_off_farm"="investment_offarm",  "Work_off_farm"="work_offarm",
                          "Create_finance_reserve"="creat_finance_reserve",
                          "Gender"="gender", "Age"="age", "Successor"="successor"))


