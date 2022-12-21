#####################################
###     Descriptive stats        ####
###  Need to run ds_reg_RC.R to  ####
###   to get regression data     ####
#####################################

library(ggplot2)



## first run ds_reg_Rc to compile data for regression

# Table 1: prevention measures by variety

descdat <- regdf2
shares <- descdat %>% group_by(Susceptibility,variety) %>% 
  summarise(varea = sum(Area), vprev=sum(prev_n), n=n(), invshare=sum(invest_n)/sum(prev_n)) %>% 
  ungroup(Susceptibility) %>%  
  mutate(pct = varea / sum(varea), prevshare = vprev/n)

shares$percent <- paste0(round(shares$pct*100, 1),  "\\%")
shares$prevpercent <- round(shares$prevshare*100, 1)
shares$Area <- round(shares$varea, 0)
#shares$numpct <- paste0(shares$n, " (", shares$percent, "\\%)")
shares$UsedPrevention <- paste0(shares$prevpercent,  "\\%")

sharetab <- shares[, c("Susceptibility", "variety","n","Area", "percent", "UsedPrevention")]  %>% group_by(Susceptibility) %>% arrange(Area, .by_group = TRUE)


# Table 2: summary stats

descdf0 <- regdf2 %>% summarise(across(.cols = everything(), list(mean = mean, sd = sd)))

desc_mean <- regdf2 %>% summarise(across(c(prevention_n, Average_perceived_infest, Average_labor_measure, 
                                           Average_invest_measure, Average_insecticide, 
                                           Risk_willingness_market,
                                           Risk_willingness_production,
                                           Risk_willingness_finance,Hail_insurance,
                                           Ag_diversification,Investment_off_farm,Work_off_farm,Create_finance_reserve, 
                                           Earning_from_viticulture, 
                                           Processing_direct_marketing, 
                                           Previous_exp_yield_loss, 
                                           Previous_exp_additional_cost, 
                                           Male, Successor,
                                           Age,  Area), 
                                         list(mean = mean)))

desc_sd <- regdf2 %>% summarise(across(c(prevention_n, Average_past_perceived_infest, Average_past_labor_measure, 
                                         Average_past_invest_measure, Average_past_insecticide, 
                                         Risk_willingness_market,
                                         Risk_willingness_production,
                                         Risk_willingness_finance,Hail_insurance,
                                         Ag_diversification,Investment_off_farm,Work_off_farm,Create_finance_reserve, 
                                         Earning_from_viticulture, 
                                         Processing_direct_marketing, 
                                         Previous_exp_yield_loss, 
                                         Previous_exp_additional_cost,
                                         Male, Successor,
                                         Age,  Area), 
                                       list(sd = sd)))

descdf <- as.data.frame(t(desc_mean))
descdf$sd <- as.numeric(t(desc_sd))
row.names(descdf) <- substring(row.names(descdf), 1, nchar(row.names(descdf))-5)
descdf <- round(descdf, digits = 2)


## Figure A1: correlations

cordat <- dplyr::select(regdf2, 
                        Risk_willingness_market,
                        Risk_willingness_production,
                        Risk_willingness_finance,Hail_insurance,
                        Earning_from_viticulture, 
                        Ag_diversification,Investment_off_farm,Work_off_farm,Create_finance_reserve, 
                        Earning_from_viticulture, 
                        Processing_direct_marketing)
correlation1 <- cor(cordat, method=c("pearson"), use = "complete.obs")

t1<-corrplot(correlation1,type = "upper", col=colorRampPalette(c("lightblue", "white", "lightblue",  
                                                                 "dodgerblue3","blue"))(100), 
             col.lim=c(-.25,1), 
             order = "original", 
             tl.col = "black", tl.srt = 30,addCoef.col = "black",mar=c(1,0,1,0), 
             title="", tl.cex = 0.7, 
             cl.cex=1, number.cex=0.85)





#Table A1: pest management strategy over two periods 
round(prop.table(table(df_1n_otvar_grow$measure_1, df_1n_otvar_grow$measure_n), margin = 1)*100, digits=1)
