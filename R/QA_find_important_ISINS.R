library(tidyverse)
library(tidyr)

direct_audit <- readRDS("C:/test/PACTA_2020_TEST/30_Processed_Inputs/PACTA_2020_TEST_audit_file.rda")

top_n <- 500



# Arrange by decreasing Value

direct_audit_ <- direct_audit %>% arrange(investor_name, portfolio_name, -value_usd) 


# create a unique ID for by combining Investor and portfolio name


direct_audit_id <- transform(direct_audit_, id = paste0(investor_name, "&&&", portfolio_name))


# Filter Equitys and Bonds
direct_audit_id <- subset(direct_audit_id, asset_type == "Equity" | asset_type =="Bonds")

# Group by ID and get Top 10 Values
direct_audit_top10 <- Reduce(rbind, by(direct_audit_id, direct_audit_id["id"], head, n=top_n))
rownames(direct_audit_top10) <-NULL

# Get incorrect ISINS from the TOP 10 Values
direct_audit_top10_inv_top10 <- subset(direct_audit_top10, !is.na(investor_name) & valid_input != 1)

# Get all incorrect ISINs
direct_audit_id_inv <- direct_audit_id[direct_audit_id$valid_input == 0,]

# Get Top 10 from incorrect ISINs
direct_audit_id_inv_top10 <- Reduce(rbind, by(direct_audit_id_inv, direct_audit_id_inv["id"], head, n=top_n))
rownames(direct_audit_top10_inv) <-NULL








saveRDS(direct_audit_top10_inv,"C:/Users/2diiBerlin/Dropbox (2° Investing)/PortCheck_v2/10_Projects/ACA_Direct/30_Processed_Inputs/ACA_Direct_Top10_not_included_ISINS_in_each_portfolio.rda")
saveRDS(direct_audit_inv_top10,"C:/Users/2diiBerlin/Dropbox (2° Investing)/PortCheck_v2/10_Projects/ACA_Direct/30_Processed_Inputs/ACA_Direct_not_included_Top10_ISINS_in_each_portfolio.rda")
saveRDS(direct_audit_top10,"C:/Users/2diiBerlin/Dropbox (2° Investing)/PortCheck_v2/10_Projects/ACA_Direct/30_Processed_Inputs/ACA_Direct_Top10_in_each_portfolio.rda")


write.csv(direct_audit_top10_inv,"C:/Users/2diiBerlin/Dropbox (2° Investing)/PortCheck_v2/10_Projects/ACA_Direct/30_Processed_Inputs/ACA_Direct_Top10_not_included_ISINS_in_each_portfolio.csv",row.names = FALSE)
write.csv(direct_audit_inv_top10,"C:/Users/2diiBerlin/Dropbox (2° Investing)/PortCheck_v2/10_Projects/ACA_Direct/30_Processed_Inputs/ACA_Direct_not_included_Top10_ISINS_in_each_portfolio.csv",row.names = FALSE)
write.csv(direct_audit_top10,"C:/Users/2diiBerlin/Dropbox (2° Investing)/PortCheck_v2/10_Projects/ACA_Direct/30_Processed_Inputs/ACA_Direct_Top10_in_each_portfolio.csv",row.names = FALSE)



###FUND ANALYSIS


### Choose online relevant information from TotalPortfolio
funds_in_portfolio <- portfolio_total %>% filter(direct_holding == 0)  %>% select(fund_isin,isin, investor_name, portfolio_name, holding_id,value_usd, original_value_usd, valid_input)
funds_in_portfolio <- funds_in_portfolio %>% mutate(valid_bool_1 = !is.na(funds_in_portfolio$TwoD.Valid), valid_bool_2 = ifelse(valid_bool_1, ifelse(funds_in_portfolio$TwoD.Valid==1, 1, 0), 0))

### Add rows that are necessary for calculation: weight, valid_weight, a rows just with 1 for an easy calculation of the number of ISINs in a Fund
funds_in_portfolio <- funds_in_portfolio %>% mutate(FundISINweight = ValueUSD/OriginalValue, valid_weight = valid_bool_2 * FundISINweight, n = 1)


### Group ISIS by Fund.ISIN and reduce dataframe to the level of Fund.ISIN while identifing properties of the Fund
### HERE THE ACTUAL RESULT IS GENERATED
funds_in_portfolio_reduced <- funds_in_portfolio %>% group_by(Holding.ID) %>% summarize(number = sum(n), 
                                                                                        TotalWeight = sum(FundISINweight), 
                                                                                        FundCoverageISIN_rel = sum(valid_bool_2)/number,
                                                                                        FundCoverageUSD_abs = sum(valid_bool_2 * ValueUSD) )     %>% select(-number)


### Merge the Results into the Original Portfolio


TotalPortfolio_2 <- left_join(TotalPortfolio, funds_in_portfolio_reduced, by = "Holding.ID")
TotalPortfolio_2 <- TotalPortfolio_2 %>% mutate(FundCoverageUSD_rel = FundCoverageUSD_abs/OriginalValue)

###Alternative approach for merge:

#  TotalPortfolio <- merge(x = TotalPortfolio, y = funds_in_portfolio_reduced, by="Holding.ID")

#TotalPortfolio <- left_join(TotalPortfolio, funds_in_portfolio, by="ISIN")
#TotalPortfolio <- left_join(TotalPortfolio, funds_in_portfolio_reduced, by="Fund.ISIN")

### Return merged portfolio 
return(TotalPortfolio_2)

