####
#ENSURE FIRST THAT THE CONTENT OF /data/ is copied to the project output folder ;) I was to lazy to code this.
####
asset_class <- "Equity" 

portfolio_overview <-  readRDS(paste0(proc_input_path,"/", project_name, "_overview_portfolio.rda"))
company_results <- readRDS(paste0(results_path,"/", asset_class, "_results_company.rda"))
portfolio_results <- readRDS(paste0(results_path,"/", asset_class, "_results_portfolio.rda"))

market <- readRDS(paste0( data_location_ext,"/", tolower(asset_class),"_results_market.rda"))

investor_name_ = "Meta Investor"

all_results_eq <- portfolio_results
all_results_eq 

technologies <- unique(all_results_eq$technology) 
technologies <- technologies[!is.na(technologies)]

scenarios <- unique(all_results_eq$scenario)
scenarios <- scenarios[!is.na(scenarios)]


# PLEASE CLASSIFY RATING: 

# TRUE = green
# FALSE = brown
# NA = neither


technology_green <- list(list("Electric", TRUE),
                  list("Hybrid",  TRUE),
                  list("ICE",  FALSE),
                  list("CoalCap",  FALSE),
                  list("GasCap",  ""),
                  list("HydroCap",  TRUE),
                  list("NuclearCap",  ""),
                  list("OilCap",  FALSE),
                  list("RenewablesCap", TRUE),  
                  list("Freight" ,  ""),
                  list("Mix",   ""),
                  list("Other",  ""),
                  list("Passenger",  ""), 
                  list("Gas",     ""),
                  list("Oil" ,   FALSE),
                  list("Coal" ,   TRUE),
                  list("Grinding",   ""),
                  list("Integrated facility", ""), 
                  list("Ac-Electric Arc Furnace", TRUE),  
                  list("Bof Shop" ,  FALSE),
                  list("Dc-Electric Arc Furnace", TRUE),  
                  list("Open Hearth Meltshop", FALSE))

# Rating of importange 
# 0 = neglect
# 1 = low# 
# 10 = high

# NEUTRAL

# EMMSION BASED

# BASED ON THE SECTOR WEIGHT IN THE MARKET / VALUE

# 

technology_rating_only_brown <- list(list("Electric", 0),
                          list("Hybrid",0),
                          list("ICE",  10),
                          list("CoalCap",  10),
                          list("GasCap",  10),
                          list("HydroCap",  0),
                          list("NuclearCap",  10),
                          list("OilCap",  10),
                          list("RenewablesCap", 0),  
                          list("Freight" , 0),
                          list("Mix",   0),
                          list("Other",  0),
                          list("Passenger",  0), 
                          list("Gas",     10),
                          list("Oil" ,   10),
                          list("Coal" ,   10),
                          list("Grinding",  10),
                          list("Integrated facility", 0), 
                              list("Ac-Electric Arc Furnace", 0),  
                          list("Bof Shop" ,  0),
                          list("Dc-Electric Arc Furnace", 10),  
                          list("Open Hearth Meltshop", 10))

technology_rating_equal <- list(list("Electric", 1),
                                     list("Hybrid",  1),
                                     list("ICE",  1),
                                     list("CoalCap",  1),
                                     list("GasCap",  1),
                                     list("HydroCap",  1),
                                     list("NuclearCap",  1),
                                     list("OilCap",  1),
                                     list("RenewablesCap", 1),  
                                     list("Freight" , 1),
                                     list("Mix",   1),
                                     list("Other",  1),
                                     list("Passenger",  1), 
                                     list("Gas",     1),
                                     list("Oil" ,   1),
                                     list("Coal" ,   1),
                                     list("Grinding",  1),
                                     list("Integrated facility", 1), 
                                     list("Ac-Electric Arc Furnace", 1),  
                                     list("Bof Shop" ,  1),
                                     list("Dc-Electric Arc Furnace", 1),  
                                     list("Open Hearth Meltshop", 1))

technology_rating_emission_based <- list(list("Electric", 0.0396),
                          list("Hybrid",  0.0153),
                          list("ICE",  0.0342),
                          list("CoalCap",  0.055849),
                          list("GasCap",  0.0141222),
                          list("HydroCap",  0.0208805),
                          list("NuclearCap",  0.0243517),
                          list("OilCap",  0.0040642),
                          list("RenewablesCap", 0.1007346),  
                          list("Freight" , 0.005),
                          list("Mix",   0.005),
                          list("Other",  0.005),
                          list("Passenger",  0.005), 
                          list("Gas",     0.119),
                          list("Oil" ,   0.231),
                          list("Coal" ,   0.23),
                          list("Grinding",  0.025),
                          list("Integrated facility", 0.025), 
                          list("Ac-Electric Arc Furnace", 0.06),  
                          list("Bof Shop" ,  0.06),
                          list("Dc-Electric Arc Furnace", 0.06),  
                          list("Open Hearth Meltshop", 0.06))










bubble_chart_green_brown<- function(data_set_ = portfolio_level_results) {
  
  
  portfolio_level_results_ <-  portfolio_level_results %>% mutate(inv_port = paste(investor_name, portofolio_name))
  
  jsonhead = paste0("var data_greenbrown_bubble_",tolower(asset_class)," = [")
  jsontail = paste0("];")                  
  
  portfolio_names_ <- unique(portfolio_level_results_$inv_port)
  i=1
  
  sizes <- portfolio_level_results$portfolio_value_usd
  max_sizes <- max(sizes, na.rm = T)
  sizes <- round(sizes/max_sizes * 5) +3
  

  for (i in 1:(length(portfolio_names_)-1)){
      portfolio_name_<- portfolio_names_[i]
      green <- portfolio_level_results_[i,"exposure_green"]
      brown <-portfolio_level_results_[i,"exposure_brown"]
      
      size_ <- sizes[i]
      ratio_ <-portfolio_level_results_[i,"ratio_green_greenbrown"]
    
    
      entry <- paste0("{\"portfolio_name\": \"", portfolio_name_,"\", \"green\": ", green ,", \"brown\":", brown,", \"v\":",ratio_,", \"z\":",size_,", \"color_\": ", "\"#ff3737\"","}, ")
      jsonhead <- paste0(jsonhead, entry)}
  
  i = i+1
  portfolio_name_<- portfolio_names_[i]
  green <- portfolio_level_results_[i,"exposure_green"]
  brown <-portfolio_level_results_[i,"exposure_brown"]
  size_ <- sizes[i]
  ratio_ <-portfolio_level_results_[i,"ratio_green_greenbrown"]
  
  
  
  
  
  entry <- paste0("{\"portfolio_name\": \"", portfolio_name_,"\", \"green\": ", green ,", \"brown\":", brown,", \"v\":",ratio_,", \"z\":",size_,", \"color_\": ", "\"#ff3737\"","} ")
  jsonhead <- paste0(jsonhead, entry)
  
  
  bubble_chart_data <- paste0(jsonhead, jsontail)
  
  return(bubble_chart_data)
}


bubble_chart <- function(data_set_ = portfolio_level_results,
                         x_axis = "exposure_green",
                         y_axis = "alignment_emission_based") {
  portfolio_level_results_ <-  portfolio_level_results %>% mutate(inv_port = paste(investor_name, portofolio_name))
  
  jsonhead = paste0("var data_portfolio_bubble_", tolower(asset_class), " = [")
  jsontail = paste0("];")                  
  
  portfolio_names_ <- unique(portfolio_level_results_$inv_port)
                    i=1
  
  #alignment_min <- min(portfolio_level_results_[y_axis], na.rm =TRUE)
  #if (alignment_min<(-1)){alignment_min=-1}
  #alignment_max <- max(portfolio_level_results_[y_axis], na.rm =TRUE)
  #if (alignment_max>1){alignment_max=-1}
  
  #alignment_span <- abs(alignment_max-alignment_min) 
  
  sizes <- portfolio_level_results$portfolio_value_usd
  max_sizes <- max(sizes, na.rm = T)
  sizes <- round(sizes/max_sizes * 5) +3
  
  for (i in 1:(length(portfolio_names_)-1)){
  portfolio_name_<- portfolio_names_[i]
  exposure_ <- portfolio_level_results_[i,x_axis]
  alignement_ <-portfolio_level_results_[i,y_axis]
  #alignement_ <- (alignement_ - alignment_min)/alignment_span 
  size_ <- sizes[i]
  green_brown_ratio_ =portfolio_level_results_[i,"ratio_green_greenbrown"]
  if (alignement_< (-1)){alignement_=0}
  
  
  entry <- paste0("{\"portfolio_name\": \"", portfolio_name_,"\", \"ald_sector\":\"Automotive\",\"plan_tech_share\": ", exposure_ ,", \"y\":  ", alignement_,", \"z\": ", size_, ", \"v\": ", green_brown_ratio_,", \"color_\": ", "\"#ff3737\""," }, ")
  jsonhead <- paste0(jsonhead, entry)}
                    
  i = i+1
  portfolio_name_<- portfolio_names_[i]
  exposure_ <- portfolio_level_results_[i,x_axis]
  alignement_ <-portfolio_level_results_[i,y_axis]
  #alignement_ <- (alignement_ -alignment_min)/alignment_span 
  if (alignement_<(-1)){alignement_=0}
  
  
  entry <- paste0("{\"portfolio_name\": \"", portfolio_name_,"\", \"ald_sector\":\"Automotive\",\"plan_tech_share\": ", exposure_ ,", \"y\":  ", alignement_,", \"z\": ", size_, ", \" v\":  ", green_brown_ratio_,", \"color_\": ", "\"#ff3737\"","}")
  jsonhead <- paste0(jsonhead, entry)
  
  
 bubble_chart_data <- paste0(jsonhead, jsontail)
 
  return(bubble_chart_data)
}



portfolio_exposure_green_and_brown <- function(investor_name_ = investor_name_, 
                                     portfolio_name_ = portfolio_name_, 
                                     technology_green_ = technology_green,
                                     scenario_ = "SDS",
                                     scenario_geography_ = "Global",
                                     data_set_ = all_results_eq){
  
  exposure_green <- 0
  exposure_brown <- 0
  x=1
  for (x in 1: length(technologies)){
    
    technology_ <- technologies[x]
    exposure_ <- portfolio_exposure_technology(investor_name_ = investor_name_,
                                                                                         portfolio_name_ = portfolio_name_,  
                                                                                         technology_ = technology_,
                                                                                         scenario_ = "SDS",
                                                                                         scenario_geography_ = "Global",
                                                                                         data_set__ = data_set_)
    if (is.na(exposure_)){exposure_ <- 0}
    crit_ <- is_green(technology_ = technology_,technology_green__=technology_green_)
    
    if(crit_ == TRUE){exposure_green <- exposure_green + exposure_}
    if(crit_ == FALSE){exposure_brown <-exposure_brown + exposure_ }
    
  } 
  
  return(c(exposure_green,exposure_brown))
  
  
  
}

is_green <- function(technology_ = "Electric",
                      technology_green__ = technology_green){
  return_ <- "not_in_list"
  i=1
  for (i in 1:length(technology_green__)){
    if ( (technology_green__[[i]][[1]]) == technology_){return_ <- technology_green__[[i]][[2]]}
    }
    
  return(return_)  
    
  }
  
  



portfolio_alignment_weighted <- function(investor_name_ = investor_name_, 
                                         portfolio_name_ = portfolio_name_, 
                                         technology_rating_ = technology_rating,
                                         scenario_ = "SDS",
                                         scenario_geography_ = "Global",
                                         data_set_ = all_results_eq){
  total_weight <- 0
  total_sum_of_weighted_alignment <- 0
  i=3
  for (i in 1:length(technology_rating_)){
       technology_ <- technology_rating_[[i]][[1]]
       weight_ <- technology_rating_[[i]][[2]]
       
       alignment_ <- portfolio_alignment_technology(investor_name_ = investor_name_, 
                                      portfolio_name_ = portfolio_name_, 
                                      technology_ = technology_,
                                      scenario_ = scenario_,
                                      scenario_geography_ = scenario_geography_,
                                      data_set__ = data_set_)
       
       if(!is.na(alignment_)) {total_sum_of_weighted_alignment <- (total_sum_of_weighted_alignment +  alignment_ * weight_)
       total_weight <- (total_weight + weight_ )}
  }
  
  return(total_sum_of_weighted_alignment/total_weight)
       }



portfolio_exposure_technology <- function(investor_name_ = investor_name_, 
                                          portfolio_name_ = portfolio_name_, 
                                          technology_ = "OilCap",
                                          scenario_ = "SDS",
                                          scenario_geography_ = "Global",
                                          data_set__ = all_results_eq)
  {start_year_ <- min(unique(data_set__$year), na.rm = TRUE)
  
  
  data_set___ <- data_set__ %>% subset(investor_name == investor_name_ &
                                  portfolio_name == portfolio_name_ &  
                                  technology == technology_ &
                                  year == start_year_ &
                                  scenario == scenario_ &
                                  scenario_geography == scenario_geography_)                  
  
  return(data_set___$plan_carsten[1])} 

portfolio_alignment_technology <- function(investor_name_ = investor_name_, 
                                          portfolio_name_ = portfolio_name_, 
                                          technology_ = "OilCap",
                                          scenario_ = "SDS",
                                          scenario_geography_ = "Global",
                                          data_set__ = all_results_eq){
  
    end_year_ <- max(unique(data_set__$year), na.rm = TRUE)


    data_set__ <- data_set__ %>% subset(investor_name == investor_name_ &
                                  portfolio_name == portfolio_name_ &  
                                  technology == technology_ &
                                  year == end_year_ &
                                  scenario == scenario_ &
                                  scenario_geography == scenario_geography_)                  

  return(data_set__$trajectory_alignment[1]) } 




evaluate_company_level_results <- function(data_set = company_results,
                                             scenario = "SDS" ){
  
  
  company_level_results <- data.frame()
  
  company_results_ <- company_results %>% subset( scenario == "SDS" &
                                                    scenario_geography == "Global" &
                                                    year == "2019") %>% 
                                          mutate(inv_port = paste0(investor_name, portfolio_name))
  
  companies <- unique(company_results_$company_name)
  i=1
  for (i in 1: length(companies)){
    company_name_ <- companies[i]
  
    company_level_results[i, "company_name"] <- company_name_
    
    company_result <- company_results_%>% subset( company_name == company_name_ & investor_name != "Meta Investor") 
    meta_investor <- company_results_%>% subset( company_name == company_name_ & investor_name == "Meta Investor") 
    
    company_level_results[i, "number_of_portfolios"]  <- length(unique(company_result$inv_port))
    company_level_results[i, "number_of_investors"] <-  length(unique(company_result$investor_name))   
    
    company_level_results[i, "mean_exposure"] <- mean(company_result$port_weight)
    company_level_results[i, "median_exposure"] <- median(company_result$port_weight)
    
    company_level_results[i,"total_exposure"] <- mean(meta_investor$port_weight)
    
    company_level_results[i,"sector"] <- unique(company_result$ald_sector)[1]
    
  }
  return(company_level_results)
}

market_share_delta <- function(investor_name_ = investor_name_,
                    portfolio_name_ = portfolio_name_, 
                    market_ = market,
                    sector_ = sector_,
                    scenario_ = scenario,
                    scenario_geography_ = "Global",
                    allocation_ = "portfolio_weight",
                    data_set__ = data_set_){
  
  start_year <- min(unique(market_$year),na.rm=T)
  end_year <- max(unique(market_$year),na.rm=T)
  
  technologies_in_sector <- market_ %>% subset(ald_sector == sector_) %>% pull(technology) %>% unique()
  
  
  market_size_start_year <- market_ %>% subset(portfolio_name =="GlobalMarket" &
                                                 allocation == allocation_ &
                                                  ald_sector == sector_ & 
                                                 scenario == scenario_ &
                                                 scenario_geography == scenario_geography_ &
                                                 year == start_year) %>% pull(plan_tech_prod) %>% sum()
  
  market_size_end_year <- market_ %>% subset(portfolio_name =="GlobalMarket" &
                                                 allocation == allocation_ &
                                                 ald_sector == sector_ & 
                                                 scenario == scenario_ &
                                                 scenario_geography == scenario_geography_ &
                                                 year == end_year) %>% pull(plan_tech_prod) %>% sum()
  
  portfolio_size_start_year_ <- data_set__ %>% subset(allocation == allocation_ &
                                                    ald_sector == sector_ & 
                                                    scenario == scenario_ &
                                                    scenario_geography == scenario_geography_ &
                                                    year == start_year) %>% pull(plan_tech_prod) %>% sum(na.rm = TRUE)
  
  market_share_start_year  <- portfolio_size_start_year_/market_size_start_year
  
  portfolio_size_end_year_data <- data_set__ %>% subset(allocation == allocation_ &
                                                        ald_sector == sector_ & 
                                                        scenario == scenario_ &
                                                        scenario_geography == scenario_geography_ &
                                                        year == end_year) 
  for (i in 1:length(technologies_in_sector)){
    technology__ <- portfolio_size_end_year_data[i, "technology"]
    portfolio_size_end_year_data[i, "final_size"] <- ifelse(green_brown(technology__)=="green", 
                                                            portfolio_size_end_year_data[i, "plan_tech_prod"],
                                                            portfolio_size_end_year_data[i, "scen_tech_prod"])
    
  }
  portfolio_size_end_year <- sum(portfolio_size_end_year_data$final_size, na.rm = TRUE)
  
  market_share_end_year  <- portfolio_size_end_year/market_size_end_year
  
  return(market_share_end_year-market_share_start_year)
}

evaluate_portfolio_level_results <- function(data_set = all_results_eq,
                                             portfolio_overview_ = portfolio_overview,
                                             market_ = market,
                                             scenario = "SDS"){
  
  portfolio_level_results <- data.frame()
  sectors_ <- unique(all_results_eq$ald_sector)
  
  investor_list <- unique(data_set$investor_name) 
  i =1
  n_row = 1
  for (i in 1:length(investor_list)){
    investor_name_ <- investor_list[i]
    data_set_ <- data_set %>% subset(investor_name == investor_name_)
    portfolio_overview__ <- portfolio_overview_ %>% subset(investor_name == investor_name_ &
                                                           asset_type == asset_class)
    
    portfolio_list <- unique(data_set_$portfolio_name) 
    j =1
    
    
    for (j in 1: length(portfolio_list)){
      portfolio_name_ <- portfolio_list[j]
      
      portfolio_level_results[n_row, "investor_name"] <- investor_name_
      portfolio_level_results[n_row, "portofolio_name"]<- portfolio_name_
      portfolio_level_results[n_row, "portfolio_value_usd"] <- portfolio_overview__ %>% subset(portfolio_name == portfolio_name_) %>% pull(asset_value_usd) %>% mean() 
      portfolio_level_results[n_row, "alignment_neutral"]<- portfolio_alignment_weighted(investor_name_ = investor_name_, 
                                                                                        portfolio_name_ = portfolio_name_, 
                                                                                        technology_rating_ = technology_rating_equal,
                                                                                        scenario_ = scenario,
                                                                                        scenario_geography_ = "Global",
                                                                                        data_set_ = data_set_)
      
      portfolio_level_results[n_row, "alignment_only_brown"]<- portfolio_alignment_weighted(investor_name_ = investor_name_, 
                                                                                         portfolio_name_ = portfolio_name_, 
                                                                                         technology_rating_ = technology_rating_only_brown,
                                                                                         scenario_ = scenario,
                                                                                         scenario_geography_ = "Global",
                                                                                         data_set_ = data_set_)
      
      portfolio_level_results[n_row, "alignment_emission_based"]<- portfolio_alignment_weighted(investor_name_ = investor_name_, 
                                                                                         portfolio_name_ = portfolio_name_, 
                                                                                         technology_rating_ = technology_rating_emission_based,
                                                                                         scenario_ = scenario,
                                                                                         scenario_geography_ = "Global",
                                                                                         data_set_ = data_set_)
      
      
      
      
      
      
      x=1
      for (x in 1: length(technologies)){
        
        technology_ <- technologies[x]
        columnn_name <- paste0("alignment_",tolower(technology_))
        portfolio_level_results[n_row, columnn_name]       <- portfolio_alignment_technology(investor_name_ = investor_name_,
                                                                                            portfolio_name_ = portfolio_name_,  
                                                                                            technology_ = technology_,
                                                                                            scenario_ = scenario,
                                                                                            scenario_geography_ = "Global",
                                                                                            data_set__ = data_set_)
      }
      
      
      Exposure_Green_and_Brown <- portfolio_exposure_green_and_brown(investor_name_ = investor_name_, 
                                                                                 portfolio_name_ = portfolio_name_, 
                                                                                 technology_green_ = technology_green,
                                                                                 scenario_ = scenario,
                                                                                 scenario_geography_ = "Global",
                                                                                 data_set_ = data_set_)
      
      portfolio_level_results[n_row, "exposure_green"]<- Exposure_Green_and_Brown[1]
      
      portfolio_level_results[n_row, "exposure_brown"]<- Exposure_Green_and_Brown[2]
      
      
      
      portfolio_level_results[n_row, "ratio_green_greenbrown"] <- ifelse ((portfolio_level_results[n_row, "exposure_brown"]+portfolio_level_results[n_row, "exposure_green"])>0,
                                                                          (portfolio_level_results[n_row, "exposure_green"]/(portfolio_level_results[n_row, "exposure_brown"]+portfolio_level_results[n_row, "exposure_green"])),
                                                                          0)
      
      x=1
      for (x in 1: length(technologies)){
            
            technology_ <- technologies[x]
            columnn_name <- paste0("exposure_",tolower(technology_))
            portfolio_level_results[n_row, columnn_name]       <- portfolio_exposure_technology(investor_name_ = investor_name_,
                                                                                                   portfolio_name_ = portfolio_name_,  
                                                                                                   technology_ = technology_,
                                                                                                   scenario_ = scenario,
                                                                                                   scenario_geography_ = "Global",
                                                                                                   data_set__ = data_set_)
      }
      x=3
      
      
      for (x in 1: length(sectors_)){
        
        sector_ <- sectors_[x]
        if (is.na(sector_) == FALSE)
          
        {columnn_name <- paste0("market_share_delta_",tolower(sector_))
        portfolio_level_results[n_row, columnn_name]       <- market_share_delta(investor_name_ = investor_name_,
                                                                                            portfolio_name_ = portfolio_name_, 
                                                                                            market_ = market_,
                                                                                            sector_ = sector_,
                                                                                            scenario_ = scenario,
                                                                                            scenario_geography_ = "Global",
                                                                                            data_set__ = data_set_)
      
        }}
      
      n_row = n_row +1
    }
    
    
  }
  return(portfolio_level_results)
  
}
 



#####
#RUN & EXPORT

#all_results_eq <- all_results_eq %>% subset(portfolio_name == "")


portfolio_level_results <- evaluate_portfolio_level_results(data_set = all_results_eq,
                                                            portfolio_overview_ = portfolio_overview,
                                                            scenario = "SDS")

write.csv(portfolio_level_results, paste0(outputs_path, "/Portfolio_Overview_Exposure_", asset_class,".csv"))



company_level_results <- evaluate_company_level_results(data_set = all_results_eq)
write.csv(company_level_results, paste0(outputs_path, "/Company_Overview_Exposure_", asset_class,".csv"))


bubble_chart_data <- bubble_chart(portfolio_level_results, x_axis = "exposure_green", y_axis = "alignment_emission_based")
write(bubble_chart_data, file= paste0(outputs_path, "/data/data_portfolio_bubble_", tolower(asset_class),".js"))


bubble_chart_data <- bubble_chart(portfolio_level_results, x_axis = "exposure_green", y_axis = "alignment_emission_based")
write(bubble_chart_data, file= paste0(outputs_path, "/data/data_portfolio_bubble_", tolower(asset_class),".js"))

bubble_chart_data_green_brown <- bubble_chart_green_brown(portfolio_level_results)
write(bubble_chart_data_green_brown, file= paste0(outputs_path, "/data/data_greenbrown_bubble_", tolower(asset_class),".js"))

