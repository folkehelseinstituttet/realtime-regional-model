#######################

#Function to get the age distribution of each municipality
#Using a vector (one element for each age group) of hospitalization probabilities as an input
#It returns a data frame with one value for each municipality

get_age_distributions <- function(hospital_prob){
  
  
  age_groups<- c(0,10,20,30,40,50,60,70,80)
  
  stopifnot(length(hospital_prob) == length(age_groups))
  
  #Load population
  population<-fhidata::norway_population_b2020
  #Filter the population
  population<-population[(year==2020)&(level=="county")]
  #Create age_groups
  municipalities<-unique(population[,location_code])
  population[,age_group:=findInterval(population[,age],age_groups)] # New variable with age group (1 to 9): V1.

  # Aggregate (sum) population in age groups.
  population<-population[,sum(pop), by = .(location_code,age_group)]
  
  #Distributions storage
  #distributions<-c()
  distributions <- matrix(ncol = length(municipalities), nrow=length(hospital_prob))
  colnames(distributions) <- municipalities
  
  #Note, already checked that there are 106 different ages in all the municipalities (not needed with age groups)
  for (mun in municipalities){
    #distributions<- cbind( distributions, hospital_prob*(population[location_code==mun,V1]/sum(population[location_code==mun,V1])))
    distributions[, mun] <- hospital_prob*(population[location_code==mun,V1]/sum(population[location_code==mun,V1]))
  }
  
  return (colSums(distributions))
}


# NEW: This function computes hospitalization probabilities for each municipality
# in each period between the changepoint. The function read in data on positive 
# tests, which must be aggregated by the time periods.
#
# Return a matrix with 365 rows (one for each municipality) and one column
# per time period. The matrix has row.names with municipality codes and column names
# for each time period (sorted in increasing time).
get_hosp_prob_municipality <- function(path_testdata,extend_to=NULL){
  
  # TODO: Extend to: for making predictions. Find distributions for the future.
  # For hte first 3 weeks after data, use the last probability for the municipailty.
  # After that, all the way to extend_to, use the base probabilites.
  
  # Base hospitalization probability.
  hosp_prob_base <- c(0.001, 0.001, 0.005, 0.011, 0.014, 0.029, 0.058 , 0.0930 , 0.223)
  
  # Definition of age groups.
  age_groups<- c(0,10,20,30,40,50,60,70,80)
  
  stopifnot(length(hosp_prob_base) == length(age_groups))
  
  
  #path_testdata <- 'andelerPerDagFylke28PreDagersVinduWeightNational50.csv'
  
  
  # #Load population, aggregate by age groups, for all municipalities.
  fhidata::norway_population_b2020 %>% 
    dplyr::filter(year == 2020,
                  level == 'county') %>%
    dplyr::rename(county_code = location_code) %>%
    dplyr::mutate(agegrp = findInterval(age, age_groups)) %>% 
    dplyr::group_by(agegrp, county_code) %>% 
    dplyr::summarise(pop_county = sum(pop), .groups='drop') %>% 
    dplyr::group_by(county_code) %>% 
    dplyr::mutate(popdistr_county = pop_county / sum(pop_county)) %>% 
    dplyr::ungroup() ->population
  
  # Load data on positive tests per county per month (age groups in columns).
  smoothed_age_distr_raw <- read.table(path_testdata, 
                                       sep=';', dec=',', header=TRUE, stringsAsFactors = FALSE)
  
  smoothed_age_distr_raw$SympStartDag <- as.Date(smoothed_age_distr_raw$SympStartDag)
  last_data_date <- max(smoothed_age_distr_raw$SympStartDag)
  
    # extend_to can not be before the last date of the data.
  if (!is.null(extend_to)){
    if (extend_to <=  last_data_date){
      extend_to <- last_data_date
    }
  }

  
  # Convert to long format, some renaming etc.
  smoothed_age_distr_raw %>% 
    dplyr::select(-X, -N) %>% 
    # make 100% sure the data is sorted correctly.
    dplyr::arrange(SympStartDag) %>% 
    tidyr::pivot_longer(cols = matches('^X[0-9]'), 
                 names_to = 'agegrp',
                 values_to = 'AgeDistrInfectedSmoothed') %>% 
    dplyr::group_by(Fylke, SympStartDag) %>% 
    dplyr::mutate(agegrp = order(agegrp)) %>% 
    dplyr::ungroup() -> smoothed_age_distr
  

  if (!is.null(extend_to)){
    if (extend_to > last_data_date){
      
      new_rows <- expand.grid(Fylke = unique(smoothed_age_distr$Fylke),
                              agegrp = unique(smoothed_age_distr$agegrp),
                              SympStartDag = seq(max(smoothed_age_distr_raw$SympStartDag)+1, extend_to, by=1),
                              stringsAsFactors = FALSE)
      
      new_rows %>% 
        mutate(first3weeks = (SympStartDag - last_data_date) <= 21) -> new_rows
      
      smoothed_age_distr %>% 
        dplyr::filter(SympStartDag == max(SympStartDag)) %>% 
        dplyr::select(-SympStartDag) -> distrs_final_day
      
      
      new_rows %>% 
        dplyr::left_join(distrs_final_day, 
                         by=c('Fylke', 'agegrp')) %>% 
        mutate(AgeDistrInfectedSmoothed = ifelse(first3weeks, AgeDistrInfectedSmoothed, NA)) %>% 
        dplyr::select(-first3weeks) -> new_rows
      
      smoothed_age_distr %>% 
        bind_rows(new_rows) -> smoothed_age_distr
      
      
    }
  }

  
  smoothed_age_distr %>%
    dplyr::left_join(population,
                     by=c('Fylke' = 'county_code', 'agegrp')) %>% 
    # Compute the weights based on the age distriubtion of the population
    # in each municipality.
    dplyr::mutate(WW = ifelse(!is.na(AgeDistrInfectedSmoothed),
                              AgeDistrInfectedSmoothed / popdistr_county, 
                              1), # 
                  hosp_prob_base = hosp_prob_base[agegrp],
                  hosp_prob = hosp_prob_base*WW) %>% 
    dplyr::group_by(SympStartDag, Fylke) %>% 
    # Compute the adjusted hospitalization probabilities adjusted
    dplyr::mutate(pp = hosp_prob * (pop_county / sum(pop_county))) %>% 
    dplyr::summarise(pp = sum(pp), .groups='drop') %>%  
    tidyr::pivot_wider(id_cols = 'Fylke',
                       names_from = 'SympStartDag',
                       values_from = 'pp') -> hosp_probs_scaled_res_tmp
  

  # Convert into a matrix with municip codes as row.names.  
  hosp_probs_scaled_res <- as.matrix(hosp_probs_scaled_res_tmp[, -1])
  row.names(hosp_probs_scaled_res) <- hosp_probs_scaled_res_tmp$Fylke
  
  # Make sure the columns are sorted increasingly by date.
  stopifnot(all(colnames(hosp_probs_scaled_res) == sort(colnames(hosp_probs_scaled_res))))

  #B1.1.7 FACTORS
  
  get_factors<-function(t){
    return((1000*exp(-0.073*t)+ 1.6*10*exp(0.073*t))/(1000*exp(-0.073*t) + 10*exp(0.073*t)))
  }
  
  factors<-get_factors(as.numeric(as.Date(colnames(hosp_probs_scaled_res))-as.Date("2021-01-01")))
  for (i in 1:nrow(hosp_probs_scaled_res)){
    hosp_probs_scaled_res[i,]=factors*hosp_probs_scaled_res[i,]
  }
  
  
  
  return(hosp_probs_scaled_res)
}


# get_icu_prob <- function(hospital_prob, icu_probs){
#   
#   
#   age_groups<- c(0,10,20,30,40,50,60,70,80)
#   
#   
#   stopifnot(length(hospital_prob) == length(icu_probs),
#             length(hospital_prob) == length(age_groups))
#   
#   #Load population
#   population<-fhidata::norway_population_b2020
#   #Filter the population
#   population<-population[(year==2020)&(level=="municip")]
#   #Create age_groups
#   municipalities<-unique(population[,location_code])
# 
#   population[,age_group:=findInterval(population[,age],age_groups)]
#   population<-population[,sum(pop), by = .(location_code,age_group)]
#   
#   #Distributions storage
#   distributions<-c()
#   
#   
#   #Note, already checked that there are 106 different ages in all the municipalities (not needed with age groups)
#   for (mun in municipalities){
#     d <- hospital_prob*(population[location_code==mun,V1])
#     
#     distributions<- cbind( distributions, d*icu_probs/sum(d))
#   }
#   colnames(distributions)<-municipalities
#   return (colSums(distributions))
# }





#######################
#Function to get hospitalization, icu and deaths for each municipality. This is run using a loop in the main function.
#Using same probability for hospitalization and death (depending on the age groups of each municipality)
extract_municipality <- function(
  modeloutput,
  municipality, #Municipality name
  gp_prob_scaled,
  length_of_illness,
  hosp_probs_scaled,
  # hosp_prob_changepoints,
  mort_probs_scaled, #Fatality rate
  icu_probs, #Probability of going to ICU while hospitalized
  hosp_parameters_changepoints, #Probability of going to ICU while hospitalized
  t_before_hosp_mu,
  t_before_hosp_size,
  days_of_stay_mu,
  days_of_stay_size,
  days_of_stay_icu_mu,
  days_of_stay_icu_size,
  icu_delay_mu,
  icu_delay_size,
  t_after_icu_mu,
  t_after_icu_size,
  p_delay_admission,
  p_testing_positive,
  onset_to_test,
  p_delay_testing,
  predict_from
){
  
  
  #Select municipality from data
  if ("c_imported_incidence"%in%colnames(modeloutput)){
    municip_hosp<-modeloutput[location_code==municipality,.(location_code,day,date,c_symp_incidence,c_asymp_incidence,c_imported_incidence)]
    municip_hosp[,c_incidence:=c_symp_incidence+c_asymp_incidence+c_imported_incidence]
  }else{
  municip_hosp<-modeloutput[location_code==municipality,.(location_code,day,date,c_symp_incidence,c_asymp_incidence)]
  municip_hosp[,c_incidence:=c_symp_incidence+c_asymp_incidence]
  }
#  municip_hosp<-municip_hosp[,c(1,2,5)]
  n_days<-max(modeloutput[,day])
  
  #Data frame to store the results
  hosp_icu_deaths<-data.frame(matrix(0,ncol=7,nrow=n_days))
  hosp_incidence<-rep(0,n_days)
  colnames(hosp_icu_deaths)<-c("hosp","icu","deaths","hosp_inc", "icu_inc", "gp_inc", "illness")
  
  prob_mun<-hosp_probs_scaled[municipality,]
  
  
  mort_prob<-mort_probs_scaled[municipality]
#  icu_prob<-icu_probs_scaled[municipality]
#  gp_prob_scaled <- gp_prob_scaled[municipality]
  
  #Sampling auxiliary functions (independent so they can be changed without touching the rest of the code)

  ## get_gp <- function(x) (
  ##   return(rbinom(1,size=x,prob=gp_prob_scaled))
  ## )

  ## get_length_illness <- function(x){
  ##    return(rpois(n = x,lambda = length_of_illness))
  ## }
  
  ###Split 
  #First of April
  changepoints<-c(hosp_parameters_changepoints)-min(as.Date(modeloutput$date))
  icu_probs_t<-changepoints[changepoints>0]
  icu_probs<-icu_probs[changepoints>0] #SAME CHANGEPOINT USED IN ALL THE FUNCTIONS
  #Function to split the icumask
  splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))
  
  
  
  
  process_incidence<-function(x,y){
    return(rbinom(1,size=x,prob=y))
  }
  
  ################
  get_t_before_hosp<-function(x,daynumber){
    if (x==0){
      return(0)
    }
    else {
      return(rnbinom(n=x, mu=t_before_hosp_mu[[findInterval(daynumber,changepoints)+1]], size=t_before_hosp_size[[findInterval(daynumber,changepoints)+1]]))
    }
  }
  
  ################
  get_time_after_icu<-function(icumask){
    z<-splitAt(icumask,changepoints)
    
    total<-c()
    for (i in 1:length(z)){
      mask<-z[[i]]
      mask[mask==1]<-rnbinom(n=sum(mask),mu = t_after_icu_mu[[i]],size=t_after_icu_size[[i]])
      total<-c(total,mask)
      
    }
  return(total)
  }

  ###############
  get_icu<-function(x, t){
    #Returns a vector of 1 and zeros with the length of the number of new cases that day
    if(length(icu_probs) == 1){
      icu_prob2 <- icu_probs[1]
    }else{
      icu_prob2 <- icu_probs[length(icu_probs)]
      for(i in 1:length(icu_probs_t)){
        if( t < icu_probs_t[i]){
          icu_prob2 <- icu_probs[i]
          break
        }
      }
    }
    return(as.integer(runif(x) < icu_prob2))
  }
  ###############
  
  get_icu_delay<-function(icumask){
    z<-splitAt(icumask,changepoints)
    total<-c()
    
    for (i in 1:length(z)){
      mask<-z[[i]]
      mask[mask==1] <-rnbinom(n=sum(mask),mu = icu_delay_mu[[i]],size=icu_delay_size[[i]])
      total<-c(total,mask)    
    }
    return (total)
  }
  ######################
  
  #Different sampling distribution for the days of stay depending on whether the patient is going to ICU or not
  get_length_of_stay<-function(icumask){
    z<-splitAt(icumask,changepoints)
    total<-c()
    for (i in 1:length(z)){
      mask<-z[[i]]
      mask_hosp <- mask==0
      mask_icu <- mask == 1
  
      n <- sum(mask_hosp)
      mask[mask_hosp]<- rnbinom(n=n, mu=days_of_stay_mu[[i]], size=days_of_stay_size[[i]])-1
    

      mask[mask_icu]<-rnbinom(n=sum(mask_icu),
                               mu=days_of_stay_icu_mu[[i]], size=days_of_stay_icu_size[[i]])-1
    
    total<-c(total,mask)
    }
    return(total)
  }
  ###############
  get_deaths<-function(x){
    #Returns a death mask according to the municipality probability of death
    return(as.integer(runif(n=x)<mort_prob))
  }
  get_time_until_death<-function(x){
    shape<-(1/0.45)^2
    scale<-(18.8/shape)
    return (rgamma(n=x,shape = shape,scale=scale))
  }
  


  #Vector of hospitalized cases
  #Probabilities to be applied each date...
  # NEW: prehosp: now uses the daily estimated probabilities, not the piecewise flat between changepoints.
  
  #prehosp<-mapply(process_incidence,municip_hosp$c_incidence,prob_mun[findInterval(municip_hosp$date,hosp_prob_changepoints)+1])

  #prob_mun_expanded <- rep(prob_mun[length(prob_mun)], times=length(municip_hosp$c_incidence)) # repeat last probability
  #prob_mun_expanded[1:length(prob_mun)] <- prob_mun # fill in the first part of the vector with probabilities.
  #prehosp <- mapply(process_incidence,municip_hosp$c_incidence,prob_mun_expanded)
  
  prob_mun_idx <- (!(names(prob_mun) < min(municip_hosp$date))) & (!(names(prob_mun) > max(municip_hosp$date))) # some prob_mun are from before the model starts. Remove them.
  prehosp <- mapply(process_incidence,municip_hosp$c_incidence,prob_mun[prob_mun_idx])
  
  
  #prehosp<-sapply(municip_hosp[,c_incidence],process_incidence)
  nonhosp<-municip_hosp[,c_incidence]-prehosp
  
  for (i in 1:n_days){
  ## # Primary health-care
  ##   hosp_icu_deaths$gp_inc[i] <- get_gp(municip_hosp[i,c_incidence])
  ##   #Illness
  ##   if(municip_hosp[i, c_symp_incidence] > 0){
  ##     for(ill in 1:municip_hosp[i, c_symp_incidence]){
  ##       end_date_illness <- i + get_length_illness(1)
  ##       if(end_date_illness > n_days){
  ##         end_date_illness = n_days
  ##       }
  ##       hosp_icu_deaths$illness[i:end_date_illness] <- hosp_icu_deaths$illness[i:end_date_illness] + 1
  ##     }
  ##   }
    p<-prehosp[i]
    np<-nonhosp[i]
    if (p>0){
      t_before<-get_t_before_hosp(p,i)
      icumask<-get_icu(p, i)
      lengths_of_stay<-get_length_of_stay(icumask)
      icu_delays<-get_icu_delay(icumask)
      after_icu<-get_time_after_icu(icumask)
      deaths<-get_deaths(p)
      for (j in 1:p){
        
        if ((i+t_before[j])<=n_days){
          hosp_icu_deaths$hosp_inc[i+t_before[j]]<-(hosp_icu_deaths$hosp_inc[i+t_before[j]]+1)
          if (icumask[j]==1){
            if((i+t_before[j] + icu_delays[j]) <= n_days){
              hosp_icu_deaths$icu_inc[i+t_before[j] + icu_delays[j]]<-(hosp_icu_deaths$icu_inc[i+t_before[j]+ icu_delays[j]]+1)
            }

            hosp_icu_deaths$hosp[(i+t_before[j]):min(i+t_before[j]+icu_delays[j],n_days)]<-(hosp_icu_deaths$hosp[(i+t_before[j]):min(i+t_before[j]+icu_delays[j],n_days)]+1)
            if (i+t_before[j]+icu_delays[j]<=n_days){
              hosp_icu_deaths$icu[(i+t_before[j]+icu_delays[j]):min(i+t_before[j]+lengths_of_stay[j]+icu_delays[j],n_days)]<-(hosp_icu_deaths$icu[(i+t_before[j]+icu_delays[j]):min(i+t_before[j]+icu_delays[j]+lengths_of_stay[j],n_days)]+1)  
            }
          
            if (i+t_before[j]+icu_delays[j]+lengths_of_stay[j]<=n_days){
              
              hosp_icu_deaths$hosp[(i+t_before[j]+icu_delays[j]+lengths_of_stay[j]):min(i+t_before[j]+icu_delays[j]+lengths_of_stay[j]+after_icu[j],n_days)]<-(hosp_icu_deaths$hosp[(i+t_before[j]+icu_delays[j]+lengths_of_stay[j]):min(i+t_before[j]+icu_delays[j]+lengths_of_stay[j]+after_icu[j],n_days)]+1)
              
            }
            
          }
          else{
            hosp_icu_deaths$hosp[(i+t_before[j]):min(i+t_before[j]+lengths_of_stay[j],n_days)]<-(hosp_icu_deaths$hosp[(i+t_before[j]):min(i+t_before[j]+lengths_of_stay[j],n_days)]+1)  
          }
        }
        if ((i+t_before[j]+lengths_of_stay[j])<=n_days){  
          hosp_icu_deaths$deaths[i+t_before[j]+lengths_of_stay[j]]<-(hosp_icu_deaths$deaths[i+t_before[j]+lengths_of_stay[j]]+deaths[j])
        }
      }
    }
    if (np>0){
      deaths<-get_deaths(np)
      periods_before_death<-get_time_until_death(np)
      for (j in 1:np){
        if (i+periods_before_death[j]<=n_days){
          hosp_icu_deaths$deaths[i+periods_before_death[j]]<-(hosp_icu_deaths$deaths[i+periods_before_death[j]]+deaths[j])
        }
      }
    }
  }
  setDT(hosp_icu_deaths)
  hosp_icu_deaths<-hosp_icu_deaths[1:n_days,]
  hosp_icu_deaths[,location_code:=rep(municipality,n_days)]
  hosp_icu_deaths[,day:=seq(1:n_days)]

  ## Add delay
  if(n_days > length(p_delay_admission)){
    p_delay_admission <- c(p_delay_admission, rep(1, n_days-length(p_delay_admission)))
  }

  if(!is.null(predict_from)){
    dates <- municip_hosp$date
    date_index <- match( predict_from, dates)
    p_delay_admission <- c(rep(1, n_days-date_index), p_delay_admission[1:date_index])
    
  }

  hosp_icu_deaths[, hosp_inc_with_delay:= rbinom(n_days, hosp_inc, rev(p_delay_admission))]

  
  # Add positive tests

  if(n_days > length(p_delay_testing)){
    p_delay_testing <- c(p_delay_testing, rep(1, n_days-length(p_delay_testing)))
  }
  if(!is.null(predict_from)){
    dates <- municip_hosp$date
    date_index <- match(predict_from, dates)
    p_delay_testing <- c(rep(1, n_days-date_index), p_delay_testing[1:date_index]) 
  }    

  hosp_icu_deaths[, shifted_incidence:=0]
  hosp_icu_deaths[(onset_to_test +1):nrow(hosp_icu_deaths), shifted_incidence:=municip_hosp$c_incidence[1:(nrow(hosp_icu_deaths) - onset_to_test)]]
  hosp_icu_deaths[, tested:= rbinom(nrow(hosp_icu_deaths), shifted_incidence, p_testing_positive)]
  hosp_icu_deaths[, shifted_incidence:=NULL]
  hosp_icu_deaths[, tested_with_delay:= rbinom(n_days, tested, rev(p_delay_testing))]
  return(hosp_icu_deaths)
}


# Function to deterministically nowcast based on delay probabilities
# DO NOT CHANGE p_delay without chaning it in process_simulation_list function
# Current estimated reporting delay for positive tests: c(0.067, 0.59, 0.90, 0.97)
# For negative tests: c(0.16, 0.74, 0.92, 0.98)
nowcast_cases = function(data, p_delay = c(0.067, 0.59, 0.90, 0.97)){
    n_days = length(data)
    p_delay_all = c(p_delay, rep(1, n_days - length(p_delay)))
    data_nowcasted = data/rev(p_delay_all)
    return(data_nowcasted)
}

##################################
#MAIN FUNCTION to process just one simulation
##################################
#@Adizlois
#Different distributions for the days of stay are used for ward hospitalization or ICU.
#Note that only those people hospitalized die. The mort_prob parameter reflects the overall probability of dying after hospitalization



run_hosp<-function(model_output,
                   #Probability of hospitalization. Vector that will be scaled using age distributions in each municipality
                   #hosp_probs=c(0.00019,0.00046,0.0019,0.0038,0.0082,0.027,0.094,0.20,0.36)/0.15, 
                   gp_prob_scaled,
                   length_of_illness,
                   hosp_probs_scaled=hosp_probs_scaled,
                   #hosp_prob_changepoints=hosp_prob_changepoints,
                   mort_probs_scaled=mort_probs_scaled, #General probability of dying after hospitalized (accounting for deaths outside hospital
                   icu_probs=c(), #Probability of going to ICU while hospitalized
                   hosp_parameters_changepoints=c(), #Probability of going to ICU while hospitalized
                   t_before_hosp_mu=9.08,
                   t_before_hosp_size=3.96,
                   days_of_stay_mu=6.13,
                   days_of_stay_size=2.0345,
                   days_of_stay_icu_mu=16.75,
                   days_of_stay_icu_size=2.94,
                   icu_delay_mu=0.9,
                   icu_delay_size=1.1,
                   t_after_icu_mu=7.75,
		   t_after_icu_size=0.7,
		   p_delay_admission=c(0.5, 0.7, 0.9),
                   p_testing_positive=0.05,
                   onset_to_testing=3,
                   p_delay_testing=c(0.1, 0.66, 0.90, 0.97),
                   predict_from=NULL
){
  
  require(data.table)
  
  #Reading the number of days of the simulation
  n_days<-max(model_output[,day])
  
  final_matrix<-c()
  
  for (municipality in  unique(model_output[,location_code])){
    #    cat("\r Expanding municipality  ",municipality)
    final_matrix<-rbind(final_matrix,extract_municipality(model_output,municipality, gp_prob_scaled, length_of_illness,hosp_probs_scaled,
                                                          # hosp_prob_changepoints,
                                                          mort_probs_scaled,
                                                          icu_probs,
                                                          hosp_parameters_changepoints,
                                                          t_before_hosp_mu,
                                                          t_before_hosp_size,
                                                          days_of_stay_mu,
                                                          days_of_stay_size,
                                                          days_of_stay_icu_mu,
                                                          days_of_stay_icu_size,
                                                          icu_delay_mu,
                                                          icu_delay_size,
                                                          t_after_icu_mu,
							  t_after_icu_size,
                                                          p_delay_admission,
                                                          p_testing_positive,
                                                          onset_to_testing,
                                                          p_delay_testing,
                                                          predict_from
                                                          ))
    
  }
  # cat(" ........Done.","\n")
  

  return(merge(model_output,final_matrix,by=c("location_code","day")))
}

#####################################


#Simulations wrapper -> Takes two lists of simulations of the same size, the seeding list, and the forecasting seed.
#It returns a list of dataframes with deaths, icus and hospitalizations for every single simulation.
#So length seeding list = length forecast = length output
process_simulation_list<-function(
  results,
  # NEW: hosp_probs input not needed. The probabilities and weights are coded into
  # the get_hosp_prob_municipality() function.
    # hosp_probs=list(c(0.17*0.001, 0.51*0.001, 1.27*0.005, 1.26*0.011, 1.30*0.014, 1.37*0.029,0.92*0.058 ,0.81*0.0930 , 1.46*0.223),
    #                 c(0.48*0.001, 0.93*0.001, 1.35*0.005, 1.77*0.011, 1.11*0.014, 1.10*0.029,0.51*0.058 ,0.52*0.0930 , 0.54*0.223),
    #                 c(0.86*0.001, 0.96*0.001, 1.75*0.005, 1.41*0.011, 1.05*0.014, 0.87*0.029,0.53*0.058 ,0.24*0.0930 , 0.65*0.223),
    #                 c(0.50*0.001, 0.94*0.001, 1.97*0.005, 1.27*0.011, 0.96*0.014, 1.04*0.029,0.60*0.058 ,0.44*0.0930 , 0.18*0.223),
    #                 c(0.42*0.001, 1.13*0.001, 3.06*0.005, 1.04*0.011, 0.82*0.014, 0.77*0.029,0.26*0.058 ,0.27*0.0930 , 0.17*0.223),
    #                 c(0.41*0.001, 1.13*0.001, 2.30*0.005, 1.30*0.011, 0.92*0.014, 0.82*0.029,0.51*0.058 ,0.36*0.0930 , 0.43*0.223),
    #                 c(0.38*0.001, 1.05*0.001, 2.11*0.005, 1.31*0.011, 1.09*0.014, 0.92*0.029,0.53*0.058 ,0.36*0.0930 , 0.34*0.223),
    #                 c(0.58*0.001, 1.51*0.001, 1.42*0.005, 1.21*0.011, 1.12*0.014, 0.94*0.029,0.62*0.058 ,0.42*0.0930 , 0.55*0.223),
    #                 c(0.64*0.001, 1.20*0.001, 1.41*0.005, 1.22*0.011, 1.14*0.014, 1.03*0.029,0.65*0.058 ,0.53*0.0930 , 0.64*0.223),
    #               c(0.001, 0.001, 0.005, 0.011, 0.014, 0.029,0.058 ,0.0930 , 0.223)),
  #hosp_prob_changepoints=c(as.Date("2020-05-01"),as.Date("2020-06-01"),as.Date("2020-07-01"),as.Date("2020-08-01"),
  #                          as.Date("2020-09-01"),as.Date("2020-10-01"),as.Date("2020-11-01"),as.Date("2020-12-01"),as.Date("2025-05-01")),
  mort_probs=c(0.00001, 0.00001, 0.00007, 0.0002, 0.0006, 0.002,0.009, 0.024 ,0.1),
  gp_probs=10/3*c(0.00,0.0004,0.0110,0.034,0.043,0.082,0.118,0.1620,0.160),
#  icu_probs=0.5*c(0.05,0.05,0.05,0.05,0.063,0.122,0.274,0.432,0.709),
  icu_probs=c(0.16, 0.076),
  hosp_parameters_changepoints=c(as.Date("2020-08-01")),
  length_of_illness=7,
  t_before_hosp_mu=list(9.66,7.77),
  t_before_hosp_size=list(2.90,3.83),
  days_of_stay_mu=list(6.07,5.70),
  days_of_stay_size=list(1.96,1.31),
  days_of_stay_icu_mu=list(15.63,10.64),
  days_of_stay_icu_size=list(1.93,1.76),
  icu_delay_mu=list(3.03,2.96),
  icu_delay_size=list(1.456,1.552),
  t_after_icu_mu=list(14.613,12.239),
  t_after_icu_size=list(1.99,1.672),
  p_delay_admission_general=c(0.53, 0.77, 0.82, 0.91),
  p_delay_admission_monday=c(0.32, 0.49, 0.68, 0.86),
  p_testing_positive=0.05,
  onset_to_test=3,
  p_delay_testing=c(0.067, 0.59, 0.90, 0.97),# DO NOT CHANGE p_delay_testing without chaning it in nowcast_cases function
  parallel=TRUE,
  path_testdata='andelerPerDagFylke28PreDagersVinduWeightNational50.csv',
  predict_from=NULL
){
  require(matrixStats)  
  hospitalization<-list()
  require(data.table)
  
  #SET THE LAST HOSP PROB CHANGEPOINT TO BE THREE WEEKS AHEAD, SO WE DO NOT CORRECT WITH THE AGE PROFILE IN THE NUMBER OF 
  #TESTS IN THE LONG PREDICTIONS-> THEREFORE THE LAST HOSP_PROB SHOULD BE THE ONES IN THE PAPER
  # NEW: I commented out this.
  # if (!is.null(predict_from)){
  #   hosp_prob_changepoints[length(hosp_prob_changepoints)]<-predict_from+22
  # }
  # 
  #Vector of hospitalization and death probabilities
  #taken from: https://www.medrxiv.org/content/10.1101/2020.03.04.20031104v1.full.pdf
  # Also https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(20)30183-5/fulltext
  
  #################################################################
  # NEW: Do not use the get_age_distributions(), bu use instead
  # get_hosp_prob_municipality().
  
  # hosp_probs_scaled<-sapply(hosp_probs,get_age_distributions)
  
  # Compute hosp_probs_scaled differently.
  # Autmoatically extend the probability vectors to the length of the results.
   hosp_probs_scaled <- get_hosp_prob_municipality(path_testdata = path_testdata,extend_to = max(results[[1]]$date))

  #########################################################
  
  gp_prob_scaled<-get_age_distributions(gp_probs)
  mort_probs_scaled<-get_age_distributions(mort_probs)

  if(is.null(predict_from)){
    if(weekdays(max(results[[1]][, date]))=="Sunday"){
      p_delay_admission <- p_delay_admission_monday
    }else{
      p_delay_admission <- p_delay_admission_general
    }
    
  }else{
    if(weekdays(predict_from)=="Sunday"){
      p_delay_admission <- p_delay_admission_monday
    }else{
      p_delay_admission <- p_delay_admission_general
    }
  }
#  icu_probs_scaled <- get_icu_prob(hosp_probs, icu_probs)
  #ASSUMING SAME LENGTH FOR BOTH --> IN THE WEEKS TO COME, THIS PROCESS SHOULD BE MAKE USING THE REAL DATA OF INFECTIONS UP UNTIL THE PRESENT
  
  run_inner <- function(i){
    #    cat("\n Simulation number ", i)
    h<-run_hosp(results[[i]],
                                        #hosp_probs=c(0.00019,0.00046,0.0019,0.0038,0.0082,0.027,0.094,0.20,0.36)/0.15, #Probability of hospitalization. Vector that will be scaled using age distributions in each municipality
                gp_prob_scaled=gp_prob_scaled,
                length_of_illness=length_of_illness,
                hosp_probs_scaled=hosp_probs_scaled,
                mort_probs_scaled=mort_probs_scaled,
                #hosp_prob_changepoints=hosp_prob_changepoints,
                icu_probs=icu_probs, #Probability of going to ICU while hospitalized
                hosp_parameters_changepoints=hosp_parameters_changepoints, #Probability of going to ICU while hospitalized
                t_before_hosp_mu=t_before_hosp_mu,
                t_before_hosp_size=t_before_hosp_size,
                days_of_stay_mu=days_of_stay_mu,
                days_of_stay_size=days_of_stay_size,
                days_of_stay_icu_mu=days_of_stay_icu_mu,
                days_of_stay_icu_size=days_of_stay_icu_size,
                icu_delay_mu=icu_delay_mu,
                icu_delay_size=icu_delay_size,
                t_after_icu_mu=t_after_icu_mu,
		t_after_icu_size=t_after_icu_size,
		p_delay_admission=p_delay_admission,
                p_testing_positive=p_testing_positive,
                onset_to_test=onset_to_test,
                p_delay_testing=p_delay_testing,
                predict_from=predict_from

    )
    return(h)
  }
  
  if(parallel){
    hospitalization <- parallel::mclapply(1:length(results), run_inner, mc.cores=parallel::detectCores())
  }else{
    hospitalization <- lapply(1:length(results), run_inner)
  }
  
  return(hospitalization)  
}  
