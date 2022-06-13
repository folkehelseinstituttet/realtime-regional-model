

to_sykehus <- function(results, hosp_data=NULL){
  if(is.null(hosp_data)){
    #hosp_data <- readxl::read_excel(fs::path(org::project$data_raw, "ref_sykehus.xlsx")) %>% janitor::clean_names()
    hosp_data <- readxl::read_excel("/cluster/projects/nn9755k/solveig/ref_sykehus.xlsx") %>% janitor::clean_names()
  }
  setDT(hosp_data)
  hosp_data[, location_code:=paste0("municip", stringr::str_pad(kommune_nr_ny, 4, pad="0"))]
  hosp_data[, hf:=omrade_2020]
  d <- hosp_data[results, on=c("location_code"="location_code")]
  

  d[, .(hosp=sum(hosp*andel), icu=sum(icu*andel)), by=.(hf, date, day)]
}


to_fylke_data <- function(results){
  # This function takes the simulation hosp. results and converts them to fylke level
  f_res <- list()
  municips<-fhidata::norway_locations_b2020[,c(1,3)]
  colnames(municips) <- c("location_code", "county_code")
  for (res in results){
    tmp = merge(res, municips, by = "location_code")
    f_res[[length(f_res) + 1]] = tmp[,list(hosp = sum(hosp), icu = sum(icu), hosp_inc = sum(hosp_inc), hosp_inc_with_delay = sum(hosp_inc_with_delay), icu_inc = sum(icu_inc), c_symp_incidence = sum(c_symp_incidence), c_asymp_incidence = sum(c_asymp_incidence), tested = sum(tested), tested_with_delay = sum(tested_with_delay)), by = .(day, county_code, date)]
  }
  return(f_res)
}


to_on_data <- function(results){
  # This function takes the simulation hosp. results, and converts them to Oslo vs rest of Norway, 
  # coded as county03 and rest, respectively. 
  on <- list()
  results <- to_fylke_data(results)
  for (res in results){
    subData <- res[county_code != "county03", ]
    subData <- subData[, list(hosp = sum(hosp), icu = sum(icu), hosp_inc = sum(hosp_inc), icu_inc = sum(icu_inc)), by = .(day, date)]
    subData$county <- "rest"
    Oslo <- res[county_code == "county03",]
    Oslo <- Oslo[,county_code:=NULL]
    Oslo$county <- "county03"
    on[[length(on) + 1]] <- rbind(Oslo, subData)
  }
  return(on)
}


from_hosp_to_on <- function(){
  # This function returns the hospital data on Oslo vs rest of Norway level, 
  # where Ahus has been handled as a special case, with the proportion of Ahus cases going to Oslo
  # being equal to the population of Oslo that is covered by Ahus, divided by the total population covered by Ahus
  
  hosp_data <- readxl::read_excel("/cluster/projects/nn9755k/solveig/ref_sykehus.xlsx") %>% janitor::clean_names()
  #hosp_data <- readxl::read_excel("/home/solveng/beredskap_nytt_coronavirus_2019-mcmc/modelling/pipeline/data/ref_sykehus.xlsx") %>% janitor::clean_names()
  case_data <- real_sykehus_data()
  municips<-fhidata::norway_locations_b2020[,c(1,3)]
  colnames(municips) <- c("kommune_nr_ny", "county_code")
  hosp_data$kommune_nr_ny = paste0("municip", stringr::str_pad(hosp_data$kommune_nr_ny, 4, pad="0"))
  hosp_data = merge(hosp_data, municips, by = "kommune_nr_ny")
  
  
  population <- fhidata::norway_population_b2020[level == "municip" & year == 2019]
  population <- population[, sum(pop), by = .(location_code)]
  
  
  nOslo <- hosp_data$andel[hosp_data$kommune_nr_ny == "municip0301" & hosp_data$omrade_2020 == "Akershus universitetssykehus"] * population$V1[population$location_code == "municip0301"]
  nRest <- sum(population$V1[which(population$location_code %in% hosp_data$kommune_nr_ny[hosp_data$omrade_2020 == "Akershus universitetssykehus"] & population$location_code != "municip0301")])
  
  nHosp <- sort(unique(case_data$hf))
  mat <- matrix(0, ncol = length(nHosp), nrow = 2)
  colnames(mat) = nHosp
  rownames(mat) = c("county03", "rest")
  for (hosp in nHosp){
    county <- hosp_data$county_code[hosp_data$omrade_2020 == hosp][1]
    if(hosp == "Akershus universitetssykehus"){
      mat[rownames(mat) == "county03", colnames(mat) == hosp] = nOslo/(nRest + nOslo)
      mat[rownames(mat) == "rest", colnames(mat) == hosp] = 1 - nOslo/(nRest + nOslo)
    }
    else if(county == "county03"){
      mat[rownames(mat) == county, colnames(mat) == hosp] = 1
    }
    else{
      mat[rownames(mat) == "rest", colnames(mat) == hosp] = 1
    }
  }
  
  dates <- as.Date(integer(), origin = "1970-01-01")
  cases <- NULL
  county <- NULL
  for (i in 1:length(unique(case_data$date))){
    dato = unique(case_data$date)[i]
    subHosp <- case_data[case_data$date == dato, ]
    subHosp <- subHosp[order(subHosp$hf), ]$N
    dates <- c(dates, rep(as.Date(dato), 2))
    cases <- c(cases, mat %*% subHosp)
    county <- c(county, c("county03", "rest"))
  }
  data <- data.table("date" = dates, "N" = cases, "county" = county)
  return(data)
}

from_hosp_to_county <- function(){
  # This function returns the hospital data on Oslo vs rest of Norway level, 
  # where Ahus has been handled as a special case, with the proportion of Ahus cases going to Oslo
  # being equal to the population of Oslo that is covered by Ahus, divided by the total population covered by Ahus
  
  hosp_data <- readxl::read_excel("/cluster/projects/nn9755k/solveig/ref_sykehus.xlsx") %>% janitor::clean_names()
  case_data <- real_sykehus_data()
  municips<-fhidata::norway_locations_b2020[,c(1,3)]
  colnames(municips) <- c("kommune_nr_ny", "county_code")
  hosp_data$kommune_nr_ny = paste0("municip", stringr::str_pad(hosp_data$kommune_nr_ny, 4, pad="0"))
  hosp_data = merge(hosp_data, municips, by = "kommune_nr_ny")
  
  
  population <- fhidata::norway_population_b2020[level == "municip" & year == 2019]
  population <- population[, sum(pop), by = .(location_code)]
  
  nHosp <- sort(unique(case_data$hf))
  mat <- matrix(0, ncol = length(nHosp), nrow = 11)
  colnames(mat) = nHosp
  rownames(mat) = unique(municips$county_code)
  for (hosp in nHosp){
    county <- unique(hosp_data$county_code[hosp_data$omrade_2020 == hosp])
    mun <- hosp_data$kommune_nr_ny[hosp_data$omrade_2020 == hosp]
    tot <- sum(population$V1[which(population$location_code %in% mun)] * hosp_data$andel[hosp_data$omrade_2020 == hosp])
    for (co in county){
      mun2 <- hosp_data$kommune_nr_ny[hosp_data$omrade_2020 == hosp & hosp_data$county_code == co]
      ncounty <- sum(population$V1[which(population$location_code %in% mun2)] * hosp_data$andel[hosp_data$omrade_2020 == hosp & hosp_data$county_code == co])
      mat[rownames(mat) == co, colnames(mat) == hosp] <- ncounty/tot
    }
  }  
  dates <- as.Date(integer(), origin = "1970-01-01")
  cases <- NULL
  county <- NULL
  for (i in 1:length(unique(case_data$date))){
    dato = unique(case_data$date)[i]
    subHosp <- case_data[case_data$date == dato, ]
  #  subHosp <- subHosp[order(subHosp$hf), ]$N
    subHosp2 = NULL
    for (j in 1:length(nHosp)){
      subHosp2 = rbind(subHosp2, subHosp[subHosp$hf == nHosp[j], ])
    }
    subHosp2 = subHosp2$N
    dates <- c(dates, rep(as.Date(dato), length(unique(municips$county_code))))
    cases <- c(cases, mat %*% subHosp2)
    county <- c(county, unique(municips$county_code))
  }
  data <- data.table("date" = dates, "N" = cases, "county_code" = county)
  return(data)
}


real_sykehus_data <- function(){
  real_data <- readxl::read_excel("/cluster/projects/nn9755k/solveig/sykehus_data.xlsx")
  #real_data <- readxl::read_excel("/home/solveng/beredskap_nytt_coronavirus_2019-mcmc/modelling/pipeline/data/sykehus_data.xlsx")
  setDT(real_data)
  #real_data$date = excel_numeric_to_date(as.numeric(real_data$date))
#  real_data$date[1] = as.Date("2020-03-08")
  real_data <- real_data[hf!="Sunnaas sykehus HF"]
  real_data <- real_data[hf!="Sunnaas Sykehus HF"]
  real_data <- real_data %>% mutate(hf=recode(
                                      hf,
                                      "Akershus universitetssykehus HF" = "Akershus universitetssykehus",
                                      "Haraldsplass Diakonale Sykehus" = "Helse Bergen HF",
                                      "Helse Bergen HF + Privat med avtale med Helse Vest i Bergen" = "Helse Bergen HF",
                                      "Private med avtale med Helse Sør-Øst i Oslo" ="Oslo universitetssykehus",
                                      "Helse Møre og Romsdal" = "Helse Møre og Romsdal HF",
                                      "Helse Nord-Trøndelag" ="Helse Nord Trøndelag HF",
                                      "Oslo universitetssykehus HF" ="Oslo universitetssykehus",
                                      "St. Olavs hospital" ="St. Olavs hospital HF",
                                      "Sykehuset i Vestfold HF" ="Sykehuset i Vestfold",
                                      "Sykehuset Innlandet HF" ="Sykehuset Innlandet",
                                      "Sykehuset Telemark HF" = "Sykehuset Telemark",
                                      "Sykehuset Østfold HF" = "Sykehuset Østfold"  ,
                                      "Sørlandet sykehus HF" ="Sørlandet sykehus",
                                      "Universitetssykehuset Nord-Norge HF"="Universitetssykehuset i Nord-Norge HF",
                                      "Finnmarkssykehuset HF" = "Finnmarksykehuset HF",
                                      "Vestre Viken HF" ="Vestre Viken")
                                    )
  setDT(real_data)
  real_data[, date:=as.Date(date)]
  real_data$N <- as.numeric(real_data$N)
  setDT(real_data)
  real =  real_data[, .(N=sum(N)), keyby=.(hf, date)]
  real[, date:=as.Date(date)]
  return(real)
}

county_incidence_data <- function(filename="fylke_data.csv"){
  # This function reads and prepares incidence data on county level
  hosp_data <- read.csv(filename, sep= ";", stringsAsFactors = FALSE)
  setDT(hosp_data)
  #hosp_data[, date:=as.Date(date, format = "%d/%m/%Y")]
  hosp_data[, date:=as.Date(date, format = "%d.%m.%Y")]
  hosp_data$N <- as.numeric(hosp_data$N)
  hosp_data$N_prev <- as.numeric(hosp_data$N_prev)
  return(hosp_data)
}

county_incidence_data2 <- function(filename="fylke_data.csv"){
  # This function reads and prepares incidence data on county level
  hosp_data <- read.csv(filename, sep= ";", stringsAsFactors = FALSE)
  setDT(hosp_data)
  #hosp_data[, date:=as.Date(date, format = "%d/%m/%Y")]
  hosp_data[, date:=as.Date(date)]
  hosp_data$N <- as.numeric(hosp_data$N)
  hosp_data$N_prev <- as.numeric(hosp_data$N_prev)
  return(hosp_data)
}


least_squares <- function(x,y, weights=NULL){
  if(is.null(weights)){
    weights <- rep(1, length(x))
  }
  S = sum( (log(x + 1) - log(y + 1))^2 * weights / sum(weights))
}

runDailyCalib1Reff = function(initial_calibration_results, hospitalisations_df, mobility_matrices, predict_from = Sys.Date(), initial_calibration_end = as.Date("2020-03-20"), start_date = as.Date("2020-02-17"), models_to_keep = 100, Reff_change_dates=c(as.Date("2020-03-16"))){
    secondary_calibration_start <- initial_calibration_end + lubridate::days(1)
    secondary_calibration_end <- c(max(hospitalisations_df$date))
    initial_calibration_result <- initial_calibration_result[ order(initial_calibration_result$Error) ,]
	
    all_params <- add_secondary_calibrations(initial_calibration_result,
                                             secondary_calibration_start,
                                             secondary_calibration_end,
                                             models_to_keep,
                                              start_date,
                                              hospitalisations_df,
                                              Reff_change_dates,
                                              mobility_matrices,
                                              N_sims=1
                                              )
                                    
    return(all_params)
}


runDailyCalib2Reff = function(hospitalisations_df, mobility_matrices, predict_from = Sys.Date(), initial_calibration_end = as.Date("2020-03-20"), start_date = as.Date("2020-02-16"), models_to_keep = 100){
    Reff_change_dates <- c(as.Date("2020-03-09"), as.Date("2020-03-16"))
    secondary_calibration_start <- c(as.Date("2020-03-21"), as.Date("2020-03-28"))
    secondary_calibration_end <- c(as.Date("2020-03-27"), max(hospitalisations_df$date)) 

    #initial_calibration_result <- readRDS("/data/solveng/results/inical/initial_parameters.RDS") # linear least squres
    #initial_calibration_result <- readRDS("/home/solveng/results/initial_parameters.RDS") # log least squares
    initial_calibration_result <- readRDS("/home/solveng/results/2020-04-02/initial_parameters.RDS") # log least squares, new parametrisation
    initial_calibration_result <- initial_calibration_result[ order(initial_calibration_result$Error) ,]
    
    
    all_params <- add_secondary_calibrations_sim(initial_calibration_result,
                                             secondary_calibration_start,
                                             secondary_calibration_end,
                                            models_to_keep,
                                             start_date,
                                             hospitalisations_df,
                                             Reff_change_dates,
                                             mobility_matrices, N_sims = 1)
    saveRDS(all_params, fs::path( org::project$results_today, "parameters.RDS") )
    return(all_params)
}  

add_secondary_calibrations_sim <- function(initial_results,
                                       calibration_start_dates,
                                       calibration_end_dates,
                                       models_to_keep,
                                       start_date,
                                       hospitalisations_df,
                                       reff_change_dates,
                                       mobility_matrices,
                                       Reffs = c(),
                                       N_sims=2){
    oneSim <- function(i){
        m <- initial_results[i, ]
	new_Reff <- calibrate_reff_sim(start_date = start_date,
		    		       end_date = calibration_end_dates,
				       calibrate_from = calibration_start_dates,
				       amp_factor = m$AMP_factor,
				       R0 = m$R0,
				       hospitalisations_df,
				       reff_change_dates,
				       mobility_matrices,
				       N_sims = N_sims)
      new_Reff <- new_Reff[order(new_Reff$Error), ]
      error <- new_Reff[1, ]$Error
      Reff1 <- new_Reff[1, ]$Reff1
      Reff2 <- new_Reff[1, ]$Reff2
      return(c(Reff1, Reff2, error)) 					
    }
   # fit <- parallel::mclapply(1:models_to_keep, oneSim, mc.cores= parallel::detectCores())
    fit <- parallel::mclapply(1:models_to_keep, oneSim, mc.cores= 50)
    fit <- matrix(unlist(fit), ncol = 3, byrow = T)
    fit <- cbind(initial_results[1:models_to_keep, ], fit)
    colnames(fit) <- c("R0", "Amp", "InError","Reff1","Reff2", "Error")
    fit <- data.frame(fit)
    return(fit)
}

calibrate_reff_sim = function(start_date,
                           end_date,
                           calibrate_from,
                           amp_factor,
                           R0,
                           hospitalisations_df,
                           reff_change_dates,
                           mobility_matrices,
                           N_sims=2){
  min_reff1 = 1.4
  max_reff1 = 3.0
  min_reff2 = 0.4
  max_reff2 = 1.6
  reff_range1 <- seq(min_reff1, max_reff1, 0.1)
  reff_range2 <- seq(min_reff2, max_reff2, 0.1)
  oneSim <- function(i){
    results <- list()
    print(i)
    Reff1 <- reff_range1[i]
    for (j in 1:length(reff_range2)){
    	Reff2  <- reff_range2[j]
	error <- c()
    	result <- run_model(mobility_matrices, 
                               start_date = start_date, 
               		       end_date = end_date[2],
                               predict_from = end_date[2],
                               N_sim = N_sims,
                               R_0 = R0,
                               reff_change_date=reff_change_dates,
                       	       Reffs=c(Reff1, Reff2),
                               parallel = F, 
                               AMP_factor = amp_factor)
            hosp_result <- process_simulation_list(result)
    	    #results[[length(results) +1]] <- list(hosp_result[[1]][date >=calibrate_from[1] & date <=end_date[2]])
	    results[[length(results) + 1]] <- lapply(hosp_result, function(x) x[date >=calibrate_from[1] & date <=end_date[2]])
    	    ls_weights <- rep(1, nrow(hospitalisations_df))
	    n_date <- which(hospitalisations_df$date == end_date[1])
	    ls_weights[(n_date - 3):nrow(hospitalisations_df)] <- 13
    	    ls_weights<- ls_weights[which(hospitalisations_df$date >= calibrate_from[1] & hospitalisations_df$date <= end_date[2])]
	    error <- c(error, calc_ls(results, hospitalisations_df[date >= calibrate_from[1] & date <= end_date[2], N], keys = c("icu", "hosp"), weights = ls_weights)) 
    }
    indWinner <- which(error == min(error))
    return(c(Reff1, reff_range2[indWinner], error[indWinner]))
 }
    # fit <- parallel::mclapply(1:length(reff_range1), oneSim, mc.cores= parallel::detectCores())
    fit <- parallel::mclapply(1:length(reff_range1), oneSim, mc.cores= 2)
    fit <- matrix(unlist(fit), ncol = 3, byrow = T)
    colnames(fit) <- c("Reff1", "Reff2", "Error")
    fit <- data.frame(fit)
    return(fit)    
}



#Fitting R0
calibrate_grid_search_R <- function(){
  results <- list()
  R0s <- seq(1.5, 1.7, by=0.1)
  for(R0 in R0s){
    c_results <- run_model(mobility_matrices,
                           start_date = start_date,
                           end_date =max(hospitalisations_df[, date]),
                           predict_from=max(hospitalisations_df[, date]),
                           N_sim=2,
                           R0_1=R0,
                           R0_2=R0,
                           parallel=T,
                           AMP_factor=1.5
    )
    c_results <- process_simulation_list(c_results)
    results[[length(results) +1]] <- c_results
  }
  
  ls_values <- calc_ls(results, hospital_data, keys=c("icu", "hosp"))
  R0_best <- R0s[which.min(ls_values)]
  return(R0_best)
}


# Calibrate R0 and amplification factor, ABC-like procedure
calibrate_R_amp = function(N_sims, start_date, end_date, hospitalisations_df, mobility_matrices, N_cores=1){
  min_R0 <- 1.5
  max_R0 <- 3.0
  
  min_AMP <- 1.0
  max_AMP <- 1.5
  current_hosp_data <- hospitalisations_df[date <= end_date & date >= as.Date("2020-03-10")]

  ls_weights <- c(rep(1, nrow(current_hosp_data) - 4), rep(10, 4))

  oneSim <- function(i){
    results <- list()
    R0_now <- runif(1, min_R0, max_R0)
    amp_now <- runif(1, min_AMP, max_AMP)
    result <- run_model(mobility_matrices, 
                        start_date = start_date, 
                        end_date = end_date,
                        predict_from = end_date,
                        N_sim = 10,
                        R_0 = R0_now, 
                        Reffs = c(),
                        parallel = F, 
                        AMP_factor = amp_now)
    hosp_result <- process_simulation_list(result)
    results[[length(results) +1]] <- lapply(hosp_result, function(x) x[date >= as.Date("2020-03-10")])
    return(c(R0_now, amp_now, calc_ls(results, current_hosp_data$N, keys=c("icu", "hosp"), weights=ls_weights)))
  }
  
  fit <- parallel::mclapply(1:N_sims, oneSim, mc.cores= N_cores)
  fit <- matrix(unlist(fit), ncol = 3, nrow = N_sims, byrow = T)
  colnames(fit) <- c("R0", "AMP_factor", "Error")
  fit <- as.data.frame(fit)
  fit[order(fit$Error), ]
  return(fit)
}


# Calibrate R0 and amplification factor, ABC-like procedure
## calibrate_R_amp_red = function(N_sims){
##   min_R0 <- 1.8
##   max_R0 <- 3.0
  
##   min_AMP <- 1.0
##   max_AMP <- 1.3
  
##   min_red <- 0
##   max_red <- 0.8
  
##   oneSim <- function(i){
##     results <- list()
##     R0_now <- runif(1, min_R0, max_R0)
##     amp_now <- runif(1, min_AMP, max_AMP)
##     red_now <- runif(1, min_red, max_red)
##     result <- run_model(mobility_matrices, 
##                         start_date = start_date, 
##                         end_date = max(hospitalisations_df[, date]),
##                         predict_from = max(hospitalisations_df[, date]),
##                         N_sim = 1,
##                         R0_1 = R0_now, 
##                         R0_2 = (1 - red_now) * R0_now,
##                         parallel = F, 
##                         AMP_factor = amp_now)
##     hosp_result <- process_simulation_list(result)
##     results[[length(results) +1]] <- hosp_result
##     return(c(R0_now, amp_now, red_now, calc_ls(results, hospital_data, keys=c("icu", "hosp"))))
##   }
  
##   fit <- parallel::mclapply(1:N_sims, oneSim, mc.cores= 1)
##   fit <- matrix(unlist(fit), ncol = 4, nrow = N_sims, byrow = T)
##   colnames(fit) <- c("R0", "AMP_factor", "Reduction_factor", "Error")
##   fit <- as.data.frame(fit)
##   fit[order(fit$Error), ]
##   return(fit)
## }

calibrate_reff <- function(start_date,
                           end_date,
                           calibrate_from,
                           amp_factor,
                           R0,
                           Reffs,
                           hospitalisations_df,
                           reff_change_dates,
                           mobility_matrices,
                           N_sims=1){
  min_reff = 0.4
  max_reff = 2.5
  reff_range <- seq(min_reff, max_reff, 0.05)
  oneSim <- function(i){
    print(i)
    results <- list()
    Reff <- reff_range[i]
    result <- run_model(mobility_matrices, 
                        start_date = start_date, 
                        end_date = end_date,
                        predict_from = end_date,
                        N_sim = N_sims,
                        R_0 = R0,
                        reff_change_date=reff_change_dates,
                        Reffs=c(Reffs, Reff),
                        parallel = F, 
                        AMP_factor = amp_factor)
    hosp_result <- process_simulation_list(result)
    results[[length(results) + 1]] <- lapply(hosp_result, function(x) x[date >=calibrate_from & date <=end_date])
    ls_weights <- rep(1, nrow(hospitalisations_df))
    ls_weights<- ls_weights[which(hospitalisations_df$date >= calibrate_from & hospitalisations_df$date <= end_date)]
    n <- length(ls_weights)
    n_back <- end_date - as.Date("2020-03-28")
    ls_weights[(n-n_back):n] <- 13
    return(c(Reff, calc_ls(results, hospitalisations_df[date >=calibrate_from & date <=end_date, N],
                           keys=c("icu", "hosp"), weights = ls_weights)))
  }
  
  fit <- parallel::mclapply(1:length(reff_range), oneSim, mc.cores= parallel::detectCores())
#  fit <- lapply(1:length(reff_range), oneSim)
  fit <- matrix(unlist(fit), ncol = 2, nrow = length(reff_range), byrow = T)
  colnames(fit) <- c("Reff", "Error")
  fit <- data.frame(fit)
  
  return(fit)
}
  
add_secondary_calibrations <- function(initial_results,
                                       calibration_start_dates,
                                       calibration_end_dates,
                                       models_to_keep,
                                       start_date,
                                       hospitalisations_df,
                                       reff_change_dates,
                                       mobility_matrices,
                                       Reffs = c(),
                                       N_sims=2){
  out <-list()
  for(i in 1:models_to_keep){
    m <- initial_results[i, ]
    new_reffs <- c()
    errors <- c()
    for(calib in 1:length(calibration_start_dates)){
      new_Reff <- calibrate_reff(
        start_date=start_date,
        end_date=calibration_end_dates[calib],
        calibrate_from=calibration_start_dates[calib],
        amp_factor=m$AMP_factor,
        R0=m$R0,
        Reffs=new_reffs,
        hospitalisations_df,
        reff_change_dates,
        mobility_matrices,
        N_sims=N_sims
      )
      new_Reff <- new_Reff[order(new_Reff$Error), ]
      error <- new_Reff[1, ]$Error
      new_Reff <- new_Reff[1, ]$Reff
      new_reffs <- c(new_reffs, new_Reff)
      errors <- c(errors, error)
    }
    out[[length(out) +1]] <- list(initial_params=m, "reffs"=new_reffs, "errors"
 = errors)
  }
  return(out)
}


extract_tot <- function(ds, key="hosp",fylke_filter=NULL){
  if (!is.null(fylke_filter)){
    municips<-fhidata::norway_locations_b2020[,c(1,4)]
    colnames(municips)<-c("location_code","county")
    hosps<-merge(ds,municips,by="location_code")
    hosps<-hosps[county==fylke_filter]
    ds<-hosps[,2:(ncol(hosps)-1)]
  }
  ds[, .(tot=sum(get(key))), keyby=.(day)][, tot]
}


calc_ls <- function(list_sims, data, keys=c("hosp"), weights=NULL){
  ls_values <- c()
  for(x in 1:length(list_sims)){
    mat <- c()
    for(sim in list_sims[[x]]){
      cum <- rep(0, length(extract_tot(sim, keys[1])))
      for(key in keys){
        cum <- cum + extract_tot(sim, key)
      }
      mat <- cbind(mat,cum)
    }
    means <- rowMeans(mat)
    l <- length(means)
    ls_values <- c(ls_values, least_squares(means, data, weights=weights))
  }
  return(ls_values)
}


create_scenario_tables <- function(results, today, location=NULL){
  new_results <- list()

  postfix = ""
  if(!is.null(location)){
    if(sum(grep("county", location))){
      locs <- fhidata::norway_locations_b2020[county_code == location, municip_code]
    }else{
      locs <- c(location)
    }

    for(res in results){
      new_sims <- list()
      for(sim in res$results){
        new_sims[[length(new_sims) + 1]] <-  sim[location_code %in% locs]
      }
      new_results[[length(new_results) +1]] <- list("R0"=res$R0,
                                                    results=new_sims)
    }
    results <- new_results
    postfix <- location
  }

  icu_data<- data.frame()
  hosp_data<- data.frame()
  prev_data<- data.frame()
  for(res in results){
    hosp_data <- rbind(hosp_data,
                       data.frame(extract_values(res[["results"]], keys=c("hosp", "icu")))
                       %>% mutate(R0=res[["R0"]]))
    icu_data <- rbind(icu_data,
                       data.frame(extract_values(res[["results"]], keys=c("icu")))
                       %>% mutate(R0=res[["R0"]]))
    prev_data <- rbind(prev_data,
                       data.frame(extract_values(res[["results"]], keys=c("b_I", "b_Ia")))
                       %>% mutate(R0=res[["R0"]]))
  }
  setDT(icu_data)
  setDT(prev_data)
  setDT(hosp_data)
  one_week = today + lubridate::days(7)
  two_week = today + lubridate::days(14)
  three_week = today + lubridate::days(21)
  

  format_row <- function(data){
    out <- c()
    for(row in 1:nrow(data)){
      out <- c(out, glue::glue("{round(data[row,mean_])} ({round(data[row, 1])} - {round(data[row,2])})"))
    }
    return(out)
  }

  out <- c()
  for(d in list(one_week, two_week, three_week)){
    out <- rbind(out, c(glue::glue("Pervalens {format(d, '%d/%m')}"), format_row(prev_data[date==d])))
  }
  for(d in list(one_week, two_week, three_week)){
    out <- rbind(out, c(glue::glue("Sykehus {format(d, '%d/%m')}"), format_row(hosp_data[date==d])))
  }
  for(d in list(one_week, two_week, three_week)){
    out <- rbind(out, c(glue::glue("Intensiv {format(d, '%d/%m')}"), format_row(icu_data[date==d])))
  }
  
  colnames(out) <- c("", "R0=2.4", "R0=0.9", "R0=1.3")

  stargazer::stargazer(out, type="html",
                       out=fs::path( org::project$results_today, glue::glue("scenarios{postfix}.xlsx")))

}

runPredictions1Reff = function(all_params, mobility_matrices, models_to_keep = 100, N_sim = 10, Reff_change_dates = c(as.Date("2020-03-14")), predict_from = Sys.Date(), start_date = as.Date("2020-02-17"), longrun = FALSE){
    if(longrun){
        end_date <- as.Date("2021-04-30")
    }
    else{
        end_date <- Sys.Date() + 21
    }
    print(end_date)    

    runOneModel <- function(i){
		R0  <-  all_params[[i]][["initial_params"]][["R0"]]
		Amp <-  all_params[[i]][["initial_params"]][["AMP_factor"]]
		Reffs <- all_params[[i]][["reffs"]]

		print(i)
		main_results <- run_model(mobility_matrices,
					  start_date = start_date,
                                  	  predict_from=predict_from,
                                  	  end_date = end_date,
                                  	  N_sim=N_sim,
					  R_0=R0,
                                  	  Reffs=Reffs,
                                  	  reff_change_dates=Reff_change_dates,
                                  	  parallel=F,
                                  	  AMP_factor= Amp )
        return(main_results)
    }
    main_results <- unlist(parallel::mclapply(1:models_to_keep, runOneModel, mc.cores= 200), recursive = FALSE)
    main_results <- process_simulation_list(main_results, parallel=T)
    return(main_results)
}


runPredictions2Reff = function(all_params, mobility_matrices, models_to_keep = 100, Reff_change_dates = c(as.Date("2020-03-09"), as.Date("2020-03-16")),  N_sim = 10, predict_from = Sys.Date(), start_date = as.Date("2020-02-17"), longrun = FALSE){
    if(longrun){
        end_date <- as.Date("2021-04-30")
    }
    else{
        end_date <- Sys.Date() + 21
    }
    

    runOneModel <- function(i){
		R0  <-  all_params$R0[i]
		Amp <-  all_params$Amp[i]
		Reffs <- c(all_params$Reff1[i], all_params$Reff2[i])
		
		main_results <- run_model(mobility_matrices,
				  start_date = start_date,
                                  predict_from=predict_from,
                                  end_date = end_date,
                                  N_sim=N_sim,
				  R_0=R0,
                                  Reffs=Reffs,
                                  reff_change_dates=Reff_change_dates,
                                  parallel=T,
                                  AMP_factor= Amp)
        return(main_results)
    }
    main_results <- unlist(parallel::mclapply(1:models_to_keep, runOneModel, mc.cores= 30), recursive = FALSE)
    main_results <- process_simulation_list(main_results, parallel=T)
}





plot_scenarios <- function(results, keys, out_name, start_date, xmin, xmax){
  plot_data<- data.frame()
  for(res in results){
    plot_data <- rbind(plot_data,
                       data.frame(extract_values(res[["results"]], keys=keys))
                                  %>% mutate(R0=res[["R0"]]))
  }
  plot_data <- plot_data %>% mutate(date=rep(1:(nrow(results[[1]][["results"]][[1]])/356) +start_date,
                                             length(results)))
  colnames(plot_data ) <- c("c5", "m","c95", "R0", "date")
  plot_data$R0 <- factor(plot_data$R0)
  q <- ggplot(plot_data) + geom_ribbon(aes(x=date, ymin=c5, ymax=c95, fill=R0),alpha=0.4)  + fhiplot::theme_fhi_lines() + xlab("Date") + ylab("Number in Hospital") + fhiplot::scale_fill_fhi("Reff") + xlim(xmin, xmax)
  ggsave(fs::path(org::project$results_today, out_name), width=9, height=6)
}


extract_values <- function(sims, keys=c("hosp"),desired_date=NULL , cumulate=F,fylke_filter="Norway"){
  first=T

  from_date<-"2020-02-18"
  for (key in keys){
    if (first){
      mat<-data.table(sapply(sims,"[[",key))
      first<-F
    }
    else{
      mat<-mat+data.table(sapply(sims,"[[",key))
    }
  }
  setDT(mat)
  
  if (fylke_filter!="Norway"){
    municips<-fhidata::norway_locations_b2020[,c(1,4)]
    colnames(municips)<-c("location_code","county")
    mat.fylke<-cbind(sims[[1]][,1:2],mat)
    mat.fylke<-merge(mat.fylke,municips,by="location_code")
    mat.fylke<-mat.fylke[county==fylke_filter]
    mat<-mat.fylke[,2:(ncol(mat.fylke)-1)]
  }
  else {
    mat<-cbind(sims[[1]][,2],mat)  
  }
  
  
  cols_chosen <- colnames(mat)[2:ncol(mat)]
  
  mat<-mat[ , lapply(.SD, sum), keyby = day, .SDcols = cols_chosen]
  
  if (cumulate){
    mat<-mat[ , lapply(.SD, cumsum), .SDcols = cols_chosen]  
  }
  
  mat_<-as.matrix(mat)
  mat.quant<-setDT(as.data.frame(rowQuantiles(mat_,probs=c(0.025,0.975))))
  colnames(mat.quant)<-c("firstq","lastq")
  mat.quant[,meanq:=rowMeans(mat_)]
  mat.quant[,dates:=seq.Date(as.Date(from_date),to=(as.Date(from_date)+(nrow(mat)-1)),by=1)]
  
  if (!is.null(desired_date)){
    return (mat.quant[dates==as.Date(desired_date),])
  }
  else {
    return (mat.quant)
  }
}

plot_values <- function(sims, keys=c("hosp"), day1="2020-02-18",enddate=NULL, cumulate=F,ylabel=NULL,fylke_filter=NULL,title=NULL,filename=NULL){
  require(ggplot2)
  require(tidybayes)
  #Define series
  day1<-as.Date(day1)
  
  
  #Grab simulations
  mat <- c()
  for(sim in sims){
    cum <- rep(0, length(extract_tot(sim, keys[1],fylke_filter=fylke_filter)))
    for(key in keys){
      cum <- cum + extract_tot(sim, key,fylke_filter=fylke_filter)
    }
    mat <- cbind(mat,cum)
  }
  #Run if cumulate parameter
  if(cumulate){
    mat <- apply(mat, 2, cumsum)
  }
  
  days <- as.Date("2020-02-17",origin="1970-01-01") + lubridate::days(1:nrow(mat))
  #Add dates
  mat<-data.table(mat)
  mat[,dates:=days]
  #Slice according to day1
  mat<-mat[(as.integer(day1-as.Date("2020-02-18"))+1):nrow(mat),]
  #Wide to long format
  mat_melt<-melt(mat,id.vars="dates")
  
  if (!is.null(enddate)){
    mat_melt<-mat_melt[dates<=as.Date(enddate)]
  }
  
  q<-ggplot(data=mat_melt,aes(x = dates, y = value)) +
    stat_lineribbon(aes(y = value), .width = c(.95, .90, .75,.5), color = "#08519C") +
    scale_fill_brewer()+xlab("Date") + ylab(ylabel)
  q <- q + ggtitle(title)+theme(plot.title = element_text(hjust = 0.5,size = 18))
  if (!is.null(filename)){
    ggsave (fs::path( org::project$results_today,filename))
  }
  return(q)
}

                                            
plot_real <- function(sims, keys=c("hosp"), day1="2020-02-18",ylabel=NULL,real_data=NULL,today, filename="Best_fit.png", title=""){ 
  #Real data should be used with a column "date" and a column "N" 
  if(is.null(real_data)){
    stop("Please provide dataframe with real data")
  }
  require(ggplot2)
  require(tidybayes)
  #Define series
  day1<-as.Date(day1)
  #Grab simulations
  mat <- c()
  for(sim in sims){
    cum <- rep(0, length(extract_tot(sim, keys[1])))
    for(key in keys){
      cum <- cum + extract_tot(sim, key)
    }
    mat <- cbind(mat,cum)
  }
  days <- as.Date("2020-02-17",origin="1970-01-01") + lubridate::days(1:nrow(mat))
  #Add dates
  mat<-data.table(mat)
  mat[,dates:=days]
  #Slice according to day1
  mat<-mat[(as.integer(day1-as.Date("2020-02-18"))+1):nrow(mat),]
  #Wide to long format
  mat_melt<-melt(mat,id.vars="dates")
  mat_melt<-mat_melt[dates<=today,]
  print(mat_melt)
  q <- ggplot(data=mat_melt,aes(x = dates, y = value)) +
    stat_lineribbon(aes(y = value), .width = c(.95, .90, .75,.5), color = "#08519C") +
    scale_fill_brewer()+ geom_point(data=real_data,aes(x=date,y=N),col="red")+xlab("Date") + ylab(ylabel) +ggtitle(title)
  ggsave (paste(result_path, filename, sep = ""))
  return(q)
}

                                            
create_seeding_data <- function(path, outpath="seeding.RDS"){
  col_types <- c("guess", "date", rep("guess", 13), "date", "date" , rep("guess", 17))
  d <- readxl::read_excel(path, sheet="Kasus",col_types=col_types) %>% janitor::clean_names()
  setDT(d)
  d[kasus_id==" NO-2020-000407", testdato:=dato -lubridate::days(2)]

  d <- d[!is.na(dato)]
#  d[, dato:=as.Date(dato)]
  d[, date:=innsykningsdato]
  d[is.na(date), date:=testdato - lubridate::days(3)]
  d[is.na(date), date:=dato]
  d[ date >=Sys.Date(), date:=dato - lubridate::days(4)]
  d[ date <= as.Date("2020-02-17"), date:=dato - lubridate::days(4)]
  imported <- d[type_eksponering=="Reise", .(date, kommune)]
  
  
  imported[grep("Oslo", kommune), kommune:="Oslo"]
  imported[kommune == "Frogner", kommune:="Oslo"]
  imported[kommune == "Vestre Aker", kommune:="Oslo"]
  imported[kommune == "Nordre Follo", kommune:="Nodre Follo"]
  imported[kommune == "Follo", kommune:="Nodre Follo"]
  imported[kommune == "Raelingen", kommune:="Rælingen"]
  imported[kommune == "Østre Toten", kommune:="Østre-Toten"]
  imported[kommune == "Egersund", kommune:="Eigersund"]
  imported[kommune == "Fagernes", kommune:="Nord-Aurdal"]
  imported[kommune == "Skiptvedt", kommune:="Skiptvet"]
  imported[kommune == "Sotra", kommune:="Øygarden"]
  imported[kommune == "Lavangen", kommune:="Loabák - Lavangen"]
  imported[kommune == "Våga", kommune:="Vågan"]

  aggregated <- imported[, .N, by=.(date, kommune)]


  data <- fhidata::norway_locations_b2020[aggregated, on=c("municip_name"="kommune")]
  data <- data[!is.na(municip_code)]

  saveRDS(data, outpath)
  return(data)
}
