suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
suppressMessages(library(magrittr))
suppressMessages(library(spread))


get_seeding <- function(first_seeding_date=as.Date("2020-02-17")){
  #Creating the seeding matrix
  inf.data <- readRDS("./seeding.RDS")
  setDT(inf.data)
  infe.seed <- inf.data
  infe.seed <- infe.seed[date >first_seeding_date,]
  infe.seed[, day:=round(lubridate::interval(first_seeding_date, date)/lubridate::days(1))]
  infe.seed <- infe.seed[,c("municip_code", "day", "N")]
  colnames(infe.seed) <- c( "location_code", "day", "n")
  infe.seed <- infe.seed[ infe.seed$n!= 0, ]
#  infe.seed <- infe.seed[, .(n=sum(n)), by=.(location_code, day)]
  return(infe.seed)
}



run_model <- function(mobility_matrices,
                      N_days,
                      start_date,
                      end_date,
                      predict_from,
                      R_0=2,
                      Reffs=c(), # One national number, or a matrix which is number of locations times number of change points
                      AMP_factor=1.5,
                      AMP_cp = NULL,
                      reff_change_dates=c(),
                      latent_period=3.0,
                      presymptomatic_period = 2.0,
                      infectious_period = 5.0,
                      presymptomatic_relative_infectiousness = 1.25,
                      asymptomatic_prob = 0.4,
                      asymptomatic_relative_infectiousness = 0.1,
                      N_sim=100,
                      seeding=NULL,
                      se1e2iiar_pop=NULL,
                      parallel=T,
		      inputSeed = NULL){

  if(is.null(se1e2iiar_pop)){
    se1e2iiar_pop <- readRDS( "/cluster/projects/nn9755k/solveig/se1e2iiar_pop.rds" )
  }
  N_days <- lubridate::interval(start_date, end_date) / lubridate::days(1) + 1

  mobility_matrices <- mobility_matrices[1:(N_days*4)]
  mun <- unique(fhidata::norway_locations_b2020$county_code)
  Rs <- NULL
  loc <- NULL
  days <- NULL
  times <- NULL
  if(length(R_0) == 1){
    if(!is.null(Reffs)){
     if(end_date >= reff_change_dates[1]){
       end_r0 <- reff_change_dates[1]
     }
    }  
    else{
      end_r0 <- end_date + 1
    }
    n1 <- lubridate::interval(start_date, end_r0) / lubridate::days(1)
    Rs <- c(Rs, rep(rep(R_0, 4 * n1), length(mun)))
    loc <- c(loc, rep(mun, each = 4 * n1))
    days <- c(days, rep(rep(1:n1, each = 4), length(mun)))
    times <- c(times, rep(c(0, 6, 12, 18), n1 * length(mun)))
    if(!is.null(Reffs)){
     if(end_date >= reff_change_dates[1]){
       for(i in 1:length(Reffs)){
          Reff <- Reffs[i]
          if(i == length(Reffs)){
            end_reff_date <- end_date + 1
          } else{
            end_reff_date <- reff_change_dates[i+1]
          }
          n2 <- lubridate::interval(reff_change_dates[i], end_reff_date) / lubridate::days(1)
          Rs <- c(Rs, rep(rep(Reffs[i], 4* n2), length(mun)))
          loc <- c(loc, rep(mun, each = 4 * n2))
          days <- c(days, rep(rep((n1 + 1):(n2 + n1), each = 4), length(mun)))
          times <- c(times, rep(c(0, 6, 12, 18), n2 * length(mun)))
          n1 <- n1 + n2
        }
      }
    }
  
}  else if(is.null(dim(reff_change_dates))){
    mun <- unique(fhidata::norway_locations_b2020$county_code)
    day <- rep(1:N_days, 4)
    time <- rep(c(0, 6, 12, 18), N_days)
    if(!is.null(Reffs)){
     if(end_date >= reff_change_dates[1]){
       end_r0 <- reff_change_dates[1]
     }
     else{
       end_r0 <- end_date + 1
     }   
    }  
    for(i in 1:length(mun)){
      ind <- which(names(R_0) == unique(fhidata::norway_locations_b2020$county_code)[i])
      n1 <- lubridate::interval(start_date, end_r0) / lubridate::days(1)
      loc <- c(loc, rep(mun[i], 4 * n1))
      days <- c(days, rep(1:n1, each = 4))
      times <- c(times, rep(c(0, 6, 12, 18), n1))
      Rs <- c(Rs, rep(R_0[ind], 4* n1))
      
      if(!is.matrix(Reffs) & !is.null(Reffs)){
        namesR <- names(Reffs)
        Reffs <- matrix(Reffs)
        rownames(Reffs) <- namesR
      }  
      if(!is.null(Reffs) & is.null(dim(reff_change_dates))){
        if(end_date >= as.Date(reff_change_dates[1]) & !is.null(Reffs)){
          for(j in 1:dim(Reffs)[2]){
           ind <- which(rownames(Reffs) == unique(fhidata::norway_locations_b2020$county_code)[i])
           if(j == dim(Reffs)[2]){
               n2 <- lubridate::interval(as.Date(reff_change_dates[j]), end_date) / lubridate::days(1) + 1
            }
           else{
            n2 <- lubridate::interval(as.Date(reff_change_dates[j]), as.Date(reff_change_dates[j + 1])) / lubridate::days(1)
           }
           loc <- c(loc, rep(mun[i], 4 * n2))
           days <- c(days, rep((n1 + 1):(n2 + n1), each = 4))
           times <- c(times, rep(c(0, 6, 12, 18), n2))
           Rs <- c(Rs, rep(Reffs[ind, j], 4 * n2))
	   n1 <- n1 + n2
          }
        }  
      }
    }
  } else if(!is.null(Reffs) & !is.null(dim(reff_change_dates))){
      mun <- unique(fhidata::norway_locations_b2020$county_code)
      day <- rep(1:N_days, 4)
      time <- rep(c(0, 6, 12, 18), N_days)
      for(i in 1:length(mun)){
        ind <- which(rownames(Reffs) == unique(fhidata::norway_locations_b2020$county_code)[i])
        if(end_date >= as.Date(reff_change_dates[ind, 1])){
          end_r0 <- as.Date(reff_change_dates[ind, 1])
        }
        else{
          end_r0 <- end_date + 1
        }
        
        n1 <- lubridate::interval(start_date, end_r0) / lubridate::days(1)
        loc <- c(loc, rep(mun[i], 4 * n1))
        days <- c(days, rep(1:n1, each = 4))
        times <- c(times, rep(c(0, 6, 12, 18), n1))
        Rs <- c(Rs, rep(R_0[ind], 4* n1))
        
        if(end_date >= as.Date(reff_change_dates[ind, 1]) & !is.null(Reffs)){
          for (j in 1:sum(!is.na(Reffs[ind, ]))){
            if(j == sum(!is.na(Reffs[ind, ]))){
              n2 <- lubridate::interval(as.Date(reff_change_dates[ind, j]), end_date) / lubridate::days(1) + 1
            }
            else{
              n2 <- lubridate::interval(as.Date(reff_change_dates[ind, j]), as.Date(reff_change_dates[ind, j + 1])) / lubridate::days(1)
            }
            loc <- c(loc, rep(mun[i], 4 * n2))
            days <- c(days, rep((n1 + 1):(n2 + n1), each = 4))
            times <- c(times, rep(c(0, 6, 12, 18), n2))
            Rs <- c(Rs, rep(Reffs[ind, j], 4 * n2))
            n1 <- n1 + n2
          }
        }
      }  
    }
  betas <-spread:::se1e2iiaR_calculate_beta_from_r0(
                     r0 = Rs,
                     a2 = 1/presymptomatic_period,
                     gamma = 1/infectious_period,
                     presymptomaticRelativeInfectiousness = presymptomatic_relative_infectiousness,
                     asymptomaticProb =  asymptomatic_prob,
                     asymptomaticRelativeInfectiousness = asymptomatic_relative_infectiousness)
		     
  betas <- data.table("location_code" = loc, "day" = days, "time" = times, "beta" = betas)
  betas <- betas[betas$day <= N_days, ]
  results <- list()
  if(is.null(seeding)){
    seeding <- get_seeding()
  }
  seeding_period <- lubridate::interval(start_date, predict_from) / lubridate::days(1)

  inner_run_model <-function(i){
    new_seeding <- copy(seeding)
#    set.seed(i)
    new_seeding$n <- new_seeding$n + rpois(length(new_seeding$n), lambda=new_seeding$n * (AMP_factor-1))
    start_se1e2iiar_pop <- copy(se1e2iiar_pop)

    d <- spread:::asymmetric_mobility_se1e2iiar(
                    se1e2iiar_pop = start_se1e2iiar_pop,
                    mobility_matrix = mobility_matrices,
                    dynamic_seeds =  new_seeding, 
                    betas = betas,
                    latent_period = latent_period,
                    presymptomatic_period = presymptomatic_period,
                    presymptomatic_relative_infectiousness =  presymptomatic_relative_infectiousness,
                    asymptomatic_prob = asymptomatic_prob,
                    asymptomatic_relative_infectiousness = asymptomatic_relative_infectiousness,
                    N = 1 )
    d[, date:=day+start_date]
    return(d)
  }

  inner_run_model_split <-function(i){
   if(is.null(inputSeed)){
      inputSeed <- i + as.numeric(Sys.time())
   }
   set.seed(inputSeed)
   new_seeding <- copy(seeding)
   new_seeding$n <- new_seeding$n + rpois(length(new_seeding$n), lambda=new_seeding$n * (AMP_factor-1))
   start_se1e2iiar_pop <- copy(se1e2iiar_pop)
    sink("/dev/null")
   d <- spread:::asymmetric_mobility_se1e2iiar(
                   se1e2iiar_pop = start_se1e2iiar_pop,
                   mobility_matrix = mobility_matrices[1:(seeding_period*4)],
                    dynamic_seeds =  new_seeding, 
                    betas = betas[betas$day <= seeding_period, ],
                    latent_period = latent_period,
                    presymptomatic_period = presymptomatic_period,
                    presymptomatic_relative_infectiousness =  presymptomatic_relative_infectiousness,
                    asymptomatic_prob = asymptomatic_prob,
                    asymptomatic_relative_infectiousness = asymptomatic_relative_infectiousness,
                    N = 1,
		    inputSeed = inputSeed)
                    
   d.b_Ia <- d[ day == max(day), b_Ia ] 
   d.b_I  <- d[ day == max(day), b_I  ]
   d.b_E1 <- d[ day == max(day), b_E1 ]
   d.b_E2 <- d[ day == max(day), b_E2 ]
   d.b_R <- d[ day == max(day), b_R ]
   start_se1e2iiar_pop[ , I  := d.b_I ]
   start_se1e2iiar_pop[ , Ia := d.b_Ia ]
   start_se1e2iiar_pop[ , E1 := d.b_E1 ]
   start_se1e2iiar_pop[ , E2 := d.b_E2 ]
   start_se1e2iiar_pop[ , R := d.b_R ]
   start_se1e2iiar_pop[ , S  := (S - (d.b_I + d.b_Ia + d.b_E1 + d.b_E2 + d.b_R) ) ]
   set.seed(NULL)
    if(end_date > predict_from){
      d2 <- spread:::asymmetric_mobility_se1e2iiar(
                       se1e2iiar_pop = start_se1e2iiar_pop,
                       mobility_matrix = mobility_matrices[((seeding_period)*4 + 1):(4*N_days)],
                       betas = betas[betas$day > seeding_period, ],
                       latent_period = latent_period,
                       presymptomatic_period = presymptomatic_period,
                       presymptomatic_relative_infectiousness =  presymptomatic_relative_infectiousness,
                       asymptomatic_prob = asymptomatic_prob,
                       asymptomatic_relative_infectiousness = asymptomatic_relative_infectiousness,
                       N = 1 )
      d2[, day:=day + max(d$day)]
      res <- rbind(d,d2)
    }else{
      res <- d
    }
    sink()
    res[, date:=start_date+day]
    set.seed(Sys.time())
    return(copy(res))
    }

  if(parallel){
    results <- parallel::mclapply(1:N_sim, inner_run_model_split, mc.cores=parallel::detectCores())
  }else{
    results <- lapply(1:N_sim, inner_run_model_split)
  }

  return(results)

}
