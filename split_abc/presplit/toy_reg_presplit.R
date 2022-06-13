

args = commandArgs(trailingOnly=TRUE)
if (length(args)<2|length(args)>3){
  stop("Two/Three arguments are needed: 1-County code, 2-Matrix with the changepoints for the sep county, 3-Matrix with the changepoints to use in the other regions")
}
sep_county <- args[1]
print(sep_county)
changepoints_sep<-as.matrix(read.csv2(args[2],stringsAsFactors = F,row.names = 1,header=FALSE))

reff_change_dates_sep<-as.Date(changepoints_sep[sep_county,changepoints_sep[sep_county,]!=""])

#BY DEFAULT WE ASSUME THAT THE CHANGEPOINTS IN THE "REST" REGION ARE THE SAME AS THOSE IN THE COUNTY THAT IS BEING CALIBRATED
if (length(args)==2){
reff_change_dates_nat = reff_change_dates_sep
}else{
  changepoints_nat<-as.matrix(read.csv2(args[3],stringsAsFactors = F,row.names = 1,header=FALSE))
  reff_change_dates_nat<-as.Date(changepoints_nat[sep_county,changepoints_nat[sep_county,]!=""])
  
}
result_path <- paste0("/cluster/projects/nn9755k/alfonso/toy_split_october/presplit/complete_sep",sep_county,"_sync/")



data_path <- "/cluster/projects/nn9755k/alfonso/toy_split_october/presplit/"
suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(library(mvtnorm))
suppressMessages(library(fhidata))
suppressMessages(library(Rmpi))
suppressMessages(library(zoo))
options(datatable.optimize=1) 

ns <- mpi.universe.size() - 1
id <- mpi.comm.rank(comm = 0)
print(paste("id", id))

include_delay_admissions = F

source("util.R")
source("expand_hospitalization_jcl5_fylkelevel.R")
source("run_model_fylkelevel.R")

run_calibration = F
make_main_res = T
R_calib = 17# Number of last calibration file, for running main results


R_tot = 200
#eps1=60000

if (sep_county%in%c("county03","county30")){
  factor_ov=3
  factor_rest=2}else{factor_ov=1
factor_rest=1.5}

eps1_ov = 3*65000*1 * 2/(20 * 11)*factor_ov
eps1_rest = 3*65000 * 9/(20 * 11)*factor_rest
#eps2=16000000
eps2_ov = 3*16500000*2/(20 * 11)*factor_ov
eps2_rest = 3*16500000*9/(20 * 11)*factor_rest
N_acc = 500
num_acc = 0

max_delay = 4

############################
##INPUT SECTION 
#############################

mobility_matrices  <- readRDS(paste(data_path, "special_matrix_oct1.RDS", sep = "" )) # <-SET

mobility_to_fylke<-function(mobility_matrices){
  process<-function(m){
    m$from<-merge(m,fhidata::norway_locations_b2020[,.(municip_code,county_code)],by.x="from",by.y="municip_code",all.x=T)$county_code
    m<-m[order(m$to),]
    m$to<-merge(m,fhidata::norway_locations_b2020[,.(municip_code,county_code)],by.x="to",by.y="municip_code",all.x=T)$county_code
    m<-m[from!=to,]
    m<-m[,.(n=sum(n)),keyby=.(from,to)]
    return(m)
  }  
  return(lapply(mobility_matrices,process))
}


mobility_matrices<-mobility_to_fylke(mobility_matrices)


start_date<-as.Date("2020-02-17")
predict_from<-as.Date("2020-07-25")+15 #Enddate    # <-SET

# ###############################
# #PREVIOUS STATES (PREVIOUS FOLDER)
# #NEEDED -> INITIAL STATES (TIMEPOINT TCAL-15), CALIBRATION FILE(E.G. R75.TXT) AND MATRIX WITH CHANGEPOINTS (E.G. REFF_MATRIX_2021-01-21)
# 
# initial.states<-readRDS("./previous/initial_states.RDS") 
# previous.calibration_file<-"./previous/R77.txt"    # <-SET
# previousR <- read.table(previous.calibration_file)
# previousmatrix<-readRDS("./previous/preAug_matrix.RDS")    # <-SET
# 
# previous_linear<-as.vector(t(previousmatrix))[!is.na(as.vector(t(previousmatrix)))]
# 
# lastR.index<-11+which(previous_linear==calibration.date)-1
# 
# print(paste0("Last R indexes -> ",lastR.index))
# 
# #############


#c(as.Date("2021-01-04"), as.Date("2021-02-05"))
#reff_change_dates_sep = c(as.Date("2021-01-04"), as.Date("2021-02-05"))

#sep_county = "county46"
ind_county = which(sort(unique(fhidata::norway_locations_b2020$county_code)) == sep_county)

max_reff = max(length(reff_change_dates_nat), length(reff_change_dates_sep))

reff_change_dates = matrix(NA, 11, max_reff)


reff_change_dates[1, 1:length(reff_change_dates_nat)] = as.character(reff_change_dates_nat)
reff_change_dates[2, 1:length(reff_change_dates_nat)] = as.character(reff_change_dates_nat)
reff_change_dates[3, 1:length(reff_change_dates_nat)] = as.character(reff_change_dates_nat)
reff_change_dates[4, 1:length(reff_change_dates_nat)] = as.character(reff_change_dates_nat)
reff_change_dates[5, 1:length(reff_change_dates_nat)] = as.character(reff_change_dates_nat)
reff_change_dates[6, 1:length(reff_change_dates_nat)] = as.character(reff_change_dates_nat)
reff_change_dates[7, 1:length(reff_change_dates_nat)] = as.character(reff_change_dates_nat)
reff_change_dates[8, 1:length(reff_change_dates_nat)] = as.character(reff_change_dates_nat)
reff_change_dates[9, 1:length(reff_change_dates_nat)] = as.character(reff_change_dates_nat)
reff_change_dates[10, 1:length(reff_change_dates_nat)] = as.character(reff_change_dates_nat)
reff_change_dates[11, 1:length(reff_change_dates_nat)] = as.character(reff_change_dates_nat)



reff_change_dates[ind_county,] = rep(NA, max_reff)
reff_change_dates[ind_county, 1:length(reff_change_dates_sep)] = as.character(reff_change_dates_sep)
Reff_change_dates = reff_change_dates


#SAVING THE MATRICES TOGETHER
# split<-list()
# split$"pre_reff"<-previousmatrix
# split$"post_reff"<-reff_change_dates
# 
# saveRDS(split, file = paste(result_path, paste0("reff_matrix_split_",Sys.Date()), sep = ""))
# print(paste0("Matrices stored as ...", "reff_matrix_split_",Sys.Date()))

#############
#############




mobility_start<-4*(start_date-as.Date("2020-02-17"))
mobility_matrices<-mobility_matrices[mobility_start:(mobility_start+4*((predict_from-start_date)+1))]

hosp_incidence <- county_incidence_data2(filename="fylke_datasync.csv")

hosp_incidence<-hosp_incidence[date<=predict_from]

hosp_incidence <- hosp_incidence[data.table(date=seq(start_date, predict_from, by=1)), on=c("date"="date")]
hosp_incidence<-hosp_incidence[!is.na(county_code), ]
hosp_incidence <- hosp_incidence[, N_prev:=NULL]






test_data <- read.csv("labDatasync.csv", sep = ";")


if (grepl(".",as.character(test_data$ProveDato)[1],fixed=T)){
  test_data$Provedato = as.Date(test_data$ProveDato, format = "%d.%m.%Y")
}else{test_data$Provedato = as.Date(test_data$ProveDato)}

setDT(test_data)

test_data[,Positive:=PositiveIkkeUtlandet]
test_data[, PositiveIkkeUtlandet := NULL]

test_data[, ProveDato:=NULL]
test_data[, X:=NULL]

test_data = test_data[,  lapply(.SD, sum), by = .(Provedato, FylkeNr)]

test_data[, date := Provedato]
test_data[, Provedato := NULL]
test_data$FylkeNr[test_data$FylkeNr == "3"] = "03"

test_data$county_code = paste("county", test_data$FylkeNr, sep="")
test_data = test_data[county_code != "county21"]
test_data[, FylkeNr := NULL]
test_data[, antall_negative := NegativeAlle]
test_data[, N:=Positive]
test_data[, antall:=Positive]

test_data <- test_data[date >= start_date]
#test_data<-test_data[date>=calibration.date]
test_data<-test_data[date<=predict_from]

test_data <- test_data[data.table(date=seq(start_date, predict_from, by=1)), on=c("date"="date")]




test_data[is.na(antall), antall:=0]
test_data[is.na(antall_negative), antall_negative:=0]

number_tests = test_data
number_tests$ntn = number_tests$antall_negative
number_tests$ntp = number_tests$antall

for_imputation = test_data[test_data$date >= max(as.Date("2020-05-01"),start_date) - 6]
test_data <- test_data[date >= as.Date("2020-05-01"), ]


#Recalibrate last R if it lasted more than 15 days. Then we need 15 backwards previous to the comparison of the incidence
#So last R -> until November 10th and we recalibrate the 

#R0 must be the previous R in the CALIBRATION FILE, not the R0 since the very beginning


seeding <- get_seeding()

seeding_to_fylke<-function(seeding){
  seeding<-seeding[seeding$location_code!="municipNA",]
  seeding<-seeding[seeding$location_code!="municip9999",]
  seeding<-merge(seeding,fhidata::norway_locations_b2020[,c("municip_code","county_code")],by.x="location_code",by.y="municip_code",all.x=T)
  seeding<-seeding[,.(day,n,county_code)]
  colnames(seeding)<-c("day","n","location_code")
  return(seeding[,.(n=sum(n)),keyby=.(location_code,day)])
}
seeding<-seeding_to_fylke(seeding)


se1e2iiar_pop <- readRDS( paste(data_path, "se1e2iiar_pop.rds", sep = "") ) 

se1e2iiar_pop_tofylke<-function(se1e2iiar_pop){
  se1e2iiar_pop<-se1e2iiar_pop[fhidata::norway_locations_b2020[,c("municip_code","county_code")],on=c("location_code"="municip_code")]
  se1e2iiar_pop<-se1e2iiar_pop[,2:ncol(se1e2iiar_pop)]
  colnames(se1e2iiar_pop)<-c(colnames(se1e2iiar_pop)[1:(ncol(se1e2iiar_pop)-1)],"location_code")
  se1e2iiar_pop<-se1e2iiar_pop[,lapply(.SD, sum), by=location_code ]
  return (se1e2iiar_pop)
}

se1e2iiar_pop<-se1e2iiar_pop_tofylke(se1e2iiar_pop)



N_runs <- 1

logsum_logspace <- function(a){
  a <- sort(a, decreasing=T)
  a <- a[a > a[1] -200]
  N <- length(a)
  if(N==1){ return(a[1])}
  a[1] + log(1 + sum(exp(a[2:N]-a[1])))
}

run_model_fylke <- function(params, rn, longrun = FALSE){ 
  
  R0_tot <- c(params[1],rep(params[2],10))
  Reffs = matrix(NA, 11, max_reff)

  for (i in 1:11){
    if(i != ind_county){
      Reffs[i, 1:length(reff_change_dates_nat)] = params[3:(3 + length(reff_change_dates_nat) - 1)] 
    }
  }

  Reffs[ind_county, 1:length(reff_change_dates_sep)] = params[(3 + length(reff_change_dates_nat)):(3 + length(reff_change_dates_nat) + length(reff_change_dates_sep) - 1)]
  
  AMP <- 1.439 #params[12 + length(reff_change_dates_nat) + length(reff_change_dates_sep)]
  rt01 <- -0.501 #params[12 + length(reff_change_dates_nat) + length(reff_change_dates_sep) + 1]
  rt1 <- 5.561e-05#params[12 + length(reff_change_dates_nat) + length(reff_change_dates_sep) + 2]
  delay <- 2#params[12 + length(reff_change_dates_nat) + length(reff_change_dates_sep) + 3]
  


  names(R0_tot) <- sort(unique(fhidata::norway_locations_b2020$county_code))
  rownames(Reffs) <- sort(unique(fhidata::norway_locations_b2020$county_code))
  
  
  end_date = predict_from  
  
  
  start_se1e2iiar_pop<-copy(se1e2iiar_pop)

  #Run model
  result <- run_model(mobility_matrices, 
                      start_date = start_date, 
                      end_date = end_date,
                      predict_from = predict_from,
                      N_sim = N_runs,
                      R_0 = R0_tot,
                      reff_change_date=Reff_change_dates,
                      Reffs=Reffs,
                      parallel = F,
                      seeding=seeding,
                      se1e2iiar_pop=start_se1e2iiar_pop,
                      AMP_factor = AMP,
                      inputSeed = rn)
  set.seed(rn)
  variables_sel = c("date", "ntn", "ntp")
  number_tests_agg = number_tests[, ..variables_sel]
  numbers_tests_agg = number_tests_agg[, lapply(.SD, sum, na.rm = TRUE), by = date]

  number_tests_nowc = nowcast_cases(numbers_tests_agg$ntn, p_delay = c(0.16, 0.74, 0.92, 0.98)) +
  nowcast_cases(numbers_tests_agg$ntp)  #Default delay prob. is for positive cases
  n_test_ma = rollmean(number_tests_nowc, k = 7, fill = (number_tests_agg$ntn + number_tests_agg$ntp)[1:6], align = "right")

  eta <- rt01 + n_test_ma * rt1
  rt <- exp(eta) / (1 + exp(eta))  		      
  hosp_results <- process_simulation_list(result, parallel=F, onset_to_test = delay, p_testing_positive = rt)
  return(hosp_results)
}

log_lik_fylke <- function(params, rn){
  hosp_results <- run_model_fylke(params, rn)
  #hosp_results <- to_fylke_data(hosp_results)
  hosp_results<-lapply(hosp_results,setNames,replace(colnames(hosp_results[[1]]),colnames(hosp_results[[1]])=="location_code","county_code"))
  np = length(params)
  l_ov <- NULL
  l_rest <- NULL
  l2_ov <- NULL
  l2_rest <- NULL
  for(hosp_res in hosp_results){
    err2_ov <- 0
    err2_rest <- 0 
    hosp_res<-hosp_res[date>=start_date,]
    reg <- hosp_res[hosp_incidence, on=c("date"="date", "county_code"="county_code")]
    reg <- reg[!is.na(N)]
    reg_ov <- reg[county_code == sep_county]
    reg_rest <- reg[county_code != sep_county]

    if(include_delay_admissions){

      l_ov <- c(l_ov, sum((reg_ov$N - reg_ov[, hosp_inc_with_delay])^2))
      l_rest <- c(l_rest, sum((reg_rest$N - reg_rest[, hosp_inc_with_delay])^2))
    }
    else{

      
      l_ov <- c(l_ov, sum((reg_ov$N - reg_ov[, hosp_inc])^2))
      l_rest <- c(l_rest, sum((reg_rest$N - reg_rest[, hosp_inc])^2))
    }
    hosp_sub <- reg[reg$date >= min(test_data$date), ]
    count = 0
    for (county in unique(reg$county_code)){
      
      reg_c = reg[county_code == county, ]
      ind1 = which(reg_c$date == max(as.Date("2020-05-01"),start_date))
      hosp_sub_c = hosp_sub[county_code == county, ]
      test_data_c = test_data[(test_data$county_code == county)&(test_data$date>=max(start_date,as.Date("2020-05-01"))), ]
      positive_c = for_imputation[for_imputation$county_code == county, ]
      
      
      #observed_ma = rollmean(test_data_c$antall, k = 7, fill = positive_c$antall[1:6], align = "right")
      observed_ma = rollmean(test_data_c$antall, k = 7, align = "right")

      if(include_delay_admissions){
        #simulated_ma = rollmean(hosp_sub_c$tested_with_delay, k = 7, fill = reg_c$tested_with_delay[(ind1 - 1): (ind1 - 6)], align = "right")
        simulated_ma = rollmean(hosp_sub_c$tested_with_delay, k = 7, align = "right")
      }
      else{
        #simulated_ma = rollmean(hosp_sub_c$tested, k = 7, fill = reg_c$tested[(ind1 - 1): (ind1 - 6)], align = "right")
        simulated_ma = rollmean(hosp_sub_c$tested, k = 7, align = "right")
      } 

      for(i in 1:length(simulated_ma)){
        if(county == sep_county){
          err2_ov <- err2_ov + (observed_ma[i] - simulated_ma[i]) ^ 2
        }
        else{
          err2_rest <- err2_rest + (observed_ma[i] - simulated_ma[i]) ^ 2
        }
        count = count + 1
      }
      
    }
    err2_ov = err2_ov/count
    err2_rest = err2_rest/count
    l2_ov = c(l2_ov, err2_ov)
    l2_rest = c(l2_rest, err2_rest)
  }
 
  return(c(mean(l_ov), mean(l_rest), mean(l2_ov), mean(l2_rest)))
}


## ABC ##
if(run_calibration){
  
  #Factor decreasing the error in each round:
  decrease.factor=0.8
  N_runs = 1
  priCov = matrix(0, nrow = 2+length(reff_change_dates_nat) + length(reff_change_dates_sep), ncol = 2+length(reff_change_dates_nat) + length(reff_change_dates_sep))
  diag(priCov) = c(rep(0.4, 2),rep(0.25, length(reff_change_dates_nat) + length(reff_change_dates_sep)))
  
  priMean <- c(rep(3.7, 2),rep(1, length(reff_change_dates_nat) + length(reff_change_dates_sep)))
  
  
  prior_all <- function(a){
    dmvnorm(a, mean = priMean, sigma = priCov)
  }
  
  r = 0
  
  
  while (r < R_tot){
    number_complete = 0
    if (r == 0){
      if(id == 0){
        errors1_ov = NULL
        errors1_rest = NULL
        errors2_ov = NULL
        errors2_rest = NULL
        R0s = NULL
        Reffs = NULL
        seeds = NULL
        results = NULL
        R0_all = NULL
        Reff_all = NULL
        errors1_ov_all = NULL
        errors1_rest_all = NULL
        errors2_ov_all = NULL
        errors2_rest_all = NULL
        number_complete = 0
        while(number_complete < N_acc){
          thisResult <- mpi.recv.Robj(source = mpi.any.source(), tag = 5 + r, comm = 0)
          number_complete = number_complete + 1
          print("number_complete")
          print(number_complete)
          results  = cbind(results, thisResult)
        }
        # send stop
        for (k in 1:ns){
          #mpi.isend(1.0, type = 2, dest = k, tag = 1, comm = 0, request=k-1)
          mpi.isend(as.integer(1), 1, k, as.integer(50 + r), comm=0)
        }
        for (j in 1:N_acc){
          R0s = rbind(R0s, results[1:2, j])
          Reffs = rbind(Reffs, results[3:(2 + length(reff_change_dates_sep) + length(reff_change_dates_nat)), j])
          errors1_ov = c(errors1_ov, results[2 +length(reff_change_dates_sep) + length(reff_change_dates_nat) + 1, j])
          errors1_rest = c(errors1_rest, results[2 + length(reff_change_dates_sep) + length(reff_change_dates_nat) + 2, j])
          errors2_ov = c(errors2_ov, results[2 + length(reff_change_dates_sep) + length(reff_change_dates_nat) + 3, j])
          errors2_rest = c(errors2_rest, results[2 + length(reff_change_dates_sep) + length(reff_change_dates_nat) + 4, j])
          seeds = c(seeds, results[2 + length(reff_change_dates_sep) + length(reff_change_dates_nat) + 5, j])
        }
        R0_all = rbind(R0_all, R0s)
        Reff_all = rbind(Reff_all, Reffs)
        errors1_ov_all = c(errors1_ov_all, errors1_ov)
        errors1_ov = sort(errors1_ov)
        eps1_ov = errors1_ov[round(decrease.factor * N_acc)]

        errors1_rest_all = c(errors1_rest_all, errors1_rest)
        errors1_rest = sort(errors1_rest)
        eps1_rest = errors1_rest[round(decrease.factor * N_acc)]

        errors2_ov_all = c(errors2_ov_all, errors2_ov)
        errors2_ov = sort(errors2_ov)
        eps2_ov = errors2_ov[round(decrease.factor * N_acc)]

        errors2_rest_all = c(errors2_rest_all, errors2_rest)
        errors2_rest = sort(errors2_rest)
        eps2_rest = errors2_rest[round(decrease.factor * N_acc)]

        print(c(r, eps1_ov, eps1_rest, eps2_ov, eps2_rest))
        W = rep(1/N_acc, N_acc)
        R0_prev = R0s
        Reff_prev = Reffs
        covMat <- cov(cbind(R0_prev, Reff_prev))
        tf2 = T
        while(tf2){
          tf2 = F
          for (i in 1:ns){
            tf = mpi.iprobe(i, tag = 5 + r, comm = 0)
            if(tf){
              discardResult <- mpi.recv.Robj(source = mpi.any.source(), tag = 5 + r, comm = 0)
              tf2 = T
            }
          }
        }
        mpi.barrier(comm=0)
        # send W, eps, covMat, R0_prev, R_prev, AMP_prev
        mpi.bcast.Robj(W, rank = 0, comm = 0)
        mpi.bcast.Robj(eps1_ov, rank = 0, comm = 0)
        mpi.bcast.Robj(eps1_rest, rank = 0, comm = 0)
        mpi.bcast.Robj(eps2_ov, rank = 0, comm = 0)
        mpi.bcast.Robj(eps2_rest, rank = 0, comm = 0)
        mpi.bcast.Robj(covMat, rank = 0, comm = 0)
        mpi.bcast.Robj(R0_prev, rank = 0, comm = 0)
        mpi.bcast.Robj(Reff_prev, rank = 0, comm = 0)

      }
      else{
        # kjør simulering
        stopInd <- as.integer(0)
        mpi.irecv(stopInd, type=1, source=0, tag=1, comm=0)
        while(stopInd < 1){
          error1_ov = 10000000000
          error1_rest = 10000000000
          error2_ov = 10000000000
          error2_rest = 10000000000
          while((error1_ov > eps1_ov | error2_ov > eps2_ov | error1_rest > eps1_rest | error2_rest > eps2_rest ) & stopInd != 1){
            params=rmvnorm(1, mean = priMean, sigma = priCov)

            while(any(params[1:(length(reff_change_dates_sep) + length(reff_change_dates_nat) + 2)] < 0) ){
              params=rmvnorm(1, mean = priMean, sigma = priCov)
            }
            rn = as.numeric(Sys.time()) + sample(1:1000000, 1)
           
            errors = log_lik_fylke(params, rn = rn)
            error1_ov = errors[1]
            error1_rest = errors[2]
            error2_ov = errors[3]
            error2_rest = errors[4]

            tf = mpi.iprobe(0, tag = as.integer(50 + r), comm = 0)
            if(tf){
              mpi.recv(stopInd, type = 1, source = 0, tag = as.integer(50 + r), comm = 0)
              stopInd = 1
            }
          }
          if(stopInd < 1){
            oneRes = (c(params, error1_ov, error1_rest, error2_ov, error2_rest, rn))
            mpi.send.Robj(oneRes, dest = 0, comm = 0, tag = 5 + r)
            tf = mpi.iprobe(0, tag = as.integer(50 + r), comm = 0)
            if(tf){
              mpi.recv(stopInd, type = 1, source = 0, tag = as.integer(50 + r), comm = 0)
              stopInd = 1
            } 
          }
        }
        mpi.barrier(comm=0)
        W = mpi.bcast.Robj(obj=NULL, rank = 0, comm = 0)
        eps1_ov = mpi.bcast.Robj(obj = NULL, rank = 0, comm = 0)
        eps1_rest = mpi.bcast.Robj(obj = NULL, rank = 0, comm = 0)
        eps2_ov = mpi.bcast.Robj(obj = NULL, rank = 0, comm = 0)
        eps2_rest = mpi.bcast.Robj(obj = NULL, rank = 0, comm = 0)
        covMat = mpi.bcast.Robj(obj = NULL, rank = 0, comm = 0)
        R0_prev = mpi.bcast.Robj(obj = NULL, rank = 0, comm = 0)
        Reff_prev = mpi.bcast.Robj(obj = NULL, rank = 0, comm = 0)

      }
    }
    else{
      if(id == 0){
        errors1_ov = NULL
        errors1_rest = NULL
        errors2_ov = NULL
        errors2_rest = NULL
        R0s = NULL
        Reffs = NULL
        seeds = NULL
        results = NULL
        print(number_complete)
        number_complete = 0
        while(number_complete < N_acc){
          print("listening for result")
          print(number_complete+1)
          thisResult <- mpi.recv.Robj(source = mpi.any.source(), tag = 5 + r, comm = 0)
          number_complete = number_complete + 1
          results  = cbind(results, thisResult)
          print("Number complete")
          print(number_complete)
        }
        # send stop
        for (k in 1:ns){
          print(paste("Sending stop to dest ", k))
          sendvar <- as.integer(1)
          #mpi.isend(1.0, type = 2, dest = k, tag = 1, comm = 0, request=k-1)
          mpi.isend(sendvar, 1, k, as.integer(200+r), comm=0)
        }
        for (j in 1:N_acc){
          R0s = rbind(R0s, results[1:2, j])
          Reffs = rbind(Reffs, results[3:(2 + length(reff_change_dates_sep) + length(reff_change_dates_nat)), j])
          errors1_ov = c(errors1_ov, results[2 + length(reff_change_dates_sep) + length(reff_change_dates_nat) + 1, j])
          errors1_rest = c(errors1_rest, results[2 + length(reff_change_dates_sep) + length(reff_change_dates_nat) + 2, j])
          errors2_ov = c(errors2_ov, results[2 + length(reff_change_dates_sep) + length(reff_change_dates_nat) + 3, j])
          errors2_rest = c(errors2_rest, results[2 + length(reff_change_dates_sep) + length(reff_change_dates_nat) + 4, j])
          seeds = c(seeds, results[2 + length(reff_change_dates_sep) + length(reff_change_dates_nat) + 5, j])
        }
        R0_all = rbind(R0_all, R0s)
        Reff_all = rbind(Reff_all, Reffs)
        errors1_ov_all = c(errors1_ov_all, errors1_ov)
        errors1_ov = sort(errors1_ov)
        eps1_ov = errors1_ov[round(decrease.factor * N_acc)]
        errors2_ov_all = c(errors2_ov_all, errors2_ov)
        errors2_ov = sort(errors2_ov)
        eps2_ov = errors2_ov[round(decrease.factor * N_acc)]
        errors1_rest_all = c(errors1_rest_all, errors1_rest)
        errors1_rest = sort(errors1_rest)
        eps1_rest = errors1_rest[round(decrease.factor * N_acc)]
        errors2_rest_all = c(errors2_rest_all, errors2_rest)
        errors2_rest = sort(errors2_rest)
        eps2_rest = errors2_rest[round(decrease.factor * N_acc)]

        print(c(r, eps1_ov, eps1_rest, eps2_ov, eps2_rest))
        W_n = W
        for (i in 1:N_acc){
          denom = 0
          for (j in 1:N_acc){
            denom = denom + W[j] * dmvnorm(results[1:(2+(length(reff_change_dates_sep) + length(reff_change_dates_nat))),i], mean =  c(R0_prev[j,], Reff_prev[j, ]), sigma = covMat)
          }
          if(denom == 0){
            denom = 0
          }
          W_n[i] = prior_all(results[1:(2+length(reff_change_dates_sep) + length(reff_change_dates_nat) ),i])/denom
        }
        
        W = W_n/sum(W_n, na.rm=T)        
 	W[is.na(W)] = min(W, na.rm=T)
   	W[is.nan(W)] = min(W, na.rm=T)
   	W[is.infinite(W)] = max(W[is.finite(W)], na.rm=T)
        R0_prev = R0s
        Reff_prev = Reffs 
        covMat <- cov(cbind(R0_prev, Reff_prev))
        # Clear all excess send objects:
        tf2 = T
        while(tf2){
          tf2 = F
          for (i in 1:ns){
            tf = mpi.iprobe(i, tag = 5 + r, comm = 0)
            if(tf){
              discardResult <- mpi.recv.Robj(source = mpi.any.source(), tag = 5 + r, comm = 0)
              tf2 = T
            }
          }
        }
        
        print("Bfore barrier")
        mpi.barrier(comm=0)
        print("after barrier")
        # send W, eps, covMat, R0_prev, R_prev, AMP_prev
        mpi.bcast.Robj(W, rank = 0, comm = 0)
        mpi.bcast.Robj(eps1_ov, rank = 0, comm = 0)
        mpi.bcast.Robj(eps1_rest, rank = 0, comm = 0)
        mpi.bcast.Robj(eps2_ov, rank = 0, comm = 0)
        mpi.bcast.Robj(eps2_rest, rank = 0, comm = 0)
        mpi.bcast.Robj(covMat, rank = 0, comm = 0)
        mpi.bcast.Robj(R0_prev, rank = 0, comm = 0)
        mpi.bcast.Robj(Reff_prev, rank = 0, comm = 0)

      }
      else{
        # kjør simulering

        stopInd <- as.integer(0)
        #mpi.irecv(stopInd, type=1, source=0, tag=as.integer(200 + r), comm = 0)
        while(stopInd < 1){
          error1_ov = 10000000000
          error1_rest = 10000000000
          error2_ov = 10000000000
          error2_rest = 10000000000
          
          #A�ADIR MUESTREO UTILIZANDO EL INDEX TB PARA EL AMPLIFICATION FACTOR Y LOS R0
          while((error1_ov > eps1_ov | error1_rest > eps1_rest | error2_ov > eps2_ov | error2_rest > eps2_rest) & stopInd != 1){

            index <- sample(1:N_acc, 1, prob = W)
            meanParams <- c(R0_prev[index, ], Reff_prev[index, ])
            ##
            params=rmvnorm(1, mean = meanParams, sigma = covMat)
            
            while(any(params[1:(2 + length(reff_change_dates_sep) + length(reff_change_dates_nat))] < 0)) {
              params=rmvnorm(1, mean = meanParams, sigma = covMat)
            }
            #params[11 + length(reff_change_dates_sep) + length(reff_change_dates_nat) + 4] = round(params[11 +	length(reff_change_dates_sep) + length(reff_change_dates_nat) + 4])
            rn = as.numeric(Sys.time()) + sample(1:1000000, 1)
            # error = -1 * log_lik_fylke(params, rn = rn)
            errors = log_lik_fylke(params, rn = rn)
            error1_ov = errors[1]
            error1_rest = errors[2]
            error2_ov = errors[3]
            error2_rest = errors[4]

            tf = mpi.iprobe(0, tag=as.integer(200+r), comm=0)
            if(tf){
              mpi.recv(stopInd, type=1, source=0, tag=as.integer(200+r), comm=0)
              stopInd = 1
            }
          }
          if(stopInd < 1){
            oneRes = (c(params, error1_ov, error1_rest, error2_ov, error2_rest, rn))
            if(id == 1){
              print("sending accepted parameter")
            }
            mpi.send.Robj(oneRes, dest = 0, comm = 0, tag = 5 + r)
            if(id == 1){
              print("finished sending")
            }
          }
          tf = mpi.iprobe(0, tag=as.integer(200+r), comm=0)
          if(tf){
            mpi.recv(stopInd, type=1, source=0, tag=as.integer(200+r), comm=0)
            stopInd = 1
          }
        }
        mpi.barrier(comm=0)
        W = mpi.bcast.Robj(obj=NULL, rank = 0, comm = 0)
        eps1_ov = mpi.bcast.Robj(obj = NULL, rank = 0, comm = 0)
        eps1_rest = mpi.bcast.Robj(obj = NULL, rank = 0, comm = 0)

        eps2_ov = mpi.bcast.Robj(obj = NULL, rank = 0, comm = 0)
        eps2_rest = mpi.bcast.Robj(obj = NULL, rank = 0, comm = 0)
        covMat = mpi.bcast.Robj(obj = NULL, rank = 0, comm = 0)
        R0_prev = mpi.bcast.Robj(obj = NULL, rank = 0, comm = 0)
        Reff_prev = mpi.bcast.Robj(obj = NULL, rank = 0, comm = 0)
      }        
    }
    
    r = r + 1
    number_complete = 0
    if(id == 0){
      resLast <- data.frame("R0" = R0s, "Reffs" = Reffs, "seeds" = seeds, "error1s_ov" = errors1_ov, "error2s_ov" = errors2_ov, "errors1_rest" = errors1_rest, "errors2_rest" = errors2_rest)
      write.table(resLast, file = paste(result_path, "R", r, ".txt", sep = ""))    
    }  
  }
  if(id == 0){
    resAll <- data.frame("R0_all" = R0_all, "Reff_all" = Reff_all,  "errors1_ov_all" = errors1_ov_all, "errors1_rest_all" = errors1_rest_all, "errors2_ov_all" = errors2_ov_all, "errors2_rest_all" = errors2_rest_all)
    write.table(resAll, file = paste(result_path, "resAll.txt", sep = ""))
  }
}

if(make_main_res){
  lastR <- read.table(paste(result_path, "R", R_calib, ".txt", sep = ""))
  npar = ncol(lastR) - 5
  # finn hvilke jeg skal kjore
  per_rank = floor(nrow(lastR)/(ns+1))
  leftover = nrow(lastR) - per_rank * (ns+1)
  leftover_start = nrow(lastR) - leftover + 1
  my_indices = id * per_rank + seq(1:(per_rank))
  if(id < leftover){
    my_indices = c(my_indices, leftover_start + id)
  }
  
  res = list()
  for (i in my_indices){
    res[[length(res) + 1]] = run_model_fylke(as.numeric(lastR[i, 1:npar]), rn = lastR[i,npar + 1],longrun=TRUE)
  }
  if (id != 0){
    print("Sending my result")
    mpi.send.Robj(res, dest = 0, tag = as.integer(300 + id), comm = 0)
    print("Done sending my result")
  }
  if(id == 0){
    print("Without importations")
    results = list()
    results = append(results, res)
    for (j in 1:ns){
      res =  mpi.recv.Robj(source = j, tag = as.integer(300 + j), comm = 0)
      results = append(results, res)
    }
    results <- unlist(results, recursive = FALSE)
    saveRDS(results, paste(result_path,"main_res.RDS", sep = ""))
  }
  
  mpi.barrier(comm = 0)  
}
#mpi.close.Rslaves(dellog = FALSE)
mpi.quit()
