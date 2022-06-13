.libPaths("/storage/R")
options(encoding = "iso-8859-1")
org::initialize_project(
  home = c(
    "home_folder"
  ),
  results = c(
    "resutls_folder"
  ),
  data_raw = c(
    "raw_data_folder"
  ),
  create_folders = TRUE
)


library(dplyr)


latest_data <- fread("path_to_hosp_data.csv")
latest_data[, date:=as.Date(date)]

latest_data_cases <- fread("path_to_test_data.csv")
latest_data_cases[, date:=as.Date(ProveDato)]

latest_data_cases[, county_code:= paste0("county", stringr::str_pad(FylkeNr, width=2, pad="0"))]
latest_data <- latest_data_cases %>% left_join(latest_data, by=c("county_code"="county_code", "date"="date"))

latest_data[is.na(Positive), Positive:=0]
latest_data[is.na(antall_nye), antall_nye:=0]
latest_data <- latest_data[county_code !="county21"]

datas <- list(
  "march"=list(
    start_date=as.Date("2021-03-01"),
    national="national_march",
    national_only_hosp="national_results_only_hosp",
    regional="regional_march",
    regional_only_hosp="regional_only_hosp",
    baseline="include"
  ),
  "april"=list(
    start_date=as.Date("2021-04-01"),
    national="national_apr.RDS",
    regional="regional_apr.RDS",
    baseline="include"
    ),
  "nov"=list(
    start_date=as.Date("2020-11-01"),
    national="national_nov.RDS",
    regional="regional_nov.RDS",
    baseline="include"
#    baseline_trend="include"
    ),
  "oct"=list(
    start_date=as.Date("2020-10-01"),
    national="national_oct.RDS",
    regional="regional_oct.RDS",
    baseline="include"
#    baseline_trend="include"
    ),
  "sep"=list(
    start_date=as.Date("2020-09-01"),
    national="national_sep.RDS",
    regional="regional_sep.RDS",
    baseline="include"
#    baseline_trend="include"
    )
)


folder <- org::project$results_today

calc_scores <- function(sim_c, data,nat_sim, out, sim_var, data_var, label, xweek, plot=F){
  n_sims <- length(unique(sim_c$sim))
  a <- dcast(sim_c, county_code~sim, value.var=sim_var)
  data <- data[week==xweek, sum(get(data_var)), by=county_code][order(county_code)]
  predictions <- as.matrix(a[, 2:(n_sims+1)])

  lims_50 <- matrixStats::rowQuantiles(predictions, prob=c(0.25, 0.75))
  c_50 <- data$V1 >= lims_50[,1] & data$V1 <= lims_50[, 2]
  
  lims_95 <- matrixStats::rowQuantiles(predictions, prob=c(0.025, 0.975))
  c_95 <- data$V1 >= lims_95[,1] & data$V1 <= lims_95[, 2]

  nat_pred <- nat_sim[, sum(get(sim_var)), by=sim][, V1]
  nat_data <- data[, sum(V1)]
  if(plot){
    plot_dt <- data.table(matrixStats::rowQuantiles(predictions, prob=c(0.025, 0.5, 0.975)))
    plot_dt <- cbind(plot_dt, data$V1)
    colnames(plot_dt) <- c("min", "med", "max", "data")
    quant <- quantile(nat_pred, probs=c(0.025, 0.5, 0.975))
    plot_dt[, county:=fhidata::norway_locations_long_b2020[2:12, location_name]]
    plot_dt <- rbind(plot_dt,
                     data.table(min=quant[1], med=quant[2], max=quant[3],
                                data=nat_data, county="National"))
    q <- ggplot(plot_dt) + geom_point(aes(x=county, y=data), color="red") +  geom_linerange(aes(x=county, ymin=min, ymax=max))
    ggsave(plot=q, filename=glue::glue("{folder}/{label}_week_{xweek}.png"), width=10)
  }
  out <- list(
    data.table(
      type=label,
      variable="county_coverage_50",
      week=xweek+1,
      value=sum(c_50)/length(c_50)
    ),
  data.table(
      type=label,
      variable="county_coverage_95",
      week=xweek+1,
      value=sum(c_95)/length(c_95)
    ),
  data.table(
      type=label,
      variable="county_ES",
      week=xweek+1,
      value=scoringRules::es_sample(data$V1, predictions)
    ),
  data.table(
      type=label,
      variable="nat_crps",
      week=xweek+1,
      value=scoringRules::crps_sample(nat_data, nat_pred)
  ))

  for(xcounty_code in unique(data[, county_code])){
    county_data <- data[county_code==xcounty_code, sum(V1)]
    county_pred <- sim_c[county_code==xcounty_code, get(sim_var)]
    out[[length(out) + 1]] <- data.table(
      type=label,
      variable=paste0(xcounty_code,"_crps"),
      week=xweek+1,
      value=scoringRules::crps_sample(county_data, county_pred)
    )
  }


  
  return(rbindlist(out))
}

calculate_validation <- function(sims, latest_data, xdate){
  incidence_day <- sims[date > xdate & date <= xdate + 20, .(hosp_inc=sum(hosp_inc),
                                                             tested=sum(tested)),
                             by=.(county_code, date, sim)]
  incidence_day[, week:= as.numeric((date-xdate), units="days") %/% 7]
  incidence <- incidence_day[, .(hosp_inc=sum(hosp_inc), tested=sum(tested)),
                                  by=.(county_code, week, sim)]
  latest_data_weeks <- latest_data[date > xdate & date <= xdate + 20]
  latest_data_weeks[, week:= as.numeric((date-xdate), units="days") %/% 7]

  ## National crps score
  nat <- sims[date > xdate & date <= xdate + 20, .(hosp_inc=sum(hosp_inc),
                                                   tested=sum(tested)), by=.(date, sim)]
  nat[, week:= as.numeric((date-xdate), units="days") %/% 7]


  out <- data.table()


  ## Weekly energy scores
  out <- list()
  for(xweek in c(0, 1, 2)){

    out[[length(out) +1]] <- calc_scores(incidence[week==xweek],
                                         latest_data_weeks[week==xweek],
                                         nat[week==xweek],
                                         out, "hosp_inc", "antall_nye", "hosp", xweek)
    out[[length(out) +1]] <- calc_scores(incidence[week==xweek],
                latest_data_weeks[week==xweek],
                nat[week==xweek],
                out, "tested", "Positive", "tested", xweek)


  }
  

  return(rbindlist(out))
}




predict_baseline <- function(data, n_days=21, N=500, include_time=FALSE, n_predict=21){
  n <-nrow(data)
  fit <- data[(n- n_days+1):n]
  fit[, day:=1:n_days]

  if(sum(fit$I)==0) fit[1, I:=1]
  
  if(include_time){
    model <- tryCatch({
      MASS::glm.nb(I ~ day, data=fit)
    }, error=function(cond){
      print("Use poisson")
      glm(I ~ day, data=fit, family="poisson")
    })
  }else{
    model <- tryCatch({
      MASS::glm.nb(I ~ 1, data=fit)
    }, error=function(cond){
      print("Use poisson")
      glm(I ~ 1, data=fit, family="poisson")
    })
  }
  predict_days=(n_days+1):(n_days + n_predict)
  intercept <- coef(model)[1]
  intercept_se <- summary(model)$coefficients[1,2]
  intercept_samples <- rnorm(N, mean=intercept, sd=intercept_se)
  if(include_time){
    beta_t <- coef(model)[2]
    beta_se <- summary(model)$coefficients[2,2]
    beta_t_samples <- rnorm(N, mean=beta_t, sd=beta_se)
  }else{
    beta_t_samples <- rep(0, N)
    
  }
  mu_values <- exp(intercept_samples + outer(beta_t_samples,predict_days))
  mu_values[mu_values > 1000] <- 1000
  if("family" %in% names(model$call)){
    predictions <- rpois(N*n_predict, lambda=mu_values)
  }else{
    predictions <- rnbinom(N*n_predict, mu=mu_values, size=model$theta)
  }
  preds <- data.table(inc=predictions, t=rep(1:n_predict, each=N), sim=rep(1:N, n_predict))[order(sim, t)]
  return(preds)
}


generate_baseline <- function(xdate, latest_data, n_trend=21, n_predict=21, include_time=TRUE){
  out <- list()
  fit_data <- latest_data[date <xdate]
  for(xcounty in unique(fit_data$county_code)){
    out[[length(out) +1]] <- cbind(predict_baseline(fit_data[county_code==xcounty] %>% mutate(I=antall_nye), include_time=include_time)
                                   %>% select(hosp_inc=inc),
                 predict_baseline(fit_data[county_code==xcounty] %>% mutate(I=Positive), include_time=include_time)%>% mutate(tested=inc)) %>% mutate(county_code=xcounty)
  }
  df <- rbindlist(out)
  df[, date:=t + xdate -1]
}



results <- list()
for(lab in names(datas)){
  all_data <- list()
  for(xtype in names(datas[[lab]])){
    start_date <- datas[[lab]][["start_date"]]
    if(xtype!="start_date"){
      print(xtype)
      if(xtype =="baseline"){
        data <- generate_baseline(start_date, latest_data, n_predict=21, n_trend=21, include_time=FALSE)
      }else if(xtype =="baseline_trend"){
        data <- generate_baseline(start_date, latest_data, n_predict=21, n_trend=21, include_time=TRUE)
      }else{
        data <- to_dt(readRDS(datas[[lab]][[xtype]]))
        data <- fhidata::norway_locations_long_b2020[data, on=c("location_code"="location_code")]
        data[, county_code:=location_code]
      }
      all_data[[xtype]] <- data
      results[[length(results) + 1]] <- calculate_validation(data, latest_data, datas[[lab]][["start_date"]]) %>% mutate(model=xtype, time=lab)
    }
  }
}
dt_results <- rbindlist(results)


selected_results <- dt_results[model %in% c("national", "regional", "baseline") & ! grepl("county[0123456789][0123456789]_crps", variable), .(value=round(mean(value),2)), by=.(type, variable, week, model)]



table_df <- dcast(selected_results, variable +type ~model + week)[order(type)][, .(variable, national_1, national_2, national_3, regional_1, regional_2, regional_3, baseline_1, baseline_2, baseline_3)][c(1,4,2,3,5,8,6,7)]

table_df$variable <- rep(c("County Energy Score", "National CRPS", "Coverage 50% intervals",
                           "Coverage 95% intervals"),2)

kable(table_df, format="latex", col.names=c("","Week 1", "Week 2", "Week 3", "Week 1", "Week 2", "Week 3", "Week 1", "Week 2", "Week 3"), booktabs=T) %>%kable_styling() %>% pack_rows("Hospital", 1,4) %>% pack_rows("Confirmed Cases", 5, 8) %>% add_header_above(c("", "National"=3, "Regional"=3, "Regional Baseline"=3))


selected_results <- selected_results %>% mutate(type = recode(type, "hosp"="Hospital Admissions",
                                                              "tested"="Confimed Cases"),
                                                model = recode(model, regional="Regional", national="National", baseline = "Regional Baseline"),
                                                variable = recode(variable,
                                                                  "county_coverage_50" = "Coverage 50% intervals",
                                                                  "county_coverage_95" = "Coverage 95% intervals",
                                                                  "county_ES" = "County Energy Score",
                                                                  "nat_crps" = "National CRPS")
                  )
                                                

ggplot(selected_results) + geom_line(aes(x=week, y=value, color=model, group=model), size=2) + facet_wrap(type~variable, scales = "free", ncol=4) + fhiplot::theme_fhi_lines() + xlab("Week") + ylab("Score") + theme(text = element_text(size=20))+ labs(color ="Model")  + scale_x_continuous(breaks= c(1,2,3))

ggsave( filename=glue::glue("figure_validation.png"), width=18, height=10)




