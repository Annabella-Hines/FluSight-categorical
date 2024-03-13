# Categorical functions 

#########################################
#pairwise comparison function 
#########################################
pairwise_comparison <- function(scores, score_type = "rps", mx, my, subset = rep(TRUE, nrow(scores)),
                                permutation_test = FALSE){
  
  scores <- scores[subset, ]  # this should subset for locations and horizons as needed, the subset df should go in the call to the function
 
  # this should set the values pulled to the specified score type (rps or bs) default is ranked probability score
   s_value <- ifelse(score_type %in% c("rps", "RPS", "rps_value", "r", "rp", "rpss", "R", "RP", "RPSS"), "rps_value", "b_value")
  
  
  # subsets of available scores for both models:
  subx <- subset(scores, model_id == mx)
  suby <- subset(scores, model_id == my)
  # merge together and restrict to overlap:
  sub <- merge(subx, suby, by = c("timezero", "location", "horizon"),
               all.x = FALSE, all.y = FALSE)
  ##### catch common problems:
  ##### no overlap between targets covered by x and y:
  if(nrow(sub) == 0){
    warning("No overlap of covered forecast targets for ", mx, "and", my, ". Returning NA.")
    return(list(ratio = NA, pval = NA, pval_fcd = NA, mx = mx, my = my))
  }
  ##### unavailable scores (likely because a model issues only point forecasts?)
  if(any(is.na(subx[[s_value]]))){
    warning("Some or all  values are NA for ", mx, ". Returning NA.")
    return(list(ratio = NA, pval = NA, pval_fcd = NA, mx = mx, my = my))
  }
  if(any(is.na(suby[[s_value]]))){
    warning("Some or all  values are NA for ", my, ". Returning NA.")
    return(list(ratio = NA, pval = NA, pval_fcd = NA, mx = mx, my = my))
  }
  
  # compute ratio:
  
  # matrices to store:
  results_ratio <- results_pval <- results_pval_fcd <- matrix(ncol = length(models),
                                                              nrow = length(models),
                                                              dimnames = list(models, models))
  
  ratio <- sum(sub[[paste0(rlang::as_name(s_value),".x")]]) / sum(sub[[paste0(rlang::as_name(s_value),".y")]])
  
  # perform permutation tests:
  if(permutation_test){
    pval <- permutationTest(sub[[paste0(rlang::as_name(s_value),".x")]], sub[[paste0(rlang::as_name(s_value),".y")]],
                            nPermutation = 999)$pVal.permut
    ##### aggregate by forecast date:
    sub_fcd <- aggregate(cbind(paste0(rlang::as_name(s_value),".x"), paste0(rlang::as_name(s_value),".y")) ~ timezero, data = sub, FUN = mean)
    # catch error if too many observations
    if(nrow(sub_fcd) > 5){
      pval_fcd <- permutationTest(sub_fcd[[paste0(rlang::as_name(s_value),".x")]], sub_fcd[[paste0(rlang::as_name(s_value),".y")]],
                                  nPermutation = 999)$pVal.permut
    }else{
      warning("Too few observations to compute p-z for ", mx, " and ", my, " with aggregation by forecast date. Returning NA.")
      pval_fcd <- NA
    }
  }else{
    pval <- NULL
    pval_fcd <- NULL
  }
  return(list(ratio = ratio, pval = pval, pval_fcd = pval_fcd, mx = mx, my = my))
}


#########################################
#Plot relative brier by location 
#########################################
#select relevant columns:
plot_by_location <- function(df, score_type = "rps", order, location_order) {
  
  scores<-df
  s_value <- ifelse(score_type %in% c("rps", "RPS", "rps_value", "r", "rp", "rpss", "R", "RP", "RPSS"), "rps_value", "b_value")
  
  # the included models and locations:
  models <- unique(df$model_id)
  locations <- unique(df$location)
  location_names <- unique(df$location_name)
  
  # df initialization 
  average_by_loc <- NULL
  
  # compute pairwise and relative brier for each location separately:
  for(i in seq_along(locations)){
    
    # select location:
    loc <- locations[i]
    loc_name <- location_names[i]
    
    # matrix to store:
    results_ratio_temp <- matrix(ncol = length(models),
                                 nrow = length(models),
                                 dimnames = list(models, models))
    
    # run pairwise comparison for chosen location:
    for(mx in seq_along(models)){
      for(my in 1:mx){
        pwc <- pairwise_comparison(scores = scores, score_type = s_value, mx = models[mx], my = models[my],
                                            permutation_test = FALSE, # disable permutation test to speed up things
                                            subset = scores$location == loc) # this will subset to the respective location inside the function
        results_ratio_temp[mx, my] <- pwc$ratio
        results_ratio_temp[my, mx] <- 1/pwc$ratio
      }
    }
    
    # compute the geometric means etc
    ind_baseline <- which(rownames(results_ratio_temp) == "FluSight-equal_cat")
    geom_mean_ratios_temp <- exp(rowMeans(log(results_ratio_temp[, -ind_baseline]), na.rm = TRUE))
    ratios_baseline_temp <- results_ratio_temp[, "FluSight-equal_cat"]
    ratios_baseline2_temp <- geom_mean_ratios_temp/geom_mean_ratios_temp["FluSight-equal_cat"]
    ss <- 1 - ratios_baseline2_temp
    exp_ss <- exp(ss)
    
    # summarize results:
    to_add <- data.frame(model_id= names(ratios_baseline2_temp),
                         location = loc,
                         location_name = loc_name,
                         relative_score = ratios_baseline2_temp,
                         log_relative_brier = log(ratios_baseline2_temp), 
                         ss = ss, 
                         exp_ss = exp_ss)
    
    # append to already stored:
 
      average_by_loc <- bind_rows(average_by_loc, to_add)

    
    # cat("Finished", loc_name, "\n")
  }
  
  average_by_loc_to_plot <- average_by_loc %>%
    filter(location_name != "American Samoa" & location_name != "Northern Mariana Islands") %>%
    mutate(relative_score_text = sprintf("%.1f", round(ss, 1)),
           log_relative_score = exp_ss) %>%
    filter(!is.na(ss)) %>%
    mutate(model_id= fct_relevel(model_id, order))
  
  
  
  # plot:
  ggplot(average_by_loc_to_plot,
         aes(x=model_id, y=location_name,
            # fill= scales::oob_squish(log_relative_brier, range = c(- 2.584963, 1)))) +
            fill = exp_ss)) +
    geom_tile() +
    geom_text(aes(label = relative_score_text), size = 2.5) +
    scale_fill_gradient2(low = "#3BBBB0", high = "#C13897", midpoint = 1, na.value = "grey50",
                         name = "Skill Score",
                         breaks = c(0,1, max(average_by_loc_to_plot$exp_ss, na.rm =TRUE)), # I don't feel great about these breaks/labels, but they are okay for now. 
                         labels =c("Negative", 0, 1)
                         ) +
    xlab(NULL) + ylab(NULL) +
    scale_y_discrete(limits = rev)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.title.x = element_text(size = 9),
          axis.text.y = element_text(size = 9),
          title = element_text(size = 9))
}


###############################################
# filter for inclusion in recent accuracy table
###############################################
##Keep only models that have submitted forecasts for at least half of the number of max BRIER forecasts
recent_accuracy_filter_b <- function(x,y) {
  x %>%
    filter(!is.na(b_value)) %>%  #remove NAs
    group_by(model_id) %>%
    mutate(n_forecasts = sum(!is.na(b_value))) %>%
    ungroup() %>%
   # filter(n_forecasts >= (max(n_forecasts)*0.5)) %>% ###Dropping this for now because we want to see all models
    droplevels()
}

###############################################
# pairwise comparison by horizon function 
###############################################

#select relevant columns:
tbl_by_horizon <- function(df, score_type = "rps") {
  
  s_value <- ifelse(score_type %in% c("rps", "RPS", "rps_value", "rpss", "RPSS"), "rps", "bs")
  
  scores<-df
  
  # the included models and locations:
  models <- unique(df$model_id)
  horizons <- unique(df$horizon)
  
  average_by_h <- NULL #initialize df for results
  
  # compute pairwise and relative WIS for each horizon separately:
  for(i in seq_along(horizons)){
    
    # select horizon:
    h <- horizons[i]
    
    # matrix to store:
    results_ratio_temp <- matrix(ncol = length(models),
                                 nrow = length(models),
                                 dimnames = list(models, models))
    
    # run pairwise comparison for chosen horizon:
    for(mx in seq_along(models)){
      for(my in 1:mx){
        pwc <- pairwise_comparison(scores = scores, score_type = s_value, mx = models[mx], my = models[my],
                                           permutation_test = FALSE, # disable permutation test to speed up things
                                           subset = scores$horizon == h) # this will subset to the respective horizon inside the function
        results_ratio_temp[mx, my] <- pwc$ratio
        results_ratio_temp[my, mx] <- 1/pwc$ratio
      }
    }
    
    # compute the geometric means etc
    ind_baseline <- which(rownames(results_ratio_temp) == "FluSight-equal_cat")
    geom_mean_ratios_temp <- exp(rowMeans(log(results_ratio_temp[, -ind_baseline]), na.rm = TRUE))
    ratios_baseline_temp <- results_ratio_temp[, "FluSight-equal_cat"]
    ratios_baseline2_temp <- geom_mean_ratios_temp/geom_mean_ratios_temp["FluSight-equal_cat"]
    
    # summarize results:
    to_add <- data.frame(model_id= names(ratios_baseline2_temp),
                         horizon = h,
                         relative_score = ratios_baseline2_temp,
                         log_relative_score = log(ratios_baseline2_temp), 
                         ss = 1-ratios_baseline2_temp) #skill score calculation here
    
    # append to already stored:

      average_by_h <- bind_rows(average_by_h, to_add)

    
    # cat("Finished", loc_name, "\n")
  }
  
  horizon_df <- average_by_h %>% pivot_wider(id_cols = model_id, names_from = horizon, names_prefix = paste0(s_value,"s_h_"), values_from = ss)
  return(horizon_df)  
}

