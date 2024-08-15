library(data.table)
library(bit64)
library(magrittr)
library(tidyverse)
library(Hmisc)

getmode <- function(v) {
  
  #----------------------------------------------------------------------------
  # Gets the most frequent value in a vector
  # 
  # Parameters:
  #   - v (vec)           : Vector of values, independent of type
  #
  # Returns: The most frequent value.
  #----------------------------------------------------------------------------
  
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

build_annual_sipp = function(year, occ = F, write = T, out = F) {
  
  
  #----------------------------------------------------------------------------
  # Constructs an annual panel of SIPP data for the given year. 
  # 
  # Parameters:
  #   - year (int)  : SIPP survey year to annualize. Lagged 1 year from time.
  #   - occ  (bool) : Flag to generate a single occupation from all of the person's
  #                        various occupations during the year.
  #
  # Returns: NULL. Writes the annual panel to data storage.
  #          Values include: Sex, age, marital status, race, educational attainment
  #                          number of dependents, number of young dependents,
  #                          whether the person is claimed as dependent,
  #                          total income, total wages/earnings, total tips,
  #                          tips as a percentage of total earnings.
  #           If occ=T, also includes occupation and industry.
  #----------------------------------------------------------------------------
  
  pu = file.path('/gpfs/gibbs/project/sarin/shared/raw_data/SIPP', year, paste0('pu',year,'.csv')) %>%
    fread(., sep = '|', select = c(
      # Select survey variables
      'SSUID','PNUM','MONTHCODE','ERESIDENCEID','ERELRPE','SPANEL','SWAVE',
      
      'WPFINWGT',
      
      # Select Demographic variables
      'ESEX','TAGE','TAGE_EHC','ERACE','EORIGIN','EEDUC', 'EDEPCLM', 'EMS', 'EFSTATUS',
      
      # Income variables for each listed occupation
      'TJB1_TXAMT', 'TJB1_MSUM', 'TJB1_OCC', 'TJB1_IND', 'AJB1_TXAMT', 'EJB1_TYPPAY3',
      'TJB2_TXAMT', 'TJB2_MSUM', 'TJB2_OCC', 'TJB2_IND', 'AJB2_TXAMT', 'EJB2_TYPPAY3',
      'TJB3_TXAMT', 'TJB3_MSUM', 'TJB3_OCC', 'TJB3_IND', 'AJB3_TXAMT', 'EJB3_TYPPAY3',
      'TJB4_TXAMT', 'TJB4_MSUM', 'TJB4_OCC', 'TJB4_IND', 'AJB4_TXAMT', 'EJB4_TYPPAY3',
      
      'TPTOTINC' #, 'EEITC', 'AEITC'
    ))
  
  names(pu) = toupper(names(pu))
  
  data = 
    pu %>%
    rename(
      INC  = TPTOTINC,
      AJB1 = AJB1_TXAMT,
      AJB2 = AJB2_TXAMT,
      AJB3 = AJB3_TXAMT,
      AJB4 = AJB4_TXAMT
    ) %>%
    mutate(
      u_ID = paste0(SSUID, PNUM), # Unique Person ID
      TIPS = rowSums(select(., contains("TXAMT")), na.rm = T),
      t_flag = TIPS > 0,
      i_flag = rowSums(ifelse(select(., contains("AJB")) > 1, 1, 0)),
      i_flag = i_flag > 0,
      EARN_SUM = rowSums(select(., contains("MSUM")), na.rm = T),
      TIPS_pct = TIPS / EARN_SUM,
      across(contains('TYPPAY'), ~ .x %% 2)
      #eitc = if_else(is.na(EEITC), 0 , EEITC %% 2)
    ) %>%
    group_by(SSUID) %>%
    mutate(
      child_count = sum(if_else(TAGE < 18 & MONTHCODE == 12, 1, 0)),
      young_child_count = sum(if_else(TAGE < 6 & MONTHCODE == 12, 1, 0))
    ) %>%
    ungroup()
  
  an_data = data %>%
    group_by(u_ID) %>%
    reframe(
      year = year,
      sex = getmode(ESEX),
      age = max(TAGE),
      marriage = getmode(EMS),
      race = getmode(ERACE),
      educ = max(EEDUC),
      n_dep = getmode(child_count),
      n_young_dep = getmode(young_child_count),
      is_dep = getmode(EDEPCLM),
      is_dep = if_else(is.na(is_dep) | is_dep == 2, F, T),
      #eitc = sum(eitc) > 1,
      #eitc_iflag = getmode(AEITC),
      
      
      weight = sum(WPFINWGT) / 12,
      inc_tot = sum(INC) / 12,
      inc_tip = sum(TIPS),
      tipped = sum(t_flag) > 0,
      inc_earn = sum(EARN_SUM),
      tips_pct = inc_tip / inc_earn,
      tips_imputed = sum(i_flag) > 0
    )  %>%
    filter(age > 17) 
  
  if(occ) {
    an_data %<>%
      mutate(
        n_jobs = length(unique(TJB1_OCC, incomparables = T)),
        occ1_t = sum(EJB1_TYPPAY3, na.rm = T) > 0,
        occ1_1 = unique(TJB1_OCC)[1],
        occ1_2 = if_else(n_jobs > 1, unique(TJB1_OCC)[2], NA),
        occ1_3 = if_else(n_jobs > 2, unique(TJB1_OCC)[3], NA),
        occ1_1 = if_else(is.na(occ1_1) & !(is.na(occ1_2)), occ1_2, occ1_1),
        occ1_2 = if_else(occ1_1 == occ1_2, NA, occ1_2),
        
        n_inds = length(unique(TJB1_IND, incomparables = T)),
        ind1_t = sum(EJB1_TYPPAY3, na.rm = T) > 0,
        ind1_1 = unique(TJB1_IND)[1],
        ind1_2 = if_else(n_inds > 1, unique(TJB1_OCC)[2], NA),
        ind1_3 = if_else(n_inds > 2, unique(TJB1_OCC)[3], NA),
        ind1_1 = if_else(is.na(ind1_1) & !(is.na(ind1_2)), ind1_2, ind1_1),
        ind1_2 = if_else(ind1_1 == ind1_2, NA, ind1_2),
        
        n_jobs = length(unique(TJB2_OCC, incomparables = T)),
        occ2_t = sum(EJB2_TYPPAY3, na.rm = T) > 0,
        occ2_1 = unique(TJB2_OCC)[1],
        occ2_2 = if_else(n_jobs > 1, unique(TJB2_OCC)[2], NA),
        occ2_3 = if_else(n_jobs > 2, unique(TJB2_OCC)[3], NA),
        occ2_1 = if_else(is.na(occ2_1) & !(is.na(occ2_2)), occ2_2, occ2_1),
        occ2_2 = if_else(occ2_1 == occ2_2, NA, occ2_2),
        
        n_inds = length(unique(TJB2_IND, incomparables = T)),
        ind2_t = sum(EJB2_TYPPAY3, na.rm = T) > 0,
        ind2_1 = unique(TJB2_IND)[1],
        ind2_2 = if_else(n_inds > 1, unique(TJB2_OCC)[2], NA),
        ind2_3 = if_else(n_inds > 2, unique(TJB2_OCC)[3], NA),
        ind2_1 = if_else(is.na(ind2_1) & !(is.na(ind2_2)), ind2_2, ind2_1),
        ind2_2 = if_else(ind2_1 == ind2_2, NA, ind2_2),
        
        n_jobs = length(unique(TJB3_OCC, incomparables = T)),
        occ3_t = sum(EJB3_TYPPAY3, na.rm = T) > 0,
        occ3_1 = unique(TJB3_OCC)[1],
        occ3_2 = if_else(n_jobs > 1, unique(TJB3_OCC)[2], NA),
        occ3_3 = if_else(n_jobs > 2, unique(TJB3_OCC)[3], NA),
        occ3_1 = if_else(is.na(occ3_1) & !(is.na(occ3_2)), occ3_2, occ3_1),
        occ3_2 = if_else(occ3_1 == occ3_2, NA, occ3_2),
        
        n_inds = length(unique(TJB3_IND, incomparables = T)),
        ind3_t = sum(EJB3_TYPPAY3, na.rm = T) > 0,
        ind3_1 = unique(TJB3_IND)[1],
        ind3_2 = if_else(n_inds > 1, unique(TJB3_OCC)[2], NA),
        ind3_3 = if_else(n_inds > 2, unique(TJB3_OCC)[3], NA),
        ind3_1 = if_else(is.na(ind3_1) & !(is.na(ind3_2)), ind3_2, ind3_1),
        ind3_2 = if_else(ind3_1 == ind3_2, NA, ind3_2),
        
        n_jobs = length(unique(TJB4_OCC, incomparables = T)),
        occ4_t = sum(EJB4_TYPPAY3, na.rm = T) > 0,
        occ4_1 = unique(TJB4_OCC)[1],
        occ4_2 = if_else(n_jobs > 1, unique(TJB4_OCC)[2], NA),
        occ4_3 = if_else(n_jobs > 2, unique(TJB4_OCC)[3], NA),
        occ4_1 = if_else(is.na(occ4_1) & !(is.na(occ4_2)), occ4_2, occ4_1),
        occ4_2 = if_else(occ4_1 == occ4_2, NA, occ4_2),
        
        n_inds = length(unique(TJB4_IND, incomparables = T)),
        ind4_t = sum(EJB4_TYPPAY3, na.rm = T) > 0,
        ind4_1 = unique(TJB4_IND)[1],
        ind4_2 = if_else(n_inds > 1, unique(TJB4_OCC)[2], NA),
        ind4_3 = if_else(n_inds > 2, unique(TJB4_OCC)[3], NA),
        ind4_1 = if_else(is.na(ind4_1) & !(is.na(ind4_2)), ind4_2, ind4_1),
        ind4_2 = if_else(ind4_1 == ind4_2, NA, ind4_2),
        
        primary_occ = occ1_1,
        primary_occ_tipped = occ1_t,
        primary_ind = ind1_1,
        
        primary_occ = if_else(is.na(primary_occ) | (!primary_occ_tipped & occ2_t), occ2_1, primary_occ),
        primary_occ_tipped = if_else(is.na(primary_occ) | (!primary_occ_tipped & occ2_t), occ2_t, primary_occ_tipped),
        primary_ind = if_else(is.na(primary_ind) | (!primary_occ_tipped & occ2_t), ind2_1, primary_ind),
        
        primary_occ = if_else(is.na(primary_occ) | (!primary_occ_tipped & occ3_t), occ3_1, primary_occ),
        primary_occ_tipped = if_else(is.na(primary_occ) | (!primary_occ_tipped & occ3_t), occ3_t, primary_occ_tipped),
        primary_ind = if_else(is.na(primary_ind) | (!primary_occ_tipped & occ3_t), ind3_1, primary_ind),
        
        primary_occ = if_else(is.na(primary_occ) | (!primary_occ_tipped & occ4_t), occ4_1, primary_occ),
        primary_occ_tipped = if_else(is.na(primary_occ) | (!primary_occ_tipped & occ4_t), occ4_t, primary_occ_tipped),
        primary_ind = if_else(is.na(primary_ind) | (!primary_occ_tipped & occ4_t), ind3_1, primary_ind),
      ) %>%
      select(!contains(c('_1', '_2', '_3')) & !ends_with('_t') & !n_jobs)
  }
  
  if(write){
    an_data %>%
      write_csv(., file.path('/gpfs/gibbs/project/sarin/shared/raw_data/SIPP', year, 'annual_data.csv'))  
  }
  
  print(paste0('SIPP Year ', year, ' annualized'))
  
  if(out) {
    return(an_data)
  }
}

condense_industry = function(year, write = F, out = T) {
  
  pu = file.path('/gpfs/gibbs/project/sarin/shared/raw_data/SIPP', year, paste0('pu',year,'.csv')) %>%
    fread(., sep = '|', select = c(
      # Select survey variables
      'SSUID','PNUM','MONTHCODE','ERESIDENCEID','ERELRPE','SPANEL','SWAVE',
      
      'WPFINWGT',
      
      # Select Demographic variables
      'ESEX','TAGE','TAGE_EHC','ERACE','EORIGIN','EEDUC', 'EDEPCLM', 'EMS', 'EFSTATUS',
      
      # Income variables for each listed occupation
      'TJB1_TXAMT', 'TJB1_MSUM', 'TJB1_OCC', 'TJB1_IND', 'AJB1_TXAMT', 'EJB1_TYPPAY3',
      'TJB2_TXAMT', 'TJB2_MSUM', 'TJB2_OCC', 'TJB2_IND', 'AJB2_TXAMT', 'EJB2_TYPPAY3',
      'TJB3_TXAMT', 'TJB3_MSUM', 'TJB3_OCC', 'TJB3_IND', 'AJB3_TXAMT', 'EJB3_TYPPAY3',
      'TJB4_TXAMT', 'TJB4_MSUM', 'TJB4_OCC', 'TJB4_IND', 'AJB4_TXAMT', 'EJB4_TYPPAY3',
      
      'TPTOTINC' #, 'EEITC', 'AEITC'
    ))
  
  names(pu) = toupper(names(pu))
  
  data = 
    pu %>%
    rename(
      INC  = TPTOTINC,
      AJB1 = AJB1_TXAMT,
      AJB2 = AJB2_TXAMT,
      AJB3 = AJB3_TXAMT,
      AJB4 = AJB4_TXAMT
    ) %>%
    mutate(
      u_ID = paste0(SSUID, PNUM), # Unique Person ID
      TIPS = rowSums(select(., contains("TXAMT")), na.rm = T),
      t_flag = TIPS > 0,
      i_flag = rowSums(ifelse(select(., contains("AJB")) > 1, 1, 0)),
      i_flag = i_flag > 0,
      EARN_SUM = rowSums(select(., contains("MSUM")), na.rm = T),
      TIPS_pct = TIPS / EARN_SUM,
      across(contains('TYPPAY'), ~ .x %% 2)
      #eitc = if_else(is.na(EEITC), 0 , EEITC %% 2)
    ) %>%
    group_by(SSUID) %>%
    mutate(
      child_count = sum(if_else(TAGE < 18 & MONTHCODE == 12, 1, 0)),
      young_child_count = sum(if_else(TAGE < 6 & MONTHCODE == 12, 1, 0))
    ) %>%
    ungroup()
  
  an_data = data %>%
    filter(t_flag) %>%
    group_by(u_ID) %>%
    reframe(
      year = year,
      sex = getmode(ESEX),
      age = max(TAGE),
      marriage = getmode(EMS),
      race = getmode(ERACE),
      educ = max(EEDUC),
      n_dep = getmode(child_count),
      n_young_dep = getmode(young_child_count),
      is_dep = getmode(EDEPCLM),
      is_dep = if_else(is.na(is_dep) | is_dep == 2, F, T),
      #eitc = sum(eitc) > 1,
      #eitc_iflag = getmode(AEITC),
      
      weight = sum(WPFINWGT) / 12,
      inc_tot = sum(INC) / 12,
      inc_tip = sum(TIPS),
      tipped = sum(t_flag) > 0,
      inc_earn = sum(EARN_SUM),
      tips_pct = inc_tip / inc_earn,
      tips_imputed = sum(i_flag) > 0,
      
      n_jobs = length(unique(TJB1_OCC, incomparables = T)),
      occ1_t = sum(EJB1_TYPPAY3, na.rm = T) > 0,
      occ1_1 = unique(TJB1_OCC)[1],
      occ1_2 = if_else(n_jobs > 1, unique(TJB1_OCC)[2], NA),
      occ1_3 = if_else(n_jobs > 2, unique(TJB1_OCC)[3], NA),
      occ1_1 = if_else(is.na(occ1_1) & !(is.na(occ1_2)), occ1_2, occ1_1),
      occ1_2 = if_else(occ1_1 == occ1_2, NA, occ1_2),
      
      n_inds = length(unique(TJB1_IND, incomparables = T)),
      ind1_t = sum(EJB1_TYPPAY3, na.rm = T) > 0,
      ind1_1 = unique(TJB1_IND)[1],
      ind1_2 = if_else(n_inds > 1, unique(TJB1_OCC)[2], NA),
      ind1_3 = if_else(n_inds > 2, unique(TJB1_OCC)[3], NA),
      ind1_1 = if_else(is.na(ind1_1) & !(is.na(ind1_2)), ind1_2, ind1_1),
      ind1_2 = if_else(ind1_1 == ind1_2, NA, ind1_2),
      
      n_jobs = length(unique(TJB2_OCC, incomparables = T)),
      occ2_t = sum(EJB2_TYPPAY3, na.rm = T) > 0,
      occ2_1 = unique(TJB2_OCC)[1],
      occ2_2 = if_else(n_jobs > 1, unique(TJB2_OCC)[2], NA),
      occ2_3 = if_else(n_jobs > 2, unique(TJB2_OCC)[3], NA),
      occ2_1 = if_else(is.na(occ2_1) & !(is.na(occ2_2)), occ2_2, occ2_1),
      occ2_2 = if_else(occ2_1 == occ2_2, NA, occ2_2),
      
      n_inds = length(unique(TJB2_IND, incomparables = T)),
      ind2_t = sum(EJB2_TYPPAY3, na.rm = T) > 0,
      ind2_1 = unique(TJB2_IND)[1],
      ind2_2 = if_else(n_inds > 1, unique(TJB2_OCC)[2], NA),
      ind2_3 = if_else(n_inds > 2, unique(TJB2_OCC)[3], NA),
      ind2_1 = if_else(is.na(ind2_1) & !(is.na(ind2_2)), ind2_2, ind2_1),
      ind2_2 = if_else(ind2_1 == ind2_2, NA, ind2_2),
      
      n_jobs = length(unique(TJB3_OCC, incomparables = T)),
      occ3_t = sum(EJB3_TYPPAY3, na.rm = T) > 0,
      occ3_1 = unique(TJB3_OCC)[1],
      occ3_2 = if_else(n_jobs > 1, unique(TJB3_OCC)[2], NA),
      occ3_3 = if_else(n_jobs > 2, unique(TJB3_OCC)[3], NA),
      occ3_1 = if_else(is.na(occ3_1) & !(is.na(occ3_2)), occ3_2, occ3_1),
      occ3_2 = if_else(occ3_1 == occ3_2, NA, occ3_2),
      
      n_inds = length(unique(TJB3_IND, incomparables = T)),
      ind3_t = sum(EJB3_TYPPAY3, na.rm = T) > 0,
      ind3_1 = unique(TJB3_IND)[1],
      ind3_2 = if_else(n_inds > 1, unique(TJB3_OCC)[2], NA),
      ind3_3 = if_else(n_inds > 2, unique(TJB3_OCC)[3], NA),
      ind3_1 = if_else(is.na(ind3_1) & !(is.na(ind3_2)), ind3_2, ind3_1),
      ind3_2 = if_else(ind3_1 == ind3_2, NA, ind3_2),
      
      n_jobs = length(unique(TJB4_OCC, incomparables = T)),
      occ4_t = sum(EJB4_TYPPAY3, na.rm = T) > 0,
      occ4_1 = unique(TJB4_OCC)[1],
      occ4_2 = if_else(n_jobs > 1, unique(TJB4_OCC)[2], NA),
      occ4_3 = if_else(n_jobs > 2, unique(TJB4_OCC)[3], NA),
      occ4_1 = if_else(is.na(occ4_1) & !(is.na(occ4_2)), occ4_2, occ4_1),
      occ4_2 = if_else(occ4_1 == occ4_2, NA, occ4_2),
      
      n_inds = length(unique(TJB4_IND, incomparables = T)),
      ind4_t = sum(EJB4_TYPPAY3, na.rm = T) > 0,
      ind4_1 = unique(TJB4_IND)[1],
      ind4_2 = if_else(n_inds > 1, unique(TJB4_OCC)[2], NA),
      ind4_3 = if_else(n_inds > 2, unique(TJB4_OCC)[3], NA),
      ind4_1 = if_else(is.na(ind4_1) & !(is.na(ind4_2)), ind4_2, ind4_1),
      ind4_2 = if_else(ind4_1 == ind4_2, NA, ind4_2),
      
      primary_occ = occ1_1,
      primary_occ_tipped = occ1_t,
      primary_ind = ind1_1,
      
      primary_occ = if_else(is.na(primary_occ) | (!primary_occ_tipped & occ2_t), occ2_1, primary_occ),
      primary_occ_tipped = if_else(is.na(primary_occ) | (!primary_occ_tipped & occ2_t), occ2_t, primary_occ_tipped),
      primary_ind = if_else(is.na(primary_ind) | (!primary_occ_tipped & occ2_t), ind2_1, primary_ind),
      
      primary_occ = if_else(is.na(primary_occ) | (!primary_occ_tipped & occ3_t), occ3_1, primary_occ),
      primary_occ_tipped = if_else(is.na(primary_occ) | (!primary_occ_tipped & occ3_t), occ3_t, primary_occ_tipped),
      primary_ind = if_else(is.na(primary_ind) | (!primary_occ_tipped & occ3_t), ind3_1, primary_ind),
      
      primary_occ = if_else(is.na(primary_occ) | (!primary_occ_tipped & occ4_t), occ4_1, primary_occ),
      primary_occ_tipped = if_else(is.na(primary_occ) | (!primary_occ_tipped & occ4_t), occ4_t, primary_occ_tipped),
      primary_ind = if_else(is.na(primary_ind) | (!primary_occ_tipped & occ4_t), ind3_1, primary_ind),
    ) %>%
    select(!n_jobs)  %>%
    filter(age > 17) %>%
    return()
}


df = build_annual_sipp(2023, write=F, out=T)

df = c(2018:2020, 2023) %>%
  map(.f = ~ build_annual_sipp(.x, write = F, out = T)) %>%
  bind_rows()

df_ind = c(2018:2020, 2023) %>%
  map(.f = ~ condense_industry(.x, write = F, out = T)) %>%
  bind_rows()

