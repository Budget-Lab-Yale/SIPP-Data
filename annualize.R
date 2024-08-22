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

build_annual_sipp = function(year, write = F, out = T) {
  
  pu = file.path('/gpfs/gibbs/project/sarin/shared/raw_data/SIPP', year, paste0('pu',year,'.csv')) %>%
    fread(., sep = '|', select = c(
      # Select survey variables
      'SSUID','PNUM','MONTHCODE','ERESIDENCEID','ERELRPE','SPANEL','SWAVE',
      
      'WPFINWGT',
      
      # Select Demographic variables
      'ESEX','TAGE','TAGE_EHC','ERACE','EORIGIN','EEDUC', 'EDEPCLM', 'EMS', 'EFSTATUS',
      
      # Income variables for each listed occupation
      'TJB1_TXAMT', 'TJB1_MSUM', 'TJB1_OCC', 'TJB1_IND', 'AJB1_TXAMT', 'EJB1_TYPPAY3', 'EJB1_CLWRK',
      'TJB2_TXAMT', 'TJB2_MSUM', 'TJB2_OCC', 'TJB2_IND', 'AJB2_TXAMT', 'EJB2_TYPPAY3', 'EJB2_CLWRK',
      'TJB3_TXAMT', 'TJB3_MSUM', 'TJB3_OCC', 'TJB3_IND', 'AJB3_TXAMT', 'EJB3_TYPPAY3', 'EJB3_CLWRK',
      'TJB4_TXAMT', 'TJB4_MSUM', 'TJB4_OCC', 'TJB4_IND', 'AJB4_TXAMT', 'EJB4_TYPPAY3', 'EJB4_CLWRK',
      
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
      across(contains("TXAMT"), ~ if_else(is.na(.x), 0, .x)),
      TIPS = rowSums(select(., contains("TXAMT")), na.rm = T),
      t_flag = TIPS > 0,
      i_flag = rowSums(ifelse(select(., contains("AJB")) > 1, 1, 0)),
      i_flag = i_flag > 0,
      
      across(contains('CLWRK'), ~.x > 6),
      across(contains('TYPPAY'), ~ .x %% 2),
      
      IND_1_t = TJB1_TXAMT > 0,
      IND_2_t = TJB2_TXAMT > 0,
      IND_3_t = TJB3_TXAMT > 0,
      IND_4_t = TJB4_TXAMT > 0
      #eitc = if_else(is.na(EEITC), 0 , EEITC %% 2)
    ) %>%
    group_by(SSUID) %>%
    mutate(
      child_count = sum(if_else(TAGE < 18 & MONTHCODE == 12, 1, 0)),
      young_child_count = sum(if_else(TAGE < 6 & MONTHCODE == 12, 1, 0))
    ) %>%
    ungroup() %>%
    filter(TAGE > 17)
  
  data %<>% left_join(
    1:4 %>%
      map(
        .f = ~ data  %>%
          mutate(
            "Wages_{{.x}}" := !!sym(paste0('TJB',.x,'_MSUM')) * (!!sym(paste0('EJB',.x,'_CLWRK')) == 0),
            "W_Tip_{{.x}}" := !!sym(paste0('TJB',.x,'_TXAMT')) * (!!sym(paste0('EJB',.x,'_CLWRK')) == 0),
            "Self_{{.x}}" := !!sym(paste0('TJB',.x,'_MSUM')) * (!!sym(paste0('EJB',.x,'_CLWRK'))),
            "S_Tip_{{.x}}" := !!sym(paste0('TJB',.x,'_TXAMT')) * (!!sym(paste0('EJB',.x,'_CLWRK')))
          ) %>%
          select(u_ID, MONTHCODE, contains(c("Wages", "W_T", "Self", "S_T")))
      ) %>%
      bind_cols() %>%
      rename(u_ID = `u_ID...1`,
             MONTHCODE = `MONTHCODE...2`) %>%
      select(!contains("...")) %>%
      replace(is.na(.), 0) %>%
      mutate(
        WAGES_SUM = rowSums(select(., contains("Wage"))),
        WAGES_TIP = rowSums(select(., contains("W_Tip"))),
        SELF_SUM = rowSums(select(., contains("Self"))),
        SELF_TIP = rowSums(select(., contains("S_Tip")))
      ) %>%
      select(u_ID, MONTHCODE, WAGES_SUM, WAGES_TIP, SELF_SUM, SELF_TIP),
  )
    
    
  data %>%
    #filter(t_flag) %>%
    group_by(u_ID) %>%
    reframe(
      year = year,
      weight = sum(WPFINWGT) / 12,
      age = max(TAGE),
      n_dep = getmode(child_count),
      n_young_dep = getmode(young_child_count),
      is_dep = getmode(EDEPCLM),
      is_dep = if_else(is.na(is_dep) | is_dep == 2, F, T),
      marriage = getmode(EMS),
      inc_tip = sum(TIPS),
      
      inc_wages = sum(WAGES_SUM),
      inc_wages_tips = sum(WAGES_TIP),
      inc_self = sum(SELF_SUM),
      inc_self_tips = sum(SELF_TIP),
      
      tips_pct = inc_tip / (inc_wages + inc_self)
      #eitc = sum(eitc) > 1,
      #eitc_iflag = getmode(AEITC),
    ) %>%
    left_join(
      1:4 %>%
        map(
          .f = ~ data %>%
            filter(!!sym(paste0('EJB',.x,'_CLWRK')) == 0) %>%
            group_by(u_ID, !!sym(paste0('TJB',.x,'_IND'))) %>%
            reframe(
              tip = sum(!!sym(paste0('TJB',.x,'_TXAMT'))),
              tip = ifelse(is.na(tip), 0, tip)
            ) %>%
            rename(
              ind = !!sym(paste0('TJB',.x,'_IND'))
            )
        ) %>%
        bind_rows() %>%
        group_by(u_ID, ind) %>%
        reframe(
          tips_wages = sum(tip)
        ) %>%
        filter(!is.na(ind)) %>%
        group_by(u_ID) %>%
        mutate(
          count = 1:n()
        ) %>%
        pivot_wider(
          names_from = count,
          values_from = c(ind, tips_wages)
        )
    ) %>%
    left_join(
      1:4 %>%
        map(
          .f = ~ data %>%
            filter(!!sym(paste0('EJB',.x,'_CLWRK')) == 1) %>%
            group_by(u_ID, !!sym(paste0('TJB',.x,'_IND'))) %>%
            reframe(
              tip = sum(!!sym(paste0('TJB',.x,'_TXAMT'))),
              tip = ifelse(is.na(tip), 0, tip)
            ) %>%
            rename(
              ind = !!sym(paste0('TJB',.x,'_IND'))
            )
        ) %>%
        bind_rows() %>%
        group_by(u_ID, ind) %>%
        reframe(
          tips_self = sum(tip)
        ) %>%
        filter(!is.na(ind)) %>%
        group_by(u_ID) %>%
        mutate(
          count = 1:n()
        ) %>%
        pivot_wider(
          names_from = count,
          values_from = c(ind, tips_self)
        )
    ) %>%
    return()
}


df_ind = c(2018:2023) %>%
  map(.f = ~ build_annual_sipp(.x, write = F, out = T)) %>%
  bind_rows()

df_ind %>%
  write_csv(., '/gpfs/gibbs/project/sarin/shared/raw_data/SIPP/tip_ind_full_split_2.csv')


