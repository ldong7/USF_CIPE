##############################################################################################################################
# 1 remove commas in some monetary values
##############################################################################################################################
remove_commas <- function(df, column_names_list){
  
  num_cols <- length(column_names_list)
  for (i in 1:num_cols){
    
    # change name of column of interest in order to access easier:
    names(df)[names(df) == column_names_list[i]] <- 'change'
    
    # make necessary subs and change to class numeric:
    df$change <- as.character(df$change)
    df$change <- as.numeric(gsub(",","", df$change))
    
    # change column name back to original:
    names(df)[names(df) == 'change'] <- column_names_list[i]  
  }
  return(df)
}
##############################################################################################################################


##############################################################################################################################
# 2 recategorize countries so that R can handle(max of 32)
##############################################################################################################################
group_country <- function(data){
  # 9 categories
  US <- c('UNITED STATES')
  China <- c('CHINA')
  SEA <- c('HONG KONG', 'INDONESIA', 'VIETNAM', 'SINGAPORE', 'SOUTH KOREA', 'INDIA', 'TAIWAN', 
           'PHILIPPINES', 'JAPAN', 'MALAYSIA', 'THAILAND', 'BURMA', 'CAMBODIA', "NEPAL", "SRI LANKA",
           "MACAU", "^BURMA", 'BANGLADESH')
  AF <- c('EGYPT', 'NIGERIA', 'ETHIOPIA', 'ZIMBABWE', "SOUTH AFRICA","UGANDA", "BURKINA FASO", "BURUNDI"
          , "GUINEA")
  EU <- c('NORWAY', 'UNITED KINGDOM', 'SWEDEN', 'UKRAINE', 'FRANCE', 'SWITZERLAND', 'CZECH REPUBLIC', 
          'GERMANY', 'FINLAND', 'NETHERLANDS', 'RUSSIA', 'PORTUGAL', 'ROMANIA', 'SLOVENIA', 'BELGIUM', 
          'SPAIN', 'ITALY', 'DENMARK', 'HUNGARY', "IRELAND", "CROATIA", "GREECE",'ICELAND',
          'SERBIA', 'MOLDOVA', 'ESTONIA', 'LATVIA', "BELARUS")
  SA <- c('BRAZIL', 'MEXICO', 'COLOMBIA', 'PERU', 'ARGENTINA', 'VENEZUELA', 'EL SALVADOR', 'ECUADOR', 
          "CHILE" )
  ME <- c('SAUDI ARABIA', 'ISRAEL', 'KUWAIT', 'AFGHANISTAN', 'JORDAN', 'BAHRAIN', 'PAKISTAN',
          'UNITED ARAB EMIRATES', 'IRAN', "LEBANON")
  Other <- c('CANADA', 'AZERBAIJAN', 'AUSTRALIA', 'GUATEMALA', 'NICARAGUA', 'PANAMA', "CAYMAN ISLANDS",
             "KYRGYZSTAN", "CHRISTMAS ISLAND", "FIJI", "BARBADOS")
  na <- c(NA)
  
  l <- c()
  for(country in data$nation_of_citizenship_desc){
    if(!country %in% c(US,China,SEA, AF,EU,SA,ME,Other,na)){
      print(country)
    }
    else if (country %in% US){
      l <- append(l, 'US')
    }
    else if (country %in% China){
      l <- append(l, 'China')
    }
    else if (country %in% SEA){
      l <- append(l, 'SoutheastAsia')
    }
    else if (country %in% AF){
      l <- append(l, 'Africa')
    }
    else if (country %in% EU){
      l <- append(l, 'Europe')
    }
    else if (country %in% SA){
      l <- append(l, 'SouthAmerica')
    }
    else if (country %in% ME){
      l <- append(l, 'MiddleEast')
    }
    else if (country %in% Other){
      l <- append(l, 'Other')
    }
    else if (country %in% na){
      l <- append(l, 'US')
    }
    else{
      print(paste('Unknown categorization country: ', country, "put into 'Other'"))
      l <- append(l, 'Other')
    }
  }
  
  data$nation_of_citizenship_desc <- as.factor(l)
  return(data)
}
##############################################################################################################################


##############################################################################################################################
# 3 recategorize states so that R can handle(max of 32)
##############################################################################################################################
group_state <- function(data){
  # 4 categories
  CA <- c('California')
  GPN <- c('Alaska', 'Hawaii', 'Oregon', 'Washington')
  NEUS <- c('Maine', 'New Hampshire', 'Vermont', 'Massachusetts', 'Rhode Island', 'Connecticut', 
            'New York', 'Pennsylvania', 'New Jersey')
  
  l <- c()
  for(state in data$state){
    if(state %in% CA){
      l <- append(l,"CA")
    }
    else if (state %in% GPN){
      l <- append(l, 'Greater Pacific Northwest')
    }
    else if (state %in% NEUS){
      l <- append(l, 'Northeastern US')
    }
    else{
      l <- append(l, 'Other')
    }
  }
  
  data$state <- as.factor(l)
  return(data)
}
##############################################################################################################################


##############################################################################################################################
# 4 recategorize majors so that R can handle(max of 32)
##############################################################################################################################
group_major <- function(data){
  BHIP <- c('BHIP', 'HM')
  ARTS <- c('PASJ', 'ARTM', 'FNAR', 'DSGN')
  NURS <- c('NURB', 'NURS', 'IDHS')
  ECON <- c('ECNF', 'ECON','IDEF')
  CS <- c('CS', 'CSF', 'BSDS')
  CHEM <- c('CHEM', 'CHMN')
  BENI <- c('BENI', 'BENT')
  BUSN <- c('BADM', 'BGEM', 'BOBL', 'BMGT')
  INTL <- c('ANST', 'JNST', 'BAIS', 'LAS', 'FNST','CMPL', 'SPAN','JAPN')
  UNDC <- c('UNBN', 'UNLA', 'UNSC')
  
  l <- c()
  for(a in data$major){
    if(a %in% BHIP){
      l <- append(l, 'BHIP')
    }
    else if(a %in% ARTS){
      l <- append(l, 'ARTS')
    }
    else if(a %in% NURS){
      l <- append(l, 'NURS')
    }
    else if(a %in% ECON){
      l <- append(l, 'ECON')
    }
    else if(a %in% CS){
      l <- append(l, 'CS')
    }
    else if(a %in% CHEM){
      l <- append(l, 'CHEM')
    }
    else if(a %in% BENI){
      l <- append(l, 'BENI')
    }
    else if(a %in% BUSN){
      l <- append(l, 'BUSN')
    }
    else if(a %in% INTL){
      l <- append(l, 'INTL')
    }
    else if(a %in% UNDC){
      l <- append(l, 'UNDC')
    }
    else{
      l <- append(l, a)
    }
  }
  data$major_agg <- as.factor(l)
  return(data)
}
##############################################################################################################################


##############################################################################################################################
# 5  # put in an indicator column that has a 1 if fafsa_family_contribution was originally NA and a 0 if it was not
##############################################################################################################################
include_missing_fafsa <- function(df){
  
  df$missing_fafsa <- 0
  df$missing_fafsa[is.na(df$fafsa_family_contribution) == T] <- 1
  return(df)  
}
##############################################################################################################################


##############################################################################################################################
# 6 change integer to factor
##############################################################################################################################
int_to_factor <- function(data){
  norm <- data
  norm$id <- as.factor(norm$id)
  norm$rotc <- as.factor(norm$rotc)
  norm$athlete <- as.factor(norm$athlete)
  norm$terms_attended <- as.factor(norm$terms_attended)
  norm$major_change <- as.factor(norm$major_change)
  norm$academic_period_admitted <- as.factor(norm$academic_period_admitted)
  norm$independent <- as.factor(norm$independent)
  norm$missing_fafsa <- as.factor(norm$missing_fafsa)
  norm$inst_merit_scholar_ind <- as.factor(norm$inst_merit_scholar_ind)
  norm$inst_need_scholar_ind <- as.factor(norm$inst_need_scholar_ind)
  norm$fed_pell_grant_ind <- as.factor(norm$fed_pell_grant_ind)
  norm$cal_grant_ind <- as.factor(norm$cal_grant_ind)
  norm$registered <- as.factor(norm$registered)
  norm$graduated <- as.factor(norm$graduated)
  norm$loa <- as.factor(norm$loa)
  norm$academic_disq <- as.factor(norm$academic_disq)
  return(norm)
}
##############################################################################################################################


##############################################################################################################################
# 7 merge new dataset dorm into the original data set
##############################################################################################################################
merge_dorm <- function(data, dorm){
  colnames(dorm) <- c('id', 'term_range', 'dorm')
  dorm$id <- as.factor(dorm$id)
  dorm$dorm <- as.factor(dorm$dorm)
  total <- merge(data, dorm, by= c('id', 'term_range'))
  return(total)
}
##############################################################################################################################


##############################################################################################################################
# 8 merge new dataset gender into the original data set
##############################################################################################################################
merge_gender <- function(data, gender){
  colnames(gender) <- c('id', 'gender')
  a <- gender[!duplicated(gender$id),]

  # follwing information removed due to NDA
  #############################################

  
  
  
  #############################################
  total <- merge(data, a, by="id")
  return(total)
}
##############################################################################################################################


##############################################################################################################################
# 9 merge new dataset income into the origional dataset
##############################################################################################################################
merge_income <- function(data, income){
  colnames(income) <- c('ID','201140_201240','201220_201240','201240_201340','201320_201340','201340_201440',
                        '201420_201440','Grand Total')  
  term_select <- as.character(data$term_range[1])  
  term_income <- income[c('ID', term_select)]
  colnames(term_income) <- c('id', 'adjusted_gross_income')
  term_income$id <- as.factor(term_income$id)
  
  total <- merge(data, term_income, by= c('id'))
  return(total)
}
##############################################################################################################################


##############################################################################################################################
# 10 merge additional dataset with multiple features into the origional dataset
##############################################################################################################################
merge_additional <- function(data, add){
  colnames(add) <- tolower(colnames(add))
  add$gender <- NULL
  add$total_adjusted_gross_income <- NULL
  add$person_uid <- NULL
  add$admissions_population <- NULL
  
  add$id <- as.factor(add$id)
  add$nat_sci_2_courses <- as.factor(add$nat_sci_2_courses)
  add$fresh_seminar <- as.factor(add$fresh_seminar)
  add$muscat_part <- as.factor(add$muscat_part)
  add$muscat_invite_not_part <- as.factor(add$muscat_invite_not_part)
  add$work_study <- as.factor(add$work_study)
  add$student_worker <- as.factor(add$student_worker)
  
  total <- merge(data, add, by= c('id', 'term_range'))
  return(total)
}
##############################################################################################################################


##############################################################################################################################
# 11 standarize numerical values
##############################################################################################################################
normalize <- function(data){
  norm <- data
  norm$age <- (norm$age-mean(norm$age, na.rm = T))/sd(norm$age, na.rm = T)
  norm$credits_earned <- (norm$credits_earned-mean(norm$credits_earned, na.rm = T))/sd(norm$credits_earned, na.rm = T)
  norm$usf_gpa_credits <- (norm$usf_gpa_credits-mean(norm$usf_gpa_credits, na.rm = T))/sd(norm$usf_gpa_credits, na.rm = T)
  norm$usf_gpa_cum <- (norm$usf_gpa_cum-mean(norm$usf_gpa_cum, na.rm = T))/sd(norm$usf_gpa_cum, na.rm = T)
  norm$fafsa_family_contribution <- (norm$fafsa_family_contribution-mean(norm$fafsa_family_contribution, na.rm = T))/sd(norm$fafsa_family_contribution, na.rm = T)
  
  norm$inst_merit_scholar_amount <- (norm$inst_merit_scholar_amount-mean(norm$inst_merit_scholar_amount, na.rm = T))/sd(norm$inst_merit_scholar_amount, na.rm = T)
  norm$inst_need_scholar_amount <- (norm$inst_need_scholar_amount-mean(norm$inst_need_scholar_amount, na.rm = T))/sd(norm$inst_need_scholar_amount, na.rm = T)
  norm$fed_pell_grant_amount <- (norm$fed_pell_grant_amount-mean(norm$fed_pell_grant_amount, na.rm = T))/sd(norm$fed_pell_grant_amount, na.rm = T)
  norm$cal_grant_amount <- (norm$cal_grant_amount-mean(norm$cal_grant_amount, na.rm = T))/sd(norm$cal_grant_amount, na.rm = T)

  norm$adjusted_gross_income <- (norm$adjusted_gross_income-mean(norm$adjusted_gross_income, na.rm = T))/sd(norm$adjusted_gross_income, na.rm = T)
  norm$student_debt <- (norm$student_debt-mean(norm$student_debt, na.rm = T))/sd(norm$student_debt, na.rm = T)
  norm$parent_debt <- (norm$parent_debt-mean(norm$parent_debt, na.rm = T))/sd(norm$parent_debt, na.rm = T)
  norm$learn_ctr_hours <- (norm$learn_ctr_hours-mean(norm$learn_ctr_hours, na.rm = T))/sd(norm$learn_ctr_hours, na.rm = T)
  norm$ft_fac_perc <- (norm$ft_fac_perc-mean(norm$ft_fac_perc, na.rm = T))/sd(norm$ft_fac_perc, na.rm = T)
   
  return(norm)
}
##############################################################################################################################


##############################################################################################################################
# 12 in some features, missing value has meaning, so they are changed to zero or the mean
##############################################################################################################################
change_na_to_0 <- function(df){  
  # since they are standarized, setting them to the min is close to setting them to zero
  df$credits_earned[is.na(df$credits_earned)] <- min(df$credits_earned, na.rm = T)  
  df$inst_merit_scholar_amount[is.na(df$inst_merit_scholar_amount)] <- min(df$inst_merit_scholar_amount, na.rm = T)
  df$inst_need_scholar_amount[is.na(df$inst_need_scholar_amount)] <- min(df$inst_need_scholar_amount, na.rm = T)
  df$fed_pell_grant_amount[is.na(df$fed_pell_grant_amount)] <- min(df$fed_pell_grant_amount, na.rm = T)
  df$cal_grant_amount[is.na(df$cal_grant_amount)] <- min(df$cal_grant_amount, na.rm = T)
  df$parent_debt[is.na(df$parent_debt)] <- min(df$parent_debt, na.rm = T)
  df$student_debt[is.na(df$student_debt)] <- min(df$student_debt, na.rm = T)
  df$ft_fac_perc[is.na(df$ft_fac_perc)] <- min(df$ft_fac_perc, na.rm = T)
  
  # # since they are standarized, setting them to zero is close to setting them to the mean
  df$usf_gpa_credits[is.na(df$usf_gpa_credits)] <- 0
  df$usf_gpa_cum[is.na(df$usf_gpa_cum)] <- 0
  df$adjusted_gross_income[is.na(df$adjusted_gross_income)] <- 0
  df$fafsa_family_contribution[is.na(df$fafsa_family_contribution)] <- 0
  
  return(df)
}
##############################################################################################################################


##############################################################################################################################
# -1 add column name outcome based on origional dataset outcome dummies
##############################################################################################################################
append_student_outcome <- function(df){
  df$outcome <- 'left'
  df$outcome[df$graduated == 1] <- "graduated"
  df$outcome[ (df$loa == 1) | (df$academic_disq == 1) ] <- "left"
  df$outcome[df$registered == 1] <- "returned"
  df$outcome <- as.factor(df$outcome)
  return(df)
}
##############################################################################################################################


##############################################################################################################################
# final combined all above functions and clean data
##############################################################################################################################
cleaning <- function(data, col_list, dorm, gender, income, add){
  clean <- remove_commas(data, col_list)      #1
  colnames(clean) <- tolower(colnames(clean))
  clean <- group_country(clean)               #2
  clean <- group_state(clean)                 #3
  clean <- group_major(clean)                 #4
  clean <- include_missing_fafsa(clean)       #5
  clean <- int_to_factor(clean)               #6
  clean <- merge_dorm(clean, dorm)            #7
  clean <- merge_gender(clean, gender)        #8
  clean <- merge_income(clean, income)        #9
  clean <- merge_additional(clean, add)       #10
  clean <- normalize(clean)                   #11
  clean <- change_na_to_0(clean)              #12

  clean <- append_student_outcome(clean)      #-1

  return(clean)
}
##############################################################################################################################