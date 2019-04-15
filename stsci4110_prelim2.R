library(ggplot2)
library(lmtest)
library(dplyr)

labor <- read.csv('~/Dropbox/STSCI/STSCI4110/Prelim2/Labor_Cert_FY2016_19_V1_Posted.csv', stringsAsFactors =FALSE)

labor$PROCESSING_CENTER <- factor(sapply(labor$CASE_NUMBER, FUN=function(y) {if (grepl('A', y)) {'ATLANTA'} else {'CHICAGO'} }))
labor$DECISION_DATE <- as.Date(labor$DECISION_DATE, format='%m/%d/%y')
labor$DECISION_MONTH <- factor(months(labor$DECISION_DATE), levels=c('October', 'November', 'Decemeber'), ordered=TRUE)
labor$ADMIN <- factor(labor$ADMIN, levels=c('Obama', 'Trump'))
labor$CASE_STATUS <- factor(labor$CASE_STATUS, levels=c('Certified', 'Denied'))

labor$PW_AMOUNT_9089 <- sub(pattern=',', replacement='', x=labor$PW_AMOUNT_9089)
labor$PW_AMOUNT_9089 <- as.numeric(labor$PW_AMOUNT_9089)
labor$PW_LEVEL_9089 <- factor(labor$PW_LEVEL_9089, 
                              levels=c('N/A', 'Level I', 'Level II', 'Level III', 'Level IV'),
                              ordered=TRUE)
labor$PW_UNIT_OF_PAY_9089 <- factor(labor$PW_UNIT_OF_PAY_9089, 
                                    levels=c('Hour', 'Week', 'Bi-Weekly', 'Month', 'Year'),
                                    ordered=TRUE)

labor$JOB_INFO_EDUCATION <- factor(labor$JOB_INFO_EDUCATION, 
                                   levels=c('None', 'Other', 'High School', 'Associate\'s', 'Bachelor\'s', 'Master\'s', 'Doctorate'),
                                   ordered = TRUE)

labor$JOB_INFO_EXPERIENCE <- factor(labor$JOB_INFO_EXPERIENCE, levels=c('Y', 'N'))
labor$RECR_INFO_PROFESSIONAL_OCC <- factor(labor$RECR_INFO_PROFESSIONAL_OCC, levels=c('Y', 'N'))
labor$RECR_INFO_COLL_UNIV_TEACHER <- factor(labor$RECR_INFO_COLL_UNIV_TEACHER, levels=c('Y', 'N'))


general_class <- function(class) {
  if (length(class) == 0) {
    return('None')
  } else if (class == 'Parolee' || class == 'Not in USA') {
    return(class)
  } else {
    return(substr(class, 0, 1))
  }
}
labor$CLASS_OF_ADMISSION_GENERAL <- factor(sapply(labor$CLASS_OF_ADMISSION, FUN=general_class))
labor$CLASS_OF_ADMISSION <- factor(labor$CLASS_OF_ADMISSION)

labor$FOREIGN_WORKER_INFO_STATE <- factor(labor$FOREIGN_WORKER_INFO_STATE)
labor$COUNTRY_OF_CITIZENSHIP <- factor(labor$COUNTRY_OF_CITIZENSHIP)
labor$REGION <- factor(labor$REGION, levels=c('CANADA', 'ASIA', 'AFRICA', 'LATIN AMERICA', 'OCEANIA', 'EUROPE', 'MIDDLE EAST'))
labor$FOREIGN_WORKER_INFO_EDUCATION <- factor(labor$FOREIGN_WORKER_INFO_EDUCATION, 
                                              levels=c('None', 'Other', 'High School', 'Associate\'s', 'Bachelor\'s', 'Master\'s', 'Doctorate'),
                                              ordered = TRUE)
labor$PW_Job_Title_9089 <- factor(toupper(labor$PW_Job_Title_9089))
labor


labor$PW_Job_Category <- rep('OTHER', nrow(labor))
job_category_labels <- c('SOFTWARE', 'MEDICAL', 'FINANCIAL', 'SCIENCE', 'LEGAL', 'EDUCATION', 'FOOD', 'EXECUTIVES', 'ART', 'SKILLED', 'UNSKILLED')
job_categories <- list(c('WEB', 'INFORMATION SECURITY', 'SYSTEMS', 'DATABASE', 'DEVELOPER', 'SOFTWARE', 'UI', 'UX', 'PROGRAMMER', 'COMPUTER OCCUPATION', 'SOFTWARE'), 
                       c('ANESTHESIOLOGISTS', 'DENTAL', 'DENTIST', 'PEDIATRICIAN', 'PHARMAC', 'PSYCHIATRIST', 'PROSTHODONTISTS', 'PHYSICIAN', 'SURGEON', 'NURS', 'MENTAL HEALTH', 'HEALTH AID', 'THERAPIST'), 
                       c('ACCOUNTANT', 'ACTUARIES','AUDITOR', 'BOOKKEEPER', 'BUDGET', 'CLERK', 'CREDIT ANALYST', 'COST ESTIMATOR', 'FINANCIAL', 'TRADER'),
                       c('BIOLOGIST', 'CHEMIST', 'ENGINEER', 'EPIDEMIOLOGIST', 'GEOGRAPHER', 'LOGISTICIAN', 'PHYSICIST', 'RESEARCH ANALYST', 'RESEARCH ASSOCIATE', 'RESEARCHER', 'SCIENTIST', 'STATISTICIAN'), 
                       c('ATTORNEY', 'LAWYER', 'PARALEGAL', 'LEGAL'),
                       c('COACH', 'EDUCATION', 'INSTRUCTOR', 'LECTURER', 'PROFESSOR', 'TEACHER'),
                       c('BAKER', 'BUTCHER', 'COOK', 'CHEF', 'DISHWASHER', 'FISH', 'FOOD PREPARATION', 'MEAT', 'POULTRY', 'SERVER', 'SLAUGHTERER', 'WAITER'),
                       c('COORDINATOR', 'DIRECTOR', 'EXECUTIVE', 'MANAGER', 'PRESIDENT', 'SUPERVISOR', 'TECH LEAD', 'VP'),
                       c('ANIMATOR', 'ARCHITECT', 'ARTIST', 'DESIGNER', 'EDITOR', 'PAINTER', 'WRITER'),
                       c('BREEDER', 'CARPENTER', 'HAIR STYLIST', 'MASON', 'MECHANIC', 'TAILOR', 'TECHNICIAN', 'THREADER', 'TRAINER', 'PLUMBER', 'ROOFER', 'WATCHMAKER', 'WELDER'), 
                       c('CAREGIVER', 'DRIVER', 'LOADER', 'JANITOR', 'HOUSEKEEP', 'MAID', 'NANNY', 'LABORER', 'PORTER', 'PACKER', 'WORKER', 'OPERATOR'))
for (k in 1:length(labor$PW_Job_Title_9089)) {
  for (i in 1:length(job_categories)) {
    match <- lapply(job_categories[[i]], FUN=function(z) { grepl(z, labor$PW_Job_Title_9089[k], fixed=TRUE) })
    if (TRUE %in% match) {
      labor$PW_Job_Category[k] <- job_category_labels[i]
    }
  }
}
print(labor$PW_Job_Category)
print(sum(sapply(labor$PW_Job_Category, FUN=function(x) {if (x == 'OTHER') {1} else {0} } )))



admin_fit <- glm(CASE_STATUS ~ ADMIN, family=binomial, data=labor)
summary(admin_fit)

general_class_fit <- glm(CASE_STATUS ~ CLASS_OF_ADMISSION_GENERAL, family=binomial, data=labor)
summary(general_class_fit)
class_fit <- glm(CASE_STATUS ~ CLASS_OF_ADMISSION, family=binomial, data=labor)
summary(class_fit)

region_fit <- glm(CASE_STATUS ~ REGION, family=binomial, data=labor)
summary(region_fit)

country_fit <- glm(CASE_STATUS ~ COUNTRY_OF_CITIZENSHIP, family=binomial, data=labor)
summary(country_fit)

income_fit <- glm(CASE_STATUS ~ PW_LEVEL_9089, family=binomial, data=labor)
summary(income_fit)
