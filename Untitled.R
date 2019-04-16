library(readxl)
library(ggmosaic)
library(lmtest)
library(data.table)

clean_dataset <- function() {
  labor <- read.csv('~/Dropbox/STSCI/STSCI4110/Prelim2/Labor_Cert_FY2016_19_V1_Posted.csv', 
                    sep=',', stringsAsFactors=FALSE)
  orig_n_rows <- nrow(labor)
  
  unused_preds <- c('ID', 'CASE_NUMBER', 'COUNTRY_OF_CITIZENSHIP', 'PW_Job_Title_9089', 'JOB_INFO_JOB_TITLE', 'PW_UNIT_OF_PAY_9089')
  labor <- labor[!(names(labor) %in% unused_preds)]
  
  labor$DECISION_DATE <- as.Date(labor$DECISION_DATE, format='%m/%d/%y')
  
  labor$PW_AMOUNT_9089 <- sapply(labor$PW_AMOUNT_9089, FUN=function(y) {sub(',', '', y)})
  labor$PW_AMOUNT_9089 <- as.numeric(labor$PW_AMOUNT_9089)
  labor$ADMIN <- factor(labor$ADMIN, levels=c('Obama', 'Trump'))
  labor$CASE_STATUS <- factor(labor$CASE_STATUS, levels=c('Certified', 'Denied'))
  labor$JOB_INFO_EXPERIENCE <- factor(labor$JOB_INFO_EXPERIENCE, levels=c('Y', 'N'))
  labor$RECR_INFO_PROFESSIONAL_OCC <- factor(labor$RECR_INFO_PROFESSIONAL_OCC, levels=c('Y', 'N'))
  labor$RECR_INFO_COLL_UNIV_TEACHER <- factor(labor$RECR_INFO_COLL_UNIV_TEACHER, levels=c('Y', 'N'))
  labor$PW_LEVEL_9089 <- factor(labor$PW_LEVEL_9089, levels=c('N/A', 'Level I', 'Level II', 'Level III', 'Level IV'))
  
  regions <- c('CANADA', 'ASIA', 'AFRICA', 'LATIN AMERICA', 'OCEANIA', 'EUROPE', 'MIDDLE EAST')
  labor$REGION <- factor(labor$REGION, levels=regions)
  labor$CLASS_OF_ADMISSION <- factor(labor$CLASS_OF_ADMISSION)
  
  education <- c('None', 'Other', 'High School', 'Associate\'s', 'Bachelor\'s', 'Master\'s', 'Doctorate')
  labor$JOB_INFO_EDUCATION <- factor(labor$JOB_INFO_EDUCATION, levels=education)
  labor$FOREIGN_WORKER_INFO_EDUCATION <- factor(labor$FOREIGN_WORKER_INFO_EDUCATION, levels=education)
  
  
  # Remove row with US citizenship
  labor <- labor[-c(62,344,348,1074,2223,2465,2536,2679,2704,184),]
  no_decision_date <- subset(x=labor, subset=is.na(labor$DECISION_DATE))
  
  labor <- subset(x=labor, subset=!is.na(labor$PW_AMOUNT_9089))
  print(sprintf('Removed %d rows', orig_n_rows-nrow(labor)))
  labor <- subset(x=labor, subset=!is.na(labor$DECISION_DATE))
  print(sprintf('Removed %d rows', orig_n_rows-nrow(labor)))
  
  return(labor)
}


labor <- clean_dataset()


# change states to regions
US_REGION_LABELS <- c('NewEngland', 'MidAtlantic', 'South', 'Midwest', 'Southwest', 'West', 'NA')
US_REGION_STATES <- list(c('ME', 'MA', 'NH', 'RI', 'VT', 'CT'), c('DE', 'MD', 'NJ', 'NY', 'PA'), 
                      c('AL', 'AR', 'FL', 'GA', 'KY', 'LA', 'MS', 'MO', 'NC', 'SC', 'TN', 'VA', 'WV'),
                      c('IL', 'IN', 'IA', 'KS', 'MI', 'MN', 'NE', 'ND', 'OH', 'SD', 'WI'),
                      c('AZ', 'NM', 'OK', 'TX'), c('AK', 'CA', 'CO', 'HI', 'ID', 'MT', 'NV', 'OR', 'UT', 'WA', 'WY'))
us_region <- function(lst, states, region) {sapply(lst, FUN=function(x) {ifelse(x %in% states, region, x)} )}

for (k in 1:length(US_REGION_STATES)) {
  labor$FOREIGN_WORKER_INFO_STATE <- us_region(labor$FOREIGN_WORKER_INFO_STATE, US_REGION_STATES[[k]], US_REGION_LABELS[[k]])
}
labor$FOREIGN_WORKER_INFO_STATE[labor$FOREIGN_WORKER_INFO_STATE==""] <- 'NA'
labor$FOREIGN_WORKER_INFO_STATE <- factor(labor$FOREIGN_WORKER_INFO_STATE, levels=US_REGION_LABELS)




aicm<-glm(formula = CASE_STATUS ~ ADMIN + PW_AMOUNT_9089 + JOB_INFO_EXPERIENCE + JOB_INFO_EDUCATION
            + RECR_INFO_COLL_UNIV_TEACHER + RECR_INFO_PROFESSIONAL_OCC + REGION + DECISION_DATE, 
          family = 'binomial', data = labor)


salaries <- seq(from=min(labor$PW_AMOUNT_9089, na.rm=TRUE), to=max(labor$PW_AMOUNT_9089, na.rm=TRUE), by=1000)


create_newdata <- function(predictor) {
  salaries <- seq(from=min(labor$PW_AMOUNT_9089, na.rm=TRUE), to=max(labor$PW_AMOUNT_9089, na.rm=TRUE), by=1000)
  n <- 273 * length(levels(labor[[predictor]]))
  print(n)
  
  newdata_df <- data.frame(
    PW_AMOUNT_9089 = rep(salaries, length(levels(labor[[predictor]])) ),
    ADMIN = factor(rep('Trump', n), levels=c('Obama', 'Trump')),
    JOB_INFO_EXPERIENCE = factor(rep('Y', n), levels=c('Y', 'N')),
    JOB_INFO_EDUCATION = factor(rep('Bachelor\'s', n), levels=education),
    RECR_INFO_COLL_UNIV_TEACHER = factor(rep('N', n), levels=c('Y', 'N')),
    RECR_INFO_PROFESSIONAL_OCC = factor(rep('Y', n), levels=c('Y', 'N')),
    REGION = factor(rep('ASIA', n), levels=regions),
    DECISION_DATE = rep(median(labor$DECISION_DATE, na.rm=TRUE), n)
  )
  newdata_df[[predictor]] <- factor(sapply(levels(labor[[predictor]]), FUN=function(r) {rep(r, 273)}))
  return(newdata_df)
}

n_certified <- length(labor$CASE_STATUS[labor$CASE_STATUS == 'Certified'])
n_denied <- length(labor$CASE_STATUS[labor$CASE_STATUS == 'Denied'])
threshold <- n_denied / (n_certified + n_denied)

obama <- subset(x=labor, subset=labor$ADMIN == 'Obama')
trump <- subset(x=labor, subset=labor$ADMIN == 'Trump')

estimated_prob_plot <- function(newdata, xvar, fontsize=26) {
  newdata$CASE_STATUS <- 1 - predict(aicm, type='response', newdata=newdata)
  newdata$COLOR <- newdata[[xvar]]
  eprob_plot <- ggplot(data=newdata) +
    geom_path(aes(x=PW_AMOUNT_9089, y=CASE_STATUS, color=COLOR), show.legend=TRUE, size=4, lineend='round') + 
    labs(x='PW_AMOUNT_9089', y='P(CASE_STATUS) == \'Certified\'', title='Estimated Probability that an Applicant Is Certified') +
    labs(color = xvar) +
    theme(axis.title=element_text(size=30, face='bold'), axis.text.x=element_text(size=30)) +
    theme(axis.text.y=element_text(size=fontsize, face='bold')) +
    theme(plot.title=element_text(size=30, face='bold'))  +
    theme(legend.title=element_text(size=30, face='bold'), legend.text=element_text(size=22))
  
  ggsave(filename=sprintf('~/Dropbox/STSCI/STSCI4110/Prelim2/plots/%s_estimated_prob_plot.png', xvar), plot=eprob_plot, width=16, height=10)
  return(eprob_plot)
}

region_df <- create_newdata('REGION')
admin_df <- create_newdata('ADMIN')
admin_df$DECISION_DATE <- c(rep(median(obama$DECISION_DATE), 273), rep(median(trump$DECISION_DATE), 273))
estimated_prob_plot(region_df, 'REGION')
estimated_prob_plot(admin_df, 'ADMIN')

#**** INTERACTION TESTS *****#
predictors <- names(labor)
predictors <- predictors[!predictors %in% c('CASE_STATUS', 'DECISION_DATE', 'CLASS_OF_ADMISSION')]
pred_combinations <- combn(predictors, 2, simplify=FALSE)
pred_interactions <- vector(length=length(pred_combinations), mode='list')
pred_formulas <- vector(length=length(pred_combinations), mode='character')

for (i in 1:length(pred_interactions)) {
  preds <- pred_combinations[[i]]
  formula <- as.formula(sprintf('CASE_STATUS ~ %s:%s', preds[1], preds[2]))
  print(formula)
  fit <-  glm(formula, family='binomial', data=labor)
  #write(str(fit), file='~/Dropbox/STSCI/STSCI4110/Prelim2/models.txt', append=TRUE)
  pred_interactions[[i]] <- fit
  pred_formulas[[i]] <- sprintf('%s:%s', preds[[1]], preds[[2]])
}

pred_interactions


for (j in 1:length(pred_interactions)) {
  #print(pred_interactions[[j]]$coefficients)
  print(j)
  print(coef(summary(pred_interactions[[j]]))[,4])
  
  filename <- sprintf('~/Dropbox/STSCI/STSCI4110/Prelim2/pvals_by_predictor_names/pvals_%s.csv', pred_formulas[[j]])
  write.csv(coef(summary(pred_interactions[[j]])), file=filename)
}
##############################



subsets_to_df <- function(subsets) {
  summ <- summary(subsets)
  summ_df <- data.frame(
    which = summ$which,
    rsq = summ$rsq,
    rss = summ$rss,
    adjr2 = summ$adjr2,
    cp = summ$cp,
    bic = summ$bic,
    outmat = summ$outmat
  )
  return(summ_df)
}
subsets5 <- regsubsets(CASE_STATUS~., data=labor, nbest=5, method='exhaustive', really.big=TRUE)
save(subsets5,  file='~/Dropbox/STSCI/STSCI4110/Prelim2/subsets5.txt')
write.csv(subsets_to_df(subsets3), file='~/Dropbox/STSCI/STSCI4110/Prelim2/subsets5_summary.csv')

subset4 <- regsubsets(CASE_STATUS~., data=labor, nbest=4, method='exhaustive', really.big=TRUE)
save(subsets4,  file='~/Dropbox/STSCI/STSCI4110/Prelim2/subsets4.txt')
write.csv(subsets_to_df(subsets4), file='~/Dropbox/STSCI/STSCI4110/Prelim2/subsets4_summary.csv')

subsets3 <- regsubsets(CASE_STATUS~., data=labor, nbest=3, method='exhaustive', really.big=TRUE)
save(subsets3,  file='~/Dropbox/STSCI/STSCI4110/Prelim2/subsets3.txt')
write.csv(subsets_to_df(subsets3), file='~/Dropbox/STSCI/STSCI4110/Prelim2/subsets3_summary.csv')

subsets2 <- regsubsets(CASE_STATUS ~ ., data=labor, nbest=2, method='exhaustive', really.big=TRUE)
save(subsets2,  file='~/Dropbox/STSCI/STSCI4110/Prelim2/subsets2.txt')
write.csv(subsets_to_df(subsets2), file='~/Dropbox/STSCI/STSCI4110/Prelim2/subsets2_summary.csv')


#univariate log regression tests and LR tests at 0.05 level

#labor$DECISION_DATE
date_test <- glm(status ~ labor$DECISION_DATE, family = binomial)
summary(date_test)
#this code will be repeated for each LR test
ts <- 2028.4-2018.4
1-pchisq(ts,1) 

#labor$ADMIN
admin_test <- glm(status ~ admin, family = binomial)
summary(admin_test)
ts <- 3817.6-3782.6 
1-pchisq(ts,1)

#labor$CLASS_OF_ADMISSION
admission_test <- glm(status ~ admission, family = binomial)
summary(admission_test)
ts <- 3817.6-3386.9 
1-pchisq(ts,35)

#labor$PW_LEVEL_9089
pwlev_test <- glm(status ~ pw_level, family = binomial)
summary(pwlev_test)
ts <- 3532.7-3461.5
1-pchisq(ts,4)

#labor$PW_AMOUNT_9089
pwamn_test <- glm(status ~ labor$PW_AMOUNT_9089, family = binomial)
summary(pwamn_test)
ts <- 3803.9-3617.3
1-pchisq(ts,1)

#labor$JOB_INFO_EDUCATION
jobed_test <- glm(status ~ job_ed, family = binomial)
summary(jobed_test)
ts <- 3814.2-3403.1
1-pchisq(ts,6)

#labor$JOB_INFO_EXPERIENCE
jobexp_test <- glm(status ~ job_exp, family = binomial)
summary(jobexp_test)
ts <- 3817.6-3799.3
1-pchisq(ts,1)

#labor$REC_INFO_PROFESSIONAL_OCC
recinfo_test <- glm(status ~ rec_info_prof, family = binomial)
summary(recinfo_test)
ts <- 3810.7-3477.9
1-pchisq(ts,1)

#labor$REC_INFO_COLL_UNIV_TEACHER
reccoll_test <- glm(status ~ rec_infor_coll, family = binomial)
summary(reccoll_test)
ts <- 3806.9-3795.2
1-pchisq(ts,1)

#labor$REGION
region_test <- glm(status ~ region, family = binomial)
summary(region_test)
ts <- 3814.2-3651
1-pchisq(ts,6)

#labor$FOREIGN_WORKER_INFO_EDUCATION
foreigned_test <- glm(status ~ foreign_ed, family = binomial)
summary(foreigned_test)
ts <- 3817.6-3499.9
1-pchisq(ts,6)

#labor$FOREIGN_WORKER_INFO_STATE
foreignstate_test <- glm(status ~ foreign_state, family = binomial)
summary(foreignstate_test)
ts <- 3817.6-3572.1
1-pchisq(ts,6)


mosaic_plot <- function(xvar, ylabel, xlabel='Application Status', fontsize=26) {
  mosaic_df <- data.frame(x=labor[[xvar]], CASE_STATUS=labor$CASE_STATUS)
  mosaic <- ggplot(data=mosaic_df) +
    geom_mosaic(aes(x=product(x, CASE_STATUS), fill=x), na.rm=TRUE, show.legend=FALSE) + 
    labs(x=xlabel, y=ylabel, title=sprintf('%s and %s', xlabel, ylabel)) +
    theme(axis.title=element_text(size=30), axis.text.x=element_text(size=30, face='bold')) +
    theme(axis.text.y=element_text(size=fontsize, face='bold')) +
    theme(plot.title=element_text(size=38, face='bold'))
  ggsave(filename=sprintf('~/Dropbox/STSCI/STSCI4110/Prelim2/plots/%s_plot.png', xvar), plot=mosaic, width=13, height=10)
  return(mosaic)
}

mosaic_plot(xvar='ADMIN', ylabel='Administration', fontsize=30)
mosaic_plot(xvar='PW_LEVEL_9089', ylabel='PW Level', fontsize=30)
mosaic_plot(xvar='JOB_INFO_EDUCATION', ylabel='Job Application', fontsize=28)
mosaic_plot(xvar='JOB_INFO_EXPERIENCE', ylabel='Job Experience', fontsize=46)
mosaic_plot(xvar='RECR_INFO_PROFESSIONAL_OCC', ylabel='Rec Professional', fontsize=46)
mosaic_plot(xvar='RECR_INFO_COLL_UNIV_TEACHER', ylabel='Rec College or University', fontsize=46)
mosaic_plot(xvar='REGION', ylabel='Region', fontsize=30)
mosaic_plot(xvar='FOREIGN_WORKER_INFO_EDUCATION', xlabel='Status', ylabel='Foreign Worker Education', fontsize=30)
mosaic_plot(xvar='FOREIGN_WORKER_INFO_STATE', xlabel='Status', ylabel='Foreign Worker State', fontsize=32)
mosaic_plot(xvar='CLASS_OF_ADMISSION', ylabel='Class of Admission', fontsize=14)



#HISTOGRAM FOR AMOUNT
pw_amount_hist <- ggplot(data=labor) +
    geom_histogram(mapping = aes(x=PW_AMOUNT_9089, color=CASE_STATUS), bins=100, breaks=seq(from=0, to=300000, by=10000)) +
    labs(x='PW_AMOUNT_9089', title='PW_AMOUNT_9089 Histogram') +
    theme(axis.title=element_text(size=30), axis.text=element_text(size=22, face='bold')) +
    theme(plot.title=element_text(size=38, face='bold'))
pw_amount_hist
ggsave(filename='~/Dropbox/STSCI/STSCI4110/plots/PW_AMOUNT_9089_plot.png', plot=pw_amount_hist, width=13, height=9)


decision_date_hist <- ggplot(data=labor) +
  geom_histogram(mapping = aes(x=DECISION_DATE, color=CASE_STATUS), bins=100) +
  labs(x='DECISION_DATE', title='DECISION_DATE Histogram') +
  theme(axis.title=element_text(size=30), axis.text=element_text(size=26, face='bold')) +
  theme(plot.title=element_text(size=38, face='bold'))
decision_date_hist
ggsave(filename='~/Dropbox/STSCI/STSCI4110/plots/DECISION_DATE_plot.png', plot=decision_date_hist, width=13, height=9)




# R Code EMPIRICAL PROBS:
AMNT.fac = factor(cut(labor$PW_AMOUNT_9089, c(14580, seq(20000,272854,by=11000)) ))
AMNT.fac
table(AMNT.fac)
e.probs = tapply(status,AMNT.fac,mean) ###< THIS IS THE PROBLEM
e.probs
p = rep(0,10)
for (i in 1:22){
  p[i] = e.probs[i]/table(AMNT.fac)[i]
}
p
amnt <- glm(status ~ labor$PW_AMOUNT_9089, family="binomial")
summary(amnt)
beta0 = -0.0403
beta1 = -0.00002148
curve(expr = exp(beta0+beta1*x)/(1+exp(beta0+beta1*x)), xlim=c(14000,275000), ylim=c(0,1),
      main="Admission Status", xlab = "Wage Amount", ylab = "Probability",
      col="blue")
plot(seq(14000,275000, by = 12000), p ,main = "Status by Wage Amount", 
     xlab = "Wage", ylab = "Status", xlim=c(14000,275000), ylim=c(0,1))



  