library(ggmosaic)
library(lmtest)


labor <- clean_dataset()
aicm <- glm(formula = CASE_STATUS ~ ADMIN + PW_AMOUNT_9089 + JOB_INFO_EXPERIENCE + JOB_INFO_EDUCATION
            + RECR_INFO_COLL_UNIV_TEACHER + RECR_INFO_PROFESSIONAL_OCC + REGION + DECISION_DATE, 
            family = 'binomial', data = labor)

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


obama <- subset(x=labor, subset=labor$ADMIN == 'Obama')
trump <- subset(x=labor, subset=labor$ADMIN == 'Trump')

region_df <- create_newdata('REGION')
admin_df <- create_newdata('ADMIN')
admin_df$DECISION_DATE <- c(rep(median(obama$DECISION_DATE), 273), rep(median(trump$DECISION_DATE), 273))
estimated_prob_plot(region_df, 'REGION')
estimated_prob_plot(admin_df, 'ADMIN')

