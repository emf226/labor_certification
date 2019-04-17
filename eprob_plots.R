library(ggmosaic)
library(lmtest)


labor <- clean_dataset()
obama <- subset(x=labor, subset=labor$ADMIN == 'Obama')
trump <- subset(x=labor, subset=labor$ADMIN == 'Trump')
aicm <- glm(formula = CASE_STATUS ~ ADMIN + PW_AMOUNT_9089 + JOB_INFO_EXPERIENCE + JOB_INFO_EDUCATION
            + RECR_INFO_COLL_UNIV_TEACHER + RECR_INFO_PROFESSIONAL_OCC + REGION + DECISION_DATE, 
            family = 'binomial', data = labor)

interaction_model <- glm(CASE_STATUS~ ADMIN + PW_AMOUNT_9089 + JOB_INFO_EXPERIENCE + JOB_INFO_EDUCATION + 
                           RECR_INFO_COLL_UNIV_TEACHER + RECR_INFO_PROFESSIONAL_OCC + REGION + DECISION_DATE + 
                           ADMIN*JOB_INFO_EDUCATION + ADMIN*DECISION_DATE + ADMIN*REGION, family=binomial, data=labor)

create_newdata <- function(predictor) {
  salaries <- seq(from=min(labor$PW_AMOUNT_9089, na.rm=TRUE), to=max(labor$PW_AMOUNT_9089, na.rm=TRUE), by=1000)
  n <- 273 * length(levels(labor[[predictor]]))
  print(n)
  
  newdata_df <- data.frame(
    PW_AMOUNT_9089 = rep(salaries, length(levels(labor[[predictor]])) ),
    ADMIN = factor(rep('Obama', n), levels=c('Obama', 'Trump')),
    JOB_INFO_EXPERIENCE = factor(rep('Y', n), levels=c('Y', 'N')),
    JOB_INFO_EDUCATION = factor(rep('Bachelor\'s', n), levels=education),
    RECR_INFO_COLL_UNIV_TEACHER = factor(rep('N', n), levels=c('Y', 'N')),
    RECR_INFO_PROFESSIONAL_OCC = factor(rep('Y', n), levels=c('Y', 'N')),
    REGION = factor(rep('ASIA', n), levels=regions),
    DECISION_DATE = rep(median(labor$DECISION_DATE), n)
  )
  newdata_df[[predictor]] <- factor(sapply(levels(labor[[predictor]]), FUN=function(r) {rep(r, 273)}))
  return(newdata_df)
}

n_certified <- length(labor$CASE_STATUS[labor$CASE_STATUS == 'Certified'])
n_denied <- length(labor$CASE_STATUS[labor$CASE_STATUS == 'Denied'])
threshold <- n_denied / (n_certified + n_denied)
avg_certified_df <- data.frame(
  PW_AMOUNT_9089 <- seq(from=min(labor$PW_AMOUNT_9089, na.rm=TRUE), to=max(labor$PW_AMOUNT_9089, na.rm=TRUE), by=1000),
  CASE_STATUS <- rep(1-threshold, 273)
)


estimated_prob_plot <- function(newdata, xvar, fontsize=26) {
  newdata$CASE_STATUS <- 1 - predict(interaction_model, type='response', newdata=newdata)
  print(newdata$CASE_STATUS)
  newdata$COLOR <- newdata[[xvar]]
  eprob_plot <- ggplot(data=newdata) +
    geom_path(aes(x=PW_AMOUNT_9089, y=CASE_STATUS, color=COLOR), show.legend=TRUE, size=4, lineend='round') + 
#    scale_fill_manual(values = c('Obama'='#5D8AA8', 'Trump'='#F8756C'), aesthetics = c('colour')) +
    geom_line(aes(x=PW_AMOUNT_9089, y=CASE_STATUS), show.legend=TRUE, size=2, linetype='dashed', data=avg_certified_df) +
    labs(x='PW_AMOUNT_9089', y='P(CASE_STATUS) == \'Certified\'', title='Estimated Probability that an Applicant Is Certified') +
    labs(color = xvar) +
    theme(axis.title=element_text(size=30, face='bold'), axis.text.x=element_text(size=30)) +
    theme(axis.text.y=element_text(size=fontsize, face='bold')) +
    theme(plot.title=element_text(size=32, face='bold'))  +
    theme(legend.title=element_text(size=32, face='bold'), legend.text=element_text(size=28))
  eprob_plot <- eprob_plot + annotate(geom='text', x=260000, y=(1-threshold)-0.015, label='Mean Certification Rate')
  ggsave(filename=sprintf('~/Dropbox/STSCI/STSCI4110/Prelim2/plots/%s_estimated_prob_plot.png', xvar), plot=eprob_plot, width=17, height=10)
  return(eprob_plot)
}


region_df <- create_newdata('REGION')
admin_df <- create_newdata('ADMIN')
admin_df$DECISION_DATE <- c(rep(median(obama$DECISION_DATE), 273), rep(median(trump$DECISION_DATE), 273))
estimated_prob_plot(region_df, 'REGION')
estimated_prob_plot(admin_df, 'ADMIN')

