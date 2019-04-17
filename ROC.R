library(ModelMetrics)
library(lmtest)
library(ggplot2)


labor <- clean_dataset()
BINARY_CASE_STATUS <- sapply(labor$CASE_STATUS, FUN=function(x) {if (x == 'Certified') {0} else {1} })

roc_table <- function(fitted_vals) {
  thresholds <- seq(from=0.0, to=1.0, by=0.05)
  roc <- data.frame(
    threshold = thresholds, 
    specificity = 1 - sapply(thresholds, FUN=function(t) {tnr(BINARY_CASE_STATUS, fitted_vals, cutoff=t) }),
    sensitivity = sapply(thresholds, FUN=function(t) {tpr(BINARY_CASE_STATUS, fitted_vals, cutoff=t)})
  )
  return(roc)
}


aicm <- glm(formula = CASE_STATUS ~ ADMIN + PW_AMOUNT_9089 + JOB_INFO_EXPERIENCE + JOB_INFO_EDUCATION
            + RECR_INFO_COLL_UNIV_TEACHER + RECR_INFO_PROFESSIONAL_OCC + REGION + DECISION_DATE, 
            family = 'binomial', data=labor)
interaction_model <- glm(CASE_STATUS~ ADMIN + PW_AMOUNT_9089 + JOB_INFO_EXPERIENCE + 
           JOB_INFO_EDUCATION + RECR_INFO_COLL_UNIV_TEACHER + RECR_INFO_PROFESSIONAL_OCC + 
           REGION + DECISION_DATE+ADMIN*JOB_INFO_EDUCATION+ADMIN*DECISION_DATE+ADMIN*REGION, family=binomial, data=labor)
summary(interaction_model)
AIC(interaction_model)


roc_main <- roc_table(aicm$fitted.values)
roc_interaction <- roc_table(interaction_model$fitted.values)

roc <- data.frame(
  Fit = c(rep('InteractionModel', nrow(roc_main)), rep('NoInteractionModel', nrow(roc_main)), rep('Random', nrow(roc_main))),
  Sensitivity = c(roc_interaction$sensitivity,roc_main$sensitivity, roc_main$threshold),
  Specificity = c(roc_interaction$specificity,  roc_main$specificity, roc_main$threshold)
)

roc_curve_plot <- ggplot(data=roc, aes(x=Specificity, y=Sensitivity)) + 
  scale_fill_manual(
    values     = c('InteractionModel'='#682F60', 'NoInteractionModel'='#B1972F', 'Random'='#000000'),
    aesthetics = c('colour', 'fill')
  ) +
  geom_point(data=roc, aes(x=Specificity, y=Sensitivity, color=Fit), size=2.5) +
  geom_line(data=roc, aes(x=Specificity, y=Sensitivity, color=Fit), size=2) +
  ggtitle('ROC Curve') + xlab('1 - Specificity') + ylab('Sensitivity') +
  theme(legend.title = element_text(size = 26), legend.text = element_text(size = 22)) +
  theme(axis.title = element_text(size = 24), axis.text = element_text(size = 18)) +
  theme(plot.title = element_text(size = 30, face='bold')) 

roc_curve_plot
ggsave(filename='~/Dropbox/STSCI/STSCI4110/Prelim2/plots/ROC_curve_plot.png', plot=roc_curve_plot, width=13, height=9)


ModelMetrics::auc(aicm)
ModelMetrics::auc(interaction_model)
