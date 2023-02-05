library(TwoSampleMR)
exposure_dat <- extract_instruments("ukb-b-9405")
outcome_dat <- extract_outcome_data(snps=exposure_dat$SNP, outcomes = "ieu-b-142")
dat <- harmonise_data(exposure_dat, outcome_dat)
res <- mr(dat)

exposure_dat <- extract_instruments("ukb-b-19953")
outcome_dat <- extract_outcome_data(snps=exposure_dat$SNP, outcomes = "ieu-b-142")
dat <- harmonise_data(exposure_dat, outcome_dat)
res2 <- mr(dat)

res_tot = rbind(res,res2)
res_tot$exposure = as.factor(res_tot$exposure)
levels(res_tot$exposure) = c('Body mass index',
                             'Waist circumference')
res_tot$exposure = relevel(res_tot$exposure,
                           ref = 'Waist circumference')
res_tot$Lower = res_tot$b - 1.96*res_tot$se
res_tot$Upper = res_tot$b + 1.96*res_tot$se
res_tot$Label = paste0(round(res_tot$b,2),
                       ' (',round(res_tot$Lower,2),
                       ', ',round(res_tot$Upper,2),')')

require(ggplot2)
mr_plot = ggplot(data = res_tot,
       aes(x = method,
           y = b)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = b - 1.96*se,
                     ymax = b + 1.96*se),
                width = .2) +
  facet_wrap(~exposure) +
  ylab('Causal effect on cigarretes smoked per day') +
  xlab('MR Method') +
  geom_hline(yintercept=0, 
             linetype="dotted", color='blue') +
  theme_bw(base_size = 14) + 
  theme(plot.title = element_text(face= "bold", hjust = 0.5)) +
  geom_label(aes(label = Label),size = 3,nudge_x = .4) +
  coord_flip()

setwd('/Users/arjunbhattacharya/Desktop/TUD PGS/')
ggsave(plot = mr_plot,
       filename = 'WHR_BMI_Cigs_MRPlot.png',
       height = 4,
       width = 6)
