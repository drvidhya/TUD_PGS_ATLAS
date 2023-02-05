afr_df <- read.csv("/~/path to step 2 results/")
eur_df <- read.csv("/~/path to step 2 results/")
amr_df <- read.csv("/~/path to step 2 results/")
eas_df <- read.csv("/~/path to step 2 results/")
names(eur_df)[names(eur_df) == 'p_value_z'] <- 'p'
names(amr_df)[names(amr_df) == 'p_value_z'] <- 'p'
names(eas_df)[names(eas_df) == 'p_value_z'] <- 'p'
names(afr_df)[names(afr_df) == 'p_value_z'] <- 'p'
names(eur_df)[names(eur_df) == 'Std.Err.'] <- 'SE'
names(amr_df)[names(amr_df) == 'Std.Err.'] <- 'SE'
names(eas_df)[names(eas_df) == 'Std.Err.'] <- 'SE'
names(afr_df)[names(afr_df) == 'Std.Err.'] <- 'SE'
names(eur_df)[names(eur_df) == 'Coef.'] <- 'beta'
names(amr_df)[names(amr_df) == 'Coef.'] <- 'beta'
names(eas_df)[names(eas_df) == 'Coef.'] <- 'beta'
names(afr_df)[names(afr_df) == 'Coef.'] <- 'beta'
head(eur_df)
library(metafor)
phecode_df <- read.table("~/path/to/icd/mapping",header =TRUE)
head(phecode_df)
phecode_df_final <- unique(phecode_df$phecode)
phecode_df_final
phecodes = unique(eur_df$phecode)
Results <- NULL
for(i in 1:length(phecodes)){
  idf = rbind(eur_df[eur_df$phecode==phecodes[i],],
              amr_df[amr_df$phecode==phecodes[i],],
              eas_df[eas_df$phecode==phecodes[i],],
              afr_df[afr_df$phecode==phecodes[i],])
  i.meta <- rma.uni(yi=idf$beta,sei=idf$SE, data=idf)
  Results <- rbind(Results, c(phecode=phecodes[i],beta=i.meta$beta,se=i.meta$se,
                              zval=i.meta$zval,pval=i.meta$pval,ci.lb=i.meta$ci.lb,
                              ci.ub=i.meta$ci.ub,QEp=i.meta$QEp))}
results <- data.frame(Results)
results
write.csv(Results,"~/path/to/results")