# import cleaned data from jupyter notebook
data_raw = read.csv('/Users/606391/Documents/Projects/Country Risk Prediction/Data/python_cleaned_file.csv', header=TRUE)
# convert to factors
data_raw$byregn2 = factor(data_raw$byregn2, levels=c(1,5))
data_raw$sftpcons = factor(data_raw$sftpcons, levels=c(0,1), labels=c("Control", "Case"))
data_raw$disp4cat = as.factor(data_raw$disp4cat)
data_raw$maccat = as.factor(data_raw$maccat)
data_raw$match_id = as.factor(data_raw$match_id)

#  build conditional logit model
library(survival)
cond_log_model = clogit(I(sftpcons=="Case") ~ maccat + disp4cat + logim + sftptv2a + strata(match_id), data=data_raw)
summary_cond = summary(cond_log_model)

#train_test split data
library(caTools)
set.seed(455)
sample = sample.split(data_raw, SplitRatio=0.75, group=match_id)
train = subset(data_raw, sample == TRUE)
test = subset(data_raw, sample ==  FALSE)

#  build unconditional logit model using dummies for regions
log_model = glm(sftpcons ~ byregn2 + disp4cat + maccat + sftptv2a + logim, data=train, family=binomial(link="logit"))
summary_uncond = summary(log_model)

library(caret)
var_imp = varImp(log_model, scale = FALSE)
var_imp$Variables = rownames(var_imp)
var_imp = var_imp[order(Variables)]

ggplot(var_imp, aes(Overall, Variables), stat=identity) + geom_point()
