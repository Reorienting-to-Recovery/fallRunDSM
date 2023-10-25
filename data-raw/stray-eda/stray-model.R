library(lme4)
library(lmerTest)
library(scales)
library(boot)
library(MuMIn)
library(stats)
library(mgcv)
library(multcomp)
library(dplyr)
library(betareg)
library(ggplot2)
library(lmtest)
library(broom)
library(dotwhisker)

library(tidyverse)

data <- read_csv("data-raw/stray-eda/wetransfer_sturrock-et-al/input_data/alldata_formodel_031918.csv")[,-1] ##See Data_acquisition.R for how we got all the data together
options(na.action = "na.fail")
sum(data$tot_estnum) ##N expanded recoveries being used in model

##z-transform all predictors cept categorical variables (LH Type, site type)
cols_to_transform <- c(2:3, 7, 11:15, 21:33, 36:42)
data2 <- data %>%
  mutate(across(all_of(cols_to_transform), ~ ( . - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)),
         hatchery = as.factor(hatchery))

normalize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

data2 <- data %>%
  mutate(across(all_of(cols_to_transform), ~ normalize(.)))



## Given that the weights in betareg do not strictly have to be "integer" N trials
## [weights = "optional numeric vector of case weights" from http://cran.revolutionanalytics.com/web/packages/betareg/betareg.pdf]
## I think the most parsimonious use of them is to use the raw N recoveries but logging them so we don't assume linear increase

##As sometimes straying rate = 0 or 1 apply this transformation (from https://stackoverflow.com/questions/26385617/proportion-modeling-betareg-errors):
transf <- function(y) {
  n <- sum(!is.na(y))
  return ((y *(n - 1) + 0.5) / n)
}
summary(transf(data$stray_ind))

##WHICH COVARIATES ARE CORRELATED (APART FROM ALL THE FLOW METRICS ABOVE)
library(corrplot)
vars = subset(data2[,c(2:3,7,10:15,21:24,26,29,31:32,36:42)])
c = cor(vars)
cex.before <- par("cex")
par(cex = 0.5)
corrplot(c, method = "number")


# base model ----------------------------------
base = betareg(transf(stray_ind) ~ hatchery*dist_hatch + run_year + age + Total_N + Bay
               + Xchannel + flow.1011 + flow.J45R + flow_discrep + rel_month2 + delta_temp.1011
               + mean_PDO_retn + runsize, link = "logit",  weights = log(tot_N_rec+1), data = data2)



summary(base) #r2=0.42

# NOT INCLUDED IN BASE MODEL
# dist_hatch_ocean + runoff (betareg does not seem to like them as they covary with hatchery) - runs with each separately but neither produced a better rsq value than just hatchery
# base = betareg(transf(stray_ind) ~ dist_hatch_ocean*dist_hatch + run_year + age + Total_N + Bay
#                + Xchannel + flow.1011 + flow.J45R + flow_discrep + rel_month2 + delta_temp.1011
#                + mean_PDO_retn + runsize, link = "logit",  weights = log(tot_N_rec+1), data = data2)
# summary(base) #r2=0.41


# brood_year (better decribed by run_year)
# Category (insufficient reps within levels)
# avg_FL + rel_month + LH_Type2 + LH_Type (better decribed by rel_month2)
# dist_ocean + dist_hatchR (better described by dist_hatch)
# flow.911 + flow.1011R + flow.10 (better described by flow.1011)
# flow.J36 + flow.J45 + rel_flow (better described by flow.J45R)
# mean_PDO_outmig + PDO_discrep (better described by mean_PDO_retn)

#TRY LATER JUST IN CASE: brood_year, rel_month, LH_Type2, flow.J45, avg_FL

par(mfrow=c(2,2)); plot(base)
res <- dredge(base)
head(res)

# Global model call: betareg(formula = transf(stray_ind) ~ hatchery * dist_hatch +
#                              run_year + age + Total_N + Bay + Xchannel + flow.1011 + flow.J45R +
#                              flow_discrep + rel_month2 + delta_temp.1011 + mean_PDO_retn +
#                              runsize, data = data2, weights = log(tot_N_rec + 1), link = "logit")
# ---
#   Model selection table
#         (Int)    age Bay dlt_tmp.1011 dst_hatch flw.1011 flw.J45 flw_dsc htchr men_PDO_rtn run_yer      rns   Ttl_N
# 32220 -1.844 0.1584   +                  1.201  -0.7299          0.1346     +     -0.1722  0.2382 -0.08086 0.05044
# 24028 -1.851 0.1584   +                  1.201  -0.7368          0.1355     +     -0.1934  0.2518 -0.07971 0.05040
# 24032 -1.860 0.1567   +     -0.04045     1.203  -0.7391          0.1347     +     -0.1640  0.2566 -0.07546 0.05084
# 32252 -1.822 0.1572   +                  1.202  -0.7266 0.03366  0.1389     +     -0.1692  0.2377 -0.07309 0.05111
# 24060 -1.828 0.1573   +                  1.203  -0.7333 0.03408  0.1399     +     -0.1902  0.2512 -0.07182 0.05108
# 24064 -1.838 0.1556   +     -0.03997     1.204  -0.7357 0.03271  0.1389     +     -0.1612  0.2559 -0.06793 0.05147
#             Xch dst_hatch:htchr df   logLik     AICc delta weight
# 32220 -0.02851               + 20 5703.638 -11366.5  0.00  0.230
# 24028                        + 19 5702.578 -11366.4  0.05  0.225
# 24032                        + 20 5703.596 -11366.4  0.08  0.221
# 32252 -0.02839               + 21 5703.945 -11365.0  1.47  0.111
# 24060                        + 20 5702.893 -11365.0  1.49  0.109
# 24064                        + 21 5703.887 -11364.9  1.58  0.104


##Basically every thing is important Delta temp, relative release flow, Xchannel are less important
##And we already know that delta temp is highly correlated with mean_PDO so don't want both anyway

best1 = betareg(transf(stray_ind) ~ hatchery*dist_hatch + run_year + age + Total_N + Bay
                + flow.1011 + flow_discrep + mean_PDO_retn + runsize, link = "logit",weights = log(tot_N_rec+1), data = data2)
best1a = betareg(transf(stray_ind) ~ hatchery*dist_hatch + run_year + age + Total_N + Bay
                 + flow.1011 + flow_discrep + mean_PDO_retn + runsize, link = "cloglog",weights = log(tot_N_rec+1), data = data2)
##loglog failed
# best1b = betareg(transf(stray_ind) ~ hatchery*dist_hatch + run_year + age + Total_N + Bay
#                 + flow.1011 + flow_discrep + mean_PDO_retn + runsize, link = "loglog",weights = log(tot_N_rec+1), data = data2)
best1c = betareg(transf(stray_ind) ~ hatchery*dist_hatch + run_year + age + Total_N + Bay
                 + flow.1011 + flow_discrep + mean_PDO_retn + runsize, link = "cauchit",weights = log(tot_N_rec+1), data = data2)
best1d = betareg(transf(stray_ind) ~ hatchery*dist_hatch + run_year + age + Total_N + Bay
                 + flow.1011 + flow_discrep + mean_PDO_retn + runsize, link = "probit",weights = log(tot_N_rec+1), data = data2)

summary(best1)$pseudo.r.squared ##probit and logit highest r2 and most intuitive link to use for binary outcome
summary(best1a)$pseudo.r.squared ##probit and logit highest r2 and most intuitive link to use for binary outcome
summary(best1c)$pseudo.r.squared ##probit and logit highest r2 and most intuitive link to use for binary outcome
summary(best1d)$pseudo.r.squared ##probit and logit highest r2 and most intuitive link to use for binary outcome

lrtest(best1, best1d)
AIC(best1, best1d) ## according to AIC and likelihood ratio test, logit link is sig better than probit. But r2 higher for probit?

summary(best1) ##effect of Bay is counterintuitive(confounded by unbalanced release patterns - CNFH releasing such high N upstream)

##try removing Bay and adding in LH_Type and DCC (both sig in previous analyses and other people's work)
best1b = betareg(transf(stray_ind) ~ hatchery*dist_hatch + run_year + Xchannel + Total_N + LH_Type2 + age + flow.1011 + flow_discrep + mean_PDO_retn + runsize, link = "logit",weights = log(tot_N_rec+1), data = data2)
summary(best1b)
AIC(best1, best1b) ## Not improved and LHType and DCC = NS.

##try replacing PDO with temperature (highly correlated so dont want both in at same time)
best1b = betareg(transf(stray_ind) ~ hatchery*dist_hatch + run_year + age + Total_N + Bay + flow.1011 + flow_discrep + delta_temp.1011 + runsize, link = "logit",weights = log(tot_N_rec+1), data = data2)
summary(best1b)
AIC(best1, best1b) ## Sig worse. Stick with main model (with PDO not delta temp) but try just removing Bay

## best model - Bay (as unbalanced/confounded)
best1b = betareg(transf(stray_ind) ~ hatchery*dist_hatch + run_year + age + Total_N
                 + flow.1011 + flow_discrep + mean_PDO_retn + runsize, link = "logit",weights = log(tot_N_rec+1), data = data2)
summary(best1b)
AIC(best1, best1b) ## sig worse (AIC diff =7...but I dont think it's right to include Bay when it is confounded).

## best model (excl Bay as unbalanced/confounded) and trying release month
best1c = betareg(transf(stray_ind) ~ hatchery*dist_hatch + run_year + age + Total_N + rel_month
                 + flow.1011 + flow_discrep + mean_PDO_retn + runsize, link = "logit",weights = log(tot_N_rec+1), data = data2)
summary(best1c)
AIC(best1, best1c) ## release month improves model more than all the above!

## checking release month2 (based on WY rather than calendar year) isnt better...
best1d = betareg(transf(stray_ind) ~ hatchery*dist_hatch + run_year + age + Total_N + rel_month2
                 + flow.1011 + flow_discrep + mean_PDO_retn + runsize, link = "logit",weights = log(tot_N_rec+1), data = data2)
summary(best1d)
AIC(best1c, best1d) ## release month = better than release month2

## checking PDO discrepancy isnt better than mean PDO
best1e = betareg(transf(stray_ind) ~ hatchery*dist_hatch + run_year + age + Total_N + rel_month
                 + flow_discrep + PDO_discrep + runsize + flow.1011, link = "logit",weights = log(tot_N_rec+1), data = data2)
summary(best1e)
AIC(best1d, best1e) ## mean PDO a lot better

options("scipen"=100, "digits"=4) ##see more zeros



###AH BUT PROBIT HAS A HIGHER R2 VALUE... TRY DREGDE AGAIN WITH PROBIT LINK

base = betareg(transf(stray_ind) ~ hatchery*dist_hatch + run_year + age + LH_Type2 + Total_N + Bay
               + Xchannel + flow.1011 + flow.J45R + flow_discrep + rel_month + delta_temp.1011
               + mean_PDO_retn + runsize, link = "probit",  weights = log(tot_N_rec+1), data = data2)

summary(base) #r2=0.47

###########################################################################
###########################################################################

## GOLDEN FINAL MODEL!!!

###########################################################################
###########################################################################

hatchery_stray <- betareg(
  transf(stray_ind) ~ hatchery*dist_hatch + run_year + age + Total_N + rel_month
  + flow.1011 + flow_discrep + mean_PDO_retn, link = "probit",weights = log(tot_N_rec+1), data = data2
)


data2 |>
  select(hatchery,
         dist_hatch,
         run_year,
         age,
         Total_N,
         rel_month,
         flow.1011,
         flow_discrep,
         mean_PDO_retn
  ) |>
  glimpse()


new_data <- tibble(
  hatchery = "coleman", # hatchery of origin
  dist_hatch = .2, # hatchery to release site distance
  run_year = 0, # the run year
  age = 0,
  Total_N = ,
  rel_month = 1,
  flow.1011 =.2,
  flow_discrep = 1,
  mean_PDO_retn = 1
)

predict(hatchery_stray, newdata = new_data)











