# install.packages(c('caret','ROSE'))
# options(repos='http://cran.rstudio.com/')

library(data.table)
library(tidyverse)
library(scorecard)
library(inspectdf)
library(h2o)
library(highcharter)
library(stats)
library(DMwR)
library(ROSE)
library(caTools)
library(caret)

dat <- fread("D://DATASETS//credit_german.csv")
dat <- dat %>% rename('target' = 'creditability')
dat %>% glimpse()

dat$target <- as.factor(dat$target)
dat$target <- as.numeric(dat$target)
dat$target <- ifelse(dat$target>1, 1, 0)

# =====================================================================================

# Filling missing values ----
dat %>% inspect_na %>% view()
# dat %>% 
#   inspect_na() %>% 
#   as.data.frame() %>% 
#   filter(cnt>0)
# 
# unique_numeric_values_tbl <- raw %>%
#   select_if(is.numeric) %>%
#   map_df(~ unique(.) %>% length()) %>%
#   gather() %>%
#   arrange(value) %>%
#   mutate(key = as_factor(key))
# 
# num_unique_number <- unique_numeric_values_tbl %>%
#   filter(value < 7) %>%
#   arrange(desc(value)) %>%
#   pull(key) %>%
#   as.character()
# 
# for (v in num_unique_number) {
#   raw[[v]] <- as.character(raw[[v]])
# }
# 
# string_factor_names <- raw %>%
#   select_if(is.character) %>%
#   names()
# 
# 
# rec_obj <- recipe(~ ., data = raw) %>%
#   step_string2factor(string_factor_names) %>%
#   step_meanimpute(all_numeric()) %>%
#   step_modeimpute(all_nominal()) %>%
#   prep(stringsAsFactors = FALSE)
# 
# df <- bake(rec_obj, raw) 
# 
# df %>% inspect_na()


# Outliers ----
num_vars <- dat %>%
  select(-target) %>% 
  select_if(is.numeric) %>% 
  names()
num_vars

for_vars <- c()
for (b in 2:length(num_vars)) {
  OutVals <- boxplot(dat[[num_vars[b]]])$out
  if(length(OutVals)>0){
    for_vars[b] <- num_vars[b]
  }
}

for_vars <- for_vars %>% as.data.frame() %>% drop_na() %>% pull(.) %>% as.character()
for_vars


for (o in for_vars) {
  OutVals <- boxplot(dat[[o]])$out
  mean <- mean(dat[[o]],na.rm=T)
  
  o3 <- ifelse(OutVals>mean,OutVals,NA) %>% na.omit() %>% as.matrix() %>% t() %>% .[1,]
  o1 <- ifelse(OutVals<mean,OutVals,NA) %>% na.omit() %>% as.matrix() %>% t() %>% .[1,]
  
  val3 <- quantile(dat[[o]],0.75,na.rm = T) + 1.5*IQR(dat[[o]],na.rm = T)
  dat[which(dat[[o]] %in% o3),o] <- val3
  
  val1 <- quantile(dat[[o]],0.25,na.rm = T) - 1.5*IQR(dat[[o]],na.rm = T)
  dat[which(dat[[o]] %in% o1),o] <- val1
}



# ======================================================================================================
# BINNING ----
dat %>% glimpse()
# IV (important variables) ---
iv <- dat %>% 
  iv(y = 'target') %>%
  as_tibble() %>%
  mutate( info_value = round(info_value, 3)) %>%
  arrange( desc(info_value) )

# Exclude not important variables ---
ivars <- iv %>% 
  filter(info_value>0.02) %>% 
  select(variable) %>% 
  .[[1]] 

data_iv <- dat %>% 
  select(ivars,target)

data_iv %>% dim()


# breaking dt into train and test ---
dt_list <- split_df(data_iv, "target", ratio = 0.8, seed=123)
train <- dt_list$train 
test <- dt_list$test


# woe binning ---
bins <- data_iv %>% woebin("target")

# converting train and test into woe values
train_woe <- train %>% woebin_ply(bins) 
test_woe <- test %>% woebin_ply(bins)


names <- train_woe %>% names()                     
names <- gsub("_woe","",names)                     
names(train_woe) <- names                          
names(test_woe) <- names
train_woe %>% inspect_na(show_plot = F) 
test_woe %>% inspect_na(show_plot = F) 

# # Check normality
# num_vars <- train_woe %>%
#   select(-target) %>%
#   names()
# num_vars
# 
# norm <- c()
# for (s in 1:length(num_vars)) {
#   val <- round(e1071::skewness(train_woe[[num_vars[s]]]), 2)
#   norm[s] <- val
# }
# 
# par(mfrow=c(5, 10))  # divide graph area in 2columns & 2rows (number of variables)
# 
# for (s in 1:length(num_vars)) {
#   var.name = num_vars[s]
#   plot(density(train_woe[[num_vars[s]]]),
#        main=glue('{enexpr(var.name)}'),
#        ylab="Frequency",
#        sub=paste("Skewness:", round(e1071::skewness(train_woe[[num_vars[s]]]), 2)))
#   polygon(density(train_woe[[num_vars[s]]]), col="red")
# }

# Logistic Linear Regression Diagnostics ----
outcome <- 'target'
features <- train_woe %>% select(-target) %>% names()

f <- as.formula(paste(outcome, paste(features, collapse = " + "), sep = " ~ "))
glm <- glm(f, data = train_woe)
glm %>% summary()

# Select a formula-based model by AIC
step <- glm %>% stats::step()
step$call # copy paste

glm2 <- glm(formula = target ~ status.of.existing.checking.account + 
              duration.in.month + credit.history + age.in.years + savings.account.and.bonds + 
              purpose + present.employment.since + housing + other.installment.plans + 
              credit.amount + other.debtors.or.guarantors + installment.rate.in.percentage.of.disposable.income, 
            data = train_woe)
glm2 %>% summary()

glm2 %>% 
  coefficients() %>% 
  as.data.frame() %>%
  rownames() %>% 
  .[-1] %>% 
  as.factor() -> all.vars
all.vars %>% length()
all.vars_char <- all.vars %>% as.character()

glm2 %>% vif() %>% arrange(desc(gvif)) %>% 
  pull(variable) -> all_vars
# Multicollinrarity

hchart(cor(
  train_woe %>% 
    select(target,all.vars) %>% 
    mutate_if(is.factor,as.numeric)) %>%
    round(.,2),label = T)
# VIF - glm2
# https://www.statisticshowto.datasciencecentral.com/variance-inflation-factor/
# glm2 %>% vif() %>% arrange(desc(gvif)) %>% 
#   filter(gvif<1.24) %>% 
#   pull(variable) -> afterVIF
# 
# f <- as.formula(paste(outcome, paste(afterVIF, collapse = " + "), sep = " ~ "))
# glm3 <- glm(f, data = train_woe)
# 
# glm3 %>% summary()

glm2 %>% scorecard::vif() %>% 
  pull(variable) -> selected

step$call

hchart(cor(
  train_woe %>% 
    select(target,selected)) %>%
    round(.,2),label = T)
# ================================================================================
rose_sample_train_data <- ROSE(target ~ ., data = train_woe,  seed=123)$data
print('Number of transactions in train dataset after applying ROSE sampling method')
print(table(rose_sample_train_data$target))

rose_sample_test_data <- ROSE(target ~ ., data = test_woe,  seed=123)$data
print('Number of transactions in train dataset after applying ROSE sampling method')
print(table(rose_sample_test_data$target))

# Modeling with GLM ----
h2o.init()
train_h2o <- as.h2o(train_woe %>% select(target,selected)) 
test_h2o <- as.h2o(test_woe %>% select(target,selected))

outcome <- "target"
features <- train_woe %>% select(selected) %>% 
  names()

model <- h2o.glm(
  x = features,
  y = outcome,
  training_frame = train_h2o,
  family = "binomial", 
  seed = 123,
  nfolds = 5, #Number of folds for K-fold cross-validation
  remove_collinear_columns = T, #Collinear columns can cause problems during model fitting. This option can only be used with the 'IRLSM' solver
  #balance_classes = T, 
  max_runtime_secs = 180
)

model %>% h2o.auc() %>% round(2)

#model %>% h2o.giniCoef() %>% round(2)

model %>% h2o.performance(newdata = test_h2o) %>% h2o.auc() %>% round(2)
#model %>% h2o.performance(newdata = test_h2o) %>% h2o.giniCoef() %>% round(2)

# before balancing train_auc = 83, test_auc = 80
# after balancing train_auc = 81, test_auc = 76

model %>% h2o.std_coef_plot()

model@model$coefficients %>% as.data.frame() %>% 
  mutate(names = rownames(model@model$coefficients %>% as.data.frame())) %>%
  `colnames<-`(c('coefficients','names')) %>% 
  select(names,coefficients) %>%
  filter(coefficients != 0) %>%
  arrange(desc(coefficients))

h2o.varimp(model) %>% as.data.frame() %>% 
  pull(percentage) %>% sum()

h2o.varimp(model) %>% as.data.frame() %>% .[.$percentage>0,] %>%
  pull(variable) -> imp.vars
imp.vars %>% length()

h2o.varimp(model) %>% as.data.frame() %>% .[.$percentage != 0,] %>%
  select(variable, percentage) %>%
  hchart("pie", hcaes(x = variable, y = percentage)) %>%
  hc_colors(colors = 'orange') %>%
  hc_xAxis(visible=T) %>%
  hc_yAxis(visible=T)

model %>% h2o.performance(newdata = test_h2o) %>% 
  h2o.find_threshold_by_max_metric('f1')
pred <- model %>% h2o.predict(newdata = test_h2o) %>% as.data.frame()
pred %>% select(predict) %>% table()


# scorecard
card <- bins %>% scorecard(model@model)

# credit score, only_total_score = TRUE
train_score <- train %>% scorecard_ply(card)
test_score <- test %>% scorecard_ply(card)

# psi
psi <- perf_psi(
  score = list(train = train_score, test = test_score),
  label = list(train = train$target, test = test$target)
)
psi$psi  
#psi$pic  


# only_total_score = FALSE
train_score2 <- train %>% scorecard_ply(card, only_total_score=FALSE)
test_score2 <- test %>% scorecard_ply(card, only_total_score=FALSE)

# psi
psi2 <- perf_psi(
  score = list(train = train_score2, test = test_score2),
  label = list(train = train$target, test = test$target)
)
psi2$psi  

# AUC
perf <- h2o.performance(model, train_h2o)
train_auc<-h2o.auc(perf, xval = TRUE)

perf <- h2o.performance(model, test_h2o)
test_auc<-h2o.auc(perf, xval = TRUE)

tibble(train_auc, test_auc)


# H2O.AUTOML MODEL
data_iv$target <- as.factor(data_iv$target)
h2o.init()
h2o_data <- as.h2o(data_iv)

# Splitting the data
h2o_data <- h2o.splitFrame(h2o_data,ratios = c(0.7,0.15),seed=123)
train<-h2o_data[[1]]
validation<-h2o_data[[2]]
test<-h2o_data[[3]]

outcome <- 'target' # y - variable which we want to predict
features <- data_iv %>% select(-target) %>% names() # by using x variables predict y

# Fitting h2o model
model_automl <- h2o.automl(
  x = features,
  y = outcome,
  training_frame    = train,
  validation_frame = validation,
  leaderboard_frame = test,
  stopping_metric = "AUC",
  balance_classes = T,
  seed = 123,
  max_runtime_secs = 180)
 
model_automl@leader
