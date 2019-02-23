library(data.table)
#library(car)
#library(ROCR)
#library(pROC)
#library(caret)
library(xgboost)
library(Matrix)

events <- fread("/home/vinzent/Documents/r_scrips/mytoys/20140415_Scoring_Modell_daten.csv", sep = ';')

summary(events)

events$EINWOHNERKLASSE_WOHNORT <- as.factor(events$EINWOHNERKLASSE_WOHNORT)
events$KINDER_GEB_SAISON <- as.factor(events$KINDER_GEB_SAISON)
events$SUM_RABATT_GSCHEIN <- as.numeric(sub(',', '.', events$SUM_RABATT_GSCHEIN))
events$KUNDENALTER_BIN <- as.factor(round(events$KUNDENALTER, -1))
events$KINDER_BIN <- events$ANZ_KINDER > 0
events$CS_KONTAKT_FREQ <- events$ANZ_CS_KONTAKTE / events$ANZ_AUFTRAEGE
events$RETOURNIERT <- events$ANZ_RETOUREN > 0
events$KONTAKTIERT <- events$ANZ_CS_KONTAKTE > 0
events$GUTSCHEIN_FREQ <- events$ANZ_AUFTRAEGE_GSCHEIN / events$ANZ_AUFTRAEGE
events[ , RETOUREN_ANTEIL := min((ANZ_RETOUREN / ANZ_AUFTRAEGE), 1), by = ID]
events[ , SUM_Retouren_POS := SUM_LIEFERMENGE_RETOUREN/SUM_LIEFERMENGE_AUFTRAEGE]
events[ , RETOUREN_POS := ANZ_POSITIONEN_RETOUREN / ANZ_POSITIONEN_AUFTRAG]
events$RABATT_AVG <- events$SUM_RABATT_GSCHEIN / events$ANZ_AUFTRAEGE
events[ , POSITIONEN_AVG := ANZ_POSITIONEN_AUFTRAG / ANZ_AUFTRAEGE]
events$EINMAL_KUNDE <- events$ANZ_AUFTRAEGE == 1
events$NEWSLETTER <- events$ANZ_AUFTRAEGE_NL > 0
events$NEWSLETTER_30 <- events$ANZ_AUFTRAEGE_NL_0_TO_30 > 0
events$OPENING <- events$ANZ_NL_GEOEFFNET_0_TO_30 > 0
events$JAHRE_ERSTKAUF <- as.factor(ceiling(events$TAGE_ERSTKAUF/365))
events$JAHRE_LETZTKAUF <- as.factor(ceiling(events$TAGE_LETZTKAUF/365))
events$JAHRE_ERST_ZWEIT_KAUF <- as.factor(ceiling(events$TAGE_ERST_ZWEIT_KAUF/365))
events$JAHRE_LETZT_VORLETZT_KAUF <- as.factor(ceiling(events$TAGE_LETZT_VORLETZT_KAUF/365))
events$KAUF_NACH_FLYER <- as.factor(events$KAUF_NACH_FLYER)

# retoure != auftrag!
events[RETOUREN_ANTEIL > 1, ]


# train/test split
set.seed(111)

sample <- sample(1:nrow(events), nrow(events)*(2/3))
#dsample= downSample(sample, events[sample, ]$KAUF_NACH_FLYER)[, 1]

#usample = data.table(upSample(events[sample, ], events[sample, ]$KAUF_NACH_FLYER)[, -41], sample = "usample")
#events.new = rbind(usample, data.table(events[-sample, ], sample = "test"))

# when upsampled
#train = input[events.new$sample == "usample", ]
#test = input[events.new$sample == "test", ]


train <- events[sample, ]
train.m<- sparse.model.matrix(KAUF_NACH_FLYER ~ .-1, data = train)
#train.m = xgb.DMatrix(model.matrix(KAUF_NACH_FLYER ~ .-1, data = train), weight = ifelse(train$KAUF_NACH_FLYER == "1", 1, 0.5))
resp <- train$KAUF_NACH_FLYER
test <- events[-sample, ]
test.m <- sparse.model.matrix(KAUF_NACH_FLYER ~ .-1, data = test)



# function for balanced subsampling
balance_sub <- function(data, column_name) {
  min_balance <- data[ , .(cases = .N), by = .(get(column_name))]
    if (nrow(min_balance) == 2) {
    min_cases <- as.integer(min_balance[which.min(min_balance$cases)][,2])
    min_class <- as.integer(min_balance[which.min(min_balance$cases)][,1])-1
    max_class <- as.integer(min_balance[which.max(min_balance$cases)][,1]) -1

    min_data <- data[get(column_name) == min_class, ]
    max_data <- data[get(column_name) == max_class, ]
    max_sample <- sample(1:nrow(max_data), min_cases)
    max_data <- max_data[max_sample, ]
    rbind(min_data, max_data)
    }
  }



xgb_grid <- expand.grid(nr = c(5000),
                       md = c(9, 12),
                       cs_bytree = seq(0.8, 1.0, length.out = 2),
                       e = c(0.001),
                       g=c(1, 3),
                       mcw = c(1, 3),
                       ss = seq(0.8, 1.0, length.out = 2),
                       spw = c(0.8)
)

cv_log <- NULL

for (i in 1:nrow(xgb_grid)){
  
  cv <- xgb.cv(data = train.m, label = ifelse(resp == "1", 1, 0), nfold = 10, objective = "binary:logistic", early_stopping_rounds = 150,
             # metrics = list(),
              nrounds = xgb_grid[i, ]$nr,
              max_depth = xgb_grid[i, ]$md,
              colsample_bytree = xgb_grid[i, ]$cs_bytree,
              eta = xgb_grid[i, ]$e,
              gamma = xgb_grid[i, ]$g,
              subsample = xgb_grid[i, ]$ss,
              scale_pos_weight = xgb_grid[i, ]$spw,
              min_child_weight = xgb_grid[i, ]$mcw,
              eval_metric = "logloss",
             max_delta_step = 3
              )
  # eval_metric: probiere auc, logloss
  cv_log <- rbind(cv_log, cbind(cv$evaluation_log[cv$best_iteration], data.frame(cv$params)))
  
  gc()
}

head(cv_log[order(cv_log$test_logloss_mean), ], 50)


best_iter <- as.list(cv_log[order(cv_log$test_logloss_mean), ][1, c("iter", "max_depth", "colsample_bytree", "eta", "gamma", "subsample", "scale_pos_weight", "min_child_weight", "eval_metric")])
names(best_iter)[1] <- "nrounds"


#events.xgb.cv <- train(train.m, resp, trControl = xgb_trcontrol, tuneGrid = xgb_grid, method = "xgbTree", num.threads = 2)

#events.xgb.cv$bestTune

xgb_args <- append(list(data = train.m, label = ifelse(resp == "1", 1, 0), objective = "binary:logistic", early_stopping_rounds = 100, best_iter), best_iter)
  
events_xgb <- do.call(xgboost, args = xgb_args)

#events.xgb <- xgboost(data = train.m, label = ifelse(resp == "1", 1, 0), nround = 54, objective = "binary:logistic", early_stopping_rounds = 100, scale_pos_weight = 1, eta = 0.01, max_depth = 9, colsample_bytree = 0.8, subsample = 1, min_child_weight = 2, eval_metric = "auc")


KAUF_NACH_FLYER_PREDICTED <- predict(events_xgb, test.m)
#KAUF_NACH_FLYER_PREDICTED <- predict(events_xgb, test.m, type = "prob")[, 2]

#table(KAUF_NACH_FLYER_PREDICTED, test$KAUF_NACH_FLYER)
table((KAUF_NACH_FLYER_PREDICTED > 0.40), test$KAUF_NACH_FLYER)
roc(response = test$KAUF_NACH_FLYER, preictor = ifelse(KAUF_NACH_FLYER_PREDICTED == "1", 1, 0))
#pred = prediction(KAUF_NACH_FLYER_PREDICTED, test$KAUF_NACH_FLYER)

#perf = performance(pred,"tpr","fpr")
#performance(pred, measure = "auc")# 0.7652726 # 0.766776 # 0.7672822 # 0.7668232 # 0.7649848 # 0.7643119 # 0.7600246
                      # lasso: 0.8 0.7602244, 0.5 0.760547 0.2 0.7599164 

plot(perf)