library(data.table)
#library(car)
library(glmnet)
library(ROCR)
library(caret)
library(ggplot2)

events <- fread("/home/vinzent/Documents/repos/dataenv/task_0/20140415_Scoring_Modell_daten.csv", sep = ';')

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


# improve normalcy of data
vars <- melt(events[ , -c(1,2,6,33,34,35,37,38,45:52)])

ggplot(vars, aes(value)) + geom_density() + facet_wrap(~variable, scales = "free")


ggplot(events, aes(log(TAGE_LETZTKAUF))) + geom_density()
qqPlot(events$RETOUREN_ANTEIL, distribution = "norm")
qqPlot(sqrt(events$RETOUREN_ANTEIL), distribution = "norm")
qqPlot(log(events$TAGE_LETZT_VORLETZT_KAUF), distribution = "norm")
qqPlot(yjNormalize(events$RETOUREN_ANTEIL), distribution = "norm")

# scale numeric columns

scale_columns = names(events[, -c(1,2,6,33,34,35,37,38,45:52)])
log_columns = c("SUM_LIEFERMENGE_AUFTRAEGE", "ANZ_POSITIONEN_AUFTRAG")

sqrt_columns = c("ANZ_AUFTRAEGE_360_TO_720", "ANZ_RETOUREN_0_TO_30", "TAGE_LETZT_VORLETZT_KAUF")

events[, (log_columns) := lapply(.SD, log), .SDcols = log_columns]
events[, (sqrt_columns) := lapply(.SD, sqrt), .SDcols = sqrt_columns]
#events[, ANZ_CS_KONTAKTE := as.factor(ifelse(ANZ_CS_KONTAKTE == 0, "0", ifelse(ANZ_CS_KONTAKTE == 1, "1", "2+")))]
#events[, ANZ_KINDER := as.factor(ifelse(ANZ_KINDER == 0, "0", ifelse(ANZ_KINDER == 1, "1", "2+")))]
#events[, ANZ_AUFTRAEGE_0_TO_30 := as.factor(ifelse(ANZ_AUFTRAEGE_0_TO_30 == 0, "0", ifelse(ANZ_AUFTRAEGE_0_TO_30 == 1, "1", "2+")))]
#events[, ANZ_AUFTRAEGE_30_TO_90 := as.factor(ifelse(ANZ_AUFTRAEGE_30_TO_90 == 0, "0", ifelse(ANZ_AUFTRAEGE_30_TO_90 == 1, "1", "2+")))]
#events[, ANZ_AUFTRAEGE_90_TO_180 := as.factor(ifelse(ANZ_AUFTRAEGE_90_TO_180 == 0, "0", ifelse(ANZ_AUFTRAEGE_90_TO_180 == 1, "1", "2+")))]
#events[, ANZ_AUFTRAEGE_180_TO_360 := as.factor(ifelse(ANZ_AUFTRAEGE_180_TO_360 == 0, "0", ifelse(ANZ_AUFTRAEGE_180_TO_360 == 1, "1", ifelse(ANZ_AUFTRAEGE_180_TO_360 == 2, "2", "3+"))))]

events[, (scale_columns) := lapply(.SD, scale), .SDcols = scale_columns]


# train/test split
set.seed(111)

sample = sample(1:nrow(events), nrow(events)*(2/3))

train_dt <- events[sample, ]

upsample <- function(data, ratio, feature, sample_indices) {
  multiply_indices <- data[KAUF_NACH_FLYER == feature, which = T]
  multiply <- multiply[rep(1:nrow(multiply), each = ratio), ]
  rbind(data, multiply)
}

multiply_indices <- events[KAUF_NACH_FLYER == "1" & sample, which = T]
#dsample= downSample(sample, events[sample, ]$KAUF_NACH_FLYER)[, 1]

# ad hoc downsampling
#dsample <- sample(sample, length(sample)/3)

#usample = data.table(upSample(events[sample, ], events[sample, ]$KAUF_NACH_FLYER)[, -41], sample = "usample")
#events.new = rbind(usample, data.table(events[-sample, ], sample = "test"))

input = model.matrix( ~.-1, data = events[, -c("ID", "KAUF_NACH_FLYER")])
#input = model.matrix( ~.-1, data = events.new[, -c("ID", "KAUF_NACH_FLYER", "sample")])

# when upsampled
#train = input[events.new$sample == "usample", ]
#resp = events.new[sample == "usample", ]$KAUF_NACH_FLYER
#test = input[events.new$sample == "test", ]


train = input[sample, ]
resp = events[sample, ]$KAUF_NACH_FLYER
#weights <- ifelse(resp == 0, 1, 3)
# 5: 60 62

test = input[-sample, ]


#plot(hist(events$ANZ_CS_KONTAKTE))
#plot(hist(sqrt(events$ANZ_CS_KONTAKTE)))

yjNormalize = function(x) {
  bc = boxCox(lm(x ~ 1), family="yjPower")
  l = bc$x[which.max(bc$y)]
  yjPower(x, lambda = l)
}

qqPlot(yjNormalize(events$KUNDENALTER), distribution = "norm")
#ANZ_CS_KONTAKTE_YJ = yjPower(events$ANZ_CS_KONTAKTE, lambda = l)

l = 10^seq(6, -5, by = -.1)

event_glmn_cv <- cv.glmnet(train, resp, alpha = 0, family = "binomial", lambda = l, standardize = F, nfolds = 12)

plot(event_glmn_cv)
lmin = event_glmn_cv$lambda.min # 0.01995262 # 0.01584893 # 0.01584893

# final model with best lambda
event_glmn <- glmnet(train, resp, alpha = 0, family = "binomial", lambda = lmin, standardize = F)

str(event_glmn)

KAUF_NACH_FLYER_PREDICTED = predict(event_glmn, s = lmin, newx = test, type = "response")

#pred = prediction(KAUF_NACH_FLYER_PREDICTED, events[-sample, ]$KAUF_NACH_FLYER)

table(ifelse(KAUF_NACH_FLYER_PREDICTED > 0.36, 1, 0), events[-sample, ]$KAUF_NACH_FLYER)

glmnet_residuals <- function(data, model, lambda) {
  insample_preds <- predict(model, s = lmin, newx = data, type = "response")
  return(abs(insample_preds - ifelse(resp == "1", 1, 0)))
}


train_dt$residuals <- glmnet_residuals(train, event_glmn, lmin)

ggplot(train_dt, aes(residuals)) + geom_density()

# identify 0.5% extreme cases
users_exclude <- train_dt[residuals >= quantile(residuals, 0.995), ID]

events <- events[-users_exclude, ]


#perf = performance(pred,"tpr","fpr")
#performance(pred, measure = "auc")# 0.7652726 # 0.766776 # 0.7672822 # 0.7668232 # 0.7649848 # 0.7643119 # 0.7600246
                      # lasso: 0.8 0.7602244, 0.5 0.760547 0.2 0.7599164 

plot(perf)

# Fazit
# transformierte, "normal-nähere Variablen bringen nur geringe Verbesserungen
# sehr 0-reiche Variablen zu Faktoren - sogar Verschlechterung der Vorhehrsage
# downsampling bringt keine Veränderung
# upsampling bringt leichte Verschlechterung
# Gewichte bringen kaum Veränderung