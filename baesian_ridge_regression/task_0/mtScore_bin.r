library(pROC)
library(rms)
library(brglm)
library(corrplot)

data = read.csv('c:/users/lenovo/downloads/20140415_scoring_modell_daten.csv', header = T, sep = ';', dec = ',')

names(data) = tolower(names(data))
names(data)[1] = 'id'
data$kauf_nach_flyer = factor(data$kauf_nach_flyer)
data$kinder_geb_saison  = factor(data$kinder_geb_saison)
data$einwohnerklasse_wohnort = factor(data$einwohnerklasse_wohnort)

set.seed(123)
holdoutsample = sample(1:nrow(newdata), nrow(newdata)/3)
traindata = data[-holdoutsample,]
testdata = data[holdoutsample,]

#rem = sample(as.integer(rownames(traindata[traindata$kauf_nach_flyer == 0, ])), nrow(traindata[traindata$kauf_nach_flyer == 0, ])*0.5)
#traindata = traindata[!rownames(traindata) %in% rem, ]

conv.bin.red.glm = brglm(kauf_nach_flyer ~ kinder_geb_saison + log(anz_auftraege) + 
    factor(anz_auftraege_0_to_30 > 0) + factor(anz_auftraege_30_to_90 > 
    0) + factor(anz_auftraege_90_to_180 > 0) + log(anz_retouren + 
    0.01) + factor(anz_retouren_30_to_90 > 0) + log(anz_positionen_retouren + 0.01) + factor(anz_nl_geoeffnet_0_to_30 > 
    0) + log(tage_erstkauf) + 
    log(tage_erst_zweit_kauf + 0.01) + kinder_geb_saison:factor(anz_nl_geoeffnet_0_to_30 > 0), family = "binomial", data = traindata)

# resid .dev: 3948

testdata$pred = predict(conv.bin.red.glm, testdata, 'response')
testdata$response = testdata$pred >0.5
table(testdata$response, testdata$kauf_nach_flyer)

roc1 = roc(kauf_nach_flyer ~ pred, data = testdata)
plot(roc1, main = 'ROC-Kurve') 	# area under curve: 0.75...

testdata$decile = cut(testdata$pred, breaks = quantile(testdata$pred, probs = seq(0, 1, 0.1)), labels = 1:10)
liftm = matrix(table(testdata$decile, testdata$kauf_nach_flyer), ncol = 2)
liftch = liftm[, 2] / apply(liftm, 1, sum)
plot(liftch, xlim = c(10,1), main = 'Conversion-Wahrscheinlichkeit', xlab = 'Dezil', ylab = 'Anteil Conversions')	# lift chart looks ok...