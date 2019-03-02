
library(pROC)
library(rms)
library(brglm)
library(corrplot)


data = read.csv('c:/users/lenovo/downloads/20140415_scoring_modell_daten.csv', header = T, sep = ';', dec = ',')

names(data) = tolower(names(data))

data$ï..id = factor(data$ï..id)
names(data)[1] = 'id'
data$kauf_nach_flyer = factor(data$kauf_nach_flyer)
data$kinder_geb_saison  = factor(data$kinder_geb_saison)
data$einwohnerklasse_wohnort = factor(data$einwohnerklasse_wohnort)

	# transform some variables to nicer distributions

	#data$einwohnerklasse_wohnort_log = log(data$einwohnerklasse_wohnort)
	#data$anz_cs_kontakte_sqrt = sqrt(data$anz_cs_kontakte)
data$anz_cs_kontakte_log = log(data$anz_cs_kontakte+0.1)
data$anz_kinder_bekannt = factor(data$anz_kinder > 0)
data$anz_auftraege_log = log(data$anz_auftraege)
	#data$anz_auftraege_0_to_30_sqrt = sqrt(data$anz_auftraege_0_to_30)
data$anz_auftraege_0_to_30_log = log(data$anz_auftraege_0_to_30+0.1)
	#data$anz_auftraege_30_to_90_sqrt = sqrt(data$anz_auftraege_30_to_90)
data$anz_auftraege_30_to_90_log = log(data$anz_auftraege_30_to_90+0.1)
	#data$anz_auftraege_90_to_180_sqrt = sqrt(data$anz_auftraege_90_to_180)
data$anz_auftraege_90_to_180_log = log(data$anz_auftraege_90_to_180+0.1)
	#data$anz_auftraege_180_to_360_sqrt = sqrt(data$anz_auftraege_180_to_360)
data$anz_auftraege_180_to_360_log = log(data$anz_auftraege_180_to_360+0.1)
	#data$anz_auftraege_360_to_720_sqrt = sqrt(data$anz_auftraege_360_to_720)
data$anz_auftraege_360_to_720_log = log(data$anz_auftraege_360_to_720+0.1)
	#data$anz_retouren_sqrt = sqrt(data$anz_retouren)
data$anz_retouren_log = log(data$anz_retouren+0.1)
data$anz_retouren_bin = factor(data$anz_retouren > 0)
	#data$anz_retouren_0_to_30_sqrt = sqrt(data$anz_retouren_0_to_30)
data$anz_retouren_0_to_30_log = log(data$anz_retouren_0_to_30+0.1)
	#data$anz_retouren_30_to_90_sqrt = sqrt(data$anz_retouren_30_to_90)
data$anz_retouren_30_to_90_log = log(data$anz_retouren_30_to_90+0.1)
	#data$anz_retouren_90_to_180_sqrt = sqrt(data$anz_retouren_90_to_180)
data$anz_retouren_90_to_180_log = log(data$anz_retouren_90_to_180+0.1)
	#data$anz_retouren_180_to_360_sqrt = sqrt(data$anz_retouren_180_to_360)
data$anz_retouren_180_to_360_log = log(data$anz_retouren_180_to_360+0.1)
	#data$anz_retouren_360_to_720_sqrt = sqrt(data$anz_retouren_360_to_720)
data$anz_retouren_360_to_720_log = log(data$anz_retouren_360_to_720+0.1)
data$sum_liefermenge_auftraege_log = log(data$sum_liefermenge_auftraege)
	#data$sum_liefermenge_retouren_sqrt = sqrt(data$sum_liefermenge_retouren)
data$sum_liefermenge_retouren_log = log(data$sum_liefermenge_retouren+0.1)
data$anz_positionen_auftrag_log = log(data$anz_positionen_auftrag)
	#data$anz_positionen_retouren_sqrt = sqrt(data$anz_positionen_retouren)
data$anz_positionen_retouren_log = log(data$anz_positionen_retouren+0.1)
	#data$anz_auftraege_gschein_sqrt = sqrt(data$anz_auftraege_gschein)
data$anz_auftraege_gschein_log = log(data$anz_auftraege_gschein+0.1)
data$anz_auftraege_nl_bin = factor(data$anz_auftraege_nl > 0)
	#data$anz_nl_erhalten_0_to_30_sqrt = sqrt(data$anz_nl_erhalten_0_to_30)
data$anz_nl_erhalten_0_to_30_log = log(data$anz_nl_erhalten_0_to_30+0.1)
data$anz_nl_geoeffnet_0_to_30_bin = factor(data$anz_nl_geoeffnet_0_to_30 > 0)
data$anz_auftraege_nl_0_to_30_bin = factor(data$anz_auftraege_nl_0_to_30 > 0)
data$tage_erstkauf_log = log(data$tage_erstkauf)
data$tage_letztkauf_log = log(data$tage_letztkauf)
	#data$tage_erst_zweit_kauf_sqrt = sqrt(data$tage_erst_zweit_kauf)
data$tage_erst_zweit_kauf_log = log(data$tage_erst_zweit_kauf+0.1)
	#data$tage_letzt_vorletzt_kauf_sqrt = sqrt(data$tage_letzt_vorletzt_kauf)
data$tage_letzt_vorletzt_kauf_log = log(data$tage_letzt_vorletzt_kauf+0.1)

	# add PCA scores

conv.pca = prcomp(data[ ,c(3, 5, 34, 36:42, 44:53, 24:25, 27:28, 55, 58:61)], scale = T) # 11 PCs relevant
data = cbind(data[ ,c(1:32,34:61)], conv.pca$x[ ,1:10], data[ ,33])
names(data)[71] = 'kauf_nach_flyer'

summary(data)

	# plot correlations of PC to old variables


cpca = cor(data[ ,c(3,5,33,35:41,43:52,24:25,27:28,54,57:70)])
corrplot(cpca[1:29, 30:39], method = 'ellipse', tl.cex = 0.8, insig = 'n')

set.seed(123)
holdoutsample = sample(1:nrow(data), nrow(data)/3)
traindata = data[-holdoutsample,]
testdata = data[holdoutsample,]

	# undersampe traindata, remove 10% of non-conversions

rem = sample(as.integer(rownames(traindata[traindata$kauf_nach_flyer == 0, ])), nrow(traindata[traindata$kauf_nach_flyer == 0, ])*0.5)
traindata = traindata[!rownames(traindata) %in% rem, ]

conv.glm = brglm(kauf_nach_flyer ~ kinder_geb_saison + anz_auftraege_log + anz_retouren_log + tage_erstkauf + tage_letztkauf + anz_auftraege_90_to_180_log + anz_auftraege_180_to_360_log, data = traindata, family = 'binomial', maxit = 100)
convfull.glm = brglm(kauf_nach_flyer ~ ., data = traindata, family = 'binomial')

	# collinearity?


vif(conv.glm)


	# confusion matrix with testdata

testdata$pred = predict(conv.glm, testdata, 'response')
testdata$response = testdata$pred >0.5


	# massive bias! classifies as non-conversion

table(testdata$response, testdata$kauf_nach_flyer)


	# create deciles and lift chart

testdata$decile = cut(testdata$pred, breaks = quantile(testdata$pred, probs = seq(0, 1, 0.1)), labels = 1:10)
liftm = matrix(table(testdata$decile, testdata$kauf_nach_flyer), ncol = 2)
liftch = liftm[, 2] / apply(liftm, 1, sum)

plot(liftch)

	# ROC curve

roc1 = roc(kauf_nach_flyer ~ pred, data = testdata)
plot(roc1)

	# final model with factor interactions

convpca.glm = brglm( kauf_nach_flyer ~ PC1 + PC3 + PC4 + PC6 + PC7 + PC8 + PC10 + kinder_geb_saison + anz_kinder_bekannt + einwohnerklasse_wohnort + PC1:PC6 + PC1:PC7 + PC4:einwohnerklasse_wohnort + PC8:PC10 + PC4:PC8, family = "binomial", data = traindata, maxit = 1000)

#cost = function(kauf_nach_flyer, pi = 0) mean(abs(kauf_nach_flyer-pi) > 0.5)
#convpca.boot.glm = cv.glm(traindata, convpca.glm, 20)

	# confusion matrix

testdata$pred = predict(convpca.glm, testdata, 'response')
testdata$response = testdata$pred >0.5
table(testdata$response, testdata$kauf_nach_flyer)