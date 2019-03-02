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

	#data$anz_cs_kontakte_sqrt = sqrt(data$anz_cs_kontakte)
data$anz_cs_kontakte_log = log(data$anz_cs_kontakte+0.1)
data$anz_kinder_log = log(data$anz_kinder+0.01)
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

#	PCA nur mit Auftraege

pcaa = prcomp(data[ , c(37:42, 50,52)], scale = T)

corrplot(cor(cbind(data[ , c(37:42, 50,52)], pcaa$x)), method = 'ellipse', insig = 'n', type = 'upper')

# 	PCA retouren

pcar = prcomp(data[ , c(43,45:49, 51, 53)], scale = T)

corrplot(cor(cbind(data[ , c(43,45:49, 51, 53)], pcar$x)), method = 'ellipse', insig = 'n')

# PCA tage

pcad = prcomp(data[ ,59:62], scale = T)

corrplot(cor(cbind(data[ , 59:62], pcad$x)), method = 'ellipse', insig = 'n')


newdata = data.frame(cbind(pcaa$x, pcar$x, pcad$x))
names(newdata)[1:8] = c('PC1a', 'PC2a', 'PC3a', 'PC4a', 'PC5a', 'PC6a', 'PC7a', 'PC8a')
names(newdata)[9:16] = c('PC1r', 'PC2r', 'PC3r', 'PC4r', 'PC5r', 'PC6r', 'PC7r', 'PC8r')
names(newdata)[c(17:20)] = c('PC1t', 'PC2t', 'PC3t', 'PC4t')

newdata = cbind(data[ ,c(1:3, 6, 34:35, 53:57 )], newdata, data[ ,33])
names(newdata)[32] = 'kauf_nach_flyer'

set.seed(123)
holdoutsample = sample(1:nrow(newdata), nrow(newdata)/3)
traindata = newdata[-holdoutsample,]
testdata = newdata[holdoutsample,]


	# undersampe traindata, remove 10% of non-conversions

rem = sample(as.integer(rownames(traindata[traindata$kauf_nach_flyer == 0, ])), nrow(traindata[traindata$kauf_nach_flyer == 0, ])*0.85)
traindata = traindata[!rownames(traindata) %in% rem, ]


	# glm without PCA
conv.glm = glm(kauf_nach_flyer ~ einwohnerklasse_wohnort + kundenalter + anz_cs_kontakte_log + anz_kinder + kinder_geb_saison + anz_auftraege_log + anz_auftraege_0_to_30_log + anz_auftraege_30_to_90_log + anz_auftraege_90_to_180_log + anz_auftraege_180_to_360_log + anz_auftraege_360_to_720_log + anz_retouren_log + anz_retouren_bin + anz_retouren_90_to_180_log + anz_nl_geoeffnet_0_to_30_bin + anz_auftraege_nl_0_to_30_bin + tage_erstkauf_log + tage_erst_zweit_kauf_log + tage_letzt_vorletzt_kauf_log + kundenalter:anz_cs_kontakte_log + kundenalter:anz_retouren_log + kundenalter:anz_retouren_90_to_180_log + anz_cs_kontakte_log:tage_erstkauf_log + kinder_geb_saison:anz_nl_geoeffnet_0_to_30_bin + kinder_geb_saison:tage_erstkauf_log + einwohnerklasse_wohnort:anz_auftraege_0_to_30_log + anz_auftraege_30_to_90_log:tage_letzt_vorletzt_kauf_log + anz_cs_kontakte_log:anz_auftraege_360_to_720_log + kundenalter:anz_retouren_bin + anz_auftraege_90_to_180_log:anz_retouren_90_to_180_log, data = traindata, family = 'binomial', maxit = 1100)

	# glm using PCA
conva.glm = glm(kauf_nach_flyer ~ kundenalter + kinder_geb_saison + anz_cs_kontakte_log + PC1a + PC2a + PC3a + PC5a + PC1r + PC4r + PC2t + kundenalter:anz_cs_kontakte_log + PC2a:PC2t + anz_cs_kontakte_log:PC3a + PC3a:PC5a + PC3a:PC1r + kundenalter:PC4r, data = traindata, family = 'binomial')

	# full model stepwise selection

convfull.glm = glm(kauf_nach_flyer ~ .*., data = traindata[ ,-1], family = 'binomial')

convstep.glm = step(convfull.glm)
