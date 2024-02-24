setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("Funktionen-R-Skript 1.R")
daten <- read.csv("bearbeitet_titanic.cvs")


### Alter ###

str(daten)
metrische_stats(daten$Age) ## Fuer die metrische Variable 'Age', d.h. das Alter
## der Personen im Titanic-Datensatz, ergeben sich folgende Kennzahlen:

# $ANZAHL
# [1] 891
# 
# $MEAN
# [1] 29.3902
# 
# $MEDIAN
# [1] 30
# 
# $STANDARDABWEICHUNG
# [1] 13.26532
# 
# $MINIMUM
# [1] 0.42
# 
# $MAXIMUM
# [1] 80
# 
# $SPANNWEITE
# [1] 79.58
# 
# $VARIATIONSKOEFFIZIENT
# [1] 0.4513518
# 
# $INTERQUANTILSABSTAND
# [1] 14
# 
# $QUANTIL_25_PROZENT
# [1] 21
# 
# $QUANTIL_75_PROZENT
# [1] 35


### Ticketpreise ###


metrische_stats(daten$Fare) ## Fuer die metrische Variable 'Fare', d.h. der Ticketspreis
## im Titanic-Datensatz, ergeben sich folgende Kennzahlen:

# $ANZAHL
# [1] 891
# 
# $MEAN
# [1] 32.20421
# 
# $MEDIAN
# [1] 14.4542
# 
# $STANDARDABWEICHUNG
# [1] 49.69343
# 
# $MINIMUM
# [1] 0
# 
# $MAXIMUM
# [1] 512.3292
# 
# $SPANNWEITE
# [1] 512.3292
# 
# $VARIATIONSKOEFFIZIENT
# [1] 1.543073
# 
# $INTERQUANTILSABSTAND
# [1] 23.0896
# 
# $QUANTIL_25_PROZENT
# [1] 7.9104
# 
# $QUANTIL_75_PROZENT
# [1] 31


### Zustiegshafen ###

kategoriale_stats(daten$Embarked) ## Fuer die kategoriale Variable 'Embarked',
# daher beim welchem Hafen die Passagiere zugestiegen sind, ergeben sich folgende
# Statistiken:

# Hauefigkeitstabelle: 
#   x
# Cherbourg     Q (Queenstown)      S (Southampton)
# 0.18855219     0.08641975          0.72278339 
# 
# $Laenge
# [1] 891
# 
# $Entropie
# [1] 0.7608274
# 
# $Norm_Entropie
# [1] 0.692535
# 
# $Modus
# [1] "S" #also Southampton


### Geschlecht ####


kategoriale_stats(daten$Sex) ## Fuer die kategoriale Variable 'Sex',
# also dem Geschlecht, ergeben sich folgende
# Statistiken:
#Hauefigkeitstabelle: 
#  x
#  female     male 
#  0.352413 0.647587 

#$Laenge
#[1] 891

#$Entropie
#[1] 0.6489276

#$Norm_Entropie
#[1] 0.9362046

#$Modus
#[1] "male"


###  Zusammenhang zwischen Reiseklasse und Zustiegshafen  ###


compute_bivariate_stats_categorical(daten$Embarked, daten$Pclass)
#Kennzahlen zum Zusammenhang zwischen "Embarked" und "Pclass", 
#also der Zusammenhang zwischen Zustiegshafen und Reiseklasse:

#$Kreuztabelle
#     var2
#var1    1   2     3
#   C    85  17    66
#   Q    2   3     72
#   S    127 164  353

#$chi_quadrat
#X-squared 
#123.7519 

#$chi_sq_test

#Pearson's Chi-squared test

#data:  cross_table
#X-squared = 123.75, df = 4, p-value < 2.2e-16


#$CramersV
#[1] 0.2638214


###  Zusammenhang zwischen Alter und Überleben  ###

compute_bivariate_statistics(daten$Age, daten$Survived)

#$Korrelation_Spearman
#[1] -0.05724462

#$T_Test

#Welch Two Sample t-test

#data:  metric_var by dichotom_var
#t = 2.3002, df = 666.14, p-value = 0.02174
#alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
#95 percent confidence interval:
#  0.3144906 3.9828088
#sample estimates:
#  mean in group 0 mean in group 1 
#30.21494        28.06629 


#$Cohens_d
#[1] -0.1619749

### Grafiken ###

Klasse <- daten$Pclass
Geschlecht <- daten$Sex
Zustiegshafen <-  daten$Embarked

#Grafiken zu Geschlecht, Zustiegshafen, Klasse:
visual(Geschlecht, Klasse, Zustiegshafen)


