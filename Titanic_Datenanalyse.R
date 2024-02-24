setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("Funktionen-R-Skript 1.R")
daten <- read.csv("bearbeitet_titanic.cvs")

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

kategoriale_stats(daten$Embarked) ## Fuer die kategoriale Variable 'Embarked',
# daher beim welchem Hafen die Passagiere zugestiegen sind, ergeben sich folgende
# Statistiken:

# Hauefigkeitstabelle: 
#   x
# C          Q          S 
# 0.18855219 0.08641975 0.72278339 
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
# [1] "S"
