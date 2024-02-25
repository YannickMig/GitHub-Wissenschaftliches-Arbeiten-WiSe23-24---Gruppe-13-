## Aufgabe 2a)

#Oeffnet die Hilfsfunktionen wie z.B. Entropie
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("Funktionen-R-Skript 2.R")
#install.packages("rcompanion")
library("rcompanion")


## i)
## Fuer metrische Variablen
 
metrische_stats <- function(x, functions = list(ANZAHL = length, MEAN = mean, 
                                                  MEDIAN = median, STANDARDABWEICHUNG = sd,
                                                  MINIMUM = min, MAXIMUM = max, SPANNWEITE = spannweite,
                                                  VARIATIONSKOEFFIZIENT = vari_koeff, INTERQUANTILSABSTAND = int_quant)){ 
  # Gewuenschte Statistiken koennen nach Bedarf eingefuegt werden
  result <- list()
  for(i in 1:length(functions)){
    result[i] <- functions[[i]](x) # An i'ter Stelle der Liste wird die Funktion auf die Daten (x) angewendet
    names(result)[i] <- names(functions)[i] # Der Name des i'ten Elements der Liste wird auf den Namen der i'ten Funktion geaendert
  }
  result[length(functions)+1] <- quantile(x)[[2]]
  result[length(functions)+2] <- quantile(x)[[4]]
  names(result)[length(functions)+1] <- "QUANTIL_25_PROZENT"
  names(result)[length(functions)+2] <- "QUANTIL_75_PROZENT"
  return(result)
}


#ii)
#Erstellt deskriptive Statistiken fuer kategoriale Merkmale:

kategoriale_stats <-function(x, functions = list(Laenge = length,Entropie = entropie, Norm_Entropie = normentropie, Modus = modus,
                                                 Haeufigkeitstabelle = hktabelle)){
  # Gewuenschte Statistiken koennen nach Bedarf eingefuegt werden
  # Im default sind Laenge, Entropie, normierte Entropie und eine Hauefigkeitstabelle
  result <- list()
  for(i in 1:(length(functions)-1)){
    result[i] <- functions[[i]](x) # An i'ter Stelle der Liste wird die Funktion auf die Daten (x) angewendet
    names(result)[i] <- names(functions)[i] # Der Name des i'ten Elements der Liste wird auf den Namen der i'ten Funktion geaendert
  }
  cat("\nHauefigkeitstabelle: \n") #Ueberschrift
  print(hktabelle(x)) #Gibt eine Hauefigkeitstabelle aus
  cat("\n") #Absatz
  return(result) #Ergebnisse ausgeben und Funktion beenden
}


# iii)
#Funktion fuer bivariate deskriptive Statistiken von zwei kategorialen Variablen
compute_bivariate_stats_categorical <- function(var1, var2) {
  #if (!is.factor(var1) || !is.factor(var2)) {
  #  stop("Die Eingaben müssen kategoriale Variablen sein.")
  #}

  # Kreuztabelle erstellen
  cross_table <- table(var1, var2)
  
  # Chi-Quadrat-Test durchführen
  chi_sq_test <- chisq.test(cross_table)
  
  # Berechnung der Chi-Quadrat-Statistik
  chi_square <- chisq.test(cross_table)$statistic
  
  # Anzahl der Zeilen und Spalten in der Kreuztabelle
  num_rows <- nrow(cross_table)
  num_cols <- ncol(cross_table)
  
  # Berechnung von Cramer's V
  cramers_v <- cramerV(var1,var2)
  # Rückgabe der Statistiken
  return(list(Kreuztabelle = cross_table, chi_quadrat=chi_square,   chi_sq_test = chi_sq_test, CramersV =cramers_v))
}

# iv)
# Funktion für Zusammengang zwischen metrischen und dichotomen Variable 

compute_bivariate_statistics <- function(metric_var, dichotom_var ){
  metric_var_mean = mean(metric_var) #arithmetisches Mittel
  metric_var_median = median(metric_var) #median bestimmen
  metric_var_varianz = var(metric_var) #Varianz
  metric_var_standardabw. = sqrt(metric_var_varianz) #Standardabweichung
  
  #Bestimme Haeufigkeiten der 2 Auspraegungen
  dichotomous_counts <- table(dichotom_var)
  #Kreztabelle erstellen
  cross_tab <- table(metric_var, dichotom_var)
  
  #Korrelation nach Spearman
  s <- cor(metric_var,dichotom_var, method="spearman")
  
  # T-Test
  t_test_result <- t.test(metric_var  ~ dichotom_var)
  
  # Cohens d
  cohen_d <- (mean(metric_var[dichotom_var == 1]) - mean(metric_var[dichotom_var == 0])) / sd(metric_var)
  
  return(list(Korrelation_Spearman=s, T_Test=t_test_result, Cohens_d = cohen_d))
  
}

#v)
#Visualisiert 4 kategoriale Variablen über Haeufigkeits Barplots
visual <- function(x,y,z,b = NULL){
  if(!is.null(b)){ #wenn vier Variablen da sind
    par(mfrow=c(2,2))
    barplot(hktabelle(x), main=deparse(substitute(x)),ylim=c(0:1), ylab="relative Hauegikeit") #Mit Titel der Variable
    barplot(hktabelle(y), main=deparse(substitute(y)), ylim=c(0:1),ylab="relative Hauegikeit") 
    barplot(hktabelle(z), main=deparse(substitute(z)), ylim=c(0:1),ylab="relative Hauegikeit")
    barplot(hktabelle(b), main=deparse(substitute(b)), ylim=c(0:1), ylab="relative Hauegikeit")
    par(mfrow = c(1,1))
  }
  if(is.null(b)){ #fuer drei Variablen
    par(mfrow=c(1,3)) #drei Grafiken nebeneinander
    barplot(hktabelle(x), main=deparse(substitute(x)), ylim=c(0:1), ylab="relative Hauegikeit")
    barplot(hktabelle(y), main=deparse(substitute(y)), ylim=c(0:1),ylab="relative Hauegikeit") 
    barplot(hktabelle(z), main=deparse(substitute(z)), ylim=c(0:1),ylab="relative Hauegikeit")   
   
    par(mfrow = c(1,1))
  }
}
