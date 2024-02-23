## Aufgabe 2a)

#Oeffnet die Hilfsfunktionen wie z.B. Entropie
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("Funktionen-R-Skript 2.R")


## i)
## Fuer metrische Variablen
 
deskriptive_stats <- function(x, functions = list(N = length, MEAN = mean, 
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
#Erstellt deskriptive Statistiken für kategoriale Merkmale:

kategoriale_stats <-function(x, functions = list(Laenge = length,Entropie = entropie, Norm_Entropie = nomentropie,
                                                 Haeufigkeitstabelle = hktabelle )){
  # Gewuenschte Statistiken koennen nach Bedarf eingefuegt werden
  # Im default sind Laenge, Entropie, normierte Entropie und eine Hauefigkeitstabelle
  result <- list()
  for(i in 1:3){
    result[i] <- functions[[i]](x) # An i'ter Stelle der Liste wird die Funktion auf die Daten (x) angewendet
    names(result)[i] <- names(functions)[i] # Der Name des i'ten Elements der Liste wird auf den Namen der i'ten Funktion geaendert
  }
  cat("\nHauefigkeitstabelle: \n") #Ueberschrift
  print(hktabelle(x)) #Gibt eine Hauefigkeitstabelle aus
  cat("\n") #Absatz
  return(result) #Ergebnisse ausgeben und Funktion beenden
}


# iii. 
#Funktion für bivariate deskriptive Statistiken von zwei kategorialen Variablen
compute_bivariate_stats_categorical <- function(var1, var2) {
  if (!is.factor(var1) || !is.factor(var2)) {
    stop("Die Eingaben müssen kategoriale Variablen sein.")
  }
  
  # Kreuztabelle erstellen
  cross_table <- table(var1, var2)
  
  # Chi-Quadrat-Test durchführen
  chi_sq_test <- chisq.test(cross_table)
  
  # Ausgabe der Kreuztabelle und des Chi-Quadrat-Tests
  cat("Kreuztabelle:\n")
  print(cross_table)
  
  cat("\nChi-Quadrat-Test:\n")
  print(chi_sq_test)
  
  # Rückgabe der Statistiken
  return(list(cross_table = cross_table, chi_sq_test = chi_sq_test))
}

# iv)
# Funktion für Zusammengang zwischenmetrischen und dichotomen Variable 

compute_bivariate_statistics <- function(metric_var, dichotom_var ){
  metric_var_mean = mean(metric_var) #arithmetisches Mittel
  metric_var_median = median(metric_var) #median bestimmen
  metric_var_varianz = var(metric_var) #Varianz
  metric_var_standardabw. = sqrt(metric_var_varianz) #Standardabweichung
  
  #Bestimme Häufigkeiten der 2 Auspraegungen
  dichotomous_counts <- table(dichotom_var)
  #Kreztabelle erstellen
  cross_tab <- table(metric_var, dichotom_var)
  
}
