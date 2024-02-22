## Aufgabe 2a)

## i)
## Fuer metrische Variablen

deskriptive_stats <- function(x, functions = list(N = length, MEAN = mean, MEDIAN = median, STANDARDABWEICHUNG = sd, MINIMUM = min, MAXIMUM = max)){ 
  # Gewuenschte Statistiken koennen nach Bedarf eingefuegt werden
  result <- list()
  for(i in 1:length(functions)){
    result[i] <- functions[[i]](x) # An i'ter Stelle der Liste wird die Funktion auf die Daten (x) angewendet
    names(result)[i] <- names(functions)[i] # Der Name des i'ten Elements der Liste wird auf den Namen der i'ten Funktion geaendert
  }
  return(result)
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
