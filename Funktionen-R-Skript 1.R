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