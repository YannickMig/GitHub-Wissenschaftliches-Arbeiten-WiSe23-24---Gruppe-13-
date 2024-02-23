## Aufgabe 2a
 # iv)
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
metric_var = c(rep(1, 10), rep(5, 15), rep(8, 20), rep(10, 12))
dichotom_var = c(rep("Ja", 30), rep("Nein", 27))
compute_bivariate_statistics(metric_var, dichotom_var)

print(cross_tab)
