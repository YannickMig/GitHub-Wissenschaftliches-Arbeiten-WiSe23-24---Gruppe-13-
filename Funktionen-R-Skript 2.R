#Gibt eine Tabelle mit relativen Häufigkeiten aus
hktabelle <- function(x){
  return(table(x)/length(x))
}

#Berechnet die Entropie
entropie <- function(x){
  x <- as.numeric(hktabelle(x))
  ent <- numeric(length(x))
  i <- 1
  for(i in 1:length(x)){
    ent[i] = x[i]*log(1/(x[i]))
  }
  return(sum(ent)) 
}

#Berechnet die Normierte Entropie
# -> diese liegt zwischen eins und zwei und ist einfacher zu interpretieren
normentropie <- function(x){
  x <- as.numeric(hktabelle(x))
  ent <- numeric(length(x))
  i <- 1
  for(i in 1:length(x)){
    ent[i] = x[i]*log(1/(x[i]))
  }
  return(sum(ent)/log(length(x))) 
}
