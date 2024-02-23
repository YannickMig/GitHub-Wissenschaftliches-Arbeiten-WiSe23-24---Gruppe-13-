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


## Berechnet den Modus
modus <- function(x){
  return(names(which(table(x) == max(table(x)))))
}

## Berechnet den Variationskoeffizient
vari_koeff <- function(x){
  return(sd(x)/mean(x))
}

## Berechnet die Spannweite
spannweite <- function(x){
  return(max(x)-min(x))
}

## Berechnet den Interquantilsabstand
int_quant <- function(x){
  temp <- quantile(x)
  return(temp[[4]]-temp[[2]])
}
