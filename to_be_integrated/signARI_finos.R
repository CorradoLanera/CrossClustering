library(flip)
install.packages("pdfCluster")
library(pdfCluster)

dati=read.csv(file.choose())

my.fun <- function(Y){
  adj.rand.index(Y, dati$part2)
}

flip(Y=matrix(dati$part1),X=matrix(dati$part2),statTest=my.fun,data=dati)
