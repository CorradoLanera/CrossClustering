rm(list=ls())
#setwd("/Users/paolatellaroli/Desktop/crossclusteringpackage/CrossClustering/R")
source("geneinlista.R")
source("max.proportion.function.R")
source("CrossClustering.R")

#setwd("/Users/paolatellaroli/Desktop/CrossClustering Paola/Code&Results/GSE1402")

library(GEOquery)
library(limma)
library(cluster)

gset <- getGEO("GSE38888", GSEMatrix =TRUE)
GSE19711 <- getGEO('mypath/GSE38888')
GSE38888 <- getGEO('GSE38888',destdir="C:/Users/UTENTE/CC_Review/Subtyping/Vecchie Analisi/Brain")

if (length(gset) > 1) idx <- grep("GPL14965", attr(gset, "names")) else idx <- 1
gset <- gset[[idx]]

ex <- exprs(gset)

#data <- scale(log2(ex))

#d_euc <- dist(t(data), method="euclidean")

d_euc <- dist(t(ex), method="euclidean")

#apply(data, 2, sd, na.rm=T)
#apply(data, 2, mean, na.rm=T)

phenotData <- pData(gset)

tClass <- phenotData$characteristics_ch1.2 #2
names(tClass) <- rownames(phenotData)
table(tClass)

#ex <- ex[,tClass!="subtype: Borderline"]
#tClass <- tClass[tClass!="subtype: Borderline"]

 #ex <- ex[,tClass!="tissue: normal fat"]
 #tClass <- tClass[tClass!="tissue: normal fat"]
#
# ex <- ex[,tClass!="molecular classification: CIMPneg"]
# tClass <- tClass[tClass!="molecular classification: CIMPneg"]

vera_part <- numeric()
for (i in 1:length(table(tClass))){
  vera_part[tClass==names(table(tClass))[i]]=i
}

table(vera_part)

CC_euc <- CrossClustering(d_euc,k.w.min=2,k.w.max=19,k.c.max=20,out=F)

clusto <- function(x, distMat){
  cutree(hclust(distMat, method="ward.D"), k=x)
}

clucomp <- function(x, distMat){
  cutree(hclust(distMat, method="complete"), k=x)
}

members_euc.ward <- sapply(2:20, clusto, distMat=d_euc)
members_euc.comp <- sapply(2:20, clucomp, distMat=d_euc)

Sil_euc.ward <- Sil_euc.complete <- numeric(19)

for (c in 1:ncol(members_euc.ward)) {
  Sil_euc.ward[c] <- summary(silhouette(x=members_euc.ward[,c],
                                        dist=d_euc))$avg.width
  Sil_euc.complete[c] <- summary(silhouette(x=members_euc.comp[,c],
                                            dist=d_euc))$avg.width
}

names(Sil_euc.ward) <- names(Sil_euc.complete) <- paste("Clus", 2:20, sep="")

table(members_euc.ward[,which.max(Sil_euc.ward)]) # 2 clusters, sil=0.2008215
table(members_euc.comp[,which.max(Sil_euc.complete)]) # 2 clusters, sil=0.1906149

groupsGE_ward_euc <- members_euc.ward[,which.max(Sil_euc.ward)]
groupsGE_comp_euc <- members_euc.comp[,which.max(Sil_euc.complete)]

n=dim(ex)[2]

groupsGE_euc <- sapply(1:n, geneinlista, CC_euc$Cluster.list[c(1:length(CC_euc$Cluster.list))])
groupsGE_euc[groupsGE_euc=="integer(0)"]=0
if(is.null(dim(groupsGE_euc))){
  groupsGE_euc <- matrix(groupsGE_euc, ncol=1)
}
groupsGE_euc <- as.numeric(groupsGE_euc)
groupsGE_euc

Rand_CCeuc<-
  as.numeric(adjustedRand(as.integer(groupsGE_euc), as.integer(vera_part), randMethod="HA")) # 0.8949855

Rand_wardeuc<-
  as.numeric(adjustedRand(as.integer(members_euc.ward[,which.max(Sil_euc.ward)]), as.integer(vera_part), randMethod="HA")) # 0.3407673

Rand_compeuc<-
  as.numeric(adjustedRand(as.integer(members_euc.comp[,which.max(Sil_euc.complete)]), as.integer(vera_part), randMethod="HA")) # 0.5822348

my.clu_ward <- hclust(d_euc, method="ward.D")
my.clu_comp <- hclust(d_euc, method="complete")

source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
# colored dendrogram
Subtypes=substring(tClass, first=19, last=1000000L)
Subtypes[Subtypes==" luminal"] <- "Luminal"
Subtypes[Subtypes==" Triple Negative"] <- "TN"

A2Rplot(my.clu_ward,
        k=11,
        fact.sup=Subtypes,
        boxes = FALSE,
        col.up = "gray",
        col.down = 1:11, show.labels=FALSE, main="Ward")

A2Rplot(my.clu_comp,
        k=11,
        fact.sup=Subtypes,
        boxes = FALSE,
        col.up = "gray",
        col.down = 1:11, show.labels=FALSE, main="Complete")



#### KM

km1 <- km2 <- km3 <- km4 <- km5 <- km6 <- km7 <- km8 <- km9 <- km10 <- matrix(NA, ncol=19, nrow=n)
SilKm1 <- SilKm2 <- SilKm3 <- SilKm4 <- SilKm5 <- SilKm6 <- SilKm7 <- SilKm8 <- SilKm9 <- SilKm10 <- numeric(19)

data <- t(ex) # 254 samples
d <- d_euc

for (k in 2:20){
  km1[,k-1] <- kmeans(data, c=k)$cluster
  SilKm1[k-1] <- mean(silhouette(as.numeric(km1[,k-1]), dist=d)[,3])

  km2[,k-1] <- kmeans(data, c=k)$cluster
  SilKm2[k-1] <- mean(silhouette(as.numeric(km2[,k-1]), dist=d)[,3])

  km3[,k-1] <- kmeans(data, c=k)$cluster
  SilKm3[k-1] <- mean(silhouette(as.numeric(km3[,k-1]), dist=d)[,3])

  km4[,k-1] <- kmeans(data, c=k)$cluster
  SilKm4[k-1] <- mean(silhouette(as.numeric(km4[,k-1]), dist=d)[,3])

  km5[,k-1] <- kmeans(data, c=k)$cluster
  SilKm5[k-1] <- mean(silhouette(as.numeric(km5[,k-1]), dist=d)[,3])

  km6[,k-1] <- kmeans(data, c=k)$cluster
  SilKm6[k-1] <- mean(silhouette(as.numeric(km6[,k-1]), dist=d)[,3])

  km7[,k-1] <- kmeans(data, c=k)$cluster
  SilKm7[k-1] <- mean(silhouette(as.numeric(km7[,k-1]), dist=d)[,3])

  km8[,k-1] <- kmeans(data, c=k)$cluster
  SilKm8[k-1] <- mean(silhouette(as.numeric(km8[,k-1]), dist=d)[,3])

  km9[,k-1] <- kmeans(data, c=k)$cluster
  SilKm9[k-1] <- mean(silhouette(as.numeric(km9[,k-1]), dist=d)[,3])

  km10[,k-1] <- kmeans(data, c=k)$cluster
  SilKm10[k-1] <- mean(silhouette(as.numeric(km10[,k-1]), dist=d)[,3])
}

lista_km <- list(km1, km2, km3, km4, km5, km6, km7, km8, km9, km10)
lista_Silkm <- list(SilKm1, SilKm2, SilKm3, SilKm4, SilKm5, SilKm6,
                    SilKm7, SilKm8, SilKm9, SilKm10)

sapply(lista_Silkm, max)
max(sapply(lista_Silkm, max)) # 0.250088
sapply(lista_Silkm, which.max) # 10

RandKm <- adjustedRand(lista_km[[which.max(sapply(lista_Silkm, max))]][,sapply(lista_Silkm, which.max)[which.max(sapply(lista_Silkm, max))]],
                       as.integer(vera_part), randMethod="HA") # 0.04010903

save(lista_km, file="lista_km_GSE38888.Rdata")
save(lista_Silkm, file="lista_Silkm_GSE38888.Rdata")

load("lista_km_GSE38888.Rdata")
load("lista_Silkm_GSE38888.Rdata")

## SOM

library(kohonen)

griglia <- matrix(NA, ncol=3, nrow=70)
griglia[,1] <- c(rep(1, 19), rep(2, 10), rep(3, 7), rep(4, 5), rep(5, 4), rep(6, 4), rep(7, 3), rep(8, 3), rep(9, 3), rep(10, 2), 11:20)
griglia[,2] <- c(2:20, 1:10, 1:7, 1:5, 1:4, 1:4, 1:3, 1:3, 1:3, 1:2, rep(1, 10))
griglia[,3]  <- griglia[,1]*griglia[,2]

my.som <- matrix(NA, nrow=n, ncol=nrow(griglia))

set.seed(123)
for (c in 1:nrow(griglia)){
  my.som[,c] <- som(data, grid=somgrid(xdim=griglia[c,1], ydim=griglia[c,2],
                                       "rectangular"))$unit.classif
}

save(my.som, file="my.som_GSE38888.Rdata")
load("my.som_GSE38888.Rdata")

SilSom <- numeric(ncol(my.som))
for (c in 1:ncol(my.som)){
  SilSom[c] <- mean(silhouette(as.numeric(my.som[,c]), dist=d)[,3])
}

max(SilSom) # 0.1927923
which.max(SilSom) # 9 x 2
table(my.som[,which.max(SilSom)])

som <- matrix(NA, nrow=n, ncol=10)
colnames(som) <- paste("Prova", 1:10, sep="")
for (c in 1:10){
  som[,c] <- som(data, grid=somgrid(xdim=9, ydim=2, "rectangular"))$unit.classif
}

apply(som, 2, table)
save(som, file="som_GSE38888.Rdata")

sil_som <- numeric(10)
for (c in 1:10){
  sil_som[c] <- mean(silhouette(as.numeric(som[,c]), dist=d)[,3])
}

max(sil_som) # 0.1762618
which.max(sil_som) # 2

adjustedRand(som[,which.max(sil_som)], as.integer(vera_part), randMethod="HA")
# 0.0735831

#### Heatmap ####
cc <- groupsGE_euc[order(vera_part)]
cc[cc==2] <- 3
cc[cc==1] <- 2
cc[cc==3] <- 1

hm <- matrix(NA, nrow=n, ncol=6)
colnames(hm) <- c("Ward", "CL", "K-means", "SOM", "CC", "Real Membership")
rownames(hm) <- paste("Elem", 1:n, sep="")
hm[,1] <- members_euc.ward[,which.max(Sil_euc.ward)][order(vera_part)]
hm[,2] <- members_euc.comp[,which.max(Sil_euc.complete)][order(vera_part)]
hm[,3] <- lista_km[[which.max(sapply(lista_Silkm, max))]][,sapply(lista_Silkm, which.max)[which.max(sapply(lista_Silkm, max))]][order(vera_part)]
hm[,4] <- som[,which.max(sil_som)][order(vera_part)]
hm[,5] <- cc
hm[,6] <- vera_part[order(vera_part)]

save(hm, file="hm_GSE38888.Rdata")
load("hm_GSE38888.Rdata")

library(fields)
my_palette <- colorRampPalette(c("white", "yellow", "green", "tomato", "orangered", "red", "darkmagenta", "deeppink", "purple", "blue", "royalblue", "cadetblue", "chartreuse", "dimgray", "lightseagreen"))(n = 20)
par(mar=c(5,4.5,4,7))
image(hm, col=my_palette, axes=F, xlab="Samples")
mtext(text=colnames(hm), side=2, line=0.3, at=seq(0,1,0.16), las=1, cex=0.65)
image.plot(hm, col=my_palette, legend.only=T,
           legend.lab="Cluster Membership",
           lab.breaks=c("Outliers", "1", "2", "3", "4"))
title("Breast cancer data")

# DBSCAN
library(RANN)
library(fpc)
nearest3 <- nn2(data=t(ex), k=3)
plot(nearest3[[2]][order(nearest3[[2]][,3], decreasing=T),3], type="l", axes=F,
     ylab="2-dist measure", xlab="points")
axis(2)
axis(1, at=seq(1, 91, by=1))
abline(v=3, col="red")
nearest3[[2]][order(nearest3[[2]][,3], decreasing=T),3][3]

nearest4 <- nn2(data=t(ex), k=4)
plot(nearest4[[2]][order(nearest4[[2]][,4], decreasing=T),4], type="l", axes=F,
     ylab="3-dist measure", xlab="points")
axis(2)
axis(1, at=seq(1, 91, by=1))
abline(v=3, col="red")
nearest4[[2]][order(nearest4[[2]][,4], decreasing=T),4][3]

nearest5 <- nn2(data=t(ex), k=5)
plot(nearest5[[2]][order(nearest5[[2]][,5], decreasing=T),5], type="l", axes=F,
     ylab="4-dist measure", xlab="points")
axis(2)
axis(1, at=seq(1, 91, by=1))
abline(v=5, col="red")
nearest5[[2]][order(nearest5[[2]][,5], decreasing=T),5][5]

nearest6 <- nn2(data=t(ex), k=6)
plot(nearest6[[2]][order(nearest6[[2]][,6], decreasing=T),6], type="l", axes=F,
     ylab="5-dist measure", xlab="points")
axis(2)
axis(1, at=seq(1, 91, by=1))
abline(v=5, col="red")
nearest6[[2]][order(nearest6[[2]][,6], decreasing=T),6][5]

nearest7 <- nn2(data=t(ex), k=7)
plot(nearest7[[2]][order(nearest7[[2]][,7], decreasing=T),7], type="l", axes=F,
     ylab="6-dist measure", xlab="points")
axis(2)
axis(1, at=seq(1, 91, by=1))
abline(v=4, col="red")
nearest7[[2]][order(nearest7[[2]][,7], decreasing=T),7][4]

nearest8 <- nn2(data=t(ex), k=8)
plot(nearest8[[2]][order(nearest8[[2]][,8], decreasing=T),8], type="l", axes=F,
     ylab="7-dist measure", xlab="points")
axis(2)
axis(1, at=seq(1, 91, by=1))
abline(v=4, col="red")
nearest8[[2]][order(nearest8[[2]][,8], decreasing=T),8][4]

db4=dbscan(t(ex), eps=nearest5[[2]][order(nearest5[[2]][,5], decreasing=T),5][3], MinPts=4)$cluster
db5=dbscan(t(ex), eps=nearest6[[2]][order(nearest6[[2]][,6], decreasing=T),6][5], MinPts=5)$cluster
db6=dbscan(t(ex), eps=nearest7[[2]][order(nearest7[[2]][,7], decreasing=T),7][5], MinPts=6)$cluster
db7=dbscan(t(ex), eps=nearest8[[2]][order(nearest8[[2]][,8], decreasing=T),8][4], MinPts=7)$cluster

################
### WARD ###

conti <- table(members_euc.ward[,which.max(Sil_euc.ward)], vera_part)
conti <- as.matrix(conti)
colnames(conti) <- c("Luminal", "TN")
rownames(conti) <- paste("Clu", 1:11, sep="")
conti <- rbind(conti, as.numeric(colSums(conti)))
conti <- cbind(conti, as.numeric(rowSums(conti)))

xtable(conti, digits=0)

clu1_l <- matrix(c(6, 10, 1, 13), nrow=2)
clu2_l <- matrix(c(2, 14, 4, 10), nrow=2)
clu3_l <- matrix(c(1, 15, 0, 14), nrow=2)
clu4_l <- matrix(c(2, 14, 0, 14), nrow=2)
clu5_l <- matrix(c(2, 14, 0, 14), nrow=2)
clu6_l <- matrix(c(0, 16, 2, 12), nrow=2)
clu7_l <- matrix(c(0, 16, 2, 12), nrow=2)
clu8_l <- matrix(c(0, 16, 2, 12), nrow=2)
clu9_l <- matrix(c(0, 16, 2, 12), nrow=2)
clu10_l <- matrix(c(0, 16, 1, 13), nrow=2)
clu11_l <- matrix(c(3, 13, 0, 14), nrow=2)

clu1_t <- matrix(c(1, 13, 6, 10), nrow=2)
clu2_t <- matrix(c(4, 10, 2, 14), nrow=2)
clu3_t <- matrix(c(0, 14, 1, 15), nrow=2)
clu4_t <- matrix(c(0, 14, 2, 14), nrow=2)
clu5_t <- matrix(c(0, 14, 2, 14), nrow=2)
clu6_t <- matrix(c(2, 12, 0, 16), nrow=2)
clu7_t <- matrix(c(2, 12, 0, 16), nrow=2)
clu8_t <- matrix(c(2, 12, 0, 16), nrow=2)
clu9_t <- matrix(c(2, 12, 0, 16), nrow=2)
clu10_t <- matrix(c(1, 13, 0, 16), nrow=2)
clu11_t <- matrix(c(0, 14, 3, 13), nrow=2)

rawp_ward <- c(fisher.test(clu1_l, alternative="greater")$p.value,
               fisher.test(clu2_l, alternative="greater")$p.value,
               fisher.test(clu3_l, alternative="greater")$p.value,
               fisher.test(clu4_l, alternative="greater")$p.value,
               fisher.test(clu5_l, alternative="greater")$p.value,
               fisher.test(clu6_l, alternative="greater")$p.value,
               fisher.test(clu7_l, alternative="greater")$p.value,
               fisher.test(clu8_l, alternative="greater")$p.value,
               fisher.test(clu9_l, alternative="greater")$p.value,
               fisher.test(clu10_l, alternative="greater")$p.value,
               fisher.test(clu11_l, alternative="greater")$p.value,

               fisher.test(clu1_t, alternative="greater")$p.value,
               fisher.test(clu2_t, alternative="greater")$p.value,
               fisher.test(clu3_t, alternative="greater")$p.value,
               fisher.test(clu4_t, alternative="greater")$p.value,
               fisher.test(clu5_t, alternative="greater")$p.value,
               fisher.test(clu6_t, alternative="greater")$p.value,
               fisher.test(clu7_t, alternative="greater")$p.value,
               fisher.test(clu8_t, alternative="greater")$p.value,
               fisher.test(clu9_t, alternative="greater")$p.value,
               fisher.test(clu10_t, alternative="greater")$p.value,
               fisher.test(clu11_t, alternative="greater")$p.value)

names(rawp_ward) <- c("clu1_l", "clu2_l", "clu3_l", "clu4_l", "clu5_l", "clu6_l", "clu7_l", "clu8_l", "clu9_l", "clu10_l", "clu11_l",
                      "clu1_t", "clu2_t", "clu3_t", "clu4_t", "clu5_t", "clu6_t", "clu7_t", "clu8_t", "clu9_t", "clu10_t", "clu11_t")

adjustedp_ward <- p.adjust(rawp_ward, method="fdr")
adjustedp_ward[adjustedp_ward<0.1]

### COMP

conti <- table(members_euc.comp[,which.max(Sil_euc.complete)][order(vera_part)], vera_part)
conti <- as.matrix(conti)
colnames(conti) <- c("Luminal", "TN")
rownames(conti) <- paste("Clu", 1:11, sep="")
conti <- rbind(conti, as.numeric(colSums(conti)))
conti <- cbind(conti, as.numeric(rowSums(conti)))

xtable(conti, digits=0)

clu1_l <- matrix(c(3, 13, 5, 9), nrow=2)
clu2_l <- matrix(c(4, 12, 2, 12), nrow=2)
clu3_l <- matrix(c(1, 15, 0, 14), nrow=2)
clu4_l <- matrix(c(2, 14, 0, 14), nrow=2)
clu5_l <- matrix(c(1, 15, 1, 13), nrow=2)
clu6_l <- matrix(c(2, 14, 0, 14), nrow=2)
clu7_l <- matrix(c(0, 16, 2, 12), nrow=2)
clu8_l <- matrix(c(0, 16, 2, 12), nrow=2)
clu9_l <- matrix(c(2, 14, 0, 14), nrow=2)
clu10_l <- matrix(c(1, 15, 0, 14), nrow=2)
clu11_l <- matrix(c(0, 16, 2, 12), nrow=2)

clu1_t <- matrix(c(5, 9, 3, 13), nrow=2)
clu2_t <- matrix(c(2, 12, 4, 12), nrow=2)
clu3_t <- matrix(c(0, 14, 1, 15), nrow=2)
clu4_t <- matrix(c(0, 14, 2, 14), nrow=2)
clu5_t <- matrix(c(1, 13, 1, 15), nrow=2)
clu6_t <- matrix(c(0, 14, 2, 14), nrow=2)
clu7_t <- matrix(c(2, 12, 0, 16), nrow=2)
clu8_t <- matrix(c(2, 12, 0, 16), nrow=2)
clu9_t <- matrix(c(0, 14, 2, 14), nrow=2)
clu10_t <- matrix(c(0, 14, 1, 15), nrow=2)
clu11_t <- matrix(c(2, 12, 0, 16), nrow=2)

rawp_comp <- c(fisher.test(clu1_l, alternative="greater")$p.value,
               fisher.test(clu2_l, alternative="greater")$p.value,
               fisher.test(clu3_l, alternative="greater")$p.value,
               fisher.test(clu4_l, alternative="greater")$p.value,
               fisher.test(clu5_l, alternative="greater")$p.value,
               fisher.test(clu6_l, alternative="greater")$p.value,
               fisher.test(clu7_l, alternative="greater")$p.value,
               fisher.test(clu8_l, alternative="greater")$p.value,
               fisher.test(clu9_l, alternative="greater")$p.value,
               fisher.test(clu10_l, alternative="greater")$p.value,
               fisher.test(clu11_l, alternative="greater")$p.value,

               fisher.test(clu1_t, alternative="greater")$p.value,
               fisher.test(clu2_t, alternative="greater")$p.value,
               fisher.test(clu3_t, alternative="greater")$p.value,
               fisher.test(clu4_t, alternative="greater")$p.value,
               fisher.test(clu5_t, alternative="greater")$p.value,
               fisher.test(clu6_t, alternative="greater")$p.value,
               fisher.test(clu7_t, alternative="greater")$p.value,
               fisher.test(clu8_t, alternative="greater")$p.value,
               fisher.test(clu9_t, alternative="greater")$p.value,
               fisher.test(clu10_t, alternative="greater")$p.value,
               fisher.test(clu11_t, alternative="greater")$p.value)

names(rawp_comp) <- c("clu1_l", "clu2_l", "clu3_l", "clu4_l", "clu5_l", "clu6_l", "clu7_l", "clu8_l", "clu9_l", "clu10_l", "clu11_l",
                      "clu1_t", "clu2_t", "clu3_t", "clu4_t", "clu5_t", "clu6_t", "clu7_t", "clu8_t", "clu9_t", "clu10_t", "clu11_t")

adjustedp_comp <- p.adjust(rawp_comp, method="fdr")
adjustedp_comp[adjustedp_comp<0.1]

#### SOM #####

conti_som <- table(som[,which.max(sil_som)], vera_part)
conti_som <- as.matrix(conti_som)
conti_som <- rbind(conti_som, as.numeric(colSums(conti_som)))
conti_som <- cbind(conti_som, as.numeric(rowSums(conti_som)))
colnames(conti_som) <- c("Luminal", "TN")
rownames(conti_som) <- paste("Clu", 1:18, sep="")

xtable(conti_som, digits=0)

clu1_l <- matrix(c(2, 14, 0, 14), nrow=2)
clu2_l <- matrix(c(1, 15, 0, 14), nrow=2)
clu4_l <- matrix(c(0, 16, 2, 12), nrow=2)
clu6_l <- matrix(c(0, 16, 1, 13), nrow=2)
clu7_l <- matrix(c(1, 15, 0, 14), nrow=2)
clu8_l <- matrix(c(0, 16, 2, 12), nrow=2)
clu9_l <- matrix(c(2, 14, 1, 13), nrow=2)
clu10_l <- matrix(c(2, 14, 0, 14), nrow=2)
clu11_l <- matrix(c(1, 15, 0, 14), nrow=2)
clu12_l <- matrix(c(0, 16, 1, 13), nrow=2)
clu13_l <- matrix(c(0, 16, 3, 11), nrow=2)
clu14_l <- matrix(c(0, 16, 2, 12), nrow=2)
clu15_l <- matrix(c(0, 16, 2, 12), nrow=2)
clu16_l <- matrix(c(1, 15, 0, 14), nrow=2)
clu17_l <- matrix(c(2, 14, 0, 14), nrow=2)
clu18_l <- matrix(c(4, 12, 0, 14), nrow=2)

clu1_t <- matrix(c(0, 14, 2, 14), nrow=2)
clu2_t <- matrix(c(0, 14, 1, 15), nrow=2)
clu4_t <- matrix(c(2, 12, 0, 16), nrow=2)
clu6_t <- matrix(c(1, 13, 0, 16), nrow=2)
clu7_t <- matrix(c(0, 14, 1, 15), nrow=2)
clu8_t <- matrix(c(2, 12, 0, 16), nrow=2)
clu9_t <- matrix(c(1, 13, 2, 14), nrow=2)
clu10_t <- matrix(c(0, 14, 2, 14), nrow=2)
clu11_t <- matrix(c(0, 14, 1, 15), nrow=2)
clu12_t <- matrix(c(1, 13, 0, 16), nrow=2)
clu13_t <- matrix(c(3, 11, 0, 16), nrow=2)
clu14_t <- matrix(c(2, 12, 0, 16), nrow=2)
clu15_t <- matrix(c(2, 12, 0, 16), nrow=2)
clu16_t <- matrix(c(0, 14, 1, 15), nrow=2)
clu17_t <- matrix(c(0, 14, 2, 14), nrow=2)
clu18_t <- matrix(c(0, 14, 4, 12), nrow=2)

rawp_som <- c(fisher.test(clu1_l, alternative="greater")$p.value,
              fisher.test(clu2_l, alternative="greater")$p.value,
              fisher.test(clu4_l, alternative="greater")$p.value,
              fisher.test(clu6_l, alternative="greater")$p.value,
              fisher.test(clu7_l, alternative="greater")$p.value,
              fisher.test(clu8_l, alternative="greater")$p.value,
              fisher.test(clu9_l, alternative="greater")$p.value,
              fisher.test(clu10_l, alternative="greater")$p.value,
              fisher.test(clu11_l, alternative="greater")$p.value,
              fisher.test(clu12_l, alternative="greater")$p.value,
              fisher.test(clu13_l, alternative="greater")$p.value,
              fisher.test(clu14_l, alternative="greater")$p.value,
              fisher.test(clu15_l, alternative="greater")$p.value,
              fisher.test(clu16_l, alternative="greater")$p.value,
              fisher.test(clu17_l, alternative="greater")$p.value,
              fisher.test(clu18_l, alternative="greater")$p.value,

              fisher.test(clu1_t, alternative="greater")$p.value,
              fisher.test(clu2_t, alternative="greater")$p.value,
              fisher.test(clu4_t, alternative="greater")$p.value,
              fisher.test(clu6_t, alternative="greater")$p.value,
              fisher.test(clu7_t, alternative="greater")$p.value,
              fisher.test(clu8_t, alternative="greater")$p.value,
              fisher.test(clu9_t, alternative="greater")$p.value,
              fisher.test(clu10_t, alternative="greater")$p.value,
              fisher.test(clu11_t, alternative="greater")$p.value,
              fisher.test(clu12_t, alternative="greater")$p.value,
              fisher.test(clu13_t, alternative="greater")$p.value,
              fisher.test(clu14_t, alternative="greater")$p.value,
              fisher.test(clu15_t, alternative="greater")$p.value,
              fisher.test(clu16_t, alternative="greater")$p.value,
              fisher.test(clu17_t, alternative="greater")$p.value,
              fisher.test(clu18_t, alternative="greater")$p.value)

names(rawp_som) <- c("clu1_l", "clu2_l", "clu4_l", "clu6_l", "clu7_l", "clu8_l", "clu9_l", "clu10_l", "clu11_l", "clu12_l", "clu13_l", "clu14_l", "clu15_l", "clu16_l", "clu17_l", "clu18_l",
                     "clu1_t", "clu2_t", "clu4_t", "clu6_t", "clu7_t", "clu8_t", "clu9_t", "clu10_t", "clu11_t", "clu12_t", "clu13_t", "clu14_t", "clu15_t", "clu16_t", "clu17_t", "clu18_t")

adjustedp_som <- p.adjust(rawp_som, method="fdr")
adjustedp_som[adjustedp_som<0.1]


###  KM

conti_km <- table(lista_km[[which.max(sapply(lista_Silkm, max))]][,sapply(lista_Silkm, which.max)[which.max(sapply(lista_Silkm, max))]][order(vera_part)], vera_part)
conti_km <- as.matrix(conti_km)
colnames(conti_km) <- c("Luminal", "TN")
rownames(conti_km) <- paste("Clu", 1:11, sep="")
conti_km <- rbind(conti_km, as.numeric(colSums(conti_km)))
conti_km <- cbind(conti_km, as.numeric(rowSums(conti_km)))

xtable(conti_km, digits=0)

clu1_l <- matrix(c(1, 15, 0, 14), nrow=2)
clu2_l <- matrix(c(3, 13, 1, 13), nrow=2)
clu3_l <- matrix(c(0, 16, 2, 12), nrow=2)
clu4_l <- matrix(c(0, 16, 2, 12), nrow=2)
clu5_l <- matrix(c(1, 15, 1, 13), nrow=2)
clu6_l <- matrix(c(1, 15, 0, 14), nrow=2)
clu7_l <- matrix(c(4, 12, 5, 9), nrow=2)
clu8_l <- matrix(c(2, 14, 0, 14), nrow=2)
clu9_l <- matrix(c(2, 14, 0, 14), nrow=2)
clu10_l <- matrix(c(2, 14, 0, 14), nrow=2)
clu11_l <- matrix(c(0, 16, 3, 11), nrow=2)

clu1_t <- matrix(c(0, 14, 1, 15), nrow=2)
clu2_t <- matrix(c(1, 13, 3, 13), nrow=2)
clu3_t <- matrix(c(2, 12, 0, 16), nrow=2)
clu4_t <- matrix(c(2, 12, 0, 16), nrow=2)
clu5_t <- matrix(c(1, 13, 1, 15), nrow=2)
clu6_t <- matrix(c(0, 14, 1, 15), nrow=2)
clu7_t <- matrix(c(5, 9, 4, 12), nrow=2)
clu8_t <- matrix(c(0, 14, 2, 14), nrow=2)
clu9_t <- matrix(c(0, 14, 2, 14), nrow=2)
clu10_t <- matrix(c(0, 14, 2, 14), nrow=2)
clu11_t <- matrix(c(3, 11, 0, 16), nrow=2)

rawp_km <- c(fisher.test(clu1_l, alternative="greater")$p.value,
             fisher.test(clu2_l, alternative="greater")$p.value,
             fisher.test(clu3_l, alternative="greater")$p.value,
             fisher.test(clu4_l, alternative="greater")$p.value,
             fisher.test(clu5_l, alternative="greater")$p.value,
             fisher.test(clu6_l, alternative="greater")$p.value,
             fisher.test(clu7_l, alternative="greater")$p.value,
             fisher.test(clu8_l, alternative="greater")$p.value,
             fisher.test(clu9_l, alternative="greater")$p.value,
             fisher.test(clu10_l, alternative="greater")$p.value,
             fisher.test(clu11_l, alternative="greater")$p.value,

             fisher.test(clu1_t, alternative="greater")$p.value,
             fisher.test(clu2_t, alternative="greater")$p.value,
             fisher.test(clu3_t, alternative="greater")$p.value,
             fisher.test(clu4_t, alternative="greater")$p.value,
             fisher.test(clu5_t, alternative="greater")$p.value,
             fisher.test(clu6_t, alternative="greater")$p.value,
             fisher.test(clu7_t, alternative="greater")$p.value,
             fisher.test(clu8_t, alternative="greater")$p.value,
             fisher.test(clu9_t, alternative="greater")$p.value,
             fisher.test(clu10_t, alternative="greater")$p.value,
             fisher.test(clu11_t, alternative="greater")$p.value)

names(rawp_km) <- c("clu1_l", "clu2_l", "clu3_l", "clu4_l", "clu5_l", "clu6_l", "clu7_l", "clu8_l", "clu9_l", "clu10_l", "clu11_l",
                    "clu1_t", "clu2_t", "clu3_t", "clu4_t", "clu5_t", "clu6_t", "clu7_t", "clu8_t", "clu9_t", "clu10_t", "clu11_t")

adjustedp_km <- p.adjust(rawp_km, method="fdr")
adjustedp_km[adjustedp_km<0.1]

######### CC ##############

conti <- table(groupsGE_euc, vera_part)
conti <- as.matrix(conti)
colnames(conti) <- c("Luminal", "TN")
rownames(conti) <- c("Outliers", paste("Clu", 1:2, sep=""))
conti <- rbind(conti, as.numeric(colSums(conti)))
conti <- cbind(conti, as.numeric(rowSums(conti)))

xtable(conti, digits=0)

out_l <- matrix(c(1, 15, 1, 13), nrow=2)
out_t <- matrix(c(1, 13, 1, 15), nrow=2)

clu1_l <- matrix(c(2, 14, 13, 1), nrow=2)
clu1_t <- matrix(c(13, 1, 2, 14), nrow=2)

clu2_l <- matrix(c(13, 3, 0, 14), nrow=2)
clu2_t <- matrix(c(0, 14, 13, 3), nrow=2)

rawp <- c(fisher.test(out_l, alternative="greater")$p.value,
          fisher.test(out_t, alternative="greater")$p.value,

          fisher.test(clu1_l, alternative="greater")$p.value,
          fisher.test(clu1_t, alternative="greater")$p.value,

          fisher.test(clu2_l, alternative="greater")$p.value,
          fisher.test(clu2_t, alternative="greater")$p.value)

names(rawp) <- c("out_l", "out_t",
                 "clu1_l", "clu1_t",
                 "clu2_l", "clu2_t")

adjustedp <- p.adjust(rawp, method="fdr")
adjustedp[adjustedp<0.1]

###### OUTLIERS ######

gset <- getGEO("GSE35483", GSEMatrix =TRUE)
if (length(gset) > 1) idx <- grep("GPL14965", attr(gset, "names")) else idx <- 1
gset <- gset[[idx]]

sarcoma <- exprs(gset)

gset <- getGEO("GSE34197", GSEMatrix =TRUE)
if (length(gset) > 1) idx <- grep("GPL14965", attr(gset, "names")) else idx <- 1
gset <- gset[[idx]]

fshd <- exprs(gset)
phenotData <- pData(gset)
phenotData$characteristics_ch1 # i primi 4 samples vanno bene

data <- cbind(ex, sarcoma, fshd[,4:5])

std <- function(x) (x-mean(x))/sd(x)

std_mat <- apply(data, 2, std)

d_euc <- dist(t(std_mat), method="euclidean")

vera_part[31] <- 3
vera_part[32] <- 4
vera_part[33] <- 5

#### OVER-REPRESENTATION #####

### WARD ###

conti <- table(members_euc.ward[,which.max(Sil_euc.ward)], vera_part)
conti <- as.matrix(conti)
colnames(conti) <- c("Luminal", "TN", "Sarcoma", "FSHD", "Control")
rownames(conti) <- paste("Clu", 1:14, sep="")
#conti <- rbind(conti, as.numeric(colSums(conti)))
#conti <- cbind(conti, as.numeric(rowSums(conti)))

#xtable(conti, digits=0)

pval_l <- pval_tn <- pval_s <- pval_f <- pval_c <- numeric(dim(conti)[1])
clu_l <- clu_tn <- clu_s <- clu_f <- clu_c <- list(dim(conti[1]))
for (i in 1:dim(conti)[1]){
clu_l[[i]] <- matrix(c(conti[i,1],
                   ((apply(conti, 2, sum)[1])-conti[i,1]),
                   (apply(conti, 1, sum)[i]-conti[i,1]),
                   (sum(conti)-sum(conti[i,1], ((apply(conti, 2, sum)[1])-conti[i,1]),
                                   (apply(conti, 1, sum)[i]-conti[i,1])))), nrow=2)

pval_l[i] <- fisher.test(clu_l[[i]], alternative="greater")$p.value

clu_tn[[i]] <- matrix(c(conti[i,2],
                       ((apply(conti, 2, sum)[2])-conti[i,2]),
                       (apply(conti, 1, sum)[i]-conti[i,2]),
                       (sum(conti)-sum(conti[i,2], ((apply(conti, 2, sum)[2])-conti[i,2]),
                                       (apply(conti, 1, sum)[i]-conti[i,2])))), nrow=2)

pval_tn[i] <- fisher.test(clu_tn[[i]], alternative="greater")$p.value

clu_s[[i]] <- matrix(c(conti[i,3],
                        ((apply(conti, 2, sum)[3])-conti[i,3]),
                        (apply(conti, 1, sum)[i]-conti[i,3]),
                        (sum(conti)-sum(conti[i,3], ((apply(conti, 2, sum)[3])-conti[i,3]),
                                        (apply(conti, 1, sum)[i]-conti[i,3])))), nrow=2)

pval_s[i] <- fisher.test(clu_s[[i]], alternative="greater")$p.value

clu_f[[i]] <- matrix(c(conti[i,4],
                       ((apply(conti, 2, sum)[4])-conti[i,4]),
                       (apply(conti, 1, sum)[i]-conti[i,4]),
                       (sum(conti)-sum(conti[i,4], ((apply(conti, 2, sum)[4])-conti[i,4]),
                                       (apply(conti, 1, sum)[i]-conti[i,4])))), nrow=2)

pval_f[i] <- fisher.test(clu_f[[i]], alternative="greater")$p.value

clu_c[[i]] <- matrix(c(conti[i,5],
                       ((apply(conti, 2, sum)[5])-conti[i,5]),
                       (apply(conti, 1, sum)[i]-conti[i,5]),
                       (sum(conti)-sum(conti[i,5], ((apply(conti, 2, sum)[5])-conti[i,5]),
                                       (apply(conti, 1, sum)[i]-conti[i,5])))), nrow=2)

pval_c[i] <- fisher.test(clu_c[[i]], alternative="greater")$p.value
}


rawp_ward <- c(pval_l, pval_tn, pval_s, pval_f, pval_c)

names(rawp_ward) <- c(paste("L_", 1:dim(conti)[1], sep=""),
                      paste("TN_", 1:dim(conti)[1], sep=""),
                      paste("S_", 1:dim(conti)[1], sep=""),
                      paste("F_", 1:dim(conti)[1], sep=""),
                      paste("C_", 1:dim(conti)[1], sep=""))

adjustedp_ward <- p.adjust(rawp_ward, method="fdr")
adjustedp_ward[adjustedp_ward<0.1]

### COMP

conti <- table(members_euc.comp[,which.max(Sil_euc.complete)][order(vera_part)], vera_part)
conti <- as.matrix(conti)
colnames(conti) <- c("Luminal", "TN", "Sarcoma", "FSHD", "Control")
rownames(conti) <- paste("Clu", 1:15, sep="")
#conti <- rbind(conti, as.numeric(colSums(conti)))
#conti <- cbind(conti, as.numeric(rowSums(conti)))

#xtable(conti, digits=0)

pval_l <- pval_tn <- pval_s <- pval_f <- pval_c <- numeric(dim(conti)[1])
clu_l <- clu_tn <- clu_s <- clu_f <- clu_c <- list(dim(conti[1]))
for (i in 1:dim(conti)[1]){
  clu_l[[i]] <- matrix(c(conti[i,1],
                         ((apply(conti, 2, sum)[1])-conti[i,1]),
                         (apply(conti, 1, sum)[i]-conti[i,1]),
                         (sum(conti)-sum(conti[i,1], ((apply(conti, 2, sum)[1])-conti[i,1]),
                                         (apply(conti, 1, sum)[i]-conti[i,1])))), nrow=2)

  pval_l[i] <- fisher.test(clu_l[[i]], alternative="greater")$p.value

  clu_tn[[i]] <- matrix(c(conti[i,2],
                          ((apply(conti, 2, sum)[2])-conti[i,2]),
                          (apply(conti, 1, sum)[i]-conti[i,2]),
                          (sum(conti)-sum(conti[i,2], ((apply(conti, 2, sum)[2])-conti[i,2]),
                                          (apply(conti, 1, sum)[i]-conti[i,2])))), nrow=2)

  pval_tn[i] <- fisher.test(clu_tn[[i]], alternative="greater")$p.value

  clu_s[[i]] <- matrix(c(conti[i,3],
                         ((apply(conti, 2, sum)[3])-conti[i,3]),
                         (apply(conti, 1, sum)[i]-conti[i,3]),
                         (sum(conti)-sum(conti[i,3], ((apply(conti, 2, sum)[3])-conti[i,3]),
                                         (apply(conti, 1, sum)[i]-conti[i,3])))), nrow=2)

  pval_s[i] <- fisher.test(clu_s[[i]], alternative="greater")$p.value

  clu_f[[i]] <- matrix(c(conti[i,4],
                         ((apply(conti, 2, sum)[4])-conti[i,4]),
                         (apply(conti, 1, sum)[i]-conti[i,4]),
                         (sum(conti)-sum(conti[i,4], ((apply(conti, 2, sum)[4])-conti[i,4]),
                                         (apply(conti, 1, sum)[i]-conti[i,4])))), nrow=2)

  pval_f[i] <- fisher.test(clu_f[[i]], alternative="greater")$p.value

  clu_c[[i]] <- matrix(c(conti[i,5],
                         ((apply(conti, 2, sum)[5])-conti[i,5]),
                         (apply(conti, 1, sum)[i]-conti[i,5]),
                         (sum(conti)-sum(conti[i,5], ((apply(conti, 2, sum)[5])-conti[i,5]),
                                         (apply(conti, 1, sum)[i]-conti[i,5])))), nrow=2)

  pval_c[i] <- fisher.test(clu_c[[i]], alternative="greater")$p.value
}

rawp_comp <- c(pval_l, pval_tn, pval_s, pval_f, pval_c)

names(rawp_comp) <- c(paste("L_", 1:dim(conti)[1], sep=""),
                      paste("TN_", 1:dim(conti)[1], sep=""),
                      paste("S_", 1:dim(conti)[1], sep=""),
                      paste("F_", 1:dim(conti)[1], sep=""),
                      paste("C_", 1:dim(conti)[1], sep=""))

adjustedp_comp <- p.adjust(rawp_comp, method="fdr")
adjustedp_comp[adjustedp_comp<0.1]

######### CC ##############

conti <- table(groupsGE_euc, vera_part)
conti <- as.matrix(conti)
colnames(conti) <- c("Luminal", "TN", "Sarcoma", "FSHD", "Control")
rownames(conti) <- c("Outliers", paste("Clu", 1:2, sep=""))
#conti <- rbind(conti, as.numeric(colSums(conti)))
#conti <- cbind(conti, as.numeric(rowSums(conti)))

#xtable(conti, digits=0)

pval_l <- pval_tn <- pval_s <- pval_f <- pval_c <- numeric(dim(conti)[1])
clu_l <- clu_tn <- clu_s <- clu_f <- clu_c <- list(dim(conti[1]))
for (i in 1:dim(conti)[1]){
  clu_l[[i]] <- matrix(c(conti[i,1],
                         ((apply(conti, 2, sum)[1])-conti[i,1]),
                         (apply(conti, 1, sum)[i]-conti[i,1]),
                         (sum(conti)-sum(conti[i,1], ((apply(conti, 2, sum)[1])-conti[i,1]),
                                         (apply(conti, 1, sum)[i]-conti[i,1])))), nrow=2)

  pval_l[i] <- fisher.test(clu_l[[i]], alternative="greater")$p.value

  clu_tn[[i]] <- matrix(c(conti[i,2],
                          ((apply(conti, 2, sum)[2])-conti[i,2]),
                          (apply(conti, 1, sum)[i]-conti[i,2]),
                          (sum(conti)-sum(conti[i,2], ((apply(conti, 2, sum)[2])-conti[i,2]),
                                          (apply(conti, 1, sum)[i]-conti[i,2])))), nrow=2)

  pval_tn[i] <- fisher.test(clu_tn[[i]], alternative="greater")$p.value

  clu_s[[i]] <- matrix(c(conti[i,3],
                         ((apply(conti, 2, sum)[3])-conti[i,3]),
                         (apply(conti, 1, sum)[i]-conti[i,3]),
                         (sum(conti)-sum(conti[i,3], ((apply(conti, 2, sum)[3])-conti[i,3]),
                                         (apply(conti, 1, sum)[i]-conti[i,3])))), nrow=2)

  pval_s[i] <- fisher.test(clu_s[[i]], alternative="greater")$p.value

  clu_f[[i]] <- matrix(c(conti[i,4],
                         ((apply(conti, 2, sum)[4])-conti[i,4]),
                         (apply(conti, 1, sum)[i]-conti[i,4]),
                         (sum(conti)-sum(conti[i,4], ((apply(conti, 2, sum)[4])-conti[i,4]),
                                         (apply(conti, 1, sum)[i]-conti[i,4])))), nrow=2)

  pval_f[i] <- fisher.test(clu_f[[i]], alternative="greater")$p.value

  clu_c[[i]] <- matrix(c(conti[i,5],
                         ((apply(conti, 2, sum)[5])-conti[i,5]),
                         (apply(conti, 1, sum)[i]-conti[i,5]),
                         (sum(conti)-sum(conti[i,5], ((apply(conti, 2, sum)[5])-conti[i,5]),
                                         (apply(conti, 1, sum)[i]-conti[i,5])))), nrow=2)

  pval_c[i] <- fisher.test(clu_c[[i]], alternative="greater")$p.value
}

rawp <- c(pval_l, pval_tn, pval_s, pval_f, pval_c)

names(rawp) <- c(paste("L_", 1:dim(conti)[1], sep=""),
                      paste("TN_", 1:dim(conti)[1], sep=""),
                      paste("S_", 1:dim(conti)[1], sep=""),
                      paste("F_", 1:dim(conti)[1], sep=""),
                      paste("C_", 1:dim(conti)[1], sep=""))

adjustedp <- p.adjust(rawp, method="fdr")
adjustedp[adjustedp<0.1]
