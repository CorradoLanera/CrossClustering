pkgname <- "CrossClustering"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('CrossClustering')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("CrossClustering")
### * CrossClustering

flush(stderr()); flush(stdout())

### Name: CrossClustering
### Title: CrossClustering: a partial clustering algorithm with automatic
###   estimation of the number of clusters and identification of outliers
### Aliases: CrossClustering

### ** Examples

### Generate simulated data
toy <- matrix(NA, nrow=10, ncol=7)
colnames(toy) <- paste("Sample", 1:ncol(toy), sep="")
rownames(toy) <- paste("Gene", 1:nrow(toy), sep="")
set.seed(123)
toy[,1:2] <- rnorm(n=nrow(toy)*2, mean=10, sd=0.1)
toy[,3:4] <- rnorm(n=nrow(toy)*2, mean=20, sd=0.1)
toy[,5:6] <- rnorm(n=nrow(toy)*2, mean=5, sd=0.1)
toy[,7] <- runif(n=nrow(toy), min=0, max=1)

### toy is transposed as we want to cluster samples (columns of the original matrix)
d <- dist(t(toy), method="euclidean")

### Run CrossClustering
toyres <- CrossClustering(d, k.w.min=2, k.w.max=5, k.c.max=6, out=TRUE)



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
