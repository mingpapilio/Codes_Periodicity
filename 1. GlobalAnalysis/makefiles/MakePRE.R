# Load packages
library(raster)
library(cruts)
library(data.table)
# Read in (change the directory if needed)
# The dat.nc file is available at https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.06/cruts.2205201912.v4.06/
raw<- cruts2raster("cru_ts4.06.1901.2021.pre.dat.nc", timeRange=c("1901-01-01","2021-01-01"))
# Make spatial grids
lon= seq(-179.75, 179.75, by= 0.5)
lat= seq(-55.75, 83.75, by= 0.5)
xy<- as.data.frame(expand.grid(lon, lat))
colnames(xy)<- c("lon", "lat")
# Create a data frame for plotting
clm.data<- raster::extract(raw, xy)
# Custom functions 
## Method 1: Colwell's P with fixed binning ####
ColwellsP_Fix<- function(input.ts, breaks){
  # Modified from the source code in hydrostats package
  if(sum(is.na(input.ts))== 0&& length(input.ts)> 0){
    focal.ts<- data.frame(obs= input.ts)
    ts.leng<- length(focal.ts[,1])
    focal.ts$month<- rep(1:12, times=ts.leng/12) ## Assuming starting from Janurary ##### Can add 'start month' if needed
    focal.ts$class<- cut(focal.ts$obs, breaks, right = FALSE, include.lowest = TRUE)
    focal.table<- with(focal.ts, table(class, month))
    ##
    X <- apply(focal.table, 2, sum, na.rm = T)
    Y <- apply(focal.table, 1, sum, na.rm = T)
    Z <- sum(focal.table, na.rm = TRUE)
    HX <- -1 * sum((X/Z) * log(X/Z, base= 2), na.rm = TRUE)
    HY <- -1 * sum((Y/Z) * log(Y/Z, base= 2), na.rm = TRUE)
    HXY <- -1 * sum((focal.table/Z) * log(focal.table/Z, base= 2), na.rm = TRUE)
    s <- length(breaks)
    P <- round(1 - (HXY - HX)/log(s, base= 2), 3)
    C <- round(1 - HY/log(s, base= 2), 3)
    M <- round((HX + HY - HXY)/log(s, base= 2), 3)
    return(c(P, C, M))
  }
  else return(NA)
}
# Bins; based on the communication with Prof. Carlos Botero #
pre.idx<- seq(1,24,by=1)
pre.breaks= 2^(pre.idx/2-2)
tmp.breaks= seq(-71, 43, length.out= 115)
## Method 2; Colwell's P with dynamic binning ####
ColwellsP_Dym<- function(input.ts, breaks= 24, log=F){
  # Modified from the source code in hydrostats package
  if(sum(is.na(input.ts))== 0&& length(input.ts)> 0){
    focal.ts<- data.frame(obs= input.ts)
    ts.leng<- length(focal.ts[,1])
    focal.ts$month<- rep(1:12, times=ts.leng/12) ## Assuming starting from Janurary ##### Can add 'start month' if needed
    if(log==F){
      focal.ts$class<- cut(focal.ts$obs, breaks, right = FALSE, include.lowest = TRUE)
      focal.table<- with(focal.ts, table(class, month))
    }
    else{
      trs.ts<- data.frame(obs=log2(focal.ts$obs))
      trs.ts$month<- rep(1:12, times=ts.leng/12)
      if(min(trs.ts$obs)==-Inf){
        trs.breaks=seq(0, 23)
        trs.ts$class<- cut(trs.ts$obs, trs.breaks, right = FALSE, include.lowest = TRUE)
      }
      else{
        trs.ts$class<- cut(trs.ts$obs, breaks, right = FALSE, include.lowest = TRUE)
      }
      focal.table<- with(trs.ts, table(class, month))
    }
    ##
    X <- apply(focal.table, 2, sum, na.rm = T)
    Y <- apply(focal.table, 1, sum, na.rm = T)
    Z <- sum(focal.table, na.rm = TRUE)
    HX <- -1 * sum((X/Z) * log(X/Z, base= 2), na.rm = TRUE)
    HY <- -1 * sum((Y/Z) * log(Y/Z, base= 2), na.rm = TRUE)
    HXY <- -1 * sum((focal.table/Z) * log(focal.table/Z, base= 2), na.rm = TRUE)
    s <- breaks
    P <- round(1 - (HXY - HX)/log(s, base= 2), 3)
    C <- round(1 - HY/log(s, base= 2), 3)
    M <- round((HX + HY - HXY)/log(s, base= 2), 3)
    return(c(P, C, M))
  }
  else return(NA)
}
## Method 3: Normalised spectral entropy ####
Spc_Entropy<- function(input.ts){
  if(sum(is.na(input.ts))== 0&& length(input.ts)> 0){
    test.spcm<- spectrum(input.ts, plot=F)
    test.spec<- test.spcm$spec
    H_prime<- 0
    sum_Amp<- 0
    length_spec<- length(test.spec)
    for(i in 1:length_spec){
      sum_Amp<- sum_Amp+ test.spec[i]
    }
    for(i in 1: length_spec){
      pp<- test.spec[i]/sum_Amp
      H_prime<- H_prime- pp*log(pp)
    }
    H_prime.max<- log(length_spec)
    Evenness<- H_prime/H_prime.max
    return(round((1- Evenness),3))
  }
  else return(NA)
}
# Generate summary for plotting #####
plot.data<- matrix(NA, nrow= length(xy[,1]), ncol= 13)
colnames(plot.data)<- c("lon", "lat", "avg", "sd", "med", "cv", "prd_SpcEntropy", "prd_ColwellFix","cst_ColwellFix","ctg_ColwellFix","prd_ColwellDym","cst_ColwellDym","ctg_ColwellDym")
# Assign values to each column
plot.data[,1]<- xy[,1]
plot.data[,2]<- xy[,2]
for(i in 1: length(xy[,1])){
  input<- clm.data[i,]
  plot.data[i,3]<- round(mean(input),3)               # Average (mean)
  plot.data[i,4]<- round(sd(input), 3)                # SD
  plot.data[i,5]<- round(median(input),3)             # Median
  plot.data[i,6]<- round(sd(input)/mean(input),3)     # Coefficient of variance
  plot.data[i,7]<- Spc_Entropy(input)                 # Predictability with normalised spectral entropy
  Fix_out<- ColwellsP_Fix(input, breaks= pre.breaks)  # Get the list of Colwell's predictability, constancy, and contingency
  plot.data[i,8]<- Fix_out[1]                         # Predictability (fixed binning)
  plot.data[i,9]<- Fix_out[2]                         # Constancy (fixed binning)
  plot.data[i,10]<- Fix_out[3]                        # Contingency  (fixed binning)
  Dym_out<- ColwellsP_Dym(input, breaks= 24, log=T)   # Get the list of Colwell's predictability, constancy, and contingency
  plot.data[i,11]<- Dym_out[1]                        # Predictability (dynamic binning)
  plot.data[i,12]<- Dym_out[2]                        # Constancy (dynamic binning)
  plot.data[i,13]<- Dym_out[3]                        # Contingency (dynamic binning)
}
# Save the processed matrix
fwrite(as.data.table(plot.data), "PRE_CRU1901-2020_26Oct.csv")
