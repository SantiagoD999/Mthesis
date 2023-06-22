##################### Main Inflation 

### 1. Import Packages 

pacman::p_load(fpp2,readxl,fpp3,gets,ForecastComb,glmnet,GGally,vars,tsDyn,nowcasting,
               HDeconometrics,caret,caretForecast,TTR)

### 2. Import Data

dfinf1<-ts(read_excel("~/Documents/Graduate Thesis/Data and Code/DATABASE1.xlsx", sheet = "INFDATAB"),frequency = 12,start = c(1954,07))[,-1]
transinf1<-na.omit(read_excel("~/Documents/Graduate Thesis/Data and Code/DATABASE1.xlsx", sheet = "List of Variables")[,5]$`Initial Transformation`)
pdelayinf1<-na.omit(read_excel("~/Documents/Graduate Thesis/Data and Code/DATABASE1.xlsx", sheet = "List of Variables")[,4]$`Publication delay (approximated days)`)
surveydata<-ts(read_excel("~/Documents/Graduate Thesis/Data and Code/DATABASE1.xlsx", sheet = "SURVEYRES (M)"),frequency = 12,start = c(2019,01))[,2]

seasall<-function (x, h = NULL) {
  if (!is.ts(x)) {
    stop("Not a time series")
  }
  else {
    fr.x <- frequency(x)
  }
  if (is.null(h)) {
    if (fr.x == 1) {
      stop("Non-seasonal time series")
    }
    dummy <- as.factor(cycle(x))
    dummy.mat <- matrix(0, ncol = frequency(x), nrow = length(x))
    nrow <- 1:length(x)
    for (i in 1:(frequency(x))) dummy.mat[dummy == paste(i), 
                                          i] <- 1
    colnames(dummy.mat) <- if (fr.x == 12) {
      month.abb[1:12]
    }
    else if (fr.x == 4) {
      c("Q1", "Q2", "Q3","Q4")
    }
    else {
      paste("S", 1:(fr.x ), sep = "")
    }
    return(dummy.mat)
  }
  else {
    return(seasonaldummy(ts(rep(0, h), start = tsp(x)[2] + 
                              1/fr.x, frequency = fr.x)))
  }
}

BASE3<-function(base, trans, NA.replace = F, aggregate = F, k.ma = 3, na.prop = 1, h = 0) {
  if (is.null(trans)) {
    stop("trans can not to be NULL")
  }
  if (sum(is.na(trans)) != 0) {
    stop("trans does not support missings values")
  }
  if (length(trans) != ncol(base)) {
    stop("the number of elements in the vector must be equal to the number of columns of base")
  }
  if (sum(!names(table(trans)) %in% c(0:7)) != 0) {
    stop("the only available transformations are 0, 1, 2, 3, 4, 5, 6, and 7.")
  }
  if (na.prop < 0 | na.prop > 1) {
    stop("na.prop must be between 0 and 1.")
  }
  base1 <- base
  for (j in 1:ncol(base)) {
    base1[, j] <- NA
    if (trans[j] == 3) {
      temp <- (diff(base[, j])/stats::lag(base[, j], -1))*100
      base1[-1, j] <- temp
    }
    else if (trans[j] == 1) {
      temp <- diff(base[, j])
      base1[-1, j] <- temp
    }
    else if (trans[j] == 2) {
      temp <- diff(diff(base[, j]))
      base1[-c(1:2), j] <- temp
    }
    else if (trans[j] == 4) {
      temp <- diff(diff(base[, j], 12))
      base1[-c(1:13), j] <- temp
    }
    else if (trans[j] == 5) {
      temp <- diff(base[, j], 12)
      base1[-c(1:12), j] <- temp
    }
    else if (trans[j] == 6) {
      temp <- (diff(base[, j], 12)/stats::lag(base[, j], 
                                              -12))*100
      base1[-c(1:12), j] <- temp
    }
    else if (trans[j] == 7) {
      temp <- diff(base[, j], 3)/stats::lag(base[, j], 
                                            -3)
      base1[-c(1:3), j] <- temp
    }
    else if (trans[j] == 0) {
      base1[, j] <- base[, j]
    }
  }
  if (aggregate == T) {
    for (j in 1:ncol(base)) {
      base1[, j] <- stats::filter(base1[, j], c(1, 2, 3, 
                                                2, 1), sides = 1)
    }
  }
  colnames(base1) <- colnames(base)
  SerOk <- colSums(is.na(base1)) < (nrow(base1) * na.prop)
  base2 <- base1[, which(SerOk)]
  if (sum(SerOk) == 1) {
    stop("the procedure can not be done with only one series available.")
  }
  if (sum(!SerOk) > 0) {
    warning(paste(sum(!SerOk), "series ruled out due to lack in observations (more than", 
                  round(na.prop * 100, 2), "% NA)."))
  }
  seriesdeletadas <- colnames(base1[, which(!SerOk)])
  print(seriesdeletadas)
  base3 <- base2 * NA
  for (i in 1:ncol(base2)) {
    na <- is.na(base2[, i])
    na2 <- NULL
    for (j in 1:length(na)) {
      na2[j] <- ifelse(sum(na[j:length(na)]) == length(j:length(na)), 
                       1, 0)
    }
    suppressWarnings({
      na_position <- min(which(na2 == 1)) - 1
    })
    if (length(which(na2 == 1)) == 0) {
      na_position <- nrow(base2)
    }
  }
  base4 <- ts(rbind(base2, matrix(NA, nrow = h, ncol = ncol(base2))), 
              start = start(base2), frequency = 12)
  return(base4)
}

dfinf2<-subset(BASE3(dfinf1,trans = transinf1),start=2)

transinf3<-NULL
for (i in 1:NCOL(dfinf2)){
  transinf3[[i]]<-ndiffs(na.omit(dfinf2[,i]),test = "adf",alpha = 0.01,lags=12,selectlags="AIC")
}  

dfinf3<-subset(BASE3(dfinf2,trans = unlist(transinf3)),start=2)

DFINF2TS<-as_tsibble(dfinf2,pivot_longer = FALSE)

### 3. Descriptive Statistics

variable<-"INF"
relevantdata<-dfinf3

# Groups of variables 

monetary<-c("M1","M3","CONCR","HOCR","COMCR","MICROCR")

activity<-c("ISE","UNEM","PRATE","OCRATE")

prices<-c("INFIPP","INFIPPAC","INFIPPM","INFIPPIN","INFIPPLC","INFIPPIM","INFIPPEX","INFEXF","INFEXFR",
          "FOODINF","FOODRINF","NHPI")

interestr<-c("TES 1","TES 5","TES 10","DTF90","TPM","CRERTOT")

surveys<-c("ICC","ICI","ICCom")
             
external<-c("TRM","EXIIPC","EXIIPP","EMBICOL","TEXP","AEXP","MEXP","FEEXP","TIMP","AIMP","MIMP","FEIMP","OTIMP")

global<-c("OILP","10TY","VIX","INFUSA","EMBI","EMBILATINO","GSCPI","FOODINPRI","SSTA3P4")

gtrends<-colnames(as.data.frame(relevantdata)[ , grepl( "GT" , names( as.data.frame(relevantdata) ) ) ])

# Time Plots

autoplot(na.omit(relevantdata[,variable]),color="black")+
  labs(y="%",x="Date",title = "Consumer's Inflation Rate (CPI)",subtitle = "Monthly Change Rate")+
  guides(colour = guide_legend(title = "Series"))

# Histogram

# hist1<-hist(window(datainf2[,variable],end=c(1990,12)),plot = FALSE,breaks = 20)
# hist2<-hist(window(datainf2[,variable],start=c(1991,01)),plot = FALSE,breaks = 20)
# 
# plot(hist1,col=rgb(0,0,1,1/4),main="Histogram of Inflation Rates",xlab="%",xlim=c(-2,8),ylim=100)
# plot(hist2,col=rgb(1,0,0,0.5),add=TRUE)
# 
# legend("topright", inset=.05, title="Periods of Time",
#        c("Pre_1991","Post_1991"), fill=c(rgb(0,0,1,1/4),rgb(1,0,0,0.5)))

period1<-na.omit(window(dfinf2[,variable],end=c(1990,12)))
period2<-na.omit(window(dfinf2[,variable],start=c(1991,01)))

histo<-as_tsibble(cbind("Pre 1991"=period1,"Post 1991"=period2))

t.test(value ~ key, data = histo)

ggplot(histo, aes(x= value, fill= key,  color = key)) +
  geom_histogram(position="identity", alpha=0.3) +
  labs(x = "%", y = "Count", fill = "Period", title = "Histogram of Consumer's Inflation Rates (CPI)",subtitle = "Monthly Change Rate ")+guides(color = FALSE) 

isat(zoo(as.data.frame(na.omit(dfinf2[,"INF"]))),plot = TRUE,t.pval = 0.001,sis = TRUE,iis = TRUE,print.searchinfo = FALSE)

# Correlation Plots

groups<-list(monetary,activity,prices,interestr,surveys,external,global,gtrends)

lowerFn <- function(data, mapping, method = "lm", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(colour = "blue") +
    geom_smooth(method = method, color = "red", ...)
}

for (group in groups){
  print(ggpairs(
    na.omit(as.data.frame(relevantdata[,c(variable,group)])), lower = list(continuous = wrap(lowerFn, method = "lm",se=TRUE)),
    diag = list(continuous = wrap("barDiag", colour = "blue")),
    upper = list(continuous = wrap("cor", size = 3))
  ))
}

# Seasonal Plots

ggsubseriesplot(na.omit(dfinf2[,variable])) +
  ylab("%") +
  ggtitle("Seasonal Plot: Consumer's Inflation Rate (CPI)",subtitle = "Monthly Change Rate")

ggseasonplot(na.omit(dfinf2[,variable]),year.labels=TRUE, year.labels.left=TRUE) +
  ylab("%") +
  ggtitle("Consumer's Inflation Rate (CPI)",subtitle = "Monthly Change Rater")

summary(tslm(na.omit(dfinf2[,variable])~season))

ggPacf(na.omit(dfinf2[,variable]))+
  ggtitle("Partial Autocorrelation Function: Consumer's Inflation Rate (CPI)",subtitle = "Monthly Change Rate")

ggAcf(na.omit(dfinf2[,variable]))+
  ggtitle("Autocorrelation Function: Consumer's Inflation Rate (CPI)",subtitle = "Monthly Change Rate")

### 4. Model estimation and Evaluation

# Univariate

h<-1
fcst_dates1 <- seq.Date(from = as.Date("2017-01-12"),to =as.Date("2023-02-12"),by ="months") # 24 months of train then from 2015 to before 2017. 
apply_lags <- function(mydf, k) {
  lag(mydf, n = k)
}
lags <- seq(1:h)
label = glue::glue("{lags}") %>% 
  as.character()
lag_functions <- setNames(paste("apply_lags(., ", lags, ")"), nm = label)

frw1<-NULL
for (date in fcst_dates1) {
  vintage<-PRTDB(dfinf2,pdelayinf1,vintage = date)
  vintage1<-as.data.frame(vintage)
  vintage1[NROW(vintage1)+50,]<-NA
  vintage1<-vintage1 %>% 
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions))
  #X<-zoo(cbind(vintage1,"trend"=rep(1:NROW(vintage1))))
  X<-zoo(vintage1)
  X1<-cbind("INF"=X[,1],X[ , colSums(is.na(X[(length(na.omit(X[,1]))+h),]))==0])
  frw1<-c(frw1,tail(na.omit(X1)[,"INF"],1))
}
forecast::accuracy(frw1,as.numeric(dfinf2[774:822,1]))
autoplot(ts(dfinf2[774:822,1]))+autolayer(ts(frw1))

fmean1<-NULL
for (date in fcst_dates1) {
  vintage<-PRTDB(dfinf2,pdelayinf1,vintage = date)
  vintage1<-as.data.frame(vintage)
  vintage1[NROW(vintage1)+50,]<-NA
  vintage1<-vintage1 %>% 
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions))
  #X<-zoo(cbind(vintage1,"trend"=rep(1:NROW(vintage1))))
  X<-zoo(vintage1)
  X1<-cbind("INF"=X[,1],X[ , colSums(is.na(X[(length(na.omit(X[,1]))+h),]))==0])
  fmean1<-c(fmean1,mean(na.omit(X1)[,"INF"]))
}
forecast::accuracy(fmean1,as.numeric(dfinf2[774:822,1]))
autoplot(ts(dfinf2[774:822,1]))+autolayer(ts(fmean1))

farima<-NULL
for (date in fcst_dates1) {
  vintage<-PRTDB(dfinf2,pdelayinf1,vintage = date)
  vintage1<-as.data.frame(vintage)
  vintage1[NROW(vintage1)+50,]<-NA
  vintage1<-vintage1 %>% 
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions))
  #X<-zoo(cbind(vintage1,"trend"=rep(1:NROW(vintage1))))
  X<-zoo(vintage1)
  X1<-ts(cbind("INF"=X[,1],X[ , colSums(is.na(X[(length(na.omit(X[,1]))+h),]))==0]),start=start(vintage),frequency = 12)
  mod<-auto.arima(na.omit(X1)[,"INF"])
  farima<-c(farima,forecast::forecast(mod,h=h)$mean[1:h][h])
}
forecast::accuracy(farima,as.numeric(dfinf2[774:822,1]))
autoplot(ts(dfinf2[774:822,1]))+autolayer(ts(farima))

fets<-NULL
for (date in fcst_dates1) {
  vintage<-PRTDB(dfinf2,pdelayinf1,vintage = date)
  vintage1<-as.data.frame(vintage)
  vintage1[NROW(vintage1)+50,]<-NA
  vintage1<-vintage1 %>% 
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions))
  #X<-zoo(cbind(vintage1,"trend"=rep(1:NROW(vintage1))))
  X<-zoo(vintage1)
  X1<-ts(cbind("INF"=X[,1],X[ , colSums(is.na(X[(length(na.omit(X[,1]))+h),]))==0]),start=start(vintage),frequency = 12)
  mod<-ets(na.omit(X1)[,"INF"])
  fets<-c(fets,forecast::forecast(mod,h=h)$mean[1:h][h])
}
forecast::accuracy(fets,as.numeric(dfinf2[774:822,1]))
autoplot(ts(dfinf2[774:822,1]))+autolayer(ts(fets))

ftheta<-NULL
for (date in fcst_dates1) {
  vintage<-PRTDB(dfinf2,pdelayinf1,vintage = date)
  vintage1<-as.data.frame(vintage)
  vintage1[NROW(vintage1)+50,]<-NA
  vintage1<-vintage1 %>% 
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions))
  #X<-zoo(cbind(vintage1,"trend"=rep(1:NROW(vintage1))))
  X<-zoo(vintage1)
  X1<-ts(cbind("INF"=X[,1],X[ , colSums(is.na(X[(length(na.omit(X[,1]))+h),]))==0]),start=start(vintage),frequency = 12)
  ftheta<-c(ftheta,thetaf(na.omit(X1)[,"INF"],h=h)$mean[1:h][h])
}
forecast::accuracy(ftheta,as.numeric(dfinf2[774:822,1]))
autoplot(ts(dfinf2[774:822,1]))+autolayer(ts(ftheta))

f2diff<-NULL
for (date in fcst_dates1){
  vintage<-PRTDB(dfinf2,delay = pdelayinf1,vintage = date)
  vintage1<-as.data.frame(vintage)
  vintage1[NROW(vintage1)+50,]<-NA
  vintage1<-vintage1 %>% 
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions))
  X<-zoo(vintage1)
  XD<-zoo(cbind("s"=X[,1],diff(X)))
  X1<-cbind("INF"=XD[,2],XD[ , colSums(is.na(XD[(length(na.omit(XD[,1]))+h),]))==0])
  f2diff<-c(f2diff,((h)*tail(na.omit(X1)[,"INF"],1)+tail(na.omit(X[,"INF"]),1)))
}
forecast::accuracy(f2diff,as.numeric(dfinf2[774:822,1]))
autoplot(ts(dfinf2[774:822,1]))+autolayer(ts(f2diff))

eets<-fets[1:49]-dfinf2[774:822,1]
efrw1<-frw1[1:49]-dfinf2[774:822,1]
earima<-farima[1:49]-dfinf2[774:822,1]
emean<-fmean1[1:49]-dfinf2[774:822,1]
e2diff<-f2diff[1:49]-dfinf2[774:822,1]
etheta<-ftheta[1:49]-dfinf2[774:822,1]

dm.test(eets,efrw1,h=h,varestimator = "bartlett")
dm.test(earima,efrw1,h=h,varestimator = "bartlett")
dm.test(emean,efrw1,h=h,varestimator = "bartlett")
dm.test(e2diff,efrw1,h=h,varestimator = "bartlett")
dm.test(etheta,efrw1,h=h,varestimator = "bartlett")

# Multivariate Bechmarks

h<-13
fcst_dates1 <- seq.Date(from = as.Date("2018-01-12"),to =as.Date("2022-02-12"),by ="months")
apply_lags <- function(mydf, k) {
  lag(mydf, n = k)
}
lags <- seq(1:h)
label = glue::glue("{lags}") %>% 
  as.character()
lag_functions <- setNames(paste("apply_lags(., ", lags, ")"), nm = label)

frw1<-NULL
for (date in fcst_dates1) {
  vintage<-PRTDB(dfinf2,pdelayinf1,vintage = date)
  vintage1<-as.data.frame(vintage)
  vintage1[NROW(vintage1)+50,]<-NA
  vintage1<-vintage1 %>% 
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions))
  #X<-zoo(cbind(vintage1,"trend"=rep(1:NROW(vintage1))))
  X<-zoo(vintage1)
  X1<-cbind("INF"=X[,1],X[ , colSums(is.na(X[(length(na.omit(X[,1]))+h),]))==0])
  frw1<-c(frw1,tail(na.omit(X1)[,"INF"],1))
}
forecast::accuracy(frw1,as.numeric(dfinf2[774:822,1]))

fvar<-NULL
for (date in fcst_dates1) {
  vintage<-PRTDB(dfinf2,delay=pdelayinf1,vintage = date)
  modvar<-vars::VAR(na.omit(vintage[,c("INF","INFIPP","M3","DTF90","FOODINF","CRERTOT")]),type="const",lag.max = 12,ic="SC")
  fvar<-c(fvar,predict(modvar,n.ahead=h)$fcst[[1]][h])
}
forecast::accuracy(fvar,as.numeric(dfinf2[774:822,1]))
autoplot(ts(dfinf2[774:822,1]))+autolayer(ts(fvar))

f1vardiff<-NULL
for (date in fcst_dates1) {
  vintage<-PRTDB(dfinf2,delay=pdelayinf1,vintage = date)
  lagsdiff<-lags.select(na.omit(vintage[,c("INF","INFIPP","M3","DTF90","FOODINF","CRERTOT")]),include = "const",lag.max = 12)
  modelvardiff<-lineVar(na.omit(vintage[,c("INF","INFIPP","M3","DTF90","FOODINF","CRERTOT")]),lag=lagsdiff$BIC_min[2],include = "const",model = "VAR",I="diff")
  f1vardiff<-c(f1vardiff,as.numeric(predict(modelvardiff,n.ahead=h)[,"INF"])[h])
}
forecast::accuracy(f1vardiff,as.numeric(dfinf2[774:822,1]))
autoplot(ts(dfinf2[774:822,1]))+autolayer(ts(f1vardiff))

forvar<-NULL
for (i in 1:50){
  mod<-vars::VAR(na.omit(dfinf2[1:(760+i),c("INF","INFIPP","M3","DTF90","FOODINF","CRERTOT")]),type="const",lag.max = 12,ic="SC")
  forvar[[i]]<-predict(mod,n.ahead=h)$fcst[[1]][h]
}
forecast::accuracy(unlist(forvar),as.numeric(dfinf2[774:822,1]))
autoplot(ts(dfinf2[774:822,1]))+autolayer(ts(forvar))

evar<-fvar[1:49]-dfinf2[774:822,1]
evardiff<-f1vardiff[1:49]-dfinf2[774:822,1]
efrw1<-frw1[1:49]-dfinf2[774:822,1]

dm.test(evar,efrw1,h=h,varestimator = "bartlett")
dm.test(evardiff,efrw1,h=h,varestimator = "bartlett")

# GETS

h<-1
fcst_dates1 <- seq.Date(from = as.Date("2017-01-12"),to =as.Date("2023-02-12"),by ="months")
apply_lags <- function(mydf, k) {
  lag(mydf, n = k)
}
lags <- seq(1:12)
label = glue::glue("{lags}") %>% 
  as.character()
lag_functions <- setNames(paste("apply_lags(., ", lags, ")"), nm = label)

frw1<-NULL
for (date in fcst_dates1) {
  vintage<-PRTDB(dfinf2,pdelayinf1,vintage = date)
  vintage1<-as.data.frame(vintage)
  vintage1[NROW(vintage1)+50,]<-NA
  vintage1<-vintage1 %>% 
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions))
  #X<-zoo(cbind(vintage1,"trend"=rep(1:NROW(vintage1))))
  X<-zoo(vintage1)
  X1<-cbind("INF"=X[,1],X[ , colSums(is.na(X[(length(na.omit(X[,1]))+h),]))==0])
  frw1<-c(frw1,tail(na.omit(X1)[,"INF"],1))
}
forecast::accuracy(frw1,as.numeric(dfinf2[774:822,1]))

fseas<-NULL
for (date in fcst_dates1){
  vintage<-PRTDB(dfinf2,delay = pdelayinf1,vintage = date)
  NAs<-matrix(rep(NA),nrow = 50,ncol=ncol(vintage))
  vintage<-ts(rbind(vintage,NAs),start = start(vintage),frequency = 12)
  seas<-as.data.frame(seasonaldummy(vintage[,1]))
  vintage1<-as.data.frame(vintage)
  vintage1<-vintage1 %>%
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions))
  X<-zoo(cbind(vintage1,seas))
  X1<-cbind("INF"=X[,1],X[ , colSums(is.na(X[(length(na.omit(X[,1]))+h),]))==0])
  X1<-X1[,grepl( "\\bINF_" , names( as.data.frame(X1) ))==0]
  #X1<-X1[,c("INF","INF_13","INF_14")]
  X1isat<-cbind(X1,iim(X1),sim(X1),tim(X1))
  length1<-max(sapply(X1[,-1], Position, f=Negate(is.na)))
  moarxil<-getsm(isat(X1[(length1-12):NROW(X1),1],uis= X1[,-which(names(X1) %in% c("INF",colnames(seas)))],mxreg = X1[,colnames(seas)],t.pval = 0.0001,sis = TRUE,print.searchinfo = FALSE,do.pet = TRUE,tis=TRUE,iis=TRUE,ar=1:12),t.pval = 0.0001,keep=c(1,14:24),print.searchinfo = FALSE,ar.LjungB = NULL,arch.LjungB = NULL)   
  fseas<-c(fseas,predict(moarxil,newmxreg=X1isat[(length(na.omit(X1isat[,"INF"]))+1):(length(na.omit(X1isat[,"INF"]))+h),intersect(moarxil$aux$mXnames,colnames(X1isat))],n.ahead = h)[h])#+tail(as.numeric(moarxil$residuals),1)
}
forecast::accuracy(fseas,dfinf2[774:822,1])
autoplot(ts(dfinf2[774:822,1]))+autolayer(ts(fseas))

fic<-NULL
for (date in fcst_dates1){
  vintage<-PRTDB(dfinf2,delay = pdelayinf1,vintage = date)
  NAs<-matrix(rep(NA),nrow = 50,ncol=ncol(vintage))
  vintage<-ts(rbind(vintage,NAs),start = start(vintage),frequency = 12)
  seas<-as.data.frame(seasonaldummy(vintage[,1]))
  vintage1<-as.data.frame(vintage)
  vintage1<-vintage1 %>%
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions))
  X<-zoo(cbind(vintage1,seas))
  X1<-cbind("INF"=X[,1],X[ , colSums(is.na(X[(length(na.omit(X[,1]))+h),]))==0])
  X1<-X1[,grepl( "\\bINF_" , names( as.data.frame(X1) ))==0]
  X1isat<-cbind(X1,iim(X1),sim(X1),tim(X1))
  length1<-max(sapply(X1[,-1], Position, f=Negate(is.na)))
  moarxil<-getsm(isat(X1[(length1-25):NROW(X1),1],uis= X1[,-which(names(X1) %in% c("INF",colnames(seas)))],mxreg=X1[,colnames(seas)],ar=25,t.pval = 0.0001,sis = TRUE,print.searchinfo = FALSE,do.pet = TRUE,tis=TRUE,iis=TRUE),t.pval = 0.0001,keep=c(1,3:13),print.searchinfo = FALSE,ar.LjungB = NULL,arch.LjungB = NULL)   
  fic<-c(fic,predict(moarxil,newmxreg=X1isat[(length(na.omit(X1isat[,"INF"]))+1):(length(na.omit(X1isat[,"INF"]))+h),intersect(moarxil$aux$mXnames,colnames(X1isat))],n.ahead = h)[h]+tail(as.numeric(moarxil$residuals),1))#
}
forecast::accuracy(fseas,dfinf2[774:822,1])
autoplot(ts(dfinf2[774:822,1]))+autolayer(ts(fic))

efseas<-fseas[1:49]-dfinf2[774:822,1]
efseas<-fic[1:49]-dfinf2[774:822,1]
efrw1<-frw1[1:49]-dfinf2[774:822,1]

dm.test(efseas,efrw1,h=h,varestimator = "bartlett")
dm.test(efic,efrw1,h=h,varestimator = "bartlett")

# Machine Learning

h<-13
fcst_dates1 <- seq.Date(from = as.Date("2016-01-12"),to =as.Date("2022-02-12"),by ="months")
apply_lags <- function(mydf, k) {
  lag(mydf, n = k)
}
lags <- seq(1:13)
label = glue::glue("{lags}") %>% 
  as.character()
lag_functions <- setNames(paste("apply_lags(., ", lags, ")"), nm = label)

frw1<-NULL
for (date in fcst_dates1) {
  vintage<-PRTDB(dfinf2,pdelayinf1,vintage = date)
  vintage1<-as.data.frame(vintage)
  vintage1[NROW(vintage1)+50,]<-NA
  vintage1<-vintage1 %>% 
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions))
  #X<-zoo(cbind(vintage1,"trend"=rep(1:NROW(vintage1))))
  X<-zoo(vintage1)
  X1<-cbind("INF"=X[,1],X[ , colSums(is.na(X[(length(na.omit(X[,1]))+h),]))==0])
  frw1<-c(frw1,tail(na.omit(X1)[,"INF"],1))
}
forecast::accuracy(frw1,as.numeric(dfinf2[774:822,1]))

max(floor((ncol(X1)-1)/3), 1)
ntree=1000
alphas1<-seq(0,1,length=100)
lambdas1 <- 10^seq(-3, 3, length = 100)
cost1<-10^seq(-4,4,length=10)
maxit = 1000
k1=seq(1:12)
frf<-NULL
for (date in fcst_dates1) {
  vintage<-PRTDB(dfinf2,delay = pdelayinf1,vintage = date)
  NAs<-matrix(rep(NA),nrow = 50,ncol=ncol(vintage))
  vintage<-ts(rbind(vintage,NAs),start = start(vintage),frequency = 12)
  seas<-as.data.frame(seasonaldummy(vintage[,1]))
  vintage1<-as.data.frame(vintage)
  vintage1<-vintage1 %>% 
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions))
  X<-zoo(cbind(vintage1,seas))
  X1<-cbind("INF"=X[,1],X[ , colSums(is.na(X[(length(na.omit(X[,1]))+h),]))==0])
  #X1<-X1[,c("INF","INF_1","INFIPP_1","M3_1","DTF90_1","EMBICOL_1","FOODINF_1","CRERTOT_1")]
  #X1<-X1[,c("INF","INF_13","INF_14")]
  myTimeControl <- trainControl(method = "timeslice",
                                initialWindow = (NROW(na.omit(X1))-h-(20-1)),
                                horizon = h,
                                fixedWindow = FALSE)
  glmnet.mod <- train(INF ~. ,
                      data = na.omit(X1),
                      method="rf",
                      trControl = myTimeControl,
                      metric='RMSE',tuneGrid=expand.grid(mtry=c(2,max(floor((ncol(X1)-1)/3), 1))),ntree=1000)
  
  frf<-c(frf,predict(glmnet.mod,X1[(length(na.omit(X1[,"INF"]))+1):(length(na.omit(X1[,"INF"]))+h),-1])[h])
}
forecast::accuracy(frf,dfinf2[774:822,1])
autoplot(ts(dfinf2[774:822,1]))+autolayer(ts(frf))

efrw1<-frw1[1:49]-dfinf2[774:822,1]
efrf<-frf[1:49]-dfinf2[774:822,1]

dm.test(efrf,efrw1,h=h,varestimator = "bartlett")

# DFM

h<-25
fcst_dates1 <- seq.Date(from = as.Date("2017-01-12"),to =as.Date("2021-02-12"),by ="months")
apply_lags <- function(mydf, k) {
  lag(mydf, n = k)
}
lags <- seq(1:25)
label = glue::glue("{lags}") %>% 
  as.character()
lag_functions <- setNames(paste("apply_lags(., ", lags, ")"), nm = label)

frw1<-NULL
for (date in fcst_dates1) {
  vintage<-PRTDB(dfinf2,pdelayinf1,vintage = date)
  vintage1<-as.data.frame(vintage)
  vintage1[NROW(vintage1)+50,]<-NA
  vintage1<-vintage1 %>% 
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions))
  #X<-zoo(cbind(vintage1,"trend"=rep(1:NROW(vintage1))))
  X<-zoo(vintage1)
  X1<-cbind("INF"=X[,1],X[ , colSums(is.na(X[(length(na.omit(X[,1]))+h),]))==0])
  frw1<-c(frw1,tail(na.omit(X1)[,"INF"],1))
}
forecast::accuracy(frw1,dfinf2[774:822,1])

fdfm <- NULL
for(date in fcst_dates1){
  vintage<-PRTDB(dfinf2,delay = pdelayinf1,vintage = date)
  NAs<-matrix(rep(NA),nrow = 50,ncol=ncol(vintage))
  vintage<-ts(rbind(vintage,NAs),start = start(vintage),frequency = 12)
  length1<-max(sapply(vintage[,-1], Position, f=Negate(is.na)))
  vintage1<-subset(ts(rbind(vintage,NAs),start = start(vintage),frequency = 12),start=length1)
  nowgdp<-nowcast(formula = INF~., frequency = rep(12,NCOL(vintage1)),data = vintage1,r =ICfactors(vintage1[,-1])$r_star,q=ICshocks(vintage1[,-1],r=ICfactors(vintage1[,-1])$r_star,1)$q_star, p=1, method = "2s")
  fdfm<-c(fdfm,na.omit(nowgdp$yfcst[,3])[h])
}
forecast::accuracy(fdfm,dfinf2[774:822,1])
autoplot(ts(dfinf2[774:822,1]))+autolayer(ts(fdfm))

efrw1<-frw1[1:49]-dfinf2[774:822,1]
efdfm<-fdfm[1:49]-dfinf2[774:822,1]

dm.test(efdfm,efrw1,h=h,varestimator = "bartlett")

# Subjective forecast comparison (EME)

erw<-dfinf2[774:822,1]-frw1[1:49]
esurvey<-dfinf2[774:822,1]-surveydata
autoplot(ts(dfinf2[774:822,1]))+autolayer(ts(surveydata))+autolayer(ts(frw1))+autolayer(ts(fseas))

dm.test(e,esurvey,h=1,varestimator = "bartlett")
forecast::accuracy(frw1,dfinf2[774:822,1])
forecast::accuracy(surveydata,dfinf2[774:822,1])

# Forecast Combinations

h<-1
train.comb <-24
data<-dfinf2
delay<-pdelayinf1
# The initial date minus the training period as years 24=2015 for 25 step-ahead forecasts
observed1<-na.omit(as.numeric(data[(length(na.omit(PRTDB(data,delay,vintage ="2017-01-12")[,1]))+h):(length(na.omit(PRTDB(data,delay,vintage ="2017-01-12")[,1]))+h+train.comb-1),1]))
observed2<-na.omit(as.numeric(data[(length(na.omit(PRTDB(data,delay,vintage ="2017-01-12")[,1]))+h+train.comb):(length(na.omit(PRTDB(data,delay,vintage ="2023-02-12")[,1]))),1]))

comb_train<-cbind(observed1,
                  fets[1:length(observed1)],
                  fsvm[1:length(observed1)], 
                  fseas[1:length(observed1)],
                  farima[1:length(observed1)])

comb_test<-cbind(observed2,
                 fets[(train.comb+1):(length(observed2)+train.comb)],
                 fsvm[(train.comb+1):(length(observed2)+train.comb)],
                 fseas[(train.comb+1):(length(observed2)+train.comb)],
                 farima[(train.comb+1):(length(observed2)+train.comb)])

inputdata1<-foreccomb(observed_vector=comb_train[,1],prediction_matrix=comb_train[,-1],newobs =comb_test[,1],newpreds =comb_test[,-1],na.impute = FALSE)
combm<-c("SA","BG","CLS","CSR","EIG1","EIG2","EIG3","EIG4","InvW","LAD","MED","NG","OLS","TA","WA")

for(i in combm) { 
  nam <- paste(i)
  assign(nam, eval(parse(text=paste( "comb_",i,"(inputdata1)" ,sep = ""))))
}

rescomb<-c(SA$Accuracy_Test["Test set","RMSE"],BG$Accuracy_Test["Test set","RMSE"],CLS$Accuracy_Test["Test set","RMSE"],
  CSR$Accuracy_Test["RMSE","aic"],EIG1$Accuracy_Test["Test set","RMSE"],EIG2$Accuracy_Test["Test set","RMSE"],
  EIG3$Accuracy_Test["Test set","RMSE"],EIG4$Accuracy_Test["Test set","RMSE"],InvW$Accuracy_Test["Test set","RMSE"],
  LAD$Accuracy_Test["Test set","RMSE"],MED$Accuracy_Test["Test set","RMSE"],NG$Accuracy_Test["Test set","RMSE"],OLS$Accuracy_Test["Test set","RMSE"],
  TA$Accuracy_Test["Test set","RMSE"],WA$Accuracy_Test["Test set","RMSE"],auto_combine(inputdata1)$Accuracy_Test["Test set","RMSE"])

View(t(rescomb))

NEW<-(1/4)*(comb_test[,2]+comb_test[,3]+comb_test[,4]+comb_test[,5])
forecast::accuracy(NEW,dfinf2[774:822,1])

efrw1<-dfinf2[774:822,1]-frw1[(train.comb+1):(length(observed2)+train.comb)]
e<-dfinf2[774:822,1]-CSR$Forecasts_Test[,1]
dm.test(efrw1,e,h=h,varestimator="bartlett")

RESULTS1<-ts(cbind("Observed"=comb_test[,1],
                "ETS"=comb_test[,2],"SVM"=comb_test[,3],
                "GETSISAT"=comb_test[,4],"ARIMA"=comb_test[,5],
                "TA"=TA$Forecasts_Test,"EME"=surveydata),start=c(2019,01),frequency = 12)
autoplot(RESULTS1)+
  labs(y="%",x="Date",title = "Forecast Evaluation: Inflation (CPI)",subtitle = "Monthly Change Rate")+
  guides(colour = guide_legend(title = "Series"))

h<-13
train.comb <-24
data<-dfinf2
delay<-pdelayinf1
# The initial date minus the training period as years 24=2015 for 25 step-ahead forecasts
observed1<-na.omit(as.numeric(data[(length(na.omit(PRTDB(data,delay,vintage ="2016-01-12")[,1]))+h):(length(na.omit(PRTDB(data,delay,vintage ="2016-01-12")[,1]))+h+train.comb-1),1]))
observed2<-na.omit(as.numeric(data[(length(na.omit(PRTDB(data,delay,vintage ="2016-01-12")[,1]))+h+train.comb):(length(na.omit(PRTDB(data,delay,vintage ="2023-02-12")[,1]))),1]))

comb_train<-cbind(observed1,
                  fseas[1:length(observed1)],
                  fets[1:length(observed1)],
                  frf[1:length(observed1)],
                  fdfm[1:length(observed1)])

comb_test<-cbind(observed2,
                 fseas[(train.comb+1):(length(observed2)+train.comb)],
                 fets[(train.comb+1):(length(observed2)+train.comb)],
                 frf[(train.comb+1):(length(observed2)+train.comb)],
                 fdfm[(train.comb+1):(length(observed2)+train.comb)])

inputdata1<-foreccomb(observed_vector=comb_train[,1],prediction_matrix=comb_train[,-1],newobs =comb_test[,1],newpreds =comb_test[,-1],na.impute = FALSE)
combm<-c("SA","BG","CLS","CSR","EIG1","EIG2","EIG3","EIG4","InvW","LAD","MED","NG","OLS","TA","WA")

for(i in combm) { 
  nam <- paste(i)
  assign(nam, eval(parse(text=paste( "comb_",i,"(inputdata1)" ,sep = ""))))
}

rescomb2<-c(SA$Accuracy_Test["Test set","RMSE"],BG$Accuracy_Test["Test set","RMSE"],CLS$Accuracy_Test["Test set","RMSE"],
           CSR$Accuracy_Test["RMSE","aic"],EIG1$Accuracy_Test["Test set","RMSE"],EIG2$Accuracy_Test["Test set","RMSE"],
           EIG3$Accuracy_Test["Test set","RMSE"],EIG4$Accuracy_Test["Test set","RMSE"],InvW$Accuracy_Test["Test set","RMSE"],
           LAD$Accuracy_Test["Test set","RMSE"],MED$Accuracy_Test["Test set","RMSE"],NG$Accuracy_Test["Test set","RMSE"],OLS$Accuracy_Test["Test set","RMSE"],
           TA$Accuracy_Test["Test set","RMSE"],WA$Accuracy_Test["Test set","RMSE"],auto_combine(inputdata1)$Accuracy_Test["Test set","RMSE"])

NEW<-(1/4)*(comb_test[,2]+comb_test[,3]+comb_test[,4]+comb_test[,5])
forecast::accuracy(comb_test[,2],dfinf2[774:822,1])

efrw1<-dfinf2[774:822,1]-frw1[(train.comb+1):(length(observed2)+train.comb)]
e<-dfinf2[774:822,1]-comb_test[,2]
dm.test(efrw1,e,h=h,varestimator="bartlett")

RESULTS2<-ts(cbind("Observed"=comb_test[,1],
                   "GETSISAT"=comb_test[,2],"ETS"=comb_test[,3],
                   "RF"=comb_test[,4],"DFM"=comb_test[,5],
                   "EIG1"=EIG1$Forecasts_Test),start=c(2019,01),frequency = 12)
autoplot(RESULTS2)+
  labs(y="%",x="Date",title = "Forecast Evaluation: Inflation (CPI)",subtitle = "Monthly Change Rate")+
  guides(colour = guide_legend(title = "Series"))

h<-25
train.comb <-24
data<-dfinf2
delay<-pdelayinf1
# The initial date minus the training period as years 24=2015 for 25 step-ahead forecasts
observed1<-na.omit(as.numeric(data[(length(na.omit(PRTDB(data,delay,vintage ="2015-01-12")[,1]))+h):(length(na.omit(PRTDB(data,delay,vintage ="2015-01-12")[,1]))+h+train.comb-1),1]))
observed2<-na.omit(as.numeric(data[(length(na.omit(PRTDB(data,delay,vintage ="2015-01-12")[,1]))+h+train.comb):(length(na.omit(PRTDB(data,delay,vintage ="2023-02-12")[,1]))),1]))

comb_train<-cbind(observed1,
                  frf[1:length(observed1)],
                  farima[1:length(observed1)],
                  fmean1[1:length(observed1)],
                  fets[1:length(observed1)])

comb_test<-cbind(observed2,
                 frf[(train.comb+1):(length(observed2)+train.comb)],
                 farima[(train.comb+1):(length(observed2)+train.comb)],
                 fmean1[(train.comb+1):(length(observed2)+train.comb)],
                 fets[(train.comb+1):(length(observed2)+train.comb)])

inputdata1<-foreccomb(observed_vector=comb_train[,1],prediction_matrix=comb_train[,-1],newobs =comb_test[,1],newpreds =comb_test[,-1],na.impute = FALSE)
combm<-c("SA","BG","CLS","CSR","EIG1","EIG2","EIG3","EIG4","InvW","LAD","MED","NG","OLS","TA","WA")

for(i in combm) { 
  nam <- paste(i)
  assign(nam, eval(parse(text=paste( "comb_",i,"(inputdata1)" ,sep = ""))))
}

rescomb3<-c(SA$Accuracy_Test["Test set","RMSE"],BG$Accuracy_Test["Test set","RMSE"],CLS$Accuracy_Test["Test set","RMSE"],
           CSR$Accuracy_Test["RMSE","aic"],EIG1$Accuracy_Test["Test set","RMSE"],EIG2$Accuracy_Test["Test set","RMSE"],
           EIG3$Accuracy_Test["Test set","RMSE"],EIG4$Accuracy_Test["Test set","RMSE"],InvW$Accuracy_Test["Test set","RMSE"],
           LAD$Accuracy_Test["Test set","RMSE"],MED$Accuracy_Test["Test set","RMSE"],NG$Accuracy_Test["Test set","RMSE"],OLS$Accuracy_Test["Test set","RMSE"],
           TA$Accuracy_Test["Test set","RMSE"],WA$Accuracy_Test["Test set","RMSE"],auto_combine(inputdata1)$Accuracy_Test["Test set","RMSE"])

NEW<-(1/4)*(comb_test[,2]+comb_test[,3]+comb_test[,4]+comb_test[,5])
forecast::accuracy(comb_test[,4],dfinf2[774:822,1])

efrw1<-dfinf2[774:822,1]-frw1[(train.comb+1):(length(observed2)+train.comb)]
e<-dfinf2[774:822,1]-BG$Forecasts_Test
dm.test(efrw1,e,h=h,varestimator="bartlett")


RESULTS3<-ts(cbind("Observed"=comb_test[,1],
                   "RF"=comb_test[,2],"ARIMA"=comb_test[,3],
                   "MEAN"=comb_test[,4],"ETS"=comb_test[,5],
                   "MED"=MED$Forecasts_Test),start=c(2019,01),frequency = 12)
autoplot(RESULTS3)+
  labs(y="%",x="Date",title = "Forecast Evaluation: Inflation (CPI)",subtitle = "Monthly Change Rate")+
  guides(colour = guide_legend(title = "Series"))














