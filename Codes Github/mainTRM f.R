##################### Main Exchange Rate ########################

### 1. Import Packages and Define Functions

pacman::p_load(fpp2,readxl,fpp3,gets,ForecastComb,glmnet,GGally,vars,tsDyn,nowcasting,
               HDeconometrics,caret,caretForecast,TTR)

### 2. Import Data

dftrm1<-ts(read_excel("~/Documents/Graduate Thesis/Data and Code/DATABASE1.xlsx", sheet = "TRMDATAB"),frequency = 12,start = c(1991,11))[,-1]
transtrm1<-na.omit(read_excel("~/Documents/Graduate Thesis/Data and Code/DATABASE1.xlsx", sheet = "List of Variables (TRM)")[,5]$`Initial Transformation`)
pdelaytrm1<-na.omit(read_excel("~/Documents/Graduate Thesis/Data and Code/DATABASE1.xlsx", sheet = "List of Variables (TRM)")[,4]$`Publication delay (approximated days)`)
surveydata<-ts(read_excel("~/Documents/Graduate Thesis/Data and Code/DATABASE1.xlsx", sheet = "SURVEYRES (TRM)"),frequency = 12,start = c(2019,01))[,3]

BASE3<-function (base, trans, NA.replace = F, aggregate = F, k.ma = 3, na.prop = 1, h = 0) {
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

dftrm2<-subset(BASE3(dftrm1,trans = transtrm1),start=2)

transtrm3<-NULL
for (i in 1:NCOL(dftrm2)){
  transtrm3[[i]]<-ndiffs(na.omit(dftrm2[,i]),test = "adf",alpha = 0.01,lags=12,selectlags="AIC")
}  

dftrm3<-BASE3(dftrm2,trans = unlist(transtrm3))

DFTRM2TS<-as_tsibble(dftrm3,pivot_longer = FALSE)

### 3. Descriptive Statistics

variable<-"TRM"
relevantdata<-dftrm3

# Groups of variables 

monetary<-c("M1","M3","CONCR","HOCR","COMCR","MICROCR")

activity<-c("ISE","UNEM","PRATE","OCRATE")

prices<-c("INF","INFIPP","INFIPPAC","INFIPPM","INFIPPIN","INFIPPLC","INFIPPIM","INFIPPEX","INFEXF","INFEXFR",
          "FOODINF","FOODRINF","NHPI")

interestr<-c("TES 1","TES 5","TES 10","DTF90","TPM","CRERTOT")

surveys<-c("ICC","ICI","ICCom")

external<-c("EXIIPC","EXIIPP","EMBICOL","TEXP","AEXP","MEXP","FEEXP","TIMP","AIMP","MIMP","FEIMP","OTIMP")

global<-c("OILP","10TY","VIX","INFUSA","EMBI","EMBILATINO","GSCPI","FOODINPRI","SSTA3P4")

gtrends<-colnames(as.data.frame(relevantdata)[ , grepl( "GT" , names( as.data.frame(relevantdata) ) ) ])

# Time Plots

autoplot(na.omit(relevantdata[,variable]),color="black")+
  labs(y="%",x="Date",title = "Bilateral Exchange Rate (USD/COP)",subtitle = "Monthly Depreciation Rate")+
  guides(colour = guide_legend(title = "Series"))

isat(zoo(as.data.frame(na.omit(relevantdata[,"TRM"]))),t.pval=0.001,sis = TRUE,iis = TRUE,print.searchinfo = FALSE,plot = TRUE)

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

ggsubseriesplot(na.omit(relevantdata[,variable])) +
  ylab("%") +
  ggtitle("Seasonal Plot: Bilateral Exchange Rate (USD/COP)",subtitle = "Monthly Depreciation Rate")

ggseasonplot(na.omit(relevantdata[,variable]),year.labels=TRUE, year.labels.left=TRUE) +
  ylab("%") +
  ggtitle("Seasonal Plot: Bilateral Exchange Rate (USD/COP)",subtitle = "Monthly Depreciation Rate")

summary(tslm(na.omit(relevantdata[,variable])~season))

ggPacf(na.omit(relevantdata[,variable]))+
  ggtitle("Partial Autocorrelation Function: Bilateral Exchange Rate (USD/COP)",subtitle = "Monthly Depreciation Rate")

ggAcf(na.omit(relevantdata[,variable]))+
  ggtitle("Autocorrelation Function: Bilateral Exchange Rate (USD/COP)",subtitle = "Monthly Depreciation Rate")

### 4. Model estimation and Evaluation

# Univariate

h<-13
fcst_dates1 <- seq.Date(from = as.Date("2016-01-12"),to =as.Date("2022-02-12"),by ="months")
apply_lags <- function(mydf, k) {
  lag(mydf, n = k)
}
lags <- seq(1:h)
label = glue::glue("{lags}") %>% 
  as.character()
lag_functions <- setNames(paste("apply_lags(., ", lags, ")"), nm = label)

frw1<-NULL
for (date in fcst_dates1) {
  vintage<-PRTDB(dftrm2,pdelaytrm1,vintage = date)
  vintage1<-as.data.frame(vintage)
  vintage1[NROW(vintage1)+50,]<-NA
  vintage1<-vintage1 %>% 
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions))
  X<-zoo(vintage1)
  X1<-cbind("TRM"=X[,1],X[ , colSums(is.na(X[(length(na.omit(X[,1]))+h),]))==0])
  frw1<-c(frw1,tail(na.omit(X1)[,"TRM"],1))
}
forecast::accuracy(frw1,dftrm2[326:374,1])
autoplot(ts(dftrm2[326:374,1]))+autolayer(ts(frw1))

fmean1<-NULL
for (date in fcst_dates1) {
  vintage<-PRTDB(dftrm2,pdelaytrm1,vintage = date)
  vintage1<-as.data.frame(vintage)
  vintage1[NROW(vintage1)+50,]<-NA
  vintage1<-vintage1 %>% 
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions))
  #X<-zoo(cbind(vintage1,"trend"=rep(1:NROW(vintage1))))
  X<-zoo(vintage1)
  X1<-cbind("TRM"=X[,1],X[ , colSums(is.na(X[(length(na.omit(X[,1]))+h),]))==0])
  fmean1<-c(fmean1,mean(na.omit(X1)[,"TRM"]))
}
forecast::accuracy(fmean1,dftrm2[326:374,1])
autoplot(ts(dftrm2[326:374,1]))+autolayer(ts(fmean1))

farima<-NULL
for (date in fcst_dates1) {
  vintage<-PRTDB(dftrm2,pdelaytrm1,vintage = date)
  vintage1<-as.data.frame(vintage)
  vintage1[NROW(vintage1)+50,]<-NA
  vintage1<-vintage1 %>% 
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions))
  X<-zoo(vintage1)
  X1<-ts(cbind("TRM"=X[,1],X[ , colSums(is.na(X[(length(na.omit(X[,1]))+h),]))==0]),start=start(vintage),frequency = 12)
  mod<-auto.arima(na.omit(X1)[,"TRM"])
  farima<-c(farima,forecast::forecast(mod,h=h)$mean[1:h][h])
}
forecast::accuracy(farima,dftrm2[326:374,1])
autoplot(ts(dftrm2[326:374,1]))+autolayer(ts(farima))

fets<-NULL
for (date in fcst_dates1) {
  vintage<-PRTDB(dftrm2,pdelaytrm1,vintage = date)
  vintage1<-as.data.frame(vintage)
  vintage1[NROW(vintage1)+50,]<-NA
  vintage1<-vintage1 %>% 
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions))
  #X<-zoo(cbind(vintage1,"trend"=rep(1:NROW(vintage1))))
  X<-zoo(vintage1)
  X1<-ts(cbind("TRM"=X[,1],X[ , colSums(is.na(X[(length(na.omit(X[,1]))+h),]))==0]),start=start(vintage),frequency = 12)
  mod<-ets(na.omit(X1)[,"TRM"])
  fets<-c(fets,forecast::forecast(mod,h=h)$mean[1:h][h])
}
forecast::accuracy(fets,dftrm2[326:374,1])
autoplot(ts(dftrm2[326:374,1]))+autolayer(ts(fets))

f2diff<-NULL
for (date in fcst_dates1){
  vintage<-PRTDB(dftrm2,delay = pdelaytrm1,vintage = date)
  vintage1<-as.data.frame(vintage)
  vintage1[NROW(vintage1)+50,]<-NA
  vintage1<-vintage1 %>% 
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions))
  X<-zoo(vintage1)
  XD<-zoo(cbind("s"=X[,1],diff(X)))
  X1<-cbind("TRM"=XD[,2],XD[ , colSums(is.na(XD[(length(na.omit(XD[,1]))+h),]))==0])
  f2diff<-c(f2diff,((h)*tail(na.omit(X1[,"TRM"]),1)+tail(na.omit(X[,"TRM"]),1)))
}
forecast::accuracy(f2diff,dftrm2[326:374,1])
autoplot(ts(dftrm2[326:374,1]))+autolayer(ts(f2diff))

eets<-fets[1:49]-dftrm2[326:374,1]
efrw1<-frw1[1:49]-dftrm2[326:374,1]
earima<-farima[1:49]-dftrm2[326:374,1]
emean<-fmean1[1:49]-dftrm2[326:374,1]
e2diff<-f2diff[1:49]-dftrm2[326:374,1]

dm.test(eets,efrw1,h=h,varestimator = "bartlett")
dm.test(earima,efrw1,h=h,varestimator = "bartlett")
dm.test(emean,efrw1,h=h,varestimator = "bartlett")
dm.test(e2diff,efrw1,h=h,varestimator = "bartlett")

# Multivariate Bechmarks

h<-25
fcst_dates1 <- seq.Date(from = as.Date("2015-01-12"),to =as.Date("2021-02-12"),by ="months")
apply_lags <- function(mydf, k) {
  lag(mydf, n = k)
}
lags <- seq(1:h)
label = glue::glue("{lags}") %>% 
  as.character()
lag_functions <- setNames(paste("apply_lags(., ", lags, ")"), nm = label)

frw1<-NULL
for (date in fcst_dates1) {
  vintage<-PRTDB(dftrm2,pdelaytrm1,vintage = date)
  vintage1<-as.data.frame(vintage)
  vintage1[NROW(vintage1)+50,]<-NA
  vintage1<-vintage1 %>% 
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions))
  X<-zoo(vintage1)
  X1<-cbind("TRM"=X[,1],X[ , colSums(is.na(X[(length(na.omit(X[,1]))+h),]))==0])
  frw1<-c(frw1,tail(na.omit(X1)[,"TRM"],1))
}
forecast::accuracy(frw1,dftrm2[326:374,1])

fvar<-NULL
for (date in fcst_dates1) {
  vintage<-PRTDB(dftrm2,delay=pdelaytrm1,vintage = date)
  modvar<-vars::VAR(na.omit(vintage[,c("TRM","OILP","TES 10","10TY","EMBICOL")]),type="const",lag.max = 12,ic="SC")
  fvar<-c(fvar,predict(modvar,n.ahead=h)$fcst[[1]][h])
}
forecast::accuracy(fvar,dftrm2[326:374,1])
autoplot(ts(dftrm2[326:374,1]))+autolayer(ts(fvar))

f1vardiff<-NULL
for (date in fcst_dates1) {
  vintage<-PRTDB(dftrm2,delay=pdelaytrm1,vintage = date)
  lagsdiff<-lags.select(na.omit(vintage[,c("TRM","OILP","TES 10","10TY","EMBICOL")]),include = "const",lag.max = 12)
  modelvardiff<-lineVar(na.omit(vintage[,c("TRM","OILP","TES 10","10TY","EMBICOL")]),lag=lagsdiff$BIC_min[2],include = "const",model = "VAR",I="diff")
  f1vardiff<-c(f1vardiff,as.numeric(predict(modelvardiff,n.ahead=h)[,"TRM"])[h])
}
forecast::accuracy(f1vardiff,dftrm2[326:374,1])
autoplot(ts(dftrm2[326:374,1]))+autolayer(ts(f1vardiff))

forvar<-NULL
for (i in 1:74){
  mod<-vars::VAR(na.omit(dftrm2[1:(276+i),c("TRM","OILP","TES 10","10TY","EMBICOL")]),lag.max=12,type = "const",ic="SC")
  forvar[[i]]<-predict(mod,n.ahead=h)$fcst[[1]][h]
}
forecast::accuracy(unlist(forvar),dftrm2[326:374,1])

evar<-dftrm2[326:374,1]-fvar[1:49]
efrw1<-frw1[1:49]-dftrm2[326:374,1]
evardiff<-dftrm2[326:374,1]-f1vardiff[1:49]

dm.test(evar,efrw1,h=h,varestimator = "bartlett")
dm.test(evardiff,efrw1,h=h,varestimator = "bartlett")

# GETS

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
  vintage<-PRTDB(dftrm2,pdelaytrm1,vintage = date)
  vintage1<-as.data.frame(vintage)
  vintage1[NROW(vintage1)+50,]<-NA
  vintage1<-vintage1 %>% 
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions))
  X<-zoo(vintage1)
  X1<-cbind("TRM"=X[,1],X[ , colSums(is.na(X[(length(na.omit(X[,1]))+h),]))==0])
  frw1<-c(frw1,tail(na.omit(X1)[,"TRM"],1))
}
forecast::accuracy(frw1,dftrm2[326:374,1])

fseas<-NULL
for (date in fcst_dates1){
  vintage<-PRTDB(dftrm3,delay = pdelaytrm1,vintage = date)
  NAs<-matrix(rep(NA),nrow = 50,ncol=ncol(vintage))
  vintage<-ts(rbind(vintage,NAs),start = start(vintage),frequency = 12)
  seas<-as.data.frame(seasonaldummy(vintage[,1]))
  vintage1<-as.data.frame(vintage)
  vintage1<-vintage1 %>%
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions))
  X<-zoo(cbind(vintage1,seas))
  X1<-cbind("TRM"=X[,1],X[ , colSums(is.na(X[(length(na.omit(X[,1]))+h),]))==0])
  X1<-X1[,grepl( "\\bTRM_" , names( as.data.frame(X1) ))==0]
  #X1<-X1[,c("TRM","TRM_13","TRM_14")]
  X1isat<-cbind(X1,iim(X1),sim(X1),tim(X1))
  length1<-max(sapply(X1[,-1], Position, f=Negate(is.na)))
  moarxil<-getsm(isat(X1[(length1-13):NROW(X1),1],uis= X1[,-which(names(X1) %in% c("TRM",colnames(seas)))],mxreg = X1[,colnames(seas)],t.pval = 0.0001,ar=13,sis = TRUE,print.searchinfo = FALSE,do.pet = TRUE,tis=TRUE,iis=TRUE),t.pval = 0.0001,keep = c(1),print.searchinfo = FALSE,ar.LjungB = NULL,arch.LjungB = NULL)   
  fseas<-c(fseas,predict(moarxil,newmxreg=X1isat[(length(na.omit(X1isat[,"TRM"]))+1):(length(na.omit(X1isat[,"TRM"]))+h),intersect(moarxil$aux$mXnames,colnames(X1isat))],n.ahead = h)[h])#+tail(as.numeric(moarxil$residuals),1)
}
forecast::accuracy(fseas,dftrm2[326:374,1])
autoplot(ts(dftrm2[326:374,1]))+autolayer(ts(fseas))

fic<-NULL
for (date in fcst_dates1){
  vintage<-PRTDB(dftrm2,delay = pdelaytrm1,vintage = date)
  NAs<-matrix(rep(NA),nrow = 50,ncol=ncol(vintage))
  vintage<-ts(rbind(vintage,NAs),start = start(vintage),frequency = 12)
  seas<-as.data.frame(seasonaldummy(vintage[,1]))
  vintage1<-as.data.frame(vintage)
  vintage1<-vintage1 %>%
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions))
  X<-zoo(cbind(vintage1,seas))
  X1<-cbind("TRM"=X[,1],X[ , colSums(is.na(X[(length(na.omit(X[,1]))+h),]))==0])
  X1<-X1[,grepl( "\\bTRM_" , names( as.data.frame(X1) ))==0]
  X1isat<-cbind(X1,iim(X1),sim(X1),tim(X1))
  length1<-max(sapply(X1[,-1], Position, f=Negate(is.na)))
  moarxil<-getsm(isat(X1[(length1-12):NROW(X1),1],uis= X1[,-which(names(X1) %in% c("TRM",colnames(seas)))],mxreg=X1[,colnames(seas)],ar=1:12,t.pval = 0.0001,sis = TRUE,print.searchinfo = FALSE,do.pet = TRUE,tis=TRUE,iis=TRUE),t.pval = 0.0001,keep=c(1),print.searchinfo = FALSE,ar.LjungB = NULL,arch.LjungB = NULL)   
  fic<-c(fic,predict(moarxil,newmxreg=X1isat[(length(na.omit(X1isat[,"TRM"]))+1):(length(na.omit(X1isat[,"TRM"]))+h),intersect(moarxil$aux$mXnames,colnames(X1isat))],n.ahead = h)[h]+tail(as.numeric(moarxil$residuals),1))#
}
forecast::accuracy(fic,dftrm2[326:374,1])
autoplot(ts(dftrm2[326:374,1]))+autolayer(ts(fic))

efseas<-fseas[1:49]-dftrm2[326:374,1]
efrw1<-frw1[1:49]-dftrm2[326:374,1]
efic<-fic[1:49]-dftrm2[326:374,1]

dm.test(efseas,efrw1,h=h,varestimator = "bartlett")
dm.test(efic,efrw1,h=h,varestimator = "bartlett")

# Machine Learning

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
  vintage<-PRTDB(dftrm2,pdelaytrm1,vintage = date)
  vintage1<-as.data.frame(vintage)
  vintage1[NROW(vintage1)+50,]<-NA
  vintage1<-vintage1 %>% 
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions))
  X<-zoo(vintage1)
  X1<-cbind("TRM"=X[,1],X[ , colSums(is.na(X[(length(na.omit(X[,1]))+h),]))==0])
  frw1<-c(frw1,tail(na.omit(X1)[,"TRM"],1))
}
forecast::accuracy(frw1,dftrm2[326:374,1])

max(floor((ncol(X1)-1)/3), 1)
ntree=1000
alphas1<-seq(0,1,length=100)
lambdas1 <- 10^seq(-3, 3, length = 100)
cost1<-10^seq(-4,4,length=10)
k1<-1:12
fml<-NULL
for (date in fcst_dates1) {
  vintage<-PRTDB(dftrm2,delay = pdelaytrm1,vintage = date)
  NAs<-matrix(rep(NA),nrow = 50,ncol=ncol(vintage))
  vintage<-ts(rbind(vintage,NAs),start = start(vintage),frequency = 12)
  seas<-as.data.frame(seasonaldummy(vintage[,1]))
  vintage1<-as.data.frame(vintage)
  vintage1<-vintage1 %>% 
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions))
  X<-zoo(cbind(vintage1,seas))
  X1<-cbind("TRM"=X[,1],X[ , colSums(is.na(X[(length(na.omit(X[,1]))+h),]))==0])
  #X1<-X1[,c("TRM","TRM_13","TRM_14")]
  myTimeControl <- trainControl(method = "timeslice",
                                initialWindow = (NROW(na.omit(X1))-h-(20-1)),
                                horizon = h,
                                fixedWindow = FALSE,savePredictions = TRUE)
  glmnet.mod <- caret::train(TRM ~. ,
                      data = na.omit(X1),
                      method="glmnet",
                      trControl = myTimeControl,
                      #preProc = c("center", "scale"),
                      metric='RMSE',tuneGrid=expand.grid(alpha=1,lambda=lambdas1))
  fml<-c(fml,as.numeric(predict(glmnet.mod,X1[(length(na.omit(X1[,"TRM"]))+1):(length(na.omit(X1[,"TRM"]))+h),-1]))[h])
}
forecast::accuracy(fml,dftrm2[326:374,1])
autoplot(ts(dftrm2[326:374,1]))+autolayer(ts(fml))

efrw1<-dftrm2[326:374,1]-frw1[1:49]
efml<-dftrm2[326:374,1]-fml[1:49]

dm.test(efml,efrw1,h=h,varestimator = "bartlett")

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
  vintage<-PRTDB(dftrm2,pdelaytrm1,vintage = date)
  vintage1<-as.data.frame(vintage)
  vintage1[NROW(vintage1)+50,]<-NA
  vintage1<-vintage1 %>% 
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions))
  X<-zoo(vintage1)
  X1<-cbind("TRM"=X[,1],X[ , colSums(is.na(X[(length(na.omit(X[,1]))+h),]))==0])
  frw1<-c(frw1,tail(na.omit(X1)[,"TRM"],1))
}
forecast::accuracy(frw1,dftrm2[326:374,1])

fdfm1 <- NULL
for(date in fcst_dates1){
  vintage<-PRTDB(dftrm2,delay = pdelaytrm1,vintage = date)
  NAs<-matrix(rep(NA),nrow = 50,ncol=ncol(vintage))
  vintage<-ts(rbind(vintage,NAs),start = start(vintage),frequency = 12)
  length1<-max(sapply(vintage[,-1], Position, f=Negate(is.na)))
  vintage1<-subset(ts(rbind(vintage,NAs),start = start(vintage),frequency = 12),start=length1)
  nowgdp<-nowcast(formula = TRM~., frequency = rep(12,NCOL(vintage1)),data = vintage1,r =ICfactors(vintage1[,-1])$r_star,q=ICshocks(vintage1[,-1],r=ICfactors(vintage1[,-1])$r_star,1)$q_star, p=1, method = "2s")
  fdfm1<-c(fdfm1,na.omit(nowgdp$yfcst[,3])[h])
}
forecast::accuracy(fdfm1,dftrm2[326:374,1])
autoplot(ts(dftrm2[326:374,1]))+autolayer(ts(fdfm))

efrw1<-frw1[1:49]-dftrm2[326:374,1]
efdfm<-fdfm[1:49]-dftrm2[326:374,1]

dm.test(efdfm,efrw1,h=h,varestimator = "bartlett")

# Subjective forecast comparison (EME)

erw<-dftrm2[326:374,1]-frw1[1:49]
esurvey<-dftrm3[326:374,1]-as.numeric(surveydata)
autoplot(ts(dftrm2[326:374,1]))+autolayer(ts(surveydata))+autolayer(ts(frw1))+autolayer(ts(fseas))

dm.test(efml,esurvey,h=1,varestimator = "bartlett")
forecast::accuracy(frw1,dftrm2[326:374,1])
forecast::accuracy(surveydata,dftrm2[326:374,1])

# Forecast Combinations

h<-1
train.comb <-24
data<-dftrm2
delay<-pdelaytrm1
# The initial date minus the training period as years 24=2015 for 25 step-ahead forecasts
observed1<-na.omit(as.numeric(data[(length(na.omit(PRTDB(data,delay,vintage ="2017-01-12")[,1]))+h):(length(na.omit(PRTDB(data,delay,vintage ="2017-01-12")[,1]))+h+train.comb-1),1]))
observed2<-na.omit(as.numeric(data[(length(na.omit(PRTDB(data,delay,vintage ="2017-01-12")[,1]))+h+train.comb):(length(na.omit(PRTDB(data,delay,vintage ="2023-02-12")[,1]))),1]))

comb_train<-cbind(observed1,
                  "LASSO"=flasso[1:length(observed1)],
                  "MEAN"=fmean1[1:length(observed1)],
                  "ETS"=fets[1:length(observed1)],
                  "ENET"=fenet[1:length(observed1)])

comb_test<-cbind(observed2,
                 "LASSO"=flasso[(train.comb+1):(length(observed2)+train.comb)],
                 "MEAN"=fmean1[(train.comb+1):(length(observed2)+train.comb)],
                 "ETS"=fets[(train.comb+1):(length(observed2)+train.comb)],
                 "ENET"=fenet[(train.comb+1):(length(observed2)+train.comb)])

inputdata1<-foreccomb(observed_vector=comb_train[,1],prediction_matrix=comb_train[,-1],newobs =comb_test[,1],newpreds =comb_test[,-1],na.impute = FALSE)
combm<-c("SA","BG","CLS","CSR","EIG1","EIG2","EIG3","EIG4","InvW","LAD","MED","NG","OLS","TA","WA")

for(i in combm) { 
  nam <- paste(i)
  assign(nam, eval(parse(text=paste( "comb_",i,"(inputdata1)" ,sep = ""))))
}

auto_combine(inputdata1)$Accuracy_Test
auto_combine(inputdata1)$Forecasts_Test

results1<-c(SA$Accuracy_Test["Test set","RMSE"],BG$Accuracy_Test["Test set","RMSE"],CLS$Accuracy_Test["Test set","RMSE"],
  CSR$Accuracy_Test["RMSE","aic"],EIG1$Accuracy_Test["Test set","RMSE"],EIG2$Accuracy_Test["Test set","RMSE"],
  EIG3$Accuracy_Test["Test set","RMSE"],EIG4$Accuracy_Test["Test set","RMSE"],InvW$Accuracy_Test["Test set","RMSE"],
  LAD$Accuracy_Test["Test set","RMSE"],MED$Accuracy_Test["Test set","RMSE"],NG$Accuracy_Test["Test set","RMSE"],OLS$Accuracy_Test["Test set","RMSE"],
  TA$Accuracy_Test["Test set","RMSE"],WA$Accuracy_Test["Test set","RMSE"],auto_combine(inputdata1)$Accuracy_Test["Test set","RMSE"])

NEW<-(1/4)*(comb_test[,2]+comb_test[,3]+comb_test[,4]+comb_test[,5])
forecast::accuracy(comb_test[,5],dftrm2[326:374,1])

efrw1<-dftrm2[326:374,1]-frw1[(train.comb+1):(length(observed2)+train.comb)]
e<-dftrm2[326:374,1]-WA$Forecasts_Test
dm.test(efrw1,e,h=h,varestimator="bartlett")

RESULTS1<-ts(cbind("Observed"=comb_test[,1],
                   "LASSO"=comb_test[,2],"MEAN"=comb_test[,3],
                   "ETS"=comb_test[,4],"ENet"=comb_test[,5],
                   "CLS"=CLS$Forecasts_Test,"EME"=surveydata),start=c(2019,01),frequency = 12)
autoplot(RESULTS1)+
  labs(y="%",x="Date",title = "Forecast Evaluation: Depreciation Rate (USD/COP)",subtitle = "Monthly Change Rate")+
  guides(colour = guide_legend(title = "Series"))

autoplot(ts(comb_test[,1]))+autolayer(ts(OLS$Forecasts_Test))

OLS$Weights[1]-arx(comb_train[,1],mxreg = comb_train[,-1])$coef[2]

h<-13
train.comb <-24
data<-dftrm2
delay<-pdelaytrm1
# The initial date minus the training period as years 24=2015 for 25 step-ahead forecasts
observed1<-na.omit(as.numeric(data[(length(na.omit(PRTDB(data,delay,vintage ="2016-01-12")[,1]))+h):(length(na.omit(PRTDB(data,delay,vintage ="2016-01-12")[,1]))+h+train.comb-1),1]))
observed2<-na.omit(as.numeric(data[(length(na.omit(PRTDB(data,delay,vintage ="2016-01-12")[,1]))+h+train.comb):(length(na.omit(PRTDB(data,delay,vintage ="2023-02-12")[,1]))),1]))

comb_train<-cbind(observed1,
                  fml[1:length(observed1)],
                  fdfm[1:length(observed1)],
                  fdfm1[1:length(observed1)],
                  fseas[1:length(observed1)]) # 2 means from the I(0) representation

comb_test<-cbind(observed2,
                 fml[(train.comb+1):(length(observed2)+train.comb)],
                 fdfm[(train.comb+1):(length(observed2)+train.comb)],
                 fdfm1[(train.comb+1):(length(observed2)+train.comb)],
                 fseas[(train.comb+1):(length(observed2)+train.comb)])

inputdata1<-foreccomb(observed_vector=comb_train[,1],prediction_matrix=comb_train[,-1],newobs =comb_test[,1],newpreds =comb_test[,-1],na.impute = FALSE)
combm<-c("SA","BG","CLS","CSR","EIG1","EIG2","EIG3","EIG4","InvW","LAD","MED","NG","OLS","TA","WA")

for(i in combm) { 
  nam <- paste(i)
  assign(nam, eval(parse(text=paste( "comb_",i,"(inputdata1)" ,sep = ""))))
}

auto_combine(inputdata1)$Accuracy_Test
auto_combine(inputdata1)$Forecasts_Test

results2<-c(SA$Accuracy_Test["Test set","RMSE"],BG$Accuracy_Test["Test set","RMSE"],CLS$Accuracy_Test["Test set","RMSE"],
            CSR$Accuracy_Test["RMSE","aic"],EIG1$Accuracy_Test["Test set","RMSE"],EIG2$Accuracy_Test["Test set","RMSE"],
            EIG3$Accuracy_Test["Test set","RMSE"],EIG4$Accuracy_Test["Test set","RMSE"],InvW$Accuracy_Test["Test set","RMSE"],
            LAD$Accuracy_Test["Test set","RMSE"],MED$Accuracy_Test["Test set","RMSE"],NG$Accuracy_Test["Test set","RMSE"],OLS$Accuracy_Test["Test set","RMSE"],
            TA$Accuracy_Test["Test set","RMSE"],WA$Accuracy_Test["Test set","RMSE"],auto_combine(inputdata1)$Accuracy_Test["Test set","RMSE"])

NEW<-(1/4)*(comb_test[,2]+comb_test[,3]+comb_test[,4]+comb_test[,5])
forecast::accuracy(comb_test[,5],dftrm2[326:374,1])

efrw1<-dftrm2[326:374,1]-frw1[(train.comb+1):(length(observed2)+train.comb)]
e<-dftrm2[326:374,1]-comb_test[,5]
dm.test(efrw1,e,h=h,varestimator="bartlett")

RESULTS2<-ts(cbind("Observed"=comb_test[,1],
                   "LASSO"=comb_test[,2],"DFM"=comb_test[,3],
                   "DFM1"=comb_test[,4],"GETSISAT"=comb_test[,5],
                   "EIG1"=EIG1$Forecasts_Test),start=c(2019,01),frequency = 12)
autoplot(RESULTS2)+
  labs(y="%",x="Date",title = "Forecast Evaluation: Depreciation Rate (USD/COP)",subtitle = "Monthly Change Rate")+
  guides(colour = guide_legend(title = "Series"))

h<-25
train.comb <-24
data<-dftrm2
delay<-pdelaytrm1
# The initial date minus the training period as years 24=2015 for 25 step-ahead forecasts
observed1<-na.omit(as.numeric(data[(length(na.omit(PRTDB(data,delay,vintage ="2015-01-12")[,1]))+h):(length(na.omit(PRTDB(data,delay,vintage ="2015-01-12")[,1]))+h+train.comb-1),1]))
observed2<-na.omit(as.numeric(data[(length(na.omit(PRTDB(data,delay,vintage ="2015-01-12")[,1]))+h+train.comb):(length(na.omit(PRTDB(data,delay,vintage ="2023-02-12")[,1]))),1]))

comb_train<-cbind(observed1,
                  fenet[1:length(observed1)],
                  fsvm[1:length(observed1)],
                  fridge[1:length(observed1)],
                  fvar[1:length(observed1)])

comb_test<-cbind(observed2,
                 fenet[(train.comb+1):(length(observed2)+train.comb)],
                 fsvm[(train.comb+1):(length(observed2)+train.comb)],
                 fridge[(train.comb+1):(length(observed2)+train.comb)],
                 fvar[(train.comb+1):(length(observed2)+train.comb)])

inputdata1<-foreccomb(observed_vector=comb_train[,1],prediction_matrix=comb_train[,-1],newobs =comb_test[,1],newpreds =comb_test[,-1],na.impute = FALSE)
combm<-c("SA","BG","CLS","CSR","EIG1","EIG2","EIG3","EIG4","InvW","LAD","MED","NG","OLS","TA","WA")

for(i in combm) { 
  nam <- paste(i)
  assign(nam, eval(parse(text=paste( "comb_",i,"(inputdata1)" ,sep = ""))))
}

auto_combine(inputdata1)$Accuracy_Test
auto_combine(inputdata1)$Forecasts_Test

results3<-c(SA$Accuracy_Test["Test set","RMSE"],BG$Accuracy_Test["Test set","RMSE"],CLS$Accuracy_Test["Test set","RMSE"],
            CSR$Accuracy_Test["RMSE","aic"],EIG1$Accuracy_Test["Test set","RMSE"],EIG2$Accuracy_Test["Test set","RMSE"],
            EIG3$Accuracy_Test["Test set","RMSE"],EIG4$Accuracy_Test["Test set","RMSE"],InvW$Accuracy_Test["Test set","RMSE"],
            LAD$Accuracy_Test["Test set","RMSE"],MED$Accuracy_Test["Test set","RMSE"],NG$Accuracy_Test["Test set","RMSE"],OLS$Accuracy_Test["Test set","RMSE"],
            TA$Accuracy_Test["Test set","RMSE"],WA$Accuracy_Test["Test set","RMSE"],auto_combine(inputdata1)$Accuracy_Test["Test set","RMSE"])

NEW<-(1/4)*(comb_test[,2]+comb_test[,3]+comb_test[,4]+comb_test[,5])
forecast::accuracy(comb_test[,4],dftrm2[326:374,1])

efrw1<-dftrm2[326:374,1]-frw1[(train.comb+1):(length(observed2)+train.comb)]
e<-dftrm2[326:374,1]-comb_test[,4]
dm.test(efrw1,e,h=h,varestimator="bartlett")

RESULTS3<-ts(cbind("Observed"=comb_test[,1],
                   "ENet"=comb_test[,2],"SVM"=comb_test[,3],
                   "RIDGE"=comb_test[,4],"VAR"=comb_test[,5],
                   "EIG1"=EIG1$Forecasts_Test),start=c(2019,01),frequency = 12)
autoplot(RESULTS3)+
  labs(y="%",x="Date",title = "Forecast Evaluation: Depreciation Rate (USD/COP)",subtitle = "Monthly Change Rate")+
  guides(colour = guide_legend(title = "Series"))

##### Sensitivity Analysis ####

h<-1
fcst_dates1 <- seq.Date(from = as.Date("2017-01-12"),to =as.Date("2020-02-12"),by ="months")
apply_lags <- function(mydf, k) {
  lag(mydf, n = k)
}
lags <- seq(1:1)
label = glue::glue("{lags}") %>% 
  as.character()
lag_functions <- setNames(paste("apply_lags(., ", lags, ")"), nm = label)

frw1<-NULL
for (date in fcst_dates1) {
  vintage<-PRTDB(dftrm2,pdelaytrm1,vintage = date)
  vintage1<-as.data.frame(vintage)
  vintage1[NROW(vintage1)+50,]<-NA
  vintage1<-vintage1 %>% 
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions))
  X<-zoo(vintage1)
  X1<-cbind("TRM"=X[,1],X[ , colSums(is.na(X[(length(na.omit(X[,1]))+h),]))==0])
  frw1<-c(frw1,tail(na.omit(X1)[,"TRM"],1))
}
forecast::accuracy(frw1,dftrm2[302:338,1])
autoplot(ts(dftrm2[302:338,1]))+autolayer(ts(frw1))

fmean1<-NULL
for (date in fcst_dates1) {
  vintage<-PRTDB(dftrm2,pdelaytrm1,vintage = date)
  vintage1<-as.data.frame(vintage)
  vintage1[NROW(vintage1)+50,]<-NA
  vintage1<-vintage1 %>% 
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions))
  #X<-zoo(cbind(vintage1,"trend"=rep(1:NROW(vintage1))))
  X<-zoo(vintage1)
  X1<-cbind("TRM"=X[,1],X[ , colSums(is.na(X[(length(na.omit(X[,1]))+h),]))==0])
  fmean1<-c(fmean1,mean(na.omit(X1)[,"TRM"]))
}
forecast::accuracy(fmean1,dftrm2[302:338,1])
autoplot(ts(dftrm2[302:338,1]))+autolayer(ts(fmean1))

efrw1<-dftrm2[302:338,1]-frw1[1:37]
efmean1<-dftrm2[302:338,1]-fmean1[1:37]

dm.test(efmean1,efrw1,h=h,varestimator = "bartlett")

max(floor((ncol(X1)-1)/3), 1)
ntree=1000
alphas1<-seq(0,1,length=100)
lambdas1 <- 10^seq(-3, 3, length = 100)
cost1<-10^seq(-4,4,length=10)
k1<-1:12
fml<-NULL
for (date in fcst_dates1) {
  vintage<-PRTDB(dftrm2,delay = pdelaytrm1,vintage = date)
  NAs<-matrix(rep(NA),nrow = 50,ncol=ncol(vintage))
  vintage<-ts(rbind(vintage,NAs),start = start(vintage),frequency = 12)
  seas<-as.data.frame(seasonaldummy(vintage[,1]))
  vintage1<-as.data.frame(vintage)
  vintage1<-vintage1 %>% 
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions))
  X<-zoo(cbind(vintage1,seas))
  X1<-cbind("TRM"=X[,1],X[ , colSums(is.na(X[(length(na.omit(X[,1]))+h),]))==0])
  #X1<-X1[,c("TRM","TRM_13","TRM_14")]
  myTimeControl <- trainControl(method = "timeslice",
                                initialWindow = (NROW(na.omit(X1))-h-(20-1)),
                                horizon = h,
                                fixedWindow = FALSE,savePredictions = TRUE)
  glmnet.mod <- caret::train(TRM ~. ,
                             data = na.omit(X1),
                             method="glmnet",
                             trControl = myTimeControl,
                             #preProc = c("center", "scale"),
                             metric='RMSE',tuneGrid=expand.grid(alpha=1,lambda=lambdas1))
  fml<-c(fml,as.numeric(predict(glmnet.mod,X1[(length(na.omit(X1[,"TRM"]))+1):(length(na.omit(X1[,"TRM"]))+h),-1]))[h])
}
forecast::accuracy(fml,dftrm2[302:338,1])
autoplot(ts(dftrm2[302:338,1]))+autolayer(ts(fml))

efrw1<-dftrm2[302:338,1]-frw1[1:37]
efml<-dftrm2[302:338,1]-fml[1:37]

dm.test(efml,efrw1,h=h,varestimator = "bartlett")

# For 1-step ahead comparision with EMEE

surveydata<-ts(read_excel("~/Documents/Graduate Thesis/Data and Code/DATABASE1.xlsx", sheet = "EX2017_2020"),frequency = 12,start = c(2017,01))[,3]
forecast::accuracy(surveydata,as.numeric(dftrm2[302:338,1]))
autoplot(ts(dftrm2[302:338,1]))+autolayer(ts(surveydata))

efrw1<-frw1[1:37]-dftrm2[302:338,1]
esurveydata<-surveydata[1:37]-dftrm2[302:338,1]

dm.test(esurveydata,efrw1,h=h,varestimator = "bartlett")

#### Simple comparison using the 12th of every month ####

surveydata1<-ts(read_excel("~/Documents/Graduate Thesis/Data and Code/DATABASE1.xlsx", sheet = "SURVEYRES (TRM)"),frequency = 12,start = c(2019,01))[,3]
surveydata2<-ts(read_excel("~/Documents/Graduate Thesis/Data and Code/DATABASE1.xlsx", sheet = "EX2017_2020")[,3],frequency = 12,start = c(2017,01))

# Import packages
for (pkg in c("fpp2","fpp3","readxl","gets","nowcasting","dfms","vars","midasr","xts","plotly","tsDyn","caret",
              "GGally","ForecastComb","TTR")){
  library(pkg, character.only = TRUE)
}

datestrm<-as.Date(as.data.frame(read_excel("~/Documents/Graduate Thesis/Data and Code/DATABASE1.xlsx",sheet = "TRMNOW"))[,1])

TRM<-xts(as.data.frame(read_excel("~/Documents/Graduate Thesis/Data and Code/DATABASE1.xlsx",sheet = "TRMNOW"))[,2],order.by = datestrm) 
TRMmonthly<-to.monthly(TRM,OHLC=FALSE,indexAt = "lastof")

fcst_dates <- seq.Date(from = as.Date("2017-01-12"),to = as.Date("2020-01-12"),
                       by = "months")
delay<-c(0)

frwnowus1<-NULL
frwnowus2<-NULL

for (date in fcst_dates){
  
  data1<-cbind(TRM)
  
  for (i in 1:NCOL(data1)){
    data1[,i]<-ifelse(time(data1[,i]) <= as.Date(date)-delay[i],data1[,i], NA)
  }
  
  TRM1day12<-ROC(to.monthly(data1[,1],OHLC=FALSE,indexAt = "lastof"),n=1,type = "discrete",na.pad = FALSE)*100
  TRMmonthly1<-ROC(TRMmonthly[time(TRMmonthly)<=as.Date(date)],n=1,type = "discrete",na.pad = FALSE)*100
  
  nowusgdp<-cbind(TRMmonthly1,TRM1day12)
  colnames(nowusgdp)<-c("TRM1","TRMday")
  nowusgdp<-nowusgdp[time(nowusgdp)>=(floor_date(head(time(na.omit(nowusgdp[,-1])),1),unit = "months")+months(1)-days(1)),]
  Data1<-nowusgdp
  
  print(tail(Data1))
  
  Data1<-Data1[time(Data1)<=(floor_date(tail(time(na.omit(Data1[,1])),1),unit = "months")+months(2)-days(1))]  
  
  # Univariate
  
  Data1ts<-ts(Data1,frequency = 12,start = zoo::as.yearmon(start(Data1)))
  
  frwnowus1<-c(frwnowus1,tail(na.omit(Data1[,2]),1))
  frwnowus2<-c(frwnowus2, forecast::rwf(na.omit(Data1ts[,1]),h=1)$mean)
  
}

forecast::accuracy(frwnowus1,dftrm2[302:338,1])
forecast::accuracy(frwnowus2,dftrm2[302:338,1])
forecast::accuracy(surveydata2,dftrm2[302:338,1])

autoplot(ts(dftrm2[302:338,1]))+
  autolayer(ts(frwnowus1))+
  autolayer(ts(frwnowus2))+
  autolayer(ts(surveydata2))

efrwnowus1<-frwnowus1[1:37]-dftrm2[302:338,1]
esurveydata2<-surveydata2[1:37]-dftrm2[302:338,1]

dm.test(esurveydata2,efrwnowus1,h=1,varestimator = "bartlett")


































































