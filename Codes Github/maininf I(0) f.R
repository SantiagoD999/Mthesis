##################### Main Inflation 

### 1. Import Packages 

pacman::p_load(fpp2,readxl,fpp3,gets,ForecastComb,glmnet,GGally,vars,tsDyn,nowcasting,
               HDeconometrics,caret,caretForecast,TTR)

### 2. Import Data

dfinf1<-ts(read_excel("~/Documents/Graduate Thesis/Data and Code/DATABASE1.xlsx", sheet = "INFDATAB"),frequency = 12,start = c(1954,07))[,-1]
transinf1<-na.omit(read_excel("~/Documents/Graduate Thesis/Data and Code/DATABASE1.xlsx", sheet = "List of Variables")[,5]$`Initial Transformation`)
pdelayinf1<-na.omit(read_excel("~/Documents/Graduate Thesis/Data and Code/DATABASE1.xlsx", sheet = "List of Variables")[,4]$`Publication delay (approximated days)`)
surveydata<-ts(read_excel("~/Documents/Graduate Thesis/Data and Code/DATABASE1.xlsx", sheet = "SURVEYRES (M)"),frequency = 12,start = c(2019,01))[,2]

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
    if (trans[j] == 1) {
      temp <- diff(base[, j])
      base1[-1, j] <- temp
    }
    else if (trans[j] == 2) {
      temp <- diff(diff(base[, j]))
      base1[-(1:2), j] <- temp
    }
    else if (trans[j] == 3) {
      temp <- (diff(base[, j])/stats::lag(base[, j], -1))*100
      base1[-1, j] <- temp
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

newtrans<-c(unlist(transinf3))

dfinf3<-subset(BASE3(dfinf2,trans = newtrans),start=2)

DFINF4TS<-as_tsibble(dfinf3,pivot_longer = FALSE)
DFINF2TS<-as_tsibble(dfinf2,pivot_longer = FALSE)

### 4. Model estimation and Evaluation

# GETS

h<-1
fcst_dates1 <- seq.Date(from = as.Date("2019-01-12"),to =as.Date("2023-02-12"),by ="months")
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

fseas<-NULL
for (date in fcst_dates1[49]){
  vintageI0<-PRTDB(dfinf2,delay = pdelayinf1,vintage = date)
  vintage<-PRTDB(dfinf3,delay = pdelayinf1,vintage = date)
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
  moarxil<-getsm(isat(X1[(length1-12):NROW(X1),1],uis=X1[,-which(names(X1) %in% c("INF",colnames(seas)))],mxreg = X1[,colnames(seas)],t.pval =0.0001,ar=1:12,sis = TRUE,print.searchinfo = FALSE,do.pet = TRUE,tis=TRUE,iis=TRUE),t.pval = 0.0001,print.searchinfo = FALSE,keep=c(1,14:24),ar.LjungB = NULL,arch.LjungB = NULL)     
  fseas<-c(fseas,sum(predict(moarxil,newmxreg=X1isat[(length(na.omit(X1isat[,"INF"]))+1):(length(na.omit(X1isat[,"INF"]))+h),intersect(moarxil$aux$mXnames,colnames(X1isat))],n.ahead = h)[1:h])+as.numeric(tail(na.omit(vintageI0[,1]),1)))#
}
forecast::accuracy(fseas,as.numeric(dfinf2[774:822,1]))
autoplot(ts(dfinf2[774:822,1]))+autolayer(ts(fseas))

efrw1<-frw1[1:49]-dfinf2[774:822,1]
efseas<-fseas[1:49]-dfinf2[774:822,1]

dm.test(efseas,efrw1,h=h,varestimator = "bartlett")

# Machine Learning

h<-1
fcst_dates1 <- seq.Date(from = as.Date("2019-01-12"),to =as.Date("2023-02-12"),by ="months")
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

max(floor((ncol(X1)-1)/3), 1)
ntree=1000
alphas1<-seq(0,1,length=100)
lambdas1 <- 10^seq(-3, 3, length = 100)
cost1<-10^seq(-4,4,length=10)
k1=seq(1:12)
fml<-NULL
for (date in fcst_dates1) {
  vintageI0<-PRTDB(dfinf2,delay = pdelayinf1,vintage = date)
  vintage<-PRTDB(dfinf3,delay = pdelayinf1,vintage = date)
  NAs<-matrix(rep(NA),nrow = 50,ncol=ncol(vintage))
  vintage<-ts(rbind(vintage,NAs),start = start(vintage),frequency = 12)
  seas<-as.data.frame(seasonaldummy(vintage[,1]))
  vintage1<-as.data.frame(vintage)
  vintage1<-vintage1 %>% 
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions))
  X<-zoo(cbind(vintage1,seas))
  X1<-cbind("INF"=X[,1],X[ , colSums(is.na(X[(length(na.omit(X[,1]))+h),]))==0])
  #X1<-X1[,c("INF","INF_1","INFIPP_1","M3_1","DTF90_1","EMBICOL_1","FOODINF_1","CRERTOT_1")]
  myTimeControl <- trainControl(method = "timeslice",
                                initialWindow = (NROW(na.omit(X1))-h-(20-1)),
                                horizon = h,
                                fixedWindow = FALSE)
  glmnet.mod <- train(INF ~. ,
                      data = na.omit(X1),
                      method="glmnet",
                      trControl = myTimeControl,
                      metric='RMSE',tuneGrid=expand.grid(alpha=0,lambda=lambdas1))
  
  fml<-c(fml,sum(predict(glmnet.mod,X1[(length(na.omit(X1[,"INF"]))+1):(length(na.omit(X1[,"INF"]))+h),-1])[1:h])+as.numeric(tail(na.omit(vintageI0[,1]),1)))
}
forecast::accuracy(fml,as.numeric(dfinf2[774:822,1]))
autoplot(ts(dfinf2[774:822,1]))+autolayer(ts(fml))

efrw1<-frw1[1:49]-dfinf2[774:822,1]
efml<-fml[1:49]-dfinf2[774:822,1]

dm.test(efrw1,efml,h=h,varestimator = "bartlett")

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
  vintageI0<-PRTDB(dfinf2,delay = pdelayinf1,vintage = date)
  vintage<-PRTDB(dfinf3,delay = pdelayinf1,vintage = date)
  NAs<-matrix(rep(NA),nrow = 50,ncol=ncol(vintage))
  vintage<-ts(rbind(vintage,NAs),start = start(vintage),frequency = 12)
  length1<-max(sapply(vintage[,-1], Position, f=Negate(is.na)))
  vintage1<-subset(ts(rbind(vintage,NAs),start = start(vintage),frequency = 12),start=length1)
  nowgdp<-nowcast(formula = INF~., frequency = rep(12,NCOL(vintage1)),data = vintage1,r =ICfactors(vintage1[,-1])$r_star,q=ICshocks(vintage1[,-1],r=ICfactors(vintage1[,-1])$r_star,1)$q_star, p=1, method = "2s")
  fdfm<-c(fdfm,sum(na.omit(nowgdp$yfcst[,3])[1:h])+tail(na.omit(vintageI0[,1]),1))
}
forecast::accuracy(fdfm,dfinf2[774:822,1])
autoplot(ts(dfinf2[774:822,1]))+autolayer(ts(fdfm))

efrw1<-frw1[1:49]-dfinf2[774:822,1]
efdfm<-fdfm[1:49]-dfinf2[774:822,1]

dm.test(efdfm,efrw1,h=h,varestimator = "bartlett")

##### Sensitivity Analysis ######

h<-1
fcst_dates1 <- seq.Date(from = as.Date("2017-01-12"),to =as.Date("2020-02-12"),by ="months")
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
forecast::accuracy(frw1,as.numeric(dfinf2[750:786,1]))
autoplot(ts(dfinf2[750:786,1]))+autolayer(ts(frw1))

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
forecast::accuracy(farima,as.numeric(dfinf2[750:786,1]))
autoplot(ts(dfinf2[750:786,1]))+autolayer(ts(farima))

efrw1<-frw1[1:37]-dfinf2[750:786,1]
earima<-farima[1:37]-dfinf2[750:786,1]

dm.test(earima,efrw1,h=h,varestimator = "bartlett")

max(floor((ncol(X1)-1)/3), 1)
ntree=1000
alphas1<-seq(0,1,length=100)
lambdas1 <- 10^seq(-3, 3, length = 100)
cost1<-10^seq(-4,4,length=10)
k1=seq(1:12)
fml<-NULL
for (date in fcst_dates1) {
  vintageI0<-PRTDB(dfinf2,delay = pdelayinf1,vintage = date)
  vintage<-PRTDB(dfinf3,delay = pdelayinf1,vintage = date)
  NAs<-matrix(rep(NA),nrow = 50,ncol=ncol(vintage))
  vintage<-ts(rbind(vintage,NAs),start = start(vintage),frequency = 12)
  seas<-as.data.frame(seasonaldummy(vintage[,1]))
  vintage1<-as.data.frame(vintage)
  vintage1<-vintage1 %>% 
    mutate_at(vars(colnames(vintage1)), funs_(lag_functions))
  X<-zoo(cbind(vintage1,seas))
  X1<-cbind("INF"=X[,1],X[ , colSums(is.na(X[(length(na.omit(X[,1]))+h),]))==0])
  #X1<-X1[,c("INF","INF_1","INFIPP_1","M3_1","DTF90_1","EMBICOL_1","FOODINF_1","CRERTOT_1")]
  myTimeControl <- trainControl(method = "timeslice",
                                initialWindow = (NROW(na.omit(X1))-h-(20-1)),
                                horizon = h,
                                fixedWindow = FALSE)
  glmnet.mod <- train(INF ~. ,
                      data = na.omit(X1),
                      method="knn",
                      trControl = myTimeControl,
                      metric='RMSE',tuneGrid=expand.grid(k=k1),preProcess=c("scale","center"))
  
  fml<-c(fml,sum(predict(glmnet.mod,X1[(length(na.omit(X1[,"INF"]))+1):(length(na.omit(X1[,"INF"]))+h),-1])[1:h])+as.numeric(tail(na.omit(vintageI0[,1]),1)))
}
forecast::accuracy(fml,as.numeric(dfinf2[750:786,1]))
autoplot(ts(dfinf2[750:786,1]))+autolayer(ts(fml))

efrw1<-frw1[1:37]-dfinf2[750:786,1]
efml<-fml[1:37]-dfinf2[750:786,1]

dm.test(efrw1,efml,h=h,varestimator = "bartlett")

# For 1-step ahead comparision with EMEE

surveydata<-ts(read_excel("~/Documents/Graduate Thesis/Data and Code/DATABASE1.xlsx", sheet = "EX2017_2020"),frequency = 12,start = c(2017,01))[,2]
forecast::accuracy(surveydata,as.numeric(dfinf2[750:786,1]))
autoplot(ts(dfinf2[750:786,1]))+autolayer(ts(surveydata))

efrw1<-frw1[1:37]-dfinf2[750:786,1]
esurveydata<-surveydata[1:37]-dfinf2[750:786,1]

dm.test(esurveydata,efrw1,h=h,varestimator = "bartlett")

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
forecast::accuracy(fets,as.numeric(dfinf2[750:786,1]))
autoplot(ts(dfinf2[750:786,1]))+autolayer(ts(fets))

efrw1<-frw1[1:37]-dfinf2[750:786,1]
efets<-fets[1:37]-dfinf2[750:786,1]

dm.test(efets,efrw1,h=h,varestimator = "bartlett")

















































































