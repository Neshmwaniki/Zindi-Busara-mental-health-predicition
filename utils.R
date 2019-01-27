
# ##### COnnect to Soar package for stored files
# fstoreconnect=function(subdir){
#   ######## Define subdirectory to store R objects (subdir) 
#   oldLC <- Sys.getenv("R_LOCAL_CACHE", unset = ".R_Cache")
#   Sys.setenv(R_LOCAL_CACHE=subdir) }
# 
# 
# fstoreconnect("rstore")
# tmp=Objects()


############
group_age <- function(age){
  if (age >= 0 & age <= 25){
    return('students')
  }else if(age > 25 & age <= 40){
    return('strong working class')
  }else if (age > 40 & age <= 55){
    return('weak working class')
  }else if (age > 55){
    return('grand parents')
  }
}

group_size <- function(x){
  if (x > 0 & x <= 3){
    return('small family')
  }else if(x > 3 & x <= 6){
    return('average family')
  }else if (x > 6 & x <= 9){
    return('large family')
  }else{
    return('exlarge family')
  }
}

group_edu <- function(x){
  if (x > 0 & x <= 4){
    return('bin1')
  }else if(x > 4 & x <= 8){
    return('bin2')
  }else if (x > 8 & x <= 12){
    return('bin3')
  }else{
    return('bin4')
  }
}

group_ex = function(x){
  if(is.na(x)){
   return("None")
  }else if(x >= 0 & x <= 20){
    return('bin2')
  }else if (x > 20 & x <= 44){
    return('bin3')
  }else{
    return('bin4')
  }
}

################### Interaction functions
my.f2cnt<-function(th2, vn1, vn2, filter=TRUE) {
  df <- data.frame(f1=th2[,vn1], f2=th2[,vn2], filter=filter)
  sum1<- sqldf::sqldf("select f1, f2, count(*) as cnt from df where filter=1 group by 1,2")
  tmp<- sqldf::sqldf("select b.cnt from df a left join sum1 b on a.f1=b.f1 and a.f2=b.f2")
  tmp$cnt[is.na(tmp$cnt)]<-0
  return(tmp$cnt)
}


factorToNumeric <- function(train, test, response, variables, metrics){
  require(qdapTools, quietly = T)
  temp <- data.frame(c(rep(0,nrow(test))), row.names = NULL)
  
  for (variable in variables){
    for (metric in metrics) {
      x <- tapply(train[, response], train[,variable], metric)
      x <- data.frame(row.names(x),x, row.names = NULL)
      temp <- data.frame(temp,round(lookup(test[,variable], x),2))
      colnames(temp)[ncol(temp)] <- paste(metric,variable, sep = "_")
    }
  }
  return (temp[,-1])
}

# Encode a character or factor with its hexavegisimal value
hexv.encode = function(x, xnew = x) {
  lvls = as.character(unique(x))
  lvls = lvls[order(nchar(lvls), tolower(lvls))]
  return (as.integer(factor(xnew, levels = lvls)))
}

# Encode a feature value with its frequency in the entire dataset
freq.encode = function(x, xnew = x) {
  if (is.factor(x) || is.character(x)) {
    return (as.numeric(factor(xnew, levels = names(sort(table(x))))))
  } else {
    return (approxfun(density(x[!is.na(x)], n = length(x) / 100))(xnew))
  }
}


####
SimpleEnsemble = function(pred1,pred2,weight = 0.5){
  pred = weight*pred1 + (1 -weight) *pred2
  return(pred)
}
#########

