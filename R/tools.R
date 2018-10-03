Precifunc<-function(keyword,sizekey,text) {
  t<-0
  m<-0
  m<-sum(gregexpr(paste0(" ",tolower(keyword)," ") ,text, fixed=TRUE)[[1]] > 0)
  if (m>=5)(m<-5)
  if(m>0) t<-100/sizekey
  if(m>1) t<-t+(m*(100/(sizekey*5)))
  t<-t/2
  return(t)
}

isTarget<-function(Data) {
  x<-0;
  for (i in Data) {
    if(length(i)>0){
      if (!is.vector(i)){
        if(i=="NA"){x<-bitwOr(0,x)} else {x<-bitwOr(x,1)}
      } else {
        x<-bitwOr(x,1)
      }
    } else {
      x<-bitwAnd(0,x)
    }
  }
  if(x==0) return(FALSE)
  else return(TRUE)
}
RemoveTags<-function(content){
content<-gsub("<script\\b[^<]*>[^<]*(?:<(?!/script>)[^<]*)*</script>", "", content, perl=T)
content<-gsub("<.*?>", "", content)
content<-gsub(pattern = "\n" ," ", content)
content<-gsub("^\\s+|\\s+$", "", content)
return(content)
}
Listlength<-function(List){
 len<-0
 len<-sum(sapply(List, length))
  return(len)
}
TransposeList<-function(DATA){
TDATA<-list()
  for(i in 1:length(DATA[[1]])){
    line<-vector()
    CountNA<-0
    for (j in 1:length(DATA)){
      if(DATA[[j]][i]=="NA") CountNA<-CountNA+1
      line<-c(line,DATA[[j]][i])
    }
    if(CountNA!=4){
    TDATA<-c(TDATA,list(line))
    }
  }
return(TDATA)
}

ItemizeNode <- function(path){
  pathsplit<- unlist(strsplit(path,"/",fixed = TRUE))
  vecpaths<-vector()
  for(i in 1:length(pathsplit)){
    newpath<-""
    for(j in 1:i){
      newpath<-paste0(newpath,"/",pathsplit[j])
    }
    newpath<-substr(newpath, 2, nchar(newpath))
    vecpaths<-c(vecpaths,newpath)
  }
  return(vecpaths)
}

ReplaceX <- function(xx, real){
  xxsplit<- unlist(strsplit(xx,"/",fixed = TRUE))
  realsplit<- unlist(strsplit(real,"/",fixed = TRUE))
  for(i in 1:length(realsplit)){
    xxsplit[i]<-realsplit[i]
  }
  newpath=""
  for(j in 1:length(xxsplit)){
    newpath<- paste0(newpath,"/",xxsplit[j])
  }
  newpath<-substr(newpath, 2, nchar(newpath))
  return(newpath)
}
