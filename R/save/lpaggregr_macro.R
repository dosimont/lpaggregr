library(sqldf)

getpath<-function(vhierarchy, leavesize){
  path <- rep(-1,length(vhierarchy))
  for (h in 1:leavesize){
    path[h]=h
  }
  i=leavesize+1;
  h=1;
  while(i<=length(vhierarchy)){
    if (!(vhierarchy[path[h]] %in% path)){
      path[i]=vhierarchy[path[h]]
      i=i+1
    }
    h=h+1
  }
  path
}

omacro <- function(df, micro, p){
  df <- df[df$Parameter %in% p,]
  dfdata <- melt(micro$data)
  dfdata$Start=-1
  dfdata$End=-1
  for (i in 1:nrow(df)){
    dfdata[dfdata$Time>=df[i,"Start"]&dfdata$Time<=df[i,"End"],"Start"]=df[i,"Start"]
    dfdata[dfdata$Time>=df[i,"Start"]&dfdata$Time<=df[i,"End"],"End"]=df[i,"End"]
  }
  agg <- aggregate(value ~ Space+Type+Start+End, data = dfdata, FUN = mean)
  agg
}

hmacro <- function(df, micro, p){
  df <- df[df$Parameter %in% p,]
  dfdata <- melt(micro$data)
  vhierarchy <- micro$hierarchy
  leavesize= length(unique(dfdata$Space))
  for (r in 1:nrow(df)) {
    df[r,"Space"]<-names(vhierarchy)[df[r,"Node"]]
  }
  dfdata$Parent="0"
  path=getpath(vhierarchy,leavesize)
  for (h in 1:(length(path)-1)){
    dfdata[dfdata$Space %in% names(vhierarchy)[path[h]],"Parent"]<-names(vhierarchy)[vhierarchy[path[h]]]
    dfdata2<-dfdata[dfdata$Space %in% names(vhierarchy)[path[h]],]
    dfdata2$Space<-names(vhierarchy)[vhierarchy[path[h]]]
    dfdata=rbind(dfdata,dfdata2)
  }
  agg <- dfdata[dfdata$Space %in% df$Space,]
  agg <- aggregate(value ~ Space+Type+Time+Parent, data = agg, FUN = sum)
  agg <- sqldf('SELECT agg.Space, agg.Type, agg.Time, agg.Parent, agg.value, df.Size
             FROM agg
             INNER JOIN df
             ON (agg.Space == df.Space)')
}
