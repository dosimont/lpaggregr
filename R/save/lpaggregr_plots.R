library(ggplot2)
library(RColorBrewer)
library(treemap)
library(gplots)

hspace=0.1

string2colorRandom<- function(string){
  digested=digest(as.character(string), serialize=FALSE)
  r=substr(digested,1,10)
  r=digest(as.character(r), serialize=FALSE)
  g=substr(digested,11,20)
  g=digest(as.character(g), serialize=FALSE)
  b=substr(digested,21,30)
  b=digest(as.character(b), serialize=FALSE)
  r=substr(r,1,2)
  g=substr(g,1,2)
  b=substr(b,1,2)
  h<-paste(r,g,b,sep="")
  if ((r>230&g>230&b>230)|(r<30&g<30&b<30)){
    h = string2colorRandom(paste(string,":-o",string,sep=""))
  }
  h
}

color_generator <- function(stringlist, aggString=c("")){
  sorted<-sort(stringlist)
  hashcoded<-rep(0, length(stringlist))
  for (i in 1:length(sorted)){
    if (sorted[i]==aggString){
      hashcoded[i]=0
    }
    else{
      hashcoded[i]=string2colorRandom(sorted[i])
    }
  }
  hexed<-format(as.hexmode(hashcoded),width=6)
  color=paste("#",hexed,sep="")
  names(color)=sorted
  color
}

qualplot <- function(results){
  qualities<-results$Qualities
  popt<-results$POpt
  opt<-qualities[(qualities$Parameter %in% popt),]
  xlabel<- "Information Loss"
  ylabel<- "Complexity Reduction"
  plot<-ggplot()
  plot<-plot + geom_line(data=qualities,aes(x=Loss,y=Gain), color="black")
  plot<-plot + geom_point(data=qualities,aes(x=Loss,y=Gain), color="black")
  plot<-plot + geom_point(data=opt,aes(x=Loss,y=Gain), color="red")
  plot<-plot + theme_bw()
  plot<-plot + labs(x=xlabel,y=ylabel)
  plot
}

oplot_stacked_state <-function(agg, FUN=color_generator){
  agg <- aggregate(value ~ Type+Start+End, data = agg, FUN = mean)
  agg$Duration<-agg$End-agg$Start+1
  vcolors=FUN(unique(agg$Type))
  p<-ggplot(agg, aes(x=Start+((End-Start)/2)+0.5-(hspace/2), y=value, width=Duration-hspace, fill=Type))
  p<-p + scale_fill_manual(values = vcolors, breaks = names(vcolors), labels = names(vcolors))
  p<-p + geom_bar(stat="identity")
  p<-p + theme_bw()
  p<-p + labs(x="Time slices",y="Normalized value")
  p
}

hplot_treemap_state <-function(agg, FUN=color_generator){
  agg <- aggregate(value ~ Space+Type+Parent, data = agg, FUN = mean)
  vcolors=FUN(unique(agg$Type))
  agg$Color=vcolors[agg$Type]
  treemap(agg, index=c("Parent", "Space", "Type"), vSize="value", vColor="Color", type="color", algorithm="squarified", border.col="white", bg.labels="grey", title="")
}

hplot_treemap_perfcounter <-function(agg){
  agg <- aggregate(value ~ Space+Type+Size+Parent, data = agg, FUN = mean)
  agg$value=agg$value/agg$Size
  treemap(agg, index=c("Parent", "Space", "Type"), vSize="Size", vColor="value", type="manual", palette="RdYlBu", algorithm="squarified", border.col="white", bg.labels="grey", title="")
}
