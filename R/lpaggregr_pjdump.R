library(sqldf)
library(dplyr)
library(reshape2)
library(digest)

slicerstate <- function (trace, timeSliceNumber)
{
  trace$Type<-trace$Value
  start <- min(trace$Start)
  trace$Start <- trace$Start - start
  trace$End <- trace$End - start
  maxts <- max(trace$End)
  slicets = maxts/timeSliceNumber;
  slices <- data.frame(SliceId=1:timeSliceNumber, TsStart=(0:(timeSliceNumber-1))*slicets, TsEnd=(1:timeSliceNumber)*slicets);
  h <- sqldf('SELECT trace.ResourceId, trace.Start, trace.End, trace.Duration, trace.Type, slices.SliceId, slices.TsStart, slices.TsEnd
             FROM trace
             INNER JOIN slices
             ON (trace.Start+trace.Duration > slices.TsStart) AND (trace.End-trace.Duration < slices.TsEnd)')
  h$Duration <- NULL;
  m <- h %>% group_by(ResourceId, Start, End, SliceId, Type) %>%
    mutate(N=n(), TinTS = (min(End,TsEnd) - max(Start,TsStart))) %>%
    group_by(ResourceId, SliceId, Type, TsStart, TsEnd) %>%
    summarize (Sum=sum(TinTS), Normalized=Sum/maxts) %>%
    as.data.frame();
  p <- expand.grid(ResourceId=unique(m$ResourceId), SliceId = 1:max(m$SliceId), Type = unique(m$Type));
  p$TsStart = 0;
  p$TsEnd = 0;
  p$Sum = 0;
  p$Normalized = 0;
  n <- rbind(p, m);
  o <- n %>% group_by (ResourceId, SliceId, Type) %>%
    summarize(TsStart = max(TsStart), TsEnd = max(TsEnd), Sum = max(Sum), Normalized=max(Normalized)) %>% as.data.frame;
  return (o);
}


slicerprvcounter <- function (trace, timeSliceNumber)
{
  start <- min(trace$Start)
  trace$Start <- trace$Start - start
  trace$End <- trace$End - start
  maxts <- max(trace$End)
  trace <- trace %>% group_by(ResourceId, Type) %>% mutate(Value=lead(Value)) %>% na.omit()
  print(trace)
  trace$Value<-trace$Value/trace$Duration
  slicets = maxts/timeSliceNumber;
  slices <- data.frame(SliceId=1:timeSliceNumber, TsStart=(0:(timeSliceNumber-1))*slicets, TsEnd=(1:timeSliceNumber)*slicets);
  h <- sqldf('SELECT trace.ResourceId, trace.Type, trace.Start, trace.End, trace.Duration, trace.Value, slices.SliceId, slices.TsStart, slices.TsEnd
             FROM trace
             INNER JOIN slices
             ON (trace.Start+trace.Duration > slices.TsStart) AND (trace.End-trace.Duration < slices.TsEnd)')
  h$Duration <- NULL;
  m <- h %>% group_by(ResourceId, Type, Start, End, SliceId, Value) %>%
    mutate(N=n(), TinTS = (min(End,TsEnd) - max(Start,TsStart))) %>%
    group_by(ResourceId, SliceId, Type, TsStart, TsEnd) %>%
    summarize (Mean=weighted.mean(Value, TinTS), Normalized=Mean) %>%
    as.data.frame();
  p <- expand.grid(ResourceId=unique(m$ResourceId), SliceId = 1:max(m$SliceId), Type = unique(m$Type));
  p$TsStart = 0;
  p$TsEnd = 0;
  p$Mean = 0;
  p$Normalized = 0;
  n <- rbind(p, m);
  o <- n %>% group_by (ResourceId, SliceId, Type) %>%
    summarize(TsStart = max(TsStart), TsEnd = max(TsEnd), Mean = max(Mean), Normalized=max(Normalized)) %>% as.data.frame;
  o <- o %>% group_by(Type) %>% mutate(Normalized=Normalized/sum(Normalized))
  return (o);
}

parsepjdump <- function (file){
  
  names <- c("Nature", "ResourceId", "Type", "Start", "End", "Duration", "Depth", "Value", "a", "b", "c", "d", "e", "f", "g")
  trace <- read.table(file, sep=",", fill=TRUE, header=FALSE, strip.white=TRUE, col.names=names)
  
  trace[trace$Nature %in% 'Variable', "Value"] <- as.numeric(levels((trace[trace$Nature %in% 'Variable',"Depth"])))[(trace[trace$Nature %in% 'Variable',"Depth"])]
  trace$a <- NULL
  trace$b <- NULL
  trace$c <- NULL
  trace$d <- NULL
  trace$e <- NULL
  trace$f <- NULL
  trace$g <- NULL
  
  resources <- trace[trace$Nature %in% "Container",]
  resources$Nature <- NULL
  resources$Type <- NULL
  resources$Start <- NULL
  resources$End <- NULL
  resources$Duration <- NULL
  resources$Value <- NULL
  resources$ParentId <- resources$ResourceId
  resources$ResourceId <- resources$Depth
  resources$Depth <- NULL
  
  trace <- trace[!(trace$Nature %in% "Container"),]
  ret<-list("data"=trace,"resources"=resources)
  ret
}

pjdump2micro <- function(trace, timeSliceNumber=100, type="State", enable_hierarchy=TRUE){
  
  data <- trace$data
  if (type %in% "State"){
    data <- data[data$Nature %in% "State",]
    df <- slicerstate(data, timeSliceNumber)
  }else if (type %in% "Counter"){
    data <- data[data$Nature %in% "Variable",]
    df <- slicerprvcounter(data, timeSliceNumber)
  }
  
  time <- unique(df$SliceId)
  time <- time[order(time)]
  
  type <- unique(df$Type)
  type <- type[order(type)]
  
  if (enable_hierarchy){
    resources <- trace$resources
    
    parents <- unique(resources$ParentId)
    parents<-parents[order(parents)]
    parents<-rev(parents)
    
    #remove parents from df
    df <- df[!(df$ResourceId %in% parents),]
  }
  
  space <- unique(df$ResourceId)
  space <- space[order(space)]
  
  if (enable_hierarchy){
    hierarchy <- factor(c(as.character(space),as.character(parents)))
    hierarchy<-unique(hierarchy)
    names(hierarchy)=as.character(hierarchy)
    resources$ParentIndex=-1
    vhierarchy <- rep(-1,length(hierarchy))
    names(vhierarchy)=as.character(hierarchy)
    
    for (i in 1:length(vhierarchy)){
      resources[resources$ResourceId %in% hierarchy[i],"ParentIndex"]=match(resources[resources$ResourceId %in% hierarchy[i],"ParentId"],hierarchy)[1]
    }
    resources[1,"ParentIndex"]=0
    for (i in 1:length(vhierarchy)){
      vhierarchy[i]=resources[resources$ResourceId %in% hierarchy[i],"ParentIndex"]
    }
  }else{
    vhierarchy=0
  }
  
  dataCube <- array(0,
                    dim = c(length(space), length(type), length(time)),
                    dimnames = list("Space"=space, "Type"=type, "Time"=time)
  )
  
  for (r in 1:nrow(df)) {
    row <- df[r,]
    dataCube[as.character(row$ResourceId),as.character(row$Type),as.character(row$SliceId)] <- row$Normalized
  }
  
  ret<-list("data"=dataCube,"hierarchy"=vhierarchy)
  ret
}
