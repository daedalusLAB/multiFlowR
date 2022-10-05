dfMaker<-function(input.folders,save.csv=F, output.folder,return.empty=F) {
  
   
 input.folders<-list.dirs(input.folders, full.names=TRUE,recursive = F)
 
 
videoMaker<<- function(input.folders,output.folder,save.csv,return.empty) {
  
  
  
  files<-list.files(input.folders, pattern="*.json", full.names=TRUE)
  
  out=NULL
  emptyFrames=NULL
 
  
  
  frameMaker<<-function(file){
    require(jsonlite)
    
    rawData<-read_json( path = file)
    
    rawData<-rawData[2]
    
    rawData<-rawData[[1]]
    
    dfPoints=NULL
    for (id in 1:length(rawData)) {
    
    if(length(rawData)!=0) {
    
    points<-data.frame(unlist(rawData[[id]]),people=id)
    
    dfPoints=rbind(dfPoints,points) }else{
      
      empty<-paste(file)
      
      
    }}
    
    
    pattern<- sample(T, size=137*3, replace= T) # points triplicates
    
    
    pattern<-c(F,pattern)
    
    if(!is.null(dfPoints)){
      pattern<-rep(pattern, times=max(dfPoints$people))
    }  
    
    
    
    dfPoints<-dfPoints[pattern,]
    ##
    
    type<-rownames(dfPoints)
    type<-gsub("_2d[0-9]*", "", type)
    
    ###
    words<-gsub("_0000.*", "", file)
    
    
    words<-gsub(".*[0-9]_", "", words)
    
    
    ###
    
    frame<-gsub(paste(".*_000", sep = ""), "", file)
    
    frame<-as.numeric(gsub("_.*","", frame))
    ###
    
    name<-gsub(paste("_",words,".*", sep = ""), words, file)
    name<-gsub(paste(".*/",sep = ""), "", name)
    
    ###
    
    triplet<-c(T,F,F)
    ###
    groups <- c("x", "y", "c") # the variables of the final df
    if(!is.null(dfPoints)){
      pointsDF<-data.frame(split(dfPoints[,1], f = groups), 
                           people=dfPoints$people[triplet],
                           typePoint=  type[triplet],
                           point= c(0:24,0:69,0:20,0:20),
                           words=words,
                           frame=frame,
                           name=name) # split in 3 columns 
      return(pointsDF)
    }else{
     return(empty)
    }
  
    
  }
  
  
  for (i in 1:length(files)){
    
    c <- data.frame(frameMaker(files[i]))
    
    if (length(c)!=1) {
       out=rbind(out,c)
    }else{
      
      emptyFrames=rbind(emptyFrames,c)
    }
    
    
   
  }
  
  if (save.csv==T) {
    folder<-output.folder
    dir.create(folder,recursive = T)
    write.csv(x = out, paste(folder,"/",unique(out$name),".csv",sep = ""),row.names = F)
    
  }
  
  if (return.empty==T) {
    video<-list(out,emptyFrames)
    return(video)
  }else{
    return(out)
  }
    

  

}

 
 
 result=NULL
 dFinal<-NULL
 totalEmpty<-NULL

 for (i in 1:(length(input.folders))) {
  
  
  dfVideo<-videoMaker( input.folders[i],save.csv = save.csv,output.folder = output.folder,return.empty = return.empty )
  
  if (return.empty==F) {
    
    result=rbind(result,dfVideo)
  }else{
    
           dFinal=rbind(dFinal,dfVideo[[1]])
           totalEmpty=rbind(totalEmpty,dfVideo[[2]])
           result<-list(dFinal,totalEmpty)

  }


}
 
 return(result)
}


