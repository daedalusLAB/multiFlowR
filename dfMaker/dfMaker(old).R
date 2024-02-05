dfMaker<-function(input.folders,save.csv=F, output.folder,return.empty=F, extra.var,save.parquet=F, type.point="full") {
  
   
 input.folders<-list.dirs(input.folders, full.names=TRUE,recursive = F)
 
 
videoMaker<<- function(input.folders,output.folder,save.csv,return.empty, extra.var,save.parquet) {
  
  
  
  if (save.parquet==TRUE) {
    
    ifelse(require(arrow),"arrow is alreday installed", install.packages("arrow")) 
  }
  
  files<-list.files(input.folders, pattern="*.json", full.names=TRUE)
  
  out=NULL
  emptyFrames=NULL
 
  
  
  frameMaker<<-function(file_name, extra.var){
    
    if (require(arrow)==TRUE) {
      rawData<-read_json_arrow(file_name,as_data_frame = T) #read file
      
      rawData<-rawData[[2]][[1]]  #extract lists
      
      dfPoints=NULL
      
      
      if(sum(capture.output(rawData)!="<unspecified> [0]")!=0) {
          for (id in 1:nrow(rawData)) {
            points<-data.frame(data=unlist(rawData[id,]),people=id)
            dfPoints=rbind(dfPoints,points) } 
         }else{
            empty<-paste(file_name)
          }
      }else{
      require(jsonlite)
      rawData<-read_json( path = file_name) #read file
      
      rawData<-rawData[2]  #extract lists
      rawData<-rawData[[1]]  #extract lists
      
      dfPoints=NULL
      for (id in 1:length(rawData)) {
        
        if(length(rawData)!=0) {
          points<-data.frame(unlist(rawData[[id]]),people=id)
          dfPoints=rbind(dfPoints,points) }else{
            empty<-paste(file_name)
          }}
    }
    
    ####################### Only pose vs. full #########################################
    
    if (type.point=="full") {
      pattern<- sample(T, size=137*3, replace= T) # points triplicates
      points<-c(0:24,0:69,0:20,0:20)
    }
    
    if (type.point=="pose") {
      pattern<- sample(T, size=25*3, replace= T) # points triplicates
      points<-c(0:24)
    }
  
 #####################################################################   
      
    pattern<-c(F,pattern)
    
    if(!is.null(dfPoints)){
      pattern<-rep(pattern, times=max(dfPoints$people))
    }  
    
    
    
    dfPoints<-dfPoints[pattern,]
    
    ### type
    
    type<-rownames(dfPoints)
    type<-gsub("_2d[0-9]*", "", type)
    

    ### frame
    
    regmatches(file_name,regexec( "[0-9]{12}", file_name))->frame # frame has 12 digits
    frame<-as.numeric(frame[[1]])
    
    
    ### name
    
    name<-gsub(paste("_[0-9]{12}.*", sep = ""),"", file_name)
    name<-gsub(paste(".*/",sep = ""), "", name)
    
    
    ###
    triplet<-c(T,F,F)
    ###    
    
    groups <- c("x", "y", "c") # the variables of the final df
    

#############Extra variables###############        
  if (extra.var==TRUE) {
    
    ### words
    
    words<-gsub("_[0-9]{12}.*", "", file_name)
    words<-gsub(".*[0-9]_", "", words)
    
    ### name
    
    name<-gsub(paste("_[0-9]{12}.*", sep = ""),"", file_name)
    name<-gsub(paste(".*/",sep = ""), "", name)
    
    ### date
    
    date<-gsub(paste(".*/",sep = ""), "", file_name)
    date<-as.Date(gsub(paste("_.*",sep = ""), "", date))
    
    
    
    if(!is.null(dfPoints)){
      pointsDF<-data.frame(split(dfPoints[,1], f = groups), 
                           people=dfPoints$people[triplet],
                           typePoint=  type[triplet],
                           point= points,
                           words=words,
                           frame=frame,
                           name=name,
                           date=date) # split in 3 columns 
      return(pointsDF)
    }else{
      return(empty)
    }
  }else{
    if(!is.null(dfPoints)){
      pointsDF<-data.frame(split(dfPoints[,1], f = groups), 
                           people=dfPoints$people[triplet],
                           typePoint=  type[triplet],
                           point= points,
                           frame=frame,
                           name=name) # split in 3 columns 
      return(pointsDF)
    }else{
     return(empty)
    }}
  
    
  }
  
  
  for (i in 1:length(files)){
    
    c <- data.frame(frameMaker(files[i],extra.var = extra.var))
    
    if (length(c)!=1) {
       out=rbind(out,c)
    }else{
      
      emptyFrames=rbind(emptyFrames,c)
    }
    
    
   
  }
  
  if (save.csv==TRUE) {
    folder<-output.folder
    dir.create(folder,recursive = TRUE,showWarnings = FALSE)
    write.csv(x = out, paste(folder,"/",unique(out$name),".csv",sep = ""),row.names = F)
    
  }
  

  
  if (save.parquet==TRUE) {
    
    folder<-output.folder
    dir.create(folder,recursive = TRUE,showWarnings = FALSE)
    write_parquet(x = out, sink = paste(folder,"/",unique(out$name),".parquet",sep = ""))
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
  
  
  dfVideo<-videoMaker( input.folders[i],save.csv = save.csv,output.folder = output.folder,return.empty = return.empty, extra.var = extra.var,save.parquet = save.parquet)
  
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


