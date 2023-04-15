maxPeople<-function(path,max,full.folder,fileNames){
  if (full.folder==T) {
    list.files(path,pattern = ".csv")->files} else{
      
      read.table(file=fileNames)->files
      files[,1]->files
    }
  
  
  
  dataFiles=NULL
  for (i in 1:length(files)) {
    read.csv(paste(path,files[i],sep = ""))->dataOneFile
    if (max(dataOneFile$people)<=max) {
      
      
      dataFiles<-rbind(dataFiles,dataOneFile)
    }}
  
  return(dataFiles)
}


