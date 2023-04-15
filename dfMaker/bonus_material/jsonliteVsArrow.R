
file = "dfMaker/dfMakerExample/exampleVideos/2014-10-10_0600_US_KCAL_Entertainment_Tonight_374-380_ID933_from_beginning_to_end/2014-10-10_0600_US_KCAL_Entertainment_Tonight_374-380_ID933_from_beginning_to_end_000000000165_keypoints.json"

######### jsonlite #################
start_time <- Sys.time()

library(jsonlite)

rawData<-read_json( path = file)

rawData<-rawData[2]

rawData<-rawData[[1]]

dfPoints=NULL

for (id in 1:length(rawData)) {
  
  if(length(rawData)!=0) {
    
    points<-data.frame(data=unlist(rawData[[id]]),people=id)
    
    dfPoints=rbind(dfPoints,points) }else{
      
      empty<-paste(file)
    }}

end_time <- Sys.time()
time1<-end_time - start_time

############# read_json_arrow() #####################################
start_time <- Sys.time()
library(arrow)

rawData<-read_json_arrow(file,as_data_frame = T)

rawData<-rawData[[2]][[1]]

dfPoints2=NULL

for (id in 1:nrow(rawData)) {
  
  if(nrow(rawData)!=0) {
    
    points<-data.frame(data=unlist(rawData[id,]),people=id)
    
    dfPoints2=rbind(dfPoints2,points) }else{
      
      empty<-paste(file)
    }}

end_time <- Sys.time()
time2<-end_time - start_time

####### Corroboration ###########################

names(dfPoints2)
identical(dfPoints,dfPoints2)

print(paste("jsontlite:", time1,"arrow", time2, "diff=",time2-time1))

