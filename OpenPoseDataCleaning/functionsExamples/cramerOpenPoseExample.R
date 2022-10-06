# data loading by maxPeople

load("~/multiFlowR/OpenPoseDataCleaning/functionsRData/maxPeople.rda")

maxPeople(path = "~/multiFlowR/OpenPoseDataCleaning/functionsExamples/rawCSV/", max = 1,full.folder = T)->example


# and corrected by xyCorrector

load("~/multiFlowR/OpenPoseDataCleaning/functionsRData/xyCorrector.rda")

xyCorrector(df = example,df.full = T,set.NAs = T,fixed.point.x = 1,fixed.point.y = 1,reverse.y = T)->example


################### CramerOpenPose ##########################



load("~/multiFlowR/OpenPoseDataCleaning/functionsRData/cramerOpenPose.rda")


result<-cramerOpenPose(data = example,v.i = 5,orthonormal = T, save.video.csv = T,path.save.videos = "~/multiFlowR/OpenPoseDataCleaning/functionsExamples/cleanDataCSV/" )


head(result)


### plots

par(mfrow=c(2,2))

unique(example$name)-> dataNames # get the file names as vectors


result[result$frame==43 & result$name==dataNames[3],]->oneFrame # get a frame

result[result$frame==47 & result$name==dataNames[2],]->anotherFrame


#### before cramer ###################################333

plot(oneFrame$x2[c(1,2)],oneFrame$y2[c(1,2)], type = "o",ylim = c(min(oneFrame$y2,na.rm = T),max(oneFrame$y2,na.rm = T)),
     xlim =c(min(oneFrame$x2,na.rm = T),max(oneFrame$x2,na.rm = T)), main=" A frame Before Cramer", lwd=3, col=6 )+
  
  points(oneFrame$x2[c(2,3)],oneFrame$y2[c(2,3)],col=2, type = "o",lwd=3)+
  
  points(oneFrame$x2[c(3,4)],oneFrame$y2[c(3,4)],col=2, type = "o",lwd=3)+
  
  points(oneFrame$x2[c(2,9)],oneFrame$y2[c(2,9)],col=4, type = "o",lwd=3)+
  
  points(oneFrame$x2[c(2,6)],oneFrame$y2[c(2,6)],col=3, type = "o",lwd=3)+
  
  points(oneFrame$x2[c(6,7)],oneFrame$y2[c(6,7)],col=3, type = "o",lwd=3)+
  
  points(oneFrame$x2[c(7,8)],oneFrame$y2[c(7,8)],col=3, type = "o",lwd=3) +
  
  points(oneFrame$x2[c(1,17)],oneFrame$y2[c(1,17)],col=6, type = "o",lwd=3)+
  
  points(oneFrame$x2[c(1,16)],oneFrame$y2[c(1,16)],col=6, type = "o",lwd=3)+
  
  points(oneFrame$x2[c(16,18)],oneFrame$y2[c(16,18)],col=6, type = "o",lwd=3)+
  
  points(oneFrame$x2[c(17,19)],oneFrame$y2[c(17,19)],col=6, type = "o",lwd=3)+
  
  points(oneFrame$x2[c(9,13)],oneFrame$y2[c(9,13)],col=4, type = "o",lwd=3)+
  
  points(oneFrame$x2[c(9,10)],oneFrame$y2[c(9,10)],col=4, type = "o",lwd=3)+
  
  points(oneFrame$x2[c(13,14)],oneFrame$y2[c(13,14)],col=4, type = "o",lwd=3)+
  
  points(oneFrame$x2[c(10,11)],oneFrame$y2[c(10,11)],col=4, type = "o",lwd=3)


plot(anotherFrame$x2[c(1,2)],anotherFrame$y2[c(1,2)], type = "o",ylim = c(min(anotherFrame$y2,na.rm = T),max(anotherFrame$y2,na.rm = T)),
     xlim =c(min(anotherFrame$x2,na.rm = T),max(anotherFrame$x2,na.rm = T)), main="Another Frame before Cramer ", lwd=3, col=6 )+
  
  points(anotherFrame$x2[c(2,3)],anotherFrame$y2[c(2,3)],col=2, type = "o",lwd=3)+
  
  points(anotherFrame$x2[c(3,4)],anotherFrame$y2[c(3,4)],col=2, type = "o",lwd=3)+
  
  points(anotherFrame$x2[c(2,9)],anotherFrame$y2[c(2,9)],col=4, type = "o",lwd=3)+
  
  points(anotherFrame$x2[c(2,6)],anotherFrame$y2[c(2,6)],col=3, type = "o",lwd=3)+
  
  points(anotherFrame$x2[c(6,7)],anotherFrame$y2[c(6,7)],col=3, type = "o",lwd=3)+
  
  points(anotherFrame$x2[c(7,8)],anotherFrame$y2[c(7,8)],col=3, type = "o",lwd=3) +
  
  points(anotherFrame$x2[c(1,17)],anotherFrame$y2[c(1,17)],col=6, type = "o",lwd=3)+
  
  points(anotherFrame$x2[c(1,16)],anotherFrame$y2[c(1,16)],col=6, type = "o",lwd=3)+
  
  points(anotherFrame$x2[c(16,18)],anotherFrame$y2[c(16,18)],col=6, type = "o",lwd=3)+
  
  points(anotherFrame$x2[c(17,19)],anotherFrame$y2[c(17,19)],col=6, type = "o",lwd=3)+
  
  points(anotherFrame$x2[c(9,13)],anotherFrame$y2[c(9,13)],col=4, type = "o",lwd=3)+
  
  points(anotherFrame$x2[c(9,10)],anotherFrame$y2[c(9,10)],col=4, type = "o",lwd=3)+
  
  points(anotherFrame$x2[c(13,14)],anotherFrame$y2[c(13,14)],col=4, type = "o",lwd=3)+
  
  points(anotherFrame$x2[c(10,11)],anotherFrame$y2[c(10,11)],col=4, type = "o",lwd=3)


plot(oneFrame$nx[c(1,2)],oneFrame$ny[c(1,2)], type = "o",ylim = c(min(oneFrame$ny,na.rm = T),max(oneFrame$ny,na.rm = T)),
     xlim =c(min(oneFrame$nx,na.rm = T),max(oneFrame$nx,na.rm = T)), main=" A frame after Cramer", lwd=3, col=6 )+
  
  points(oneFrame$nx[c(2,3)],oneFrame$ny[c(2,3)],col=2, type = "o",lwd=3)+
  
  points(oneFrame$nx[c(3,4)],oneFrame$ny[c(3,4)],col=2, type = "o",lwd=3)+
  
  points(oneFrame$nx[c(2,9)],oneFrame$ny[c(2,9)],col=4, type = "o",lwd=3)+
  
  points(oneFrame$nx[c(2,6)],oneFrame$ny[c(2,6)],col=3, type = "o",lwd=3)+
  
  points(oneFrame$nx[c(6,7)],oneFrame$ny[c(6,7)],col=3, type = "o",lwd=3)+
  
  points(oneFrame$nx[c(7,8)],oneFrame$ny[c(7,8)],col=3, type = "o",lwd=3) +
  
  points(oneFrame$nx[c(1,17)],oneFrame$ny[c(1,17)],col=6, type = "o",lwd=3)+
  
  points(oneFrame$nx[c(1,16)],oneFrame$ny[c(1,16)],col=6, type = "o",lwd=3)+
  
  points(oneFrame$nx[c(16,18)],oneFrame$ny[c(16,18)],col=6, type = "o",lwd=3)+
  
  points(oneFrame$nx[c(17,19)],oneFrame$ny[c(17,19)],col=6, type = "o",lwd=3)+
  
  points(oneFrame$nx[c(9,13)],oneFrame$ny[c(9,13)],col=4, type = "o",lwd=3)+
  
  points(oneFrame$nx[c(9,10)],oneFrame$ny[c(9,10)],col=4, type = "o",lwd=3)+
  
  points(oneFrame$nx[c(13,14)],oneFrame$ny[c(13,14)],col=4, type = "o",lwd=3)+
  
  points(oneFrame$nx[c(10,11)],oneFrame$ny[c(10,11)],col=4, type = "o",lwd=3)


plot(anotherFrame$nx[c(1,2)],anotherFrame$ny[c(1,2)], type = "o",ylim = c(min(anotherFrame$ny,na.rm = T),max(anotherFrame$ny,na.rm = T)),
     xlim =c(min(anotherFrame$nx,na.rm = T),max(anotherFrame$nx,na.rm = T)), main="Another frame after Cramer", lwd=3, col=6 )+
  
  points(anotherFrame$nx[c(2,3)],anotherFrame$ny[c(2,3)],col=2, type = "o",lwd=3)+
  
  points(anotherFrame$nx[c(3,4)],anotherFrame$ny[c(3,4)],col=2, type = "o",lwd=3)+
  
  points(anotherFrame$nx[c(2,9)],anotherFrame$ny[c(2,9)],col=4, type = "o",lwd=3)+
  
  points(anotherFrame$nx[c(2,6)],anotherFrame$ny[c(2,6)],col=3, type = "o",lwd=3)+
  
  points(anotherFrame$nx[c(6,7)],anotherFrame$ny[c(6,7)],col=3, type = "o",lwd=3)+
  
  points(anotherFrame$nx[c(7,8)],anotherFrame$ny[c(7,8)],col=3, type = "o",lwd=3) +
  
  points(anotherFrame$nx[c(1,17)],anotherFrame$ny[c(1,17)],col=6, type = "o",lwd=3)+
  
  points(anotherFrame$nx[c(1,16)],anotherFrame$ny[c(1,16)],col=6, type = "o",lwd=3)+
  
  points(anotherFrame$nx[c(16,18)],anotherFrame$ny[c(16,18)],col=6, type = "o",lwd=3)+
  
  points(anotherFrame$nx[c(17,19)],anotherFrame$ny[c(17,19)],col=6, type = "o",lwd=3)+
  
  points(anotherFrame$nx[c(9,13)],anotherFrame$ny[c(9,13)],col=4, type = "o",lwd=3)+
  
  points(anotherFrame$nx[c(9,10)],anotherFrame$ny[c(9,10)],col=4, type = "o",lwd=3)+
  
  points(anotherFrame$nx[c(13,14)],anotherFrame$ny[c(13,14)],col=4, type = "o",lwd=3)+
  
  points(anotherFrame$nx[c(10,11)],anotherFrame$ny[c(10,11)],col=4, type = "o",lwd=3)

# max(result[result$name==dataNames[3],"frame"])->maxFrame
# 
# for (i in min(result$frame):maxFrame) {
#   
#   result[result$frame==i & result$name==dataNames[3],]->frame
# 
# plot(frame$x[c(1,2)],frame$y[c(1,2)], type = "o",ylim = c(min(frame$y,na.rm = T),max(frame$y,na.rm = T)),
#      xlim =c(min(frame$x,na.rm = T),max(frame$x,na.rm = T)), main="Before xyCorrector", lwd=3, col=6 )+
#   
#   points(frame$x[c(2,3)],frame$y[c(2,3)],col=2, type = "o",lwd=3)+
#   
#   points(frame$x[c(3,4)],frame$y[c(3,4)],col=2, type = "o",lwd=3)+
#   
#   points(frame$x[c(2,9)],frame$y[c(2,9)],col=4, type = "o",lwd=3)+
#   
#   points(frame$x[c(2,6)],frame$y[c(2,6)],col=3, type = "o",lwd=3)+
#   
#   points(frame$x[c(6,7)],frame$y[c(6,7)],col=3, type = "o",lwd=3)+
#   
#   points(frame$x[c(7,8)],frame$y[c(7,8)],col=3, type = "o",lwd=3) +
#   
#   points(frame$x[c(1,17)],frame$y[c(1,17)],col=6, type = "o",lwd=3)+
#   
#   points(frame$x[c(1,16)],frame$y[c(1,16)],col=6, type = "o",lwd=3)+
#   
#   points(frame$x[c(16,18)],frame$y[c(16,18)],col=6, type = "o",lwd=3)+
#   
#   points(frame$x[c(17,19)],frame$y[c(17,19)],col=6, type = "o",lwd=3)+
#   
#   points(frame$x[c(9,13)],frame$y[c(9,13)],col=4, type = "o",lwd=3)+
#   
#   points(frame$x[c(9,10)],frame$y[c(9,10)],col=4, type = "o",lwd=3)+
#   
#   points(frame$x[c(13,14)],frame$y[c(13,14)],col=4, type = "o",lwd=3)+
#   
#   points(frame$x[c(10,11)],frame$y[c(10,11)],col=4, type = "o",lwd=3)
# 
# 
# 
# 
# plot(frame$x2[c(1,2)],frame$y2[c(1,2)], type = "o",ylim = c(min(frame$y2,na.rm = T),max(frame$y2,na.rm = T)),
#      xlim =c(min(frame$x2,na.rm = T),max(frame$x2,na.rm = T)), main="After xyCorrector", lwd=3, col=6 )+
#   
#   points(frame$x2[c(2,3)],frame$y2[c(2,3)],col=2, type = "o",lwd=3)+
#   
#   points(frame$x2[c(3,4)],frame$y2[c(3,4)],col=2, type = "o",lwd=3)+
#   
#   points(frame$x2[c(2,9)],frame$y2[c(2,9)],col=4, type = "o",lwd=3)+
#   
#   points(frame$x2[c(2,6)],frame$y2[c(2,6)],col=3, type = "o",lwd=3)+
#   
#   points(frame$x2[c(6,7)],frame$y2[c(6,7)],col=3, type = "o",lwd=3)+
#   
#   points(frame$x2[c(7,8)],frame$y2[c(7,8)],col=3, type = "o",lwd=3) +
#   
#   points(frame$x2[c(1,17)],frame$y2[c(1,17)],col=6, type = "o",lwd=3)+
#   
#   points(frame$x2[c(1,16)],frame$y2[c(1,16)],col=6, type = "o",lwd=3)+
#   
#   points(frame$x2[c(16,18)],frame$y2[c(16,18)],col=6, type = "o",lwd=3)+
#   
#   points(frame$x2[c(17,19)],frame$y2[c(17,19)],col=6, type = "o",lwd=3)+
#   
#   points(frame$x2[c(9,13)],frame$y2[c(9,13)],col=4, type = "o",lwd=3)+
#   
#   points(frame$x2[c(9,10)],frame$y2[c(9,10)],col=4, type = "o",lwd=3)+
#   
#   points(frame$x2[c(13,14)],frame$y2[c(13,14)],col=4, type = "o",lwd=3)+
#   
#   points(frame$x2[c(10,11)],frame$y2[c(10,11)],col=4, type = "o",lwd=3)
# 
# 
# 
# plot(frame$nx[c(1,2)],frame$ny[c(1,2)], type = "o",ylim = c(min(frame$ny,na.rm = T),max(frame$ny,na.rm = T)),
#      xlim =c(min(frame$nx,na.rm = T),max(frame$nx,na.rm = T)), main="After Cramer", lwd=3, col=6 )+
#   
#   points(frame$nx[c(2,3)],frame$ny[c(2,3)],col=2, type = "o",lwd=3)+
#   
#   points(frame$nx[c(3,4)],frame$ny[c(3,4)],col=2, type = "o",lwd=3)+
#   
#   points(frame$nx[c(2,9)],frame$ny[c(2,9)],col=4, type = "o",lwd=3)+
#   
#   points(frame$nx[c(2,6)],frame$ny[c(2,6)],col=3, type = "o",lwd=3)+
#   
#   points(frame$nx[c(6,7)],frame$ny[c(6,7)],col=3, type = "o",lwd=3)+
#   
#   points(frame$nx[c(7,8)],frame$ny[c(7,8)],col=3, type = "o",lwd=3) +
#   
#   points(frame$nx[c(1,17)],frame$ny[c(1,17)],col=6, type = "o",lwd=3)+
#   
#   points(frame$nx[c(1,16)],frame$ny[c(1,16)],col=6, type = "o",lwd=3)+
#   
#   points(frame$nx[c(16,18)],frame$ny[c(16,18)],col=6, type = "o",lwd=3)+
#   
#   points(frame$nx[c(17,19)],frame$ny[c(17,19)],col=6, type = "o",lwd=3)+
#   
#   points(frame$nx[c(9,13)],frame$ny[c(9,13)],col=4, type = "o",lwd=3)+
#   
#   points(frame$nx[c(9,10)],frame$ny[c(9,10)],col=4, type = "o",lwd=3)+
#   
#   points(frame$nx[c(13,14)],frame$ny[c(13,14)],col=4, type = "o",lwd=3)+
#   
#   points(frame$nx[c(10,11)],frame$ny[c(10,11)],col=4, type = "o",lwd=3)
# 
# }
