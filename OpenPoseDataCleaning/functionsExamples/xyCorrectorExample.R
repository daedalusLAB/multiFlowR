
# data loading by maxPeople

load("~/multiFlowR/OpenPoseDataCleaning/functionsRData/maxPeople.rda")

maxPeople(path = "~/multiFlowR/OpenPoseDataCleaning/functionsExamples/rawData/", max = 1,full.folder = T)->example


######################## xyCorrector ##########################################

load("~/multiFlowR/OpenPoseDataCleaning/functionsRData/xyCorrector.rda")

xyCorrector(df = example,df.full = T,set.NAs = T,fixed.point.x = 1,fixed.point.y = 1,reverse.y = T)->exampleCorrected


##############################################################################

# Only pose_keypoints 

# frame plot #

unique(example$name)-> dataNames # array of video names

# for (i in 1:80) {
#   exampleCorrected[exampleCorrected$frame==i & exampleCorrected$name==dataNames[3],]->frame
#   print(is.na(frame$x[9]))
# 
#  } search for a value on point 8 (position 9 in the vector)


exampleCorrected[exampleCorrected$frame==43 & exampleCorrected$name==dataNames[3],]->frame # get a frame

par(mfrow=c(1,2))

plot(frame$x[c(1,2)],frame$y[c(1,2)], type = "o",ylim = c(min(frame$y,na.rm = T),max(frame$y,na.rm = T)),
     xlim =c(min(frame$x,na.rm = T),max(frame$x,na.rm = T)), main="Before xyCorrector", lwd=3, col=6 )+
  
  points(frame$x[c(2,3)],frame$y[c(2,3)],col=2, type = "o",lwd=3)+
  
  points(frame$x[c(3,4)],frame$y[c(3,4)],col=2, type = "o",lwd=3)+
  
  points(frame$x[c(2,9)],frame$y[c(2,9)],col=4, type = "o",lwd=3)+
  
  points(frame$x[c(2,6)],frame$y[c(2,6)],col=3, type = "o",lwd=3)+
  
  points(frame$x[c(6,7)],frame$y[c(6,7)],col=3, type = "o",lwd=3)+
  
  points(frame$x[c(7,8)],frame$y[c(7,8)],col=3, type = "o",lwd=3) +
  
  points(frame$x[c(1,17)],frame$y[c(1,17)],col=6, type = "o",lwd=3)+
  
  points(frame$x[c(1,16)],frame$y[c(1,16)],col=6, type = "o",lwd=3)+
  
  points(frame$x[c(16,18)],frame$y[c(16,18)],col=6, type = "o",lwd=3)+
  
  points(frame$x[c(17,19)],frame$y[c(17,19)],col=6, type = "o",lwd=3)+
  
  points(frame$x[c(9,13)],frame$y[c(9,13)],col=4, type = "o",lwd=3)+
  
  points(frame$x[c(9,10)],frame$y[c(9,10)],col=4, type = "o",lwd=3)+
  
  points(frame$x[c(13,14)],frame$y[c(13,14)],col=4, type = "o",lwd=3)+
  
  points(frame$x[c(10,11)],frame$y[c(10,11)],col=4, type = "o",lwd=3)




plot(frame$x2[c(1,2)],frame$y2[c(1,2)], type = "o",ylim = c(min(frame$y2,na.rm = T),max(frame$y2,na.rm = T)),
     xlim =c(min(frame$x2,na.rm = T),max(frame$x2,na.rm = T)), main="After xyCorrector", lwd=3, col=6 )+
  
  points(frame$x2[c(2,3)],frame$y2[c(2,3)],col=2, type = "o",lwd=3)+
  
  points(frame$x2[c(3,4)],frame$y2[c(3,4)],col=2, type = "o",lwd=3)+
  
  points(frame$x2[c(2,9)],frame$y2[c(2,9)],col=4, type = "o",lwd=3)+
  
  points(frame$x2[c(2,6)],frame$y2[c(2,6)],col=3, type = "o",lwd=3)+
  
  points(frame$x2[c(6,7)],frame$y2[c(6,7)],col=3, type = "o",lwd=3)+
  
  points(frame$x2[c(7,8)],frame$y2[c(7,8)],col=3, type = "o",lwd=3) +
  
  points(frame$x2[c(1,17)],frame$y2[c(1,17)],col=6, type = "o",lwd=3)+
  
  points(frame$x2[c(1,16)],frame$y2[c(1,16)],col=6, type = "o",lwd=3)+
  
  points(frame$x2[c(16,18)],frame$y2[c(16,18)],col=6, type = "o",lwd=3)+
  
  points(frame$x2[c(17,19)],frame$y2[c(17,19)],col=6, type = "o",lwd=3)+
  
  points(frame$x2[c(9,13)],frame$y2[c(9,13)],col=4, type = "o",lwd=3)+
  
  points(frame$x2[c(9,10)],frame$y2[c(9,10)],col=4, type = "o",lwd=3)+
  
  points(frame$x2[c(13,14)],frame$y2[c(13,14)],col=4, type = "o",lwd=3)+
  
  points(frame$x2[c(10,11)],frame$y2[c(10,11)],col=4, type = "o",lwd=3)

#########################################################################################
# For all frames in a video. Warning!!! It can require a lot of computational power


# par(mfrow=c(1,2))
#
# max(result[result$name==dataNames[3],"frame"])->maxFrame
#
# for (i in min(exampleCorrected$frame):maxFrame) {
#   exampleCorrected[exampleCorrected$frame==i & exampleCorrected$name==dataNames[3],]->frame
# 
#  
# 
#   plot(frame$x[c(1,2)],frame$y[c(1,2)], type = "o",ylim = c(min(frame$y,na.rm = T),max(frame$y,na.rm = T)),
#        xlim =c(min(frame$x2,na.rm = T),max(frame$x2,na.rm = T)), main="Before xyCorrector", lwd=3, col=6 )+
#     
#     points(frame$x[c(2,3)],frame$y[c(2,3)],col=2, type = "o",lwd=3)+
#     
#     points(frame$x[c(3,4)],frame$y[c(3,4)],col=2, type = "o",lwd=3)+
#     
#     points(frame$x[c(2,9)],frame$y[c(2,9)],col=4, type = "o",lwd=3)+
#     
#     points(frame$x[c(2,6)],frame$y[c(2,6)],col=3, type = "o",lwd=3)+
#     
#     points(frame$x[c(6,7)],frame$y[c(6,7)],col=3, type = "o",lwd=3)+
#     
#     points(frame$x[c(7,8)],frame$y[c(7,8)],col=3, type = "o",lwd=3) +
#     
#     points(frame$x[c(1,17)],frame$y[c(1,17)],col=6, type = "o",lwd=3)+
#     
#     points(frame$x[c(1,16)],frame$y[c(1,16)],col=6, type = "o",lwd=3)+
#     
#     points(frame$x[c(16,18)],frame$y[c(16,18)],col=6, type = "o",lwd=3)+
#     
#     points(frame$x[c(17,19)],frame$y[c(17,19)],col=6, type = "o",lwd=3)+
#     
#     points(frame$x[c(9,13)],frame$y[c(9,13)],col=4, type = "o",lwd=3)+
#     
#     points(frame$x[c(9,10)],frame$y[c(9,10)],col=4, type = "o",lwd=3)+
#     
#     points(frame$x[c(13,14)],frame$y[c(13,14)],col=4, type = "o",lwd=3)+
#     
#     points(frame$x[c(10,11)],frame$y[c(10,11)],col=4, type = "o",lwd=3)
#   
#   
# 
# 
# 
# plot(frame$x[c(1,2)],frame$y2[c(1,2)], type = "o",ylim = c(min(frame$y2,na.rm = T),max(frame$y2,na.rm = T)),
#      xlim =c(min(frame$x,na.rm = T),max(frame$x,na.rm = T)), main="After xyCorrector", lwd=3, col=6 )+
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
# }
