
rootDir=$(pwd)

rm -r $(pwd)/videosMP4W
rm -r $(pwd)/videosJSON

mkdir $(pwd)/videosMP4W
mkdir $(pwd)/videosJSON

rootDir=$(pwd)
jsonWorkDir=$(pwd)/videosJSON
videoWorkDir=$(pwd)/videosMP4W

read -e -p "Where is OpenPose installed? " OpenPosePath
echo "--------------------------------------------------------------------------"

read -e -p "Where are the videos? " path
echo "--------------------------------------------------------------------------"

read -p "Do you want the skeletons? (yes/no) " skeletons
echo "--------------------------------------------------------------------------"

read -p  "Do you want to include the extra variables from file names? (TRUE/FALSE) " extraVar
echo "--------------------------------------------------------------------------"

read -p "Do you want to save the raw data? (TRUE/FALSE) " rawSave
echo "--------------------------------------------------------------------------"

if [ "$rawSave" == "TRUE" ]; then
read -e -p "Where do you want to save raw data?" rawSavePath

else
rawSave=FALSE


fi
echo "--------------------------------------------------------------------------"

read -p "Do you want to apply clipCleaner over data? (TRUE/FALSE)" clipCleaner
echo "--------------------------------------------------------------------------"

read -e -p "Where do you want to save tidy data?" TidySavePath



 cp   $path/*.mp4  $videoWorkDir




cd  $videoWorkDir

for f in *; do
  echo "File -> $f"
done



 # cd /data/home/agora/openpose #uncomment to set OpenPose path 

cd $OpenPosePath #Don't forget ti comment this line in case OpenPose path has been set

for f in  $videoWorkDir/* ## change the number of "*" in case there are more subfolders
do

jsonData=$jsonWorkDir/${f##*/}

 ./build/examples/openpose/openpose.bin --video  $f --face --hand  --write_json $jsonData --display 0  --render_pose 0 


done
 
 

if [ "$skeletons" == "yes" ]; then
 
mkdir $rootDir/skeleton
 
skeletonDir=$rootDir/skeleton
 
for f in $videoWorkDir/* ## modificar en caso de tener diferentes niveles de subfolders
do

skeletonData=$skeletonDir/${f##*/}


./build/examples/openpose/openpose.bin --video $f  --write_video $skeletonData --display 0  --face

done
 
 fi

cd $jsonWorkDir

rename 's/.mp4//' *


cd $rootDir



Rscript dfMakerexecute.R $extraVar $rawSave $rawSavePath $clipCleaner $TidySavePath

rm -r $videoWorkDir
rm -r $jsonWorkDir
