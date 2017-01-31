#!/bin/bash

# If $'\r': command not found, in shell:
# sed -i 's/\r$//' bash_foraging_path 
 
echo -n "Enter step-size (formulated in checkPath.R): "
read step_size
 
# The addTrialstamp function prints the number of trials used to create a CI to the top-right of the CI
 function addTrialstamp {
   i=0
   # This for loop loops through all of the files that match the expression foraging_path_*. I.e. all of the images created by checkPath.R
   #for f in foraging_path_trial*
   for f in ERC_WP3_Year1_Study1_*
   do
     # This extracts the number from the filename and assigns it to the variable "num"
     # num=$(echo $f | sed 's/foraging_path_trial_//' | sed 's/\.jpg//')
     num=$(echo $f | sed 's/ERC_WP3_Year1_Study1_//' | sed 's/foraging_path_trial_//'  | sed 's/\.jpg//')
     parnum=${num:0:4}
     trialnum=${num:20:1}
     num=${num:22:5}
     parnumarray[$i]=$parnum
     trialarray[$i]=$trialnum
	
     # This removes the padding from the extracted number. This is what we want stamp onto our CI
     unpaddednum=$(echo $num | sed 's/^0*//')
     # This gives is the sequence number of the image (ffmpeg doesn't like missing values)
     filenum=$(echo "$num/"$step_size | bc)        
     i=$[i+1]
  
     # convert is part of ImageMagick. This adds the unpadded trialstamp to the CI and writes it to a file called img-$filenum.jpeg     
     #convert $f -gravity NorthEast -pointsize 32 -annotate +10+10 $unpaddednum "img-participant-$parnum-trial-$trialnum-$filenum.jpeg"
     convert $f -gravity NorthEast -pointsize 32 "img-participant-$parnum-trial-$trialnum-$filenum.jpeg"
     
     #For monitoring the progress of the script
     echo $filenum
     
   #This terminates the "what to do" part of our loop. Now the loop starts again (and so on, until we're out of items)
   done
   UNIQ_PARS=($(printf "%s\n" "${parnumarray[@]}" | sort -u))
   UNIQ_TRIALS=($(printf "%s\n" "${trialarray[@]}" | sort -u))
 }
 
 # The makeVideo function creates a timelapse video from the images created by the addTrialstamp function
 function makeVideo {
   # "-r 10": the framerate of the input (the actual framerate you will "see")
   # "-i img-%d.jpeg": use the image in the sequence that matches this pattern as the input
   # "-r:v 29.976": the framerate of the output file (we set it to something standard so media players do not choke on our file)
   # "-c:v libvpx": use the libvpx video codec (also known as the WebM codec)
   # "-qmin 4 -qmax 4": set the minimum and maximum quality to 4 (out of 0-63, with lower meaning better quality)
   # "video.mp4": the filename of the output
   ffmpeg -r 5 -i img-participant-$eachpar-trial-$eachtrial-%d.jpeg -r:v 29.976 -c:v libvpx -qmin 4 -qmax 4 "video-participant-$eachpar-trial-$eachtrial".webm
 }
 
 # Calling the functions
 addTrialstamp
 
 for eachpar in "${UNIQ_PARS[@]}"
 do
	for eachtrial in "${UNIQ_TRIALS[@]}"
	do
		makeVideo
	done
 done

 
 # Clean up
 rm img-*
 rm ERC_WP3_Year1_Study1_*
