# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
### PhD Metacontrol: Leiden University.
### Year 1, study 1: Visual and Cognitive Search and the RAT and AUT.
### Last adjustment on: 2017-02-14.
### r.van.dooren@fsw.leidenuniv.nl

# Set the current working directory by clicking "session" and "set working directory" to "source file location".

# Import libraries ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("ggplot2", "stringr", "dplyr", "tidyr")
ipak(packages)

# Extract data from array ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Select and copy datafiles for transposing arrays, unless datafiles are already processed
all.datafiles <- list.files('../Rawdata/Foraging', pattern = 'visual_foraging_path_trial', full.names = T)
processed.files <- list.files('../Processeddata', pattern = 'visual_foraging_path_array_trial', full.names = T)
if (length(processed.files) == 0) {
  nonprocesseddata <- rep(TRUE, length(all.datafiles))
} else {
  processed.data <- sprintf(paste0(str_sub(processed.files, 39, 42), '_',  str_sub(processed.files, 44, 57), 
                                   '_visual_foraging_path_trial_', str_sub(processed.files, -5, -5)))
  nonprocesseddata <- str_detect(all.datafiles, pattern = processed.data) == FALSE
}

for (f in all.datafiles[nonprocesseddata]) {
  file.copy(f, './Temporary') 
}
shell("transposePath.py")
select.files <- list.files('../Processeddata/', pattern = 'visual_foraging_path_array', full.names = T)
processedfile <- read.csv2('../Processeddata/ERC_WP3_Year1_Study1_visual_foraging_processed_array_data.txt', sep = ' ', header = T)

pb1 <- progress_estimated(length(select.files))
for (f in select.files) {
  # Read file
  dat <- read.csv2(f, sep = ',', stringsAsFactors = FALSE)
  pb1$tick()$print()
  if (!(dat$subjectID[1] %in% unique(processedfile$subjectID))) {
    print(paste0("Adding data of participant ", dat$subjectID[1], " trial ", str_sub(f, -5, -5), " to datafile"))
    dat$timespent <- as.numeric(dat$timespent)
    # Remove recorded data before polygon move
    dat <- dat[-which(is.na(dat$interval_300ms)),]
    dat$keypressed <- 0
    
    # Create tuple of coordinates and check whether a key (i.e., to change direction of polygon) was pressed
    for (eachrow in 1:nrow(dat)) {
      dat$tuple_coord[eachrow] <- list(c(round(as.numeric(dat$xcoord[eachrow])), round(as.numeric(dat$ycoord[eachrow]))))
      dat$diff[eachrow] <- abs(dat$current_angle[eachrow] - ifelse(eachrow == 1, dat$current_angle[eachrow], dat$current_angle[eachrow-1]))
      if (abs(dat$current_angle[eachrow] - ifelse(eachrow == 1, dat$current_angle[eachrow], dat$current_angle[eachrow-1])) > 0) {
        dat$keypressed[eachrow] <- 1
      }
    }
    
    # Identify start of 300ms time windows
    filterStart <- which(dat$interval_300ms == "START")
    dat$turn_angle <- NA
    dat$total_avg_turned <- NA
    
    for (eachvalue in 1:(length(filterStart))) {
      # Total_avg_turned calculated as current_angle of last index of time window - current_angle of  first index of time window
      total_avg_turned <- abs((((subset(dat, select = current_angle)[ifelse(filterStart[eachvalue] == max(filterStart), max(nrow(dat)), filterStart[eachvalue+1]-1),] -
                          subset(dat, select = current_angle)[filterStart[eachvalue],]) + 180) %% 360) - 180)
      
      # Turn_angle calculated as total amount of keypresses in time window * 35 degrees
      turn_angle <- sum(subset(dat, select = keypressed)[filterStart[eachvalue]:ifelse(filterStart[eachvalue] == max(filterStart), max(nrow(dat)), filterStart[eachvalue+1]-1),] == 1)*35
      length_turn_angle <- length(subset(dat, select = keypressed)[filterStart[eachvalue]:ifelse(filterStart[eachvalue] == max(filterStart), max(nrow(dat)), filterStart[eachvalue+1]-1),])
      
      for (eachnum in 1:length_turn_angle) {
        # Bind values to the main dataframe.
        dat$turn_angle[filterStart[eachvalue]:ifelse(filterStart[eachvalue] == max(filterStart), max(nrow(dat)), filterStart[eachvalue+1]-1)] <- turn_angle
        dat$total_avg_turned[filterStart[eachvalue]:ifelse(filterStart[eachvalue] == max(filterStart), max(nrow(dat)), filterStart[eachvalue+1]-1)] <- total_avg_turned
      }
    }
    
    # Identify start of 300ms time windows when resource encountered
    resourceStart <- which(dat$resource_encountered == "true")
    
    # Create vector with -5 values. These are subsequently replaced by max_values.
    dat$max_value <- -5
    # Check maximum turning angle per 200 ms time window after encountering a resource (Hills, Todd, & Goldstone, 2010)
    # Question: Why 200 ms? What's the logic here?
    for (eachvalue in 1:length(resourceStart)) {
      resourceIntervalStart <- dat$timespent[resourceStart[eachvalue]]
      timeStartResources <- which(dat$timespent - resourceIntervalStart <= 0.2 & dat$timespent - resourceIntervalStart > 0)
      for (eachnum in 1:length(timeStartResources)) {
        # Note: It's possible that more than one resource is encountered within a certain time window. This means that the time windows,
        # including their turning angles, can overlap. Take this into account when calculating these max_values.
        dat$max_value[timeStartResources[eachnum]] <- ifelse(dat$max_value[timeStartResources[eachnum]] >= (sum(subset(dat, select = keypressed)[resourceStart[eachvalue]:ifelse(resourceStart[eachvalue] == nrow(dat), resourceStart[eachvalue], tail(timeStartResources, 1)),] == 1)*35), 
               dat$max_value[timeStartResources[eachnum]], sum(subset(dat, select = keypressed)[resourceStart[eachvalue]:tail(timeStartResources, 1),] == 1)*35)
      }
    }
    
    # Replace remaining -5 with NA
    dat$max_value[dat$max_value == -5] <- NA
    
    # Total area explored per participant
    area_visited <- (100/(200*200)) * length(unique(dat$tuple_coord))
    
    # Prepare dataframe to be exported 
    # Note: total_turn_angle (here) and turn_angle (see analysisR.R) do not align. Latter also includes turns during pre-trial interval.
    # Note: total_avg_turned (here) and total_avg_turned (see analysisR.R) do not always align, since the windows of timing do not align.
    # More specifically, self.agent = Agent(), which creates the variable last_angle_timestamp, is called before the experiment is started.
    # This means that timer() - self.agent.last_angle_timestamp > 0.3 evaluates true on the first iteration (see visual_foraging.py)!
    frame_to_export <- data.frame(dat$subjectID[1], dat$expStartTime[1], dat$condition[1], dat$trial_num[1], max(dat$timespent) - min(dat$timespent), max(dat$food_eaten),
                                  sum(subset(dat, interval_300ms == "START")$turn_angle, na.rm = T), sum(subset(dat, resource_encountered == "true")$max_value, na.rm = T),
                                  mean(subset(dat, resource_encountered == "true")$max_value, na.rm = T), 
                                  sum(subset(dat, interval_300ms == "START")$total_avg_turned, na.rm = T), nrow(subset(dat, keypressed == 1)), area_visited)[1:1,]
    names(frame_to_export) <- c("subjectID", "expStartTime", "condition", "trial_num", "timespent", "food_eaten",
                                "total_turn_angle", "angle_after_food", "mean_angle_after_food", "total_avg_turned", "keypresses", "areavisited")
    
    # If file already exists, append new data to dataframe. Results in one file per participant, containing multiple trials.
    write.table(frame_to_export, file = '../Processeddata/ERC_WP3_Year1_Study1_visual_foraging_processed_array_data.txt', 
                append = TRUE, row.names = FALSE, col.names = ifelse(length(list.files('../Processeddata/', pattern = '_processed_array_data', full.names = T)) > 0, FALSE, TRUE))
  }
}
pb1$stop()
file.remove(list.files('./Temporary', pattern = 'visual_foraging_path_', full.names = T))

# Clean up workspace.
rm(area_visited, eachnum, eachrow, eachvalue, filterStart, keypresses, length_dat, max_val, pb1, f, frame_to_export, dat, all.datafiles,
   length_turn_angle, nonprocesseddata, processed.data, processed.files, processedfile, total_avg_turned, turn_angle, timeStartResources,
   resourceIntervalStart, resourceStart, select.files)

# Recreate foraging environment ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
all.datafiles <- list.files('../Rawdata/Foraging/', pattern = '_visual_foraging_mapSurface_trial_', full.names = T)
processed.files <- list.files('../Processeddata', pattern = 'visual_foraging_mapSurface_recreated_trial_', full.names = T)
if (length(processed.files) == 0) {
  nonprocesseddata <- rep(TRUE, length(all.datafiles))
} else {
  processed.data <- sprintf(paste0(str_sub(processed.files, 39, 42), '_',  str_sub(processed.files, 44, 57), 
                                   '_visual_foraging_mapSurface_trial_', str_sub(processed.files, -5, -5)))
  nonprocesseddata <- str_detect(all.datafiles, pattern = processed.data) == FALSE
}
for (f in all.datafiles[nonprocesseddata]) {
  file.copy(f, './Temporary') 
}
shell("recreateMap.py")

files <- list.files('../Processeddata', pattern = '_visual_foraging_mapSurface_recreated_trial_', full.names = T)
par(mfrow = c(2,3), pty = 's')
pdf('../Output/foraging_environments_by_participant.pdf', paper = "USr", width = 9.3, height = 9.3)
par(mfrow = c(2,3), pty = 's')
numplots = 0

# Recreate map surface for each participant on each trial and add them to a pdf file
pb1 <- progress_estimated(length(files))
for (currentFile in files) {
  pb1$tick()$print()
  numplots = numplots + 1
  dat <- read.csv2(currentFile, sep = ',', stringsAsFactors = FALSE, header = FALSE)
  dat <-t(dat[nrow(dat):1,])
  dat <- t(dat)[ncol(dat):1,nrow(dat):1]
  colnames(dat) <- paste("col", 1:200, sep = "")
  dat <- as.matrix(dat)
  image(dat, col = c("black", "green"), xaxt = "n", yaxt = "n") # Remove axes labels. 
  axis(1, seq(0, 1, by = 0.25), labels = c(0, 50, 100, 150, 200)) # Create our own x-label 
  axis(2, seq(0, 1, by = 0.25), labels = c(200, 150, 100, 50, 0)) # Create ouw own y-label
  title(paste0("Participant: ", str_match(currentFile, '([0-9]+)([0-9]+)')[[1]], " Trial: ", str_sub(currentFile, -5, -5), "\nCondition: ", ifelse(as.numeric(str_match(currentFile, '([0-9]+)([0-9]+)')[[1]]) %% 2 == 1, "Clumpy", "Diffuse")))
  
  if (numplots == 5) {
    # Add an empty plot at the end of each page in the PDF file
    plot.new()
    numplots = 0
  }
}
pb1$stop()
dev.off()
file.remove(list.files('./Temporary/', pattern = '_visual_foraging_', full.names = T))

# Clean up workspace
rm(dat, all.datafiles, currentFile, files, nonprocesseddata, numplots, pb1, processed.data, processed.files, f, i)

# Create movie frames ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
all.datafiles <- list.files('../Processeddata/', pattern = 'visual_foraging_path_array', full.names = T)
processed.files <- list.files('../Output', pattern = 'video-participant-', full.names = T)
if (length(processed.files) == 0) {
  nonprocesseddata <- rep(TRUE, length(all.datafiles))
} else {
  processed.data <- sprintf(paste0(str_sub(processed.files, 29, 32), '_', str_sub(processed.files, 34, 40),
                                   'trial', str_sub(processed.files, -6, -6)))
  nonprocesseddata <- str_detect(all.datafiles, pattern = processed.data) == FALSE
}
for (f in all.datafiles[nonprocesseddata]) {
  file.copy(f, './Temporary') 
}
select.files <- list.files('./Temporary', pattern = 'visual_foraging_path_array', full.names = T)
pb1 <- progress_estimated(length(select.files))
for (f in select.files) {
  dat <- read.csv2(f, sep = ',', stringsAsFactors = FALSE)
  dat$xcoord <- as.numeric(dat$xcoord)
  dat$ycoord <- as.numeric(dat$ycoord)
  if (str_detect(f, 'practice')) {
    # Create frames for movie
    for (i in seq(1, length(dat$collision_encountered), 1000)) {
      png(filename = sprintf(paste0('./Temporary/ERC_WP3_Year1_Study1_', str_match(f, '([0-9]+)([0-9]+)')[[1]], '_', 
                                     str_match(f, '([0-9]+)([0-9]+)([0-9]+)([0-9]+)([0-9]+)')[[1]], 
                                     '_foraging_path_practice', '_%05d.png'), i))
      plot(dat$xcoord[1:i], dat$ycoord[1:i], col = ifelse(dat$collision_encountered=="true", "green", "grey"), lwd = 0.1, ylim =c(200,0), xlim = c(0,200))
      dev.off()
    }
  }
  else {
    # Create frames for movie
    for (i in seq(1, length(dat$resource_encountered), 100)) {
      png(filename = sprintf(paste0('./Temporary/ERC_WP3_Year1_Study1_', str_match(f, '([0-9]+)([0-9]+)')[[1]], '_', 
                                       str_match(f, '([0-9]+)([0-9]+)([0-9]+)([0-9]+)([0-9]+)')[[1]], 
                                      '_foraging_path_trial_', str_sub(f, -5, -5), '_%05d.png'), i), width = 1000, height = 1000)
      
      plotdat <- dat[1:i,]
      foragingplot <- ggplot(plotdat, aes(xcoord, ycoord)) + geom_point(color = ifelse(plotdat$resource_encountered=="true" | plotdat$resource_encountered_here_before=="true", "green3", "gray5"), 
                                                                    size = ifelse(plotdat$resource_encountered=="true" | plotdat$resource_encountered_here_before=="true", 4.5, 2.75)) + 
        scale_y_reverse(expand = c(0,0), lim = c(200, 0)) + coord_cartesian(xlim = c(0, 200)) +
        labs(x = "X Position", y = "Y Position") + coord_fixed() +# scale_shape_identity() + geom_point(data = plotdat, mapping = aes(x = xcoord[i], y = ycoord[i], shape = 61), size = 8, fill = "red") +
        #annotate("text", x = dat$xcoord[i], y = dat$ycoord[i], label = "^", angle = dat$current_angle[i], size = 17.5) +
        annotate("text", x = 5, y = 5, vjust = 1, hjust = 0, label = max(plotdat$food_eaten), colour = "grey5", size = 22.5) +
        annotate("text", x = 195, y = 5, vjust = 1, hjust = 1, label = min(plotdat$timesteps_left), colour = "grey5", size = 22.5) +
        theme_bw() + theme(axis.line = element_line(colour = "black"), 
                           panel.grid.major = element_blank(), 
                           panel.grid.minor = element_blank(),
                           panel.background = element_rect(fill = "white"),
                           plot.margin = (unit(c(.75, .75, .75, .75), "cm")),
                           axis.text.x=element_text(size = 35, color = "black"), axis.text.y = element_text(size = 35, hjust = 1, color = "black"), 
                           axis.ticks.x = element_line(size = 1, color = "black")) +theme(axis.title = element_text(size = 35)) +
        scale_x_continuous(expand = c(0,0), limits = c(0, 200)) + ggtitle(paste0('Foraging path participant ', str_sub(f, 34, 37), ' trial ', str_sub(f, -5, -5)), '\n') +
        theme(plot.title = element_text(size = 45))
      print(foragingplot)
      dev.off()
    }
  }
  pb1$tick()$print()
}
pb1$stop()

# Remove temporary folder
unlink('./Temporary', recursive = TRUE) 


# Clear R environment
remove(list = ls())


