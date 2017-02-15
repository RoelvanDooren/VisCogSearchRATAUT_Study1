# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
### PhD Metacontrol: Leiden University.
### Year 1, study 1: Visual and Cognitive Search and the RAT and AUT.
### Last adjustment on: 2017-02-14.
### r.van.dooren@fsw.leidenuniv.nl

# Import libraries ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("ggplot2", "stringr", "dplyr", "tidyr")
ipak(packages)

# Set the current working directory by clicking "session" and "set working directory" to "source file location".
# Select datafiles
all.datafiles <- list.files('../Rawdata/Foraging', pattern = 'visual_foraging_path_trial', full.names = T, recursive = T)
file.copy(all.datafiles, './')
shell("transposePath.py")
file.remove(list.files(pattern = 'visual_foraging_path_trial', full.names = T))
select.psychopy <- list.files(pattern = 'visual_foraging_path_array', full.names = T)

# Calculate turning behaviour per 300 ms interval
dat <- read.csv2('ERC_WP3_Year1_Study1_0003_20165623222324_visual_foraging_path_array_trial_1.txt', sep = ",", stringsAsFactors = F)
dat$timespent <- as.numeric(dat$timespent)
dat <- subset(dat, timespent >= 0)
filterStart <- which(dat$interval_300ms == "START")

# Calculate the turning angle per 300 ms, create tuple of coordinates, and check whether a button was pressed
for (eachrow in 1:nrow(dat)) {
  dat$turning_angle_interval[eachrow] <- ifelse(eachrow %in% filterStart, 0, abs((((dat$current_angle[eachrow]-dat$last_angle[eachrow]) + 180) %% 360) - 180))
  dat$tuple_coord[eachrow] <- list(c(as.integer(dat$xcoord[eachrow]), as.integer(dat$ycoord[eachrow])))
  if (eachrow >= 2) {
    if (dat$turning_angle_interval[eachrow] - dat$turning_angle_interval[eachrow-1] > 0) {
      dat$keypressed[eachrow] <- 1
    }
    else {
      dat$keypressed[eachrow] <- 0
    }
  }
}

# Check turning behaviour after resource encounter
for (eachvalue in 1:(length(filterStart)-1)) {
  #max_values <- cbind(max_values, max(subset(dat, select = turning_angle_interval)[filterStart[eachvalue]:filterStart[eachvalue+1],]))
  max_val = max(subset(dat, select = turning_angle_interval)[filterStart[eachvalue]:filterStart[eachvalue+1],])
  length_dat <- length(subset(dat, select = turning_angle_interval)[filterStart[eachvalue]:filterStart[eachvalue+1],])
  for (eachnum in 1:length_dat) {
    dat$max_value[filterStart[eachvalue]:filterStart[eachvalue+1]] <- max_val
  }
}

df_turn_resources <- subset(dat, resource_encountered == "true")
mean(df_turn_resources$max_value)

# Total amount of keypresses per participant
keypresses <- nrow(subset(dat, keypressed == 1))

# Total area explored per participant
area_visited <- (100/(200*200)) * length(unique(dat$tuple_coord))

# Create dataframe to merge with visual_foraging.txt files of participants
frame_to_export <- data.frame(dat$subjectID[1], dat$expStartTime[1], dat$condition[1], dat$trial_num[1], mean(df_turn_resources$max_value), keypresses, area_visited)[1:1,]
names(frame_to_export) <- c("subjectID", "expStartTime", "condition", "trial_num", "mean_turning_after_encounter", "keypresses", "areavisited")

# Create movie frames ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
for (f in select.psychopy) {
  dat <- read.csv2(f, sep = ',', stringsAsFactors = FALSE)
  dat$xcoord <- as.numeric(dat$xcoord)
  dat$ycoord <- as.numeric(dat$ycoord)
  if (str_detect(f, 'practice')) {
    # Create frames for movie
    for (i in seq(1, length(dat$collision_encountered), 1000)) {
      jpeg(filename = sprintf(paste0('ERC_WP3_Year1_Study1_', str_match(f, '([0-9]+)([0-9]+)')[[1]], '_', 
                                     str_match(f, '([0-9]+)([0-9]+)([0-9]+)([0-9]+)([0-9]+)')[[1]], 
                                     '_foraging_path_practice', '_%05d.jpg'), i))
      plot(dat$xcoord[1:i], dat$ycoord[1:i], col = ifelse(dat$collision_encountered=="true", "green", "grey"), lwd = 0.1, ylim =c(200,0), xlim = c(0,200))
      dev.off()
    }
  }
  else {
    # Create frames for movie
    for (i in seq(1, length(dat$resource_encountered), 1000)) {
      png(filename = sprintf(paste0('ERC_WP3_Year1_Study1_', str_match(f, '([0-9]+)([0-9]+)')[[1]], '_', 
                                       str_match(f, '([0-9]+)([0-9]+)([0-9]+)([0-9]+)([0-9]+)')[[1]], 
                                      '_foraging_path_trial_', str_sub(f, -5, -5), '_%05d.png'), i), width = 1000, height = 1000)
      
      plotdat <- dat[1:i,]
      test <- ggplot(plotdat, aes(xcoord, ycoord)) + geom_point(color = ifelse(plotdat$resource_encountered=="true" | plotdat$resource_encountered_here_before=="true", "green3", "gray5"), 
                                                                    size = ifelse(plotdat$resource_encountered=="true" | plotdat$resource_encountered_here_before=="true", 4.5, 2.75)) + 
        scale_y_reverse(expand = c(0,0), lim = c(200, 0)) + coord_cartesian(xlim = c(0, 200)) +
        labs(x = "X Position", y = "Y Position") + coord_fixed() +# scale_shape_identity() + geom_point(data = plotdat, mapping = aes(x = xcoord[i], y = ycoord[i], shape = 61), size = 8, fill = "red") +
        annotate("text", x = dat$xcoord[i], y = dat$ycoord[i], label = "^", angle = dat$current_angle[i], size = 17.5) +
        annotate("text", x = 5, y = 5, vjust = 1, hjust = 0, label = max(plotdat$food_eaten), colour = "grey5", size = 22.5) +
        annotate("text", x = 195, y = 5, vjust = 1, hjust = 1, label = min(plotdat$timesteps_left), colour = "grey5", size = 22.5) +
        theme_bw() + theme(axis.line = element_line(colour = "black"), 
                           panel.grid.major = element_blank(), 
                           panel.grid.minor = element_blank(),
                           panel.background = element_rect(fill = "white"),
                           plot.margin = (unit(c(.75, .75, .75, .75), "cm")),
                           axis.text.x=element_text(size = 35, color = "black"), axis.text.y = element_text(size = 35, hjust = 1, color = "black"), 
                           axis.ticks.x = element_line(size = 1, color = "black")) +theme(axis.title = element_text(size = 35)) +
        scale_x_continuous(expand = c(0,0), limits = c(0, 200))
      print(test)
      dev.off()
    }
  }
}

# Remove copied .txt files
file.remove(list.files(pattern = 'visual_foraging_path_array', full.names = T))

# Clear R environment
remove(list = ls())


