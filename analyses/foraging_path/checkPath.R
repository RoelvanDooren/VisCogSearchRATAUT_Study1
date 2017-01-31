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
all.datafiles <- list.files('../../output', pattern = 'visual_foraging_path_trial', full.names = T, recursive = T)
file.copy(all.datafiles, './')

# Create array for each path
shell("readdata.py")
file.remove(list.files(pattern = 'visual_foraging_path_trial', full.names = T))
select.psychopy <- list.files(pattern = 'visual_foraging_path_array', full.names = T)

for (f in select.psychopy) {
  dat <- read.csv2(f, sep = ',', stringsAsFactors = FALSE)
  if (str_detect(f, 'practice')) {
    #plot(dat$xcoord, dat$ycoord, col = ifelse(dat$collision_encountered=="true", "green", "grey"), lwd = 0.1, ylim =c(200,0))
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
    plot(dat$xcoord, dat$ycoord, col = ifelse(dat$resource_encountered=="true" | dat$resource_encountered_here_before=="true", "green", "red"), lwd = ifelse(dat$resource_encountered=="true" | dat$resource_encountered_here_before=="true", 10, 1), ylim =c(200,0), xlim = c(0,200))
    # Create frames for movie
    for (i in seq(1, length(dat$resource_encountered), 100)) {
      jpeg(filename = sprintf(paste0('ERC_WP3_Year1_Study1_', str_match(f, '([0-9]+)([0-9]+)')[[1]], '_', 
                                       str_match(f, '([0-9]+)([0-9]+)([0-9]+)([0-9]+)([0-9]+)')[[1]], 
                                      '_foraging_path_trial_', str_sub(f, -5, -5), '_%05d.jpg'), i))
      plot(dat$xcoord[1:i], dat$ycoord[1:i], col = ifelse(dat$resource_encountered=="true" | dat$resource_encountered_here_before=="true", "green", "red"), lwd = ifelse(dat$resource_encountered=="true" | dat$resource_encountered_here_before=="true", 10, 0.1), ylim =c(200,0), xlim = c(0,200))
      text(192.5, 7.5,labels = dat$timesteps_left[i], cex = 1.75)
      text(7.5, 7.5, labels = dat$food_eaten[i], cex = 1.75)
      dev.off()
    }
  }
}

# Remove copied .txt files
file.remove(list.files(pattern = 'visual_foraging_path_array', full.names = T))

# Clear R environment
remove(list = ls())

#try(system("C:\\cygwin\\bin\\bash C:/Users/Roel/Desktop/hills_replication/analyses/foraging_path/bash_foraging_path.sh", intern = T, wait = T))
#print(.Platform$OS.type)

