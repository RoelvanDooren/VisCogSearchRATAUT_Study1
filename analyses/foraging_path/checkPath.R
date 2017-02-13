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

dat <- read.csv2('ERC_WP3_Year1_Study1_0001_20170201104632_visual_foraging_path_array_trial_1.txt', sep = ',', stringsAsFactors = FALSE)
plot(dat$xcoord, dat$ycoord,
     #type = ifelse(dat$resource_encountered=="true" | dat$resource_encountered_here_before=="true", "o", "o"),
     col = ifelse(dat$resource_encountered=="true" | dat$resource_encountered_here_before=="true", "green", "grey"),  
     lwd = ifelse(dat$resource_encountered=="true" | dat$resource_encountered_here_before=="true", 1, 1), 
     xlab = "X coordinate",
     ylab = "Y coordinate",
     ylim =c(200,0), xlim = c(0,200))

# For testing purposes, can be removed afterwards
dat$xcoord <- as.numeric(dat$xcoord)
dat$ycoord <- as.numeric(dat$ycoord)
ggplot(dat, aes(xcoord, ycoord)) + geom_point(color = ifelse(dat$resource_encountered=="true" | dat$resource_encountered_here_before=="true", "green3", "gray5"), 
  size = ifelse(dat$resource_encountered=="true" | dat$resource_encountered_here_before=="true", 2.5, 0.75)) + 
  scale_y_reverse(expand = c(0,0), lim = c(200, 0)) + coord_cartesian(xlim = c(0, 200)) +
  labs(x = "X Position", y = "Y Position") + coord_fixed() + #scale_shape_identity() + geom_point(data = dat, mapping = aes(x = xcoord[7407], y = ycoord[7407], shape = 61), size = 3, fill = "red") + 
  annotate("text", x = dat$xcoord[7407], y = dat$ycoord[7407], label = "^", angle = dat$current_angle[7407], size = 7.5) +
  annotate("text", x = 5, y = 5, vjust = 1, hjust = 0, label = max(dat$food_eaten), colour = "grey5", size = 7.5) +
  annotate("text", x = 195, y = 5, vjust = 1, hjust = 1, label = max(dat$timesteps_left), colour = "grey5", size = 7.5) +
  theme_bw() + theme(axis.line = element_line(colour = "black"), 
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(fill = "white"),
                     plot.margin = (unit(c(.75, .75, .75, .75), "cm")),
                     axis.text.x=element_text(size = 15, color = "black"), axis.text.y = element_text(size = 15, hjust = 1, color = "black"), 
                     axis.ticks.x = element_line(size = 1, color = "black")) +theme(axis.title = element_text(size = 15)) +
  scale_x_continuous(expand = c(0,0), limits = c(0, 200))



for (f in select.psychopy) {
  dat <- read.csv2(f, sep = ',', stringsAsFactors = FALSE)
  dat$xcoord <- as.numeric(dat$xcoord)
  dat$ycoord <- as.numeric(dat$ycoord)
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


