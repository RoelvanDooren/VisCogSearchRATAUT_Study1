# Import libraries ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("stringr", "dplyr", "tidyr", "ggplot2", "plyr", "Rmisc", "boot")
ipak(packages)

# Functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Remove outliers based on Tukey's test
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# Check foraging paths ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Call chechPath.R
source("./foraging_path/checkPath.R", chdir = T)
#remove(list = ls())

# Figure out how to call bash script (bash_foraging_path.sh)


# Load and clean datafiles ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Foraging data
foraging_df <- data.frame()
files <- list.files("../output", pattern="visual_foraging.txt", recursive=TRUE, full.names=TRUE)
for (currentFile in files) {
  foraging_df <- rbind(foraging_df,read.csv2(currentFile, sep=',', header=TRUE))
}

foraging_df <- subset(foraging_df, angle_after_food > 1 & angle_after_food < 90000)
foraging_df$angle_per_food <- foraging_df$angle_after_food / foraging_df$food_eaten

# Remove outliers foraging data
foraging_df[foraging_df$condition=="c",]$turn_angle <- remove_outliers(foraging_df[foraging_df$condition=="c",]$turn_angle)
foraging_df[foraging_df$condition=="d",]$turn_angle <- remove_outliers(foraging_df[foraging_df$condition=="d",]$turn_angle)
foraging_df[foraging_df$condition=="c",]$angle_after_food <- remove_outliers(foraging_df[foraging_df$condition=="c",]$angle_after_food)
foraging_df[foraging_df$condition=="d",]$angle_after_food <- remove_outliers(foraging_df[foraging_df$condition=="d",]$angle_after_food)

# Df containing mean foraging data
cdata_foraging <- ddply(foraging_df, c("subjectID", "condition"), summarise, mean = mean(turn_angle, na.rm = T),
                        sd   = sd(turn_angle, na.rm = T), mean_angle_after_food = mean(angle_after_food, na.rm = T),
                        median_angle_after_food = median(angle_after_food, na.rm = T), mean_angle_per_food = mean(angle_per_food, na.rm = T),
                        median_angle_per_food = median(angle_per_food, na.rm = T))

# Scrabble pre-test data
pre_scrab_df <- data.frame()
files <- list.files("../output", pattern="scrabble_pretest.txt", recursive=TRUE, full.names=TRUE)
for (currentFile in files) {
  pre_scrab_df <- rbind(pre_scrab_df,read.csv2(currentFile, fileEncoding="UTF-8-BOM", dec='.', sep=';', header=TRUE,
                                               colClasses=c("factor", "integer", "factor","integer","factor","numeric","numeric","numeric","integer","factor", "factor" ,"integer","factor", "factor")))
}

# Remove outliers scrabble pre-test data
pre_scrab_df[pre_scrab_df[,'time_in_set']<3  ,'time_in_set'] <- NA 
pre_scrab_df$time_in_set <- remove_outliers(pre_scrab_df$time_in_set)

# Df containing mean scrabble pre-test data
cdata_pre_scrab <- ddply(pre_scrab_df, c("subjectID", "condition"), summarise,
                         mean_pre = mean(time_in_set, na.rm=TRUE),
                         median_pre = median(time_in_set, na.rm=TRUE)
)

# Scrabble post-test data
post_scrab_df <- data.frame()
files <- list.files("../output", pattern="scrabble_posttest.txt", recursive=TRUE, full.names=TRUE)
for (currentFile in files) {
  post_scrab_df <- rbind(post_scrab_df,read.csv2(currentFile, fileEncoding="UTF-8-BOM", dec='.', sep=';', header=TRUE, 
                                                 colClasses=c("factor", "integer", "factor","integer","factor","numeric","numeric","numeric","integer","factor", "factor","integer","factor", "factor")))
}

# Remove outliers scrabble post-test data
post_scrab_df[post_scrab_df[,'time_in_set']<3  ,'time_in_set'] <- NA
post_scrab_df$time_in_set <- remove_outliers(post_scrab_df$time_in_set)

# Df containing mean scrabble post-test data
cdata_post_scrab <- ddply(post_scrab_df, c("subjectID", "condition"), summarise,
                          mean_post = mean(time_in_set, na.rm=TRUE),
                          median_post = median(time_in_set, na.rm=TRUE)
)


# Alternate Uses Task data
aut_df <- data.frame()
files <- list.files("../output", pattern="alternate_uses_task.txt", recursive=TRUE, full.names=TRUE)
for (currentFile in files) {
  aut_df <- rbind(aut_df,read.csv2(currentFile, header=TRUE, fileEncoding="UTF-8-BOM", dec='.', na.strings=c("NA", ""),
                                   colClasses=c("factor", "integer", "factor", "factor", "factor", "numeric", "factor", "numeric", "numeric", "numeric", "numeric", "factor", "numeric", "factor")))
}

# Remove outliers Alternate Uses Task data
aut_df$responsetime <- remove_outliers(aut_df$responsetime)


# Remote Association Task data
rat_df <- data.frame()
files <- list.files("../output", pattern="remote_association_task.txt", recursive=TRUE, full.names=TRUE)
for (currentFile in files) {
  rat_df <- rbind(rat_df,read.csv2(currentFile, header=TRUE, fileEncoding="UTF-8-BOM", dec='.', na.strings=c("NA", ""),
                                   colClasses=c("factor", "integer", "factor", "factor", "numeric", "factor", "numeric", "numeric", "numeric", "factor", "numeric", "factor", "numeric", "numeric", "numeric", "numeric")))
}

# Remove outliers Alternate Uses Task data
rat_df$setresponsetime <- remove_outliers(rat_df$setresponsetime)
rat_df$optionresponsetime <- remove_outliers(rat_df$optionresponsetime)


# Main analyses ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Do the groups differ on time_spent during baseline?
t.test(median_pre ~ condition, data = cdata_pre_scrab)
# Report output here.

# Is there an effect of condition on the difference between pre- and post? Hills says YES
cdata_pre_scrab[,'median_post'] <- cdata_post_scrab[,'median_post']
cdata_pre_scrab[,'mean_post'] <- cdata_post_scrab[,'mean_post']
cdata_pre_scrab[,'median_diff'] <- cdata_pre_scrab[,'median_post'] - cdata_pre_scrab[,'median_pre'] 
cdata_pre_scrab[,'mean_diff'] <- cdata_pre_scrab[,'mean_post'] - cdata_pre_scrab[,'mean_pre'] 

t.test(median_diff ~ condition, cdata_pre_scrab)
# Report output here.

# Plot results
means <- summarySE(cdata_pre_scrab, measurevar="median_diff", na.rm=TRUE, groupvars=c("condition"))
ggplot(means, aes(x=condition, y=median_diff, colour=condition, group = condition)) + 
  geom_errorbar(aes(ymin=median_diff-ci, ymax=median_diff+ci), width=.1) +
  geom_point() +
  ylab("Time spent in Scrabble set (exploitative)") + 
  xlab("Foraging condition") + 
  scale_x_discrete(labels=c("clustered", "diffuse")) +
  theme(axis.title.x = element_text(vjust=-0.35), 
        axis.title.y = element_text(vjust=1.15), 
        axis.ticks.x = element_blank(),
        plot.title = element_text(vjust=2), 
        plot.margin = (unit(c(.5, .5, .5, .5), "cm"))
  )

# Is there a relation between exploratory/exploitatory behavior in visual and semantic search?
new_df <- merge(cdata_pre_scrab, cdata_foraging, by = 'subjectID')

# Check diffuse condition.
new_df_d <- subset(new_df, condition.x=="d")
reg = lm(median_diff ~ poly(median_angle_after_food, 2), data=new_df_d)
summary(reg)
# Report results here

ggplot(new_df_d, aes(x=median_angle_after_food, y=median_diff)) +
  geom_point(shape=1)   +
  geom_smooth(method=lm, formula = y ~ x + I(x^2)) +
  ylab("Median time in Scrabble set (exploitative)") + 
  xlab("Turning behavior in diffuse foraging task")

# Check clumpy condition.
new_df_c <- subset(new_df, condition.x=="c")
reg = lm(median_diff ~ poly(median_angle_after_food, 2), data=new_df_c)
summary(reg)
# Report results here

ggplot(new_df_c, aes(x=median_angle_after_food, y=median_diff)) +
  geom_point(shape=1)   +
  geom_smooth(method=lm, formula = y ~ x + I(x^2)) + 
  ylab("Median time in Scrabble set (exploitative)") + 
  xlab("Turning behavior in clustered foraging task")



