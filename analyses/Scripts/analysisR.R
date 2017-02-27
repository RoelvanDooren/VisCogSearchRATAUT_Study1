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

packages <- c("stringr", "dplyr", "tidyr", "ggplot2", "plyr", "Rmisc", "boot", "plotrix", "grid", "stringr")
ipak(packages)

# Remove outliers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Based on Tukey's test. Question: Did Hills report this as well?
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# Calculate bigrams ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
calculateBigrams = function(dataframe, current_word, N_word) {
  #stringsim(penultimate_word, previous_word, method = "lv") # Levenshtein distance, also see Hills, Todd, & Goldstone (2010) page 597.
  bigram = 0
  for (eachindex in 1:ifelse(current_word == "", 1, nchar(unlist(strsplit(current_word, " "))))) {
    if (startsWith(N_word, paste(unlist(strsplit(current_word, ""))[1:eachindex], collapse = "")) == TRUE) {
      bigram = bigram + 1
    } 
    if (startsWith(paste(rev(unlist(strsplit(N_word, ""))), collapse = ""), 
                   paste(rev(unlist(strsplit(current_word, "")))[1:eachindex], collapse = "")) == TRUE) {
      bigram = bigram + 1
    }
  }
  return(bigram)
}

# Copy datafiles ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
scrabblefiles <- list.files('../../output', pattern = '_scrabble_', full.names = T)
foragingfiles <- list.files('../../output', pattern = '_visual_foraging', full.names = T)
autfiles <- list.files('../../output', pattern = '_alternate_uses_', full.names = T)
ratfiles <- list.files('../../output', pattern = '_remote_association_', full.names = T)
file.copy(scrabblefiles, '../Rawdata/Scrabble/')
file.copy(foragingfiles, '../Rawdata/Foraging')
file.copy(autfiles, '../Rawdata/AUT')
file.copy(ratfiles, '../Rawdata/RAT')

# Clean up workspace
rm(scrabblefiles, foragingfiles, autfiles, ratfiles)

# Recreate foraging paths and mapsurfaces ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Note: For movie creation, bash_foraging_path should subsequently be executed 
source("recreatePathR.R") 

# Load datafiles ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Foraging data
foraging_df <- data.frame()
files <- list.files("../Rawdata/Foraging/", pattern="visual_foraging.txt", full.names=TRUE)
for (currentFile in files) {
  foraging_df <- rbind(foraging_df,read.csv2(currentFile, sep=',', header=TRUE, stringsAsFactors = TRUE))
}

foraging_df <- foraging_df[,1:4]
foraging_processed_array <- read.csv2('../Processeddata/ERC_WP3_Year1_Study1_visual_foraging_processed_array_data.txt', sep = ' ', header = TRUE, stringsAsFactors = TRUE)
foraging_df <- merge(foraging_df, foraging_processed_array, by = c("expStartTime","subjectID", "condition", "trial_num"))

# Why should this be removed?
#foraging_df <- subset(foraging_df, angle_after_food > 1 & angle_after_food < 90000)

# Remove outliers foraging data
foraging_df$mean_angle_after_food <- as.numeric(gsub(",", ".", foraging_df$mean_angle_after_food))
foraging_df$areavisited <- as.numeric(gsub(",", ".", foraging_df$areavisited))
foraging_df[foraging_df$condition=="c",]$total_turn_angle <- remove_outliers(foraging_df[foraging_df$condition=="c",]$total_turn_angle)
foraging_df[foraging_df$condition=="d",]$total_turn_angle <- remove_outliers(foraging_df[foraging_df$condition=="d",]$total_turn_angle)
foraging_df[foraging_df$condition=="c",]$angle_after_food <- remove_outliers(foraging_df[foraging_df$condition=="c",]$angle_after_food)
foraging_df[foraging_df$condition=="d",]$angle_after_food <- remove_outliers(foraging_df[foraging_df$condition=="d",]$angle_after_food)
foraging_df[foraging_df$condition=="c",]$mean_angle_after_food <- remove_outliers(foraging_df[foraging_df$condition=="c",]$mean_angle_after_food)
foraging_df[foraging_df$condition=="d",]$mean_angle_after_food <- remove_outliers(foraging_df[foraging_df$condition=="d",]$mean_angle_after_food)

# Df containing mean foraging data
cdata_foraging <- ddply(foraging_df, c("subjectID", "condition"), summarise, mean_turn_angle = mean(total_turn_angle, na.rm = T),
                        sd_turn_angle = sd(total_turn_angle, na.rm = T),
                        mean_resource_encounter = mean(food_eaten, na.rm = T),
                        mean_angle_per_food = mean(mean_angle_after_food, na.rm = T),
                        mean_keypresses = mean(keypresses, na.rm = T),
                        mean_areaexplored = mean(areavisited, na.rm = T))

# Scrabble pre-test data
pre_scrab_df <- data.frame()
files <- list.files("../Rawdata/Scrabble", pattern="scrabble_pretest.txt", full.names=TRUE)
for (currentFile in files) {
  pre_scrab_df <- rbind(pre_scrab_df,read.csv2(currentFile, fileEncoding="UTF-8-BOM", dec='.', sep=';', header=TRUE,
                                               colClasses=c("character", "integer", "character","integer","character","numeric","numeric","numeric","integer","character", "character" ,"integer","character", "character")))
}

# Remove outliers scrabble pre-test data
pre_scrab_df[pre_scrab_df[,'time_in_set']<3  ,'time_in_set'] <- NA # Did Hills specify something alike?
pre_scrab_df$time_in_set <- remove_outliers(pre_scrab_df$time_in_set)

# Df containing mean scrabble pre-test data
cdata_pre_scrab <- ddply(pre_scrab_df, c("subjectID", "condition"), summarise,
                         mean_pre = mean(time_in_set, na.rm=TRUE),
                         median_pre = median(time_in_set, na.rm=TRUE))

# Scrabble post-test data
post_scrab_df <- data.frame()
files <- list.files("../Rawdata/Scrabble", pattern="scrabble_posttest.txt", full.names=TRUE)
for (currentFile in files) {
  post_scrab_df <- rbind(post_scrab_df,read.csv2(currentFile, fileEncoding="UTF-8-BOM", dec='.', sep=';', header=TRUE, 
                                                 colClasses=c("character", "integer", "character","integer","character","numeric","numeric","numeric","integer","character", "character" ,"integer","character", "character")))
}

# Remove outliers scrabble post-test data
post_scrab_df[post_scrab_df[,'time_in_set']<3  ,'time_in_set'] <- NA # Did Hills specify something alike?
post_scrab_df$time_in_set <- remove_outliers(post_scrab_df$time_in_set)

# Df containing mean scrabble post-test data
cdata_post_scrab <- ddply(post_scrab_df, c("subjectID", "condition"), summarise,
                          mean_post = mean(time_in_set, na.rm=TRUE),
                          median_post = median(time_in_set, na.rm=TRUE))


# Alternate Uses Task data
aut_df <- data.frame()
files <- list.files("../Rawdata/AUT/", pattern="alternate_uses_task.txt", recursive=TRUE, full.names=TRUE)
for (currentFile in files) {
  aut_df <- rbind(aut_df,read.csv2(currentFile, header=TRUE, fileEncoding="UTF-8-BOM", dec='.', na.strings=c("NA", ""),
                                   colClasses=c("factor", "integer", "factor", "factor", "factor", "numeric", "factor", "numeric", "numeric", "numeric", "numeric", "factor", "numeric", "factor")))
}

# Remove outliers Alternate Uses Task data
aut_df$responsetime <- remove_outliers(aut_df$responsetime)

# Remote Association Task data
rat_df <- data.frame()
files <- list.files("../Rawdata/RAT/", pattern="remote_association_task.txt", recursive=TRUE, full.names=TRUE)
for (currentFile in files) {
  rat_df <- rbind(rat_df,read.csv2(currentFile, header=TRUE, fileEncoding="UTF-8-BOM", dec='.', na.strings=c("NA", ""),
                                   colClasses=c("factor", "integer", "factor", "factor", "numeric", "factor", "numeric", "numeric", "numeric", "factor", "numeric", "factor", "numeric", "numeric", "numeric", "numeric")))
}

# Remove outliers Alternate Uses Task data
rat_df$setresponsetime <- remove_outliers(rat_df$setresponsetime)
rat_df$optionresponsetime <- remove_outliers(rat_df$optionresponsetime)

# Clean up workspace
rm(foraging_processed_array)

# Main analyses ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Replication of all the analyses reported in Hills, Todd, & Goldstone (2008).

# is there an effect of average turning behaviour within 200 ms after resource encounter? Hills says YES
t.test(mean_angle_per_food ~ condition, data = cdata_foraging)
# Report output here.

# Create figure for in paper
cdata_foraging$condition <- factor(cdata_foraging$condition, levels = c("c","d"), labels = c("Clumpy","Diffuse"))
ggplot(cdata_foraging, aes(condition, mean_angle_per_food, fill = mean_angle_per_food)) + 
      stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
      labs(y = "Turn angle after food", x = "Condition") +
      coord_cartesian(ylim = c(0, 40)) + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
      position = position_dodge(width = 0.9), width = 0.2) +
      scale_y_continuous(expand = c(0,0)) + #, limits = c(0, 40)) +
      theme(axis.text.x = element_text(colour = 'black') , axis.text.y = element_text(colour = 'black'), 
      axis.line.x = element_line(), axis.line.y = element_line(),      
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
      panel.border = element_blank(), panel.background = element_blank())

# Do the groups differ on time spent in lettersets during pretest? Hills says NO
t.test(mean_pre ~ condition, data = cdata_pre_scrab)
# Report output here.

# Is there an effect of condition on the difference between pre- and post? Hills says YES
cdata_pre_scrab[,'mean_post'] <- cdata_post_scrab[,'mean_post']
cdata_pre_scrab[,'mean_diff'] <- cdata_pre_scrab[,'mean_post'] - cdata_pre_scrab[,'mean_pre'] 
cdata_pre_scrab[,'mean_resource_encounter'] <- cdata_foraging[,'mean_resource_encounter']

t.test(mean_diff ~ condition, cdata_pre_scrab)
# Report output here.

# Create figure for in paper
cdata_pre_scrab$condition <- factor(cdata_pre_scrab$condition, levels = c("c","d"), labels = c("Clumpy","Diffuse"))
ggplot(cdata_pre_scrab, aes(condition, mean_diff, fill = mean_diff)) + 
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  labs(y = "Mean differnce in time spent in lettersets", x = "Condition") +
  coord_cartesian(ylim = c(-20, 20)) + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
  position = position_dodge(width = 0.9), width = 0.2) +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.text.x = element_text(colour = 'black') , axis.text.y = element_text(colour = 'black'), 
        axis.line.x = element_line(), axis.line.y = element_line(),      
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank())

# Do the groups differ on time spent in lettersets during posttest? Hills says YES
t.test(mean_post ~ condition, data = cdata_post_scrab)
# Report output here.

# Create figure for in paper
cdata_post_scrab$condition <- factor(cdata_post_scrab$condition, levels = c("c","d"), labels = c("Clumpy","Diffuse"))
ggplot(cdata_post_scrab, aes(condition, mean_post, fill = mean_post)) + 
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  labs(y = "Mean time in lettersets after foraging", x = "Condition") +
  coord_cartesian(ylim = c(0, 100)) + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
  position = position_dodge(width = 0.9), width = 0.2) +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.text.x = element_text(colour = 'black') , axis.text.y = element_text(colour = 'black'), 
        axis.line.x = element_line(), axis.line.y = element_line(),      
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank())


# Do differences in amount of resources encountered explain the priming effects? Hills says NO
lm_resources <- lm(mean_diff ~ mean_resource_encounter, data = cdata_pre_scrab)
summary(lm_resources)
# Report output here.

# To more thoroughly test this possibility, include resources encountered as a covariate in an ANOVA. 
# Hills finds a main effect of condition, but no main effect of mean_resource_encounter nor an interaction.
thorough_resources <- aov(mean_diff ~ condition * mean_resource_encounter, data = cdata_pre_scrab)
summary(thorough_resources)
# Report output here.

# Can arousal (indicated by more keypresses) account for the priming effect? Hills says NO
cdata_pre_scrab[,'mean_keypresses'] <- cdata_foraging[,'mean_keypresses']
lm_keypresses <- lm(mean_diff ~ mean_keypresses, data = cdata_pre_scrab)
summary(lm_keypresses)

# To more thoroughly test this possibility, include keypresses as a covariate in an ANOVA.
# Hills finds ???
thorough_keypresses <- aov(mean_diff ~ condition * mean_keypresses, data = cdata_pre_scrab)
summary(thorough_keypresses)

# Do the mean times to submit (in)correct words in the posttest between conditions vary? Hills says NO
for (eachrow in 1:nrow(post_scrab_df)) {
  total_rt_corwords = 0
  total_rt_incorwords = 0
  rt_corword_n = 0
  rt_incorword_n = 0
  for (i in 1:length(str_replace_all(unlist(strsplit(post_scrab_df$rt_correct_words[eachrow], " ")), "[^[:alnum:].]", ""))) {
    rt_corword_n <-  ifelse(str_replace_all(unlist(strsplit(post_scrab_df$rt_correct_words[eachrow], " ")), "[^[:alnum:].]", "")[i] == "", 0, 
                            as.numeric(gsub(",", ".", str_replace_all(unlist(strsplit(post_scrab_df$rt_correct_words[eachrow], " ")), "[^[:alnum:].]", "")[i])))
    total_rt_corwords = rt_corword_n + total_rt_corwords
  }
  for (i in 1:length(str_replace_all(unlist(strsplit(post_scrab_df$rt_incorrect_words[eachrow], " ")), "[^[:alnum:].]", ""))) {
    rt_incorword_n <- ifelse(str_replace_all(unlist(strsplit(post_scrab_df$rt_incorrect_words[eachrow], " ")), "[^[:alnum:].]", "")[i] == "", 0, 
                             as.numeric(gsub(",", ".", str_replace_all(unlist(strsplit(post_scrab_df$rt_incorrect_words[eachrow], " ")), "[^[:alnum:].]", "")[i])))
    total_rt_incorwords = rt_incorword_n + total_rt_incorwords
  }
  post_scrab_df$mean_rt_corwords[eachrow] <- ifelse(total_rt_corwords == 0, NA, total_rt_corwords/length(str_replace_all(unlist(strsplit(post_scrab_df$rt_correct_words[eachrow], " ")), "[^[:alnum:].]", "")))
  post_scrab_df$mean_rt_incorwords[eachrow] <- ifelse(total_rt_incorwords == 0, NA, total_rt_incorwords/length(str_replace_all(unlist(strsplit(post_scrab_df$rt_incorrect_words[eachrow], " ")), "[^[:alnum:].]", "")))
}

# Df containing post scrabble rt data
cdata_post_scrab_rt <- ddply(post_scrab_df, c("subjectID", "condition"), summarise,
                         mean_rt_cor = mean(mean_rt_corwords, na.rm=TRUE),
                         mean_rt_incor = mean(mean_rt_incorwords, na.rm=TRUE),
                         mean_rt_words = sum(mean_rt_cor + mean_rt_incor) /2 )

cdata_pre_scrab[,'mean_rt_cor'] <- cdata_post_scrab_rt[,'mean_rt_cor']
cdata_pre_scrab[,'mean_rt_incor'] <- cdata_post_scrab_rt[,'mean_rt_incor']
cdata_pre_scrab[,'mean_rt_words'] <- cdata_post_scrab_rt[,'mean_rt_words']

t.test(mean_rt_words ~ condition, data = cdata_pre_scrab)
# Report output here.

# HOWEVER, NOTE THAT THESE DON'T TAKE THE RM NATURE OF THE DATA INTO ACCOUNT.
# RMANOVA WITH LETTERSET SWITCH TIME AS REPEATED. AND SO HOW THAT RELATES TO CONDITION


# Do treatment conditions differ in total area explored? Hills says YES
cdata_pre_scrab[,'mean_areaexplored'] <- cdata_foraging[,'mean_areaexplored']
t.test(mean_areaexplored ~ condition, data = cdata_pre_scrab)

# Do the mean times in letterset depend on proportion explored? Hills says ??
letter_exploration <- aov(mean_diff ~ condition * mean_areaexplored, data = cdata_pre_scrab)
summary(letter_exploration)

# Is there a relation between exploratory/exploitatory behavior in visual and semantic search?
visual_semantic_df <- merge(cdata_pre_scrab, cdata_foraging, by = 'subjectID')

ggplot(visual_semantic_df, aes(x=mean_angle_per_food, y=mean_diff)) +
  geom_point(shape=1)   +
  geom_smooth(method=lm, formula = y ~ x + I(x^2)) +
  ylab("Mean time in Scrabble set") + 
  xlab("Turning behavior in foraging task")

# Check diffuse condition.
visual_semantic_df_d <- subset(visual_semantic_df, condition.x=="d")
reg <- lm(mean_diff ~ poly(mean_angle_per_food, 2), data=visual_semantic_df_d)
summary(reg)
# Report results here

ggplot(visual_semantic_df_d, aes(x=mean_angle_per_food, y=mean_diff)) +
  geom_point(shape=1)   +
  geom_smooth(method=lm, formula = y ~ x + I(x^2)) +
  ylab("Mean time in Scrabble set") + 
  xlab("Turning behavior in diffuse foraging task")

# Check clumpy condition.
visual_semantic_df_c <- subset(visual_semantic_df, condition.x=="c")
reg = lm(mean_diff ~ poly(mean_angle_per_food, 2), data=visual_semantic_df_c)
summary(reg)
# Report results here

ggplot(visual_semantic_df_c, aes(x=mean_angle_per_food, y=mean_diff)) +
  geom_point(shape=1)   +
  geom_smooth(method=lm, formula = y ~ x + I(x^2)) + 
  ylab("Median time in Scrabble set (exploitative)") + 
  xlab("Turning behavior in clustered foraging task")


# Calculate bigrams by calling the calculateBigrams function
for (eachrow in 1:nrow(pre_scrab_df)) {
  bigram_prev = 0
  bigram_penult = 0
  bigram_letterset = 0
  
  for (index in 1:length(str_replace_all(unlist(strsplit(pre_scrab_df$correct_words[eachrow], " ")), "[^[:alnum:]]", ""))) {
    current_word = str_replace_all(unlist(strsplit(pre_scrab_df$correct_words[eachrow], " ")), "[^[:alnum:]]", "")[index]
    previous_word = ifelse(index == 1, "", str_replace_all(unlist(strsplit(pre_scrab_df$correct_words[eachrow], " ")), "[^[:alnum:]]", "")[index-1])
    penultimate_word = ifelse(index == 1 | index == 2, "", str_replace_all(unlist(strsplit(pre_scrab_df$correct_words[eachrow], " ")), "[^[:alnum:]]", "")[index-2])
    letterset <- tolower(gsub(" ", "", pre_scrab_df$letterset[eachrow], fixed = T))
    all_words <- c(previous_word, penultimate_word, letterset)
    
    for (eachword in all_words) {
      bigram <- calculateBigrams(pre_scrab_df, current_word, eachword)
      if (eachword == previous_word) {
        bigram_prev <- bigram_prev + bigram
        pre_scrab_df$bigram_prev[eachrow] <- bigram_prev
      } else if (eachword == penultimate_word) {
        bigram_penult <- bigram_penult + bigram
        pre_scrab_df$bigram_penult[eachrow] <- bigram_penult
      } else {
        bigram_letterset <- bigram_letterset + bigram
        pre_scrab_df$bigram_letterset[eachrow] <- bigram_letterset
      }
    }
  }
}



post_scrab_df$bigram_penult <- 0
# Calculate bigrams by calling the calculateBigrams function
for (eachrow in 1:nrow(post_scrab_df)) {
  bigram_prev = 0
  bigram_penult = 0
  bigram_letterset = 0
  
  for (index in 1:length(str_replace_all(unlist(strsplit(post_scrab_df$correct_words[eachrow], " ")), "[^[:alnum:]]", ""))) {
    current_word = str_replace_all(unlist(strsplit(post_scrab_df$correct_words[eachrow], " ")), "[^[:alnum:]]", "")[index]
    previous_word = ifelse(index == 1, "", str_replace_all(unlist(strsplit(post_scrab_df$correct_words[eachrow], " ")), "[^[:alnum:]]", "")[index-1])
    penultimate_word = ifelse(index == 1 | index == 2, "", str_replace_all(unlist(strsplit(post_scrab_df$correct_words[eachrow], " ")), "[^[:alnum:]]", "")[index-2])
    letterset <- tolower(gsub(" ", "", post_scrab_df$letterset[eachrow], fixed = T))
    all_words <- c(previous_word, penultimate_word, letterset)
    
    for (eachword in all_words) {
      bigram <- calculateBigrams(post_scrab_df, current_word, eachword)
      if (eachword == previous_word) {
        bigram_prev <- bigram_prev + bigram
        post_scrab_df$bigram_prev[eachrow] <- bigram_prev
      } else if (eachword == penultimate_word) {
        bigram_penult <- bigram_penult + bigram
        post_scrab_df$bigram_penult[eachrow] <- bigram_penult
      } else {
        bigram_letterset <- bigram_letterset + bigram
        post_scrab_df$bigram_letterset[eachrow] <- bigram_letterset
      }
    }
  }
}


# Df containing mean scrabble pre-test bigram data
cdata_bigram_pre <- ddply(pre_scrab_df, c("subjectID", "condition"), summarise,
                          mean_prev_pre = mean(bigram_prev),
                          mean_penult_pre = mean(bigram_penult),
                          mean_letterset_pre = mean(bigram_letterset))

# Df containing mean scrabble post-test bigram data
cdata_bigram_post <- ddply(post_scrab_df, c("subjectID", "condition"), summarise,
                           mean_prev_post = mean(bigram_prev, na.rm=TRUE),
                           mean_penult_post = mean(bigram_penult, na.rm=TRUE),
                           mean_letterset_post = mean(bigram_letterset, na.rm=TRUE))

# Do the groups differ on bigram overlap during baseline?
t.test(mean_prev_pre ~ condition, data = cdata_bigram_pre)
t.test(mean_penult_pre ~ condition, data = cdata_bigram_pre)
t.test(mean_letterset_pre ~ condition, data = cdata_bigram_pre)

# Is there an effect of condition on the difference between pre- and post bigrams? Hills says YES
cdata_pre_scrab[,'mean_prev_post'] <- cdata_bigram_post[,'mean_prev_post']
cdata_pre_scrab[,'mean_penult_post'] <- cdata_bigram_post[,'mean_penult_post']
cdata_pre_scrab[,'mean_letterset_post'] <- cdata_bigram_post[,'mean_letterset_post']

cdata_pre_scrab[,'mean_diff_prev'] <- cdata_pre_scrab[,'mean_prev_post'] -cdata_bigram_pre[,'mean_prev_pre']
cdata_pre_scrab[,'mean_diff_penult'] <- cdata_pre_scrab[,'mean_penult_post'] -cdata_bigram_pre[,'mean_penult_pre']
cdata_pre_scrab[,'mean_diff_letterset'] <- cdata_pre_scrab[,'mean_letterset_post'] -cdata_bigram_pre[,'mean_letterset_pre']
t.test(mean_prev_post ~ condition, cdata_pre_scrab)
t.test(mean_penult_post ~ condition, cdata_pre_scrab)
t.test(mean_letterset_post ~ condition, cdata_pre_scrab)
t.test(mean_diff_prev ~ condition, cdata_pre_scrab)
t.test(mean_diff_penult ~ condition, cdata_pre_scrab)
t.test(mean_diff_letterset ~ condition, cdata_pre_scrab)


