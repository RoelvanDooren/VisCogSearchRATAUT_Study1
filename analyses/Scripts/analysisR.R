# PhD Metacontrol: Leiden University --------------------------------------
### Year 1, study 1: Visual and Cognitive Search and the RAT and AUT.
### Last adjustment on: 2017-03-01.
### r.van.dooren@fsw.leidenuniv.nl

# Import libraries --------------------------------------------------------
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = T)
}
packages <- c("stringr", "dplyr", "tidyr", "ggplot2", "plyr", "Rmisc", "reshape2", 
              "boot", "plotrix", "grid", "stringr", "psych", "lme4", "rPython", "car")
ipak(packages)

# Remove outliers ---------------------------------------------------------
# Based on Tukey's test
remove_outliers <- function(x, na.rm = T, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# Calculate bigrams -------------------------------------------------------
calculateBigrams <- function(dataframe, current_word, N_word) {
  bigram = 0
  vector = c()
  wordList = list(toupper(current_word), toupper(N_word))
  wordList <- lapply(wordList, function(x) {
    vector = c()
    if (x != "") {
      for (eachindex in 0:nchar(unlist(strsplit(x, " ")))) {
        if (eachindex == 0) {
          vector = c(vector, paste0('b', strsplit(unlist(strsplit(x, " ")), "")[[1]][eachindex+1]))
        } else if (eachindex == nchar(unlist(strsplit(x, " ")))) {
          vector = c(vector, paste0(strsplit(unlist(strsplit(x, " ")), "")[[1]][max(nchar(unlist(strsplit(x, " "))))], 'e'))
        } else {
          vector = c(vector, paste0(strsplit(unlist(strsplit(x, " ")), "")[[1]][eachindex], strsplit(unlist(strsplit(x, " ")), "")[[1]][eachindex+1]))
        }
      }
    }
    ;vector})
  bigram_cur <- wordList[[1]]
  bigram_N <- wordList[[2]]
  
  for (sim in bigram_cur) {
    if (sim %in% bigram_N) {
      bigram = bigram + 1
    }
  }
  bigram_ratio_smaller <- bigram / min(length(bigram_cur), length(bigram_N))
  bigram_ratio_larger <- bigram / max(length(bigram_cur), length(bigram_N))
  #stringsim(current_word, N_word, method = "lv") # Levenshtein distance, also see Hills, Todd, & Goldstone (2010) page 597.
  
  if (is.null(bigram_cur) || is.null(bigram_N)) {
    bigram_ratio_smaller = NA
    bigram_ratio_larger = NA
  }
  
  # Return bigram ratio
  return(list("bigram_ratio_smaller" = bigram_ratio_smaller, "bigram_ratio_larger" = bigram_ratio_larger))
}

# Recreate foraging paths and map surfaces --------------------------------
# Note: Execute bash_foraging_path for movie creation
# source("recreatePathR.R")  # Note: Works only on Windows

# Load datafiles ----------------------------------------------------------
# Scrabble pre-test data
pre_scrab_df <- data.frame()
files <- list.files("../Rawdata/Scrabble", pattern="scrabble_pretest.txt", full.names=T)
for (currentFile in files) {
  pre_scrab_df <- rbind(pre_scrab_df,read.csv2(currentFile, fileEncoding="UTF-8-BOM", dec='.', sep=';', header=T,
                                               colClasses=c("character", "integer", "character","integer","character","numeric","numeric","numeric","integer","character", "character" ,"integer","character", "character")))}
# Scrabble post-test data
post_scrab_df <- data.frame()
files <- list.files("../Rawdata/Scrabble", pattern="scrabble_posttest.txt", full.names=T)
for (currentFile in files) {
  post_scrab_df <- rbind(post_scrab_df,read.csv2(currentFile, fileEncoding="UTF-8-BOM", dec='.', sep=';', header=T, 
                                                 colClasses=c("character", "integer", "character","integer","character","numeric","numeric","numeric","integer","character", "character" ,"integer","character", "character")))}
# Foraging data
foraging_df <- data.frame()
files <- list.files("../Rawdata/Foraging/", pattern="visual_foraging.txt", full.names=T)
for (currentFile in files) {
  foraging_df <- rbind(foraging_df,read.csv2(currentFile, sep=',', header=T, stringsAsFactors = T))}
foraging_df <- foraging_df[,1:4]
foraging_processed_array <- read.csv2('../Processeddata/ERC_WP3_Year1_Study1_visual_foraging_processed_array_data.txt', sep = ' ', header = T, stringsAsFactors = T)
foraging_df <- merge(foraging_df, foraging_processed_array, by = c("expStartTime","subjectID", "condition", "trial_num"))

# Clean up workspace
rm(currentFile, files, packages, foraging_processed_array)

# Repair dataframes -------------------------------------------------------
# Recalculate correct_n and incorrect_n in pre_scrab_df and post_scrab_df
listdf <- list(pre_scrab_df, post_scrab_df)
listdf <- lapply(listdf, function(x) {
    for (eachrow in 1:nrow(x)) {
      x$correct_n[eachrow] <- ifelse(x$correct_words[eachrow] == "[]", 0, length(str_replace_all(unlist(strsplit(as.character(x$correct_words[eachrow]), " ")), "[^[:alnum:]]", "")))
      x$incorrect_n[eachrow] <- ifelse(x$incorrect_words[eachrow] == "[]", 0, length(str_replace_all(unlist(strsplit(x$incorrect_words[eachrow], " ")), "[^[:alnum:]]", "")))  
    }
;x})  

# Extend dataframes -------------------------------------------------------
# Calculate mean times to submit (in)correct words
listdf <- lapply(listdf, function(x) {
  for (eachrow in 1:nrow(x)) {
    total_rt_corwords = 0
    total_rt_incorwords = 0
    rt_corword_n = 0
    rt_incorword_n = 0
    for (i in 1:length(str_replace_all(unlist(strsplit(x$rt_correct_words[eachrow], " ")), "[^[:alnum:].]", ""))) {
      rt_corword_n <-  ifelse(str_replace_all(unlist(strsplit(x$rt_correct_words[eachrow], " ")), "[^[:alnum:].]", "")[i] == "", 0, 
                              as.numeric(gsub(",", ".", str_replace_all(unlist(strsplit(x$rt_correct_words[eachrow], " ")), "[^[:alnum:].]", "")[i])))
      total_rt_corwords = rt_corword_n + total_rt_corwords
    }
    for (i in 1:length(str_replace_all(unlist(strsplit(x$rt_incorrect_words[eachrow], " ")), "[^[:alnum:].]", ""))) {
      rt_incorword_n <- ifelse(str_replace_all(unlist(strsplit(x$rt_incorrect_words[eachrow], " ")), "[^[:alnum:].]", "")[i] == "", 0, 
                               as.numeric(gsub(",", ".", str_replace_all(unlist(strsplit(x$rt_incorrect_words[eachrow], " ")), "[^[:alnum:].]", "")[i])))
      total_rt_incorwords = rt_incorword_n + total_rt_incorwords
    }
    total_rt_all <- total_rt_corwords + total_rt_incorwords
    x$mean_rt_corwords[eachrow] <- ifelse(total_rt_corwords == 0, NA, total_rt_corwords/length(str_replace_all(unlist(strsplit(x$rt_correct_words[eachrow], " ")), "[^[:alnum:].]", "")))
    x$mean_rt_incorwords[eachrow] <- ifelse(total_rt_incorwords == 0, NA, total_rt_incorwords/length(str_replace_all(unlist(strsplit(x$rt_incorrect_words[eachrow], " ")), "[^[:alnum:].]", "")))
    x$mean_rt_words[eachrow] <- ifelse(total_rt_all == 0, NA, (total_rt_all/(x$correct_n[eachrow] + x$incorrect_n[eachrow])))
  }
;x})

# Calculate bigrams
listdf <- lapply(listdf, function(x) {
  for (eachrow in 1:nrow(x)) {
    prev_small = 0 ; prev_large = 0 ; penult_small = 0 ; penult_large = 0 ; set_small = 0 ; set_large = 0
    for (index in 1:length(str_replace_all(unlist(strsplit(x$correct_words[eachrow], " ")), "[^[:alnum:]]", ""))) {
      current_word = str_replace_all(unlist(strsplit(x$correct_words[eachrow], " ")), "[^[:alnum:]]", "")[index]
      previous_word = ifelse(index == 1, "", str_replace_all(unlist(strsplit(x$correct_words[eachrow], " ")), "[^[:alnum:]]", "")[index-1])
      penultimate_word = ifelse(index == 1 | index == 2, "", str_replace_all(unlist(strsplit(x$correct_words[eachrow], " ")), "[^[:alnum:]]", "")[index-2])
      letterset <- gsub(" ", "", x$letterset[eachrow], fixed = T)
      all_words <- c(previous_word, penultimate_word, letterset)
      for (eachword in all_words) {
        bigram <- calculateBigrams(x, current_word, eachword)
        for (eachname in names(bigram)) {
          if (eachword == previous_word && !(is.na(bigram))) {
            if (eachname == "bigram_ratio_smaller") {
              prev_small <- prev_small + as.numeric(bigram[eachname])
              x[eachrow, paste0(eachname, "_pre")] <- prev_small
            } else {
              prev_large <- prev_large + as.numeric(bigram[eachname])
              x[eachrow, paste0(eachname, "_pre")] <- prev_large
            }
          } else if (eachword == penultimate_word && !(is.na(bigram))) {
            if (eachname == "bigram_ratio_smaller") {
              penult_small <- penult_small + as.numeric(bigram[eachname])
              x[eachrow, paste0(eachname, "_penult")] <- penult_small
            } else {
              penult_large <- penult_large + as.numeric(bigram[eachname])
              x[eachrow, paste0(eachname, "_penult")] <- penult_large
            }
          } else if (eachword == letterset && !(is.na(bigram))) {
            if (eachname == "bigram_ratio_smaller") {
              set_small <- set_small + as.numeric(bigram[eachname])
              x[eachrow, paste0(eachname, "_letterset")] <- set_small
            } else {
              set_large <- set_large + as.numeric(bigram[eachname])
              x[eachrow, paste0(eachname, "_letterset")] <- set_large
            }
          }
        }
      }
    }
  }
;x})

# Bigram values corrected for number of correct answers submitted per letterset
listdf <- lapply(listdf, function(x) {
  for (eachrow in 1:nrow(x)) {
    # Calculate mean bigram values per letterset (taking n-words into account)
    x[eachrow, "bigram_ratio_smaller_letterset"] <- x$bigram_ratio_smaller_letterset[eachrow] / x$correct_n[eachrow]
    x[eachrow, "bigram_ratio_larger_letterset"] <- x$bigram_ratio_larger_letterset[eachrow] / x$correct_n[eachrow]
    x[eachrow, "bigram_ratio_smaller_pre"] <- x$bigram_ratio_smaller_pre[eachrow] / (x$correct_n[eachrow]-1)
    x[eachrow, "bigram_ratio_larger_pre"] <- x$bigram_ratio_larger_pre[eachrow] / (x$correct_n[eachrow]-1)
    x[eachrow, "bigram_ratio_smaller_penult"] <- x$bigram_ratio_smaller_penult[eachrow] / (x$correct_n[eachrow]-2)
    x[eachrow, "bigram_ratio_larger_penult"] <- x$bigram_ratio_larger_penult[eachrow] / (x$correct_n[eachrow]-2)
  }
;x})
  
# Clean dataframes --------------------------------------------------------
# Update dataframes (e.g., bigrams haven't yet been added)
pre_scrab_df <- data.frame(listdf[[1]])
post_scrab_df <- data.frame(listdf[[2]])

# Remove participants based on experimental logbook
listdf_all <- list(pre_scrab_df, post_scrab_df, foraging_df)
listdf_all <- lapply(listdf_all, function(x) {
  x[-which(x$subjectID == 1 | x$subjectID == 12 | x$subjectID == 16), ]
})
pre_scrab_df <- data.frame(listdf_all[[1]])
post_scrab_df <- data.frame(listdf_all[[2]])
foraging_df <- data.frame(listdf_all[[3]])

# Remove outliers: prompt for answer
choice <- menu(c("own", "hills"), title = "Remove outliers: type 'own' for our own criteria, 'hills' for Hills' criteria.")
foraging_df$mean_angle_after_food <- as.numeric(gsub(",", ".", foraging_df$mean_angle_after_food))
foraging_df$areavisited <- as.numeric(gsub(",", ".", foraging_df$areavisited))
listdf <- list(pre_scrab_df, post_scrab_df)
if (choice == 1) {
  # Our own outlier criteria
  # Remove outliers in scrabble tasks
  listdf <- lapply(listdf, function(x) {
      x[x[,'time_in_set']<1  ,'time_in_set'] <- NA 
      x$time_in_set <- remove_outliers(x$time_in_set)
  ;x})
  # Remove outliers in foraging task
  foraging_df[foraging_df$condition=="c",]$mean_angle_after_food <- remove_outliers(foraging_df[foraging_df$condition=="c",]$mean_angle_after_food)
  foraging_df[foraging_df$condition=="d",]$mean_angle_after_food <- remove_outliers(foraging_df[foraging_df$condition=="d",]$mean_angle_after_food)
} else {
  # Hills, Todd, & Goldstone (2010): completion criterion of at least 20 words in post-test
  check <- table(post_scrab_df$subjectID, post_scrab_df$nth_set == 14)[,2]
  nth_set_check <- names(check[check == 1])
  for (eachpar in nth_set_check) {
    # Only print participants who saw all 14 lettersets, and see how many words they submitted
    print(paste0("Participant ", eachpar, " submitted ", sum(subset(post_scrab_df, subjectID == eachpar)$correct_n), " correct words"))
  }
  # Hills, Todd, & Goldstone (2010): Remove participants who stayed more than 200s longer in a set as compared to the next closest participant.
  listdf <- lapply(listdf, function(x) {
    print(paste0("Max difference in time_in_set equals ", max(x$time_in_set, na.rm = T) - min(x$time_in_set, na.rm = T)))
    # Hills, Todd, & Goldstone (2010): Remove participants who stayed longer than 5 sd beyond the mean staying time, in at least one letterset
    x[x[,'time_in_set']<1  ,'time_in_set'] <- NA # This was not specified by Hills, but leaving them in doesn't make sense
    x <- x[which(abs(scale(x$time_in_set)) < 5),]
  ;x})
}

# Update dataframes
pre_scrab_df <- data.frame(listdf[[1]])
post_scrab_df <- data.frame(listdf[[2]])

# Clean up workspace. Note: can display warnings, this doesn't matter though
rm(listdf, choice, check, eachpar, nth_set, listdf_all)

# Create new dataframes ---------------------------------------------------
# Df for mixed model analysis
pre_scrab_df$prepost <- 'pre'
post_scrab_df$prepost <- 'post'
all_scrab_df <- rbind(pre_scrab_df, post_scrab_df)

# Df containing mean scrabble pre-test data
cdata_pre_scrab <- ddply(pre_scrab_df, c("subjectID", "condition"), summarise,
                         mean_time_in_set_pre = mean(time_in_set, na.rm=T),
                         total_time_lettersets_pre = sum(time_in_set, na.rm = T),
                         bigram_prev_smaller_pre = mean(bigram_ratio_smaller_pre, na.rm = T),
                         bigram_prev_larger_pre = mean(bigram_ratio_larger_pre, na.rm = T),
                         bigram_penult_smaller_pre = mean(bigram_ratio_smaller_penult, na.rm = T),
                         bigram_penult_larger_pre = mean(bigram_ratio_larger_penult, na.rm = T),
                         bigram_letterset_smaller_pre = mean(bigram_ratio_smaller_letterset, na.rm = T),
                         bigram_letterset_larger_pre = mean(bigram_ratio_larger_letterset, na.rm = T),
                         mean_rt_cor_pre = mean(mean_rt_corwords, na.rm=T),
                         mean_rt_incor_pre = mean(mean_rt_incorwords, na.rm=T),
                         mean_rt_words_pre = mean(mean_rt_words, na.rm = T))

# Df containing mean scrabble post-test data
cdata_post_scrab <- ddply(post_scrab_df, c("subjectID", "condition"), summarise,
                          mean_time_in_set_post = mean(time_in_set, na.rm = T),
                          total_time_lettersets_post = sum(time_in_set, na.rm = T),
                          bigram_prev_smaller_post = mean(bigram_ratio_smaller_pre, na.rm = T),
                          bigram_prev_larger_post = mean(bigram_ratio_larger_pre, na.rm = T),
                          bigram_penult_smaller_post = mean(bigram_ratio_smaller_penult, na.rm = T),
                          bigram_penult_larger_post = mean(bigram_ratio_larger_penult, na.rm = T),
                          bigram_letterset_smaller_post = mean(bigram_ratio_smaller_letterset, na.rm = T),
                          bigram_letterset_larger_post = mean(bigram_ratio_larger_letterset, na.rm = T),
                          mean_rt_cor_post = mean(mean_rt_corwords, na.rm=T),
                          mean_rt_incor_post = mean(mean_rt_incorwords, na.rm=T),
                          mean_rt_words_post = mean(mean_rt_words, na.rm = T))

# Df containing mean foraging data
cdata_foraging <- ddply(foraging_df, c("subjectID", "condition"), summarise,
                        mean_resource_encounter = mean(food_eaten, na.rm = T),
                        mean_angle_per_food = mean(mean_angle_after_food, na.rm = T),
                        mean_keypresses = mean(keypresses, na.rm = T),
                        mean_areaexplored = mean(areavisited, na.rm = T))

# Merge cdata_pre_scrab, cdata_post_scrab and cdata_foraging
cdata <- merge(cdata_foraging, merge(cdata_pre_scrab, cdata_post_scrab, by = c("subjectID", "condition")), by = c("subjectID", "condition"))
cdata <- cdata[with(cdata, order(subjectID)), ]
cdata$mean_rt_incor_pre[cdata$mean_rt_incor_pre == "NaN"] <- NA

# Create additional variables of interest
cdata[,'mean_time_in_set_diff'] <- cdata[,'mean_time_in_set_post'] - cdata[,'mean_time_in_set_pre'] 
cdata[,'bigram_diff_smaller_prev'] <- cdata[,'bigram_prev_smaller_post'] - cdata[,'bigram_prev_smaller_pre']
cdata[,'bigram_diff_larger_prev'] <- cdata[,'bigram_prev_larger_post'] - cdata[,'bigram_prev_larger_pre']
cdata[,'bigram_diff_smaller_penult'] <- cdata[,'bigram_penult_smaller_post'] - cdata[,'bigram_penult_smaller_pre']
cdata[,'bigram_diff_larger_penult'] <- cdata[,'bigram_penult_larger_post'] - cdata[,'bigram_penult_larger_pre']
cdata[,'bigram_diff_smaller_letterset'] <- cdata[,'bigram_letterset_smaller_post'] - cdata[,'bigram_letterset_smaller_pre']
cdata[,'bigram_diff_larger_letterset'] <- cdata[,'bigram_letterset_larger_post'] - cdata[,'bigram_letterset_larger_pre']
cdata$condition <- factor(cdata$condition, levels = c("c","d"), labels = c("Clumpy","Diffuse"))

# Clean up workspace
rm(cdata_foraging, cdata_pre_scrab, cdata_post_scrab, foraging_df, pre_scrab_df, post_scrab_df)

wide_df <- melt(all_scrab_df, 
                 id.vars = c("subjectID", "condition", "nth_set"),
                 measure.vars = "time_in_set")
wide_df <- dcast(wide_df, fun.aggregate = mean, subjectID+condition ~ nth_set, value.var = "value")

# Write cleaned datafiles to txt
write.table(cdata, 'cleaned_data_scrabble_foraging.txt')


# Main analyses -----------------------------------------------------------
# Is there an effect of average turning behaviour within 200 ms after resource encounter? Hills says YES
t.test(mean_angle_per_food ~ condition, data = cdata)
#describeBy(cdata_foraging$mean_angle_per_food, group = cdata_foraging$condition)
# Report output here.

# Do the groups differ on time spent in lettersets during pretest? Hills says NO
t.test(mean_time_in_set_pre ~ condition, data = cdata)
# Report output here.

# Is there an effect of condition on the difference between pre- and post? Hills says YES
t.test(mean_time_in_set_diff ~ condition, cdata)
# Report output here.

# Do the groups differ on time spent in lettersets during posttest? Hills says YES
t.test(mean_time_in_set_post ~ condition, data = cdata)
# Report output here.

# Do differences in amount of resources encountered explain the priming effects?
# Even if we include resources encountered as a covariate? Hills finds effect of condition,
# no main effect of resources encountered not an interaction
lm_resources <- lm(mean_time_in_set_diff ~ condition*mean_resource_encounter, data = cdata)
summary(lm_resources)
# Report output here.

# Can arousal (indicated by more keypresses) account for the priming effect? Hills says NO
# Even if we include keypresses as a covariate?
lm_keypresses <- lm(mean_time_in_set_diff ~ condition*mean_keypresses, data = cdata)
summary(lm_keypresses)

t.test(mean_rt_words_post ~ condition, data = cdata)
# Report output here.

# Do treatment conditions differ in total area explored? Hills says YES
t.test(mean_areaexplored ~ condition, data = cdata)
# Report output here

# Do the mean times in letterset depend on proportion explored? Hills says ??
letter_exploration <- aov(mean_time_in_set_diff ~ condition * mean_areaexplored, data = cdata)
summary(letter_exploration)

# Do the groups differ on bigram overlap during baseline?
cdata[,'bigram_diff_smaller_prev'] <- cdata[,'bigram_prev_smaller_post'] - cdata[,'bigram_prev_smaller_pre']
cdata[,'bigram_diff_larger_prev'] <- cdata[,'bigram_prev_larger_post'] - cdata[,'bigram_prev_larger_pre']
cdata[,'bigram_diff_smaller_penult'] <- cdata[,'bigram_penult_smaller_post'] - cdata[,'bigram_penult_smaller_pre']
cdata[,'bigram_diff_larger_penult'] <- cdata[,'bigram_penult_larger_post'] - cdata[,'bigram_penult_larger_pre']
cdata[,'bigram_diff_smaller_letterset'] <- cdata[,'bigram_letterset_smaller_post'] - cdata[,'bigram_letterset_smaller_pre']
cdata[,'bigram_diff_larger_letterset'] <- cdata[,'bigram_letterset_larger_post'] - cdata[,'bigram_letterset_larger_pre']

# Main test 1 Hills and colleagues: baseline differences
t.test(bigram_prev_larger_pre ~ condition, data = cdata)
t.test(bigram_penult_larger_pre ~ condition, data = cdata)
t.test(bigram_letterset_larger_pre ~ condition, data = cdata)

# Control statistically, test 2 Hills and colleagues: baseline differences
t.test(bigram_prev_smaller_pre ~ condition, data = cdata)
t.test(bigram_penult_smaller_pre ~ condition, data = cdata)
t.test(bigram_letterset_smaller_pre ~ condition, data = cdata)

# Post test
t.test(bigram_prev_larger_post ~ condition, data = cdata)
t.test(bigram_penult_larger_post ~ condition, data = cdata)
t.test(bigram_letterset_larger_post ~ condition, data = cdata)
# Control statistically
t.test(bigram_prev_smaller_post ~ condition, data = cdata)
t.test(bigram_penult_smaller_post ~ condition, data = cdata)
t.test(bigram_letterset_smaller_post ~ condition, data = cdata)

# Is there an effect of condition on the difference between pre- and post bigrams? Hills says YES
t.test(bigram_diff_smaller_prev ~ condition, cdata)
t.test(bigram_diff_larger_prev ~ condition, cdata)
t.test(bigram_diff_smaller_penult ~ condition, cdata)
t.test(bigram_diff_larger_penult ~ condition, cdata)
t.test(bigram_diff_smaller_letterset ~ condition, cdata)
t.test(bigram_diff_larger_letterset ~ condition, cdata)

# Mixed-effects model -----------------------------------------------------

# 2. Check whether DV is normally distributed.
# 3. Add polynomial contrasts for ordered factors (see slides week 4).
# 4. Set up model

lm_scrabble <- lmer(time_in_set ~ condition * prepost * nth_set + (1 + prepost + nth_set | subjectID), data = all_scrab_df)
summary(lm_scrabble)
Anova(lm_scrabble, type = 3, test = "F")

# 5. Check figures of model
# 6. Calculate p-values by means of conditional f-test with df-correction or parametric bootstrapping
FUN_bootMer <- function(fit) {
  return(fixef(fit))
}

boot_mymodel <- bootMer(lm_scrabble, FUN_bootMer, nsim = 100, type = "parametric", .progress ="txt", PBargs = list(style = 3))
# Get confidence intervals
boot.ci(boot_mymodel, index = 2, conf = 0.95, type = c('norm', 'basic', 'perc')) # For intervals, else index should be changed.

# Figures -----------------------------------------------------------------
# Bargraph: Mean angle per food
png(filename = '../Output/mean_angle_resource_encounter.png')
ggplot(cdata, aes(condition, mean_angle_per_food, fill = mean_angle_per_food)) + 
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  labs(y = "Turn angle after food", x = "Condition") + #ggtitle("Mean angle after resource encounter") +
  coord_cartesian(ylim = c(0, 40)) + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
  position = position_dodge(width = 0.9), width = 0.2) + scale_y_continuous(expand = c(0,0)) + 
  theme(axis.text = element_text(colour = 'black', hjust = 0.5, size = 15) , #axis.title = element_text(size = 17.5),
        axis.line = element_line(), plot.title = element_text(size = 20, hjust = 0.5),  
        panel.grid = element_blank(), panel.border = element_blank(), panel.background = element_blank())
dev.off()

# Bargraph: Difference (pre vs post) in mean time spent in lettersets
ggplot(cdata, aes(condition, mean_time_in_set_diff, fill = mean_time_in_set_diff)) + 
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  labs(y = "Mean difference in time spent in lettersets", x = "Condition") +
  coord_cartesian(ylim = c(-20, 20)) + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
  position = position_dodge(width = 0.9), width = 0.2) + scale_y_continuous(expand = c(0,0)) +
  theme(axis.text.x = element_text(colour = 'black') , axis.text.y = element_text(colour = 'black'), 
        axis.line.x = element_line(), axis.line.y = element_line(),      
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank())

# Bargraph: Mean time spent in lettersets in posttest
ggplot(cdata, aes(condition, mean_time_in_set_post, fill = mean_time_in_set_post)) + 
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  labs(y = "Mean time in lettersets after foraging", x = "Condition") +
  coord_cartesian(ylim = c(0, 100)) + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
  position = position_dodge(width = 0.9), width = 0.2) + scale_y_continuous(expand = c(0,0)) +
  theme(axis.text.x = element_text(colour = 'black') , axis.text.y = element_text(colour = 'black'), 
        axis.line.x = element_line(), axis.line.y = element_line(),      
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank())

# Bargraph: Difference (pre vs post) in mean time spent in seperate lettersets
#cdata_pre_scrab$condition <- factor(cdata_pre_scrab$condition, levels = c("c","d"), labels = c("Clumpy","Diffuse"))

# CREATE MEANDIFF VALUE IN POST_SCRAB_DF
ggplot(all_scrab_df, aes(nth_set, time_in_set, fill = nth_set)) + 
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  labs(y = "Mean difference in time spent in lettersets", x = "Condition") +
  coord_cartesian(ylim = c(0, 80)) + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
  position = position_dodge(width = 0.9), width = 0.2) + scale_y_continuous(expand = c(0,0)) +
  theme(axis.text.x = element_text(colour = 'black') , axis.text.y = element_text(colour = 'black'), 
        axis.line.x = element_line(), axis.line.y = element_line(),      
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank())


# Creativity tasks --------------------------------------------------------

# Alternate Uses Task data
aut_df <- data.frame()
files <- list.files("../Rawdata/AUT/", pattern="alternate_uses_task.txt", full.names=T)
for (currentFile in files) {
  aut_df <- rbind(aut_df,read.csv2(currentFile, header=T, fileEncoding="UTF-8-BOM", dec='.', na.strings=c("NA", ""),
                                   colClasses=c("factor", "integer", "factor", "factor", "factor", "numeric", "factor", "numeric", "numeric", "numeric", "numeric", "factor", "numeric", "factor")))
}

# Remove outliers Alternate Uses Task data
aut_df$responsetime <- remove_outliers(aut_df$responsetime)

# Remote Association Task data
rat_df <- data.frame()
files <- list.files("../Rawdata/RAT/", pattern="remote_association_task.txt", full.names=T)
for (currentFile in files) {
  rat_df <- rbind(rat_df,read.csv2(currentFile, header=T, fileEncoding="UTF-8-BOM", dec='.', na.strings=c("NA", ""),
                                   colClasses=c("factor", "integer", "factor", "factor", "numeric", "factor", "numeric", "numeric", "numeric", "factor", "numeric", "factor", "numeric", "numeric", "numeric", "numeric")))
}

# Remove outliers Alternate Uses Task data
rat_df$setresponsetime <- remove_outliers(rat_df$setresponsetime)
rat_df$optionresponsetime <- remove_outliers(rat_df$optionresponsetime)


