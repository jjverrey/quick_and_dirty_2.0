#install.packages("nlme")

library(ggplot2) #beautiful plots
library(nlme) #mixed linear effects

#************************************************************
#                     Data Preperation
#*************************************************************

moverData <- read.csv("v2_mover_date_master.csv")
masterData <- read.csv("v2_monitor_data_master.csv")


# --- 1)  First, get rid of the technical glitches in which no-one was paired up w/ a parnter.
tech_excl_before <- nrow(masterData)
masterData <- masterData[masterData$computer_overall_speed != -1,]
tech_excl_after <- nrow(masterData)

tech_excl_after - tech_excl_before
rm(tech_excl_after)
rm(tech_excl_before)

# 7 exclusions.

# --- 2)  Variable creation: let's make some variables

# -- a) Monitor Z-Scores
masterData$monitor_completion_time_z <- (masterData$completion_time - mean(masterData$completion_time))/sd(masterData$completion_time)

# -- b) Mover z-scores. Base it on values FROM moverData and NOT from masterData, as masterData
masterData$computer_solo_time_z <- (masterData$computer_solo_time/100 - mean(moverData$completion_time))/sd(moverData$completion_time)

# -- c) time of switch (in terms of % of performanfce) (MOVER)
masterData$timing_of_switch_mover <- masterData$computer_elapsed_time/masterData$computer_solo_time * 100

# -- d) time of switch (in terms of % of performanfce) (TOTAL)
masterData$timing_of_switch_total <- masterData$computer_elapsed_time/masterData$total_elapsed_time * 100

# --e) Improvement Index - Monitor
masterData$improvement_index <- 
  masterData$second_half_avg_speed - masterData$first_half_avg_speed

# -- f) Improvement Index - Mover
masterData$computer_improvement_index <- 
  masterData$computer_avg_speed_second_half - masterData$computer_avg_speed_first_half
moverData$improvement_index <- 
  moverData$second_halv_avg_speed - moverData$first_half_avg_speed
masterData$computer_improvement_index_z <- (masterData$computer_improvement_index - mean(moverData$improvement_index))/sd(moverData$improvement_index)


# -- g) Improvement after switch index - Mover (both datasets)
masterData$computer_post_switch_improvement_index <- 
  masterData$computer_avg_speed_after_switch - masterData$computer_avg_speed_before_switch

# -- h) Effect of switch on performance (boolean)
masterData$switch_hurt_performance <- masterData$effects_of_switch > 0


# --- 3) Next, let's grab a few of the mover's survey responses...

#First, initialize the new column
masterData$computer_switch_goodbad <- NA

#Next, go through all the movers and get the ID and VARIABLE o fitnerest
for(counter in c(1:nrow(moverData))){
  id_of_interest <- moverData$participant_id[counter]
  value <- moverData$switch_goodbad[counter]
  
  #Now that we have those vlaues, go through each row of our monitor data...  
  for (counter in c(1:nrow(masterData))){
    current_id <- masterData$computer_id[counter]
    #If a match is found, replace that row with the proper value from the previous loop
    if(current_id == id_of_interest){
      masterData$computer_switch_goodbad[counter] <- value
    }
  }#end of for(masterData)
}#end of for(moverData)
rm(current_id); rm(id_of_interest); rm(value)

#Finally, let's recode the variable.
masterData[masterData$computer_switch_goodbad == 1,]$computer_switch_goodbad <- "Partner didn't want switch"
masterData[masterData$computer_switch_goodbad == 2,]$computer_switch_goodbad <- "Partner wanted switch"



# --- 4) Finally, let's exclude some more variables

#First, exclude those who switched tabs, at it gives away the human-partner illusion
masterDataExclusions <- subset(masterData, !switched_tabs_critical)
#Next, exclude those who thought they were paired with a bot.
masterDataExclusions <- subset(masterDataExclusions, !bot_partner)


nrow(masterData) - nrow(masterDataExclusions) #Gets number of exclusions.


# Wow. 72 exclusions, meaning my sample size should now be around 140.


# --- 5) Let's make some subsets.

#First, let's seperate the switchers from the non switchers.
masterDataExclusions_non_switchers <- 
  subset(masterDataExclusions, game_mode == 2)
masterDataExclusions_all_switchers <- 
  subset(masterDataExclusions, game_mode == 3)


# --- 6) Subset-specific variable creation

# -- a) Effects of Switch Zs among those who switched ONLY
masterDataExclusions_all_switchers$effects_of_switch_z <- (masterDataExclusions_all_switchers$effects_of_switch - mean(masterDataExclusions_all_switchers$effects_of_switch))/sd(masterDataExclusions_all_switchers$effects_of_switch)

# -- b) Switcher improvement index Zs 
masterDataExclusions_all_switchers$improvement_index_z <- (masterDataExclusions_all_switchers$computer_improvement_index - mean(masterDataExclusions_all_switchers$computer_improvement_index))/sd(masterDataExclusions_all_switchers$computer_improvement_index)

# -- c) Computer average speed before swith Zs
masterDataExclusions_all_switchers$computer_avg_speed_before_switch_z <- (masterDataExclusions_all_switchers$computer_avg_speed_before_switch - mean(masterDataExclusions_all_switchers$computer_avg_speed_before_switch))/sd(masterDataExclusions_all_switchers$computer_avg_speed_before_switch)

# -- c) Computer average speed after switch Zs
masterDataExclusions_all_switchers$computer_avg_speed_after_switch_z <- (masterDataExclusions_all_switchers$computer_avg_speed_after_switch - mean(masterDataExclusions_all_switchers$computer_avg_speed_after_switch))/sd(masterDataExclusions_all_switchers$computer_avg_speed_after_switch)


# -- c) First 30s z-scores for switchers ONLY (non-switchers speed values of -1)

# - i) Exclude glitched speed computations (i.e. speed values of -1)
before_exclusions <- nrow(masterDataExclusions_all_switchers)
masterDataExclusions_all_switchers <- masterDataExclusions_all_switchers[masterDataExclusions_all_switchers$overall_avg_speed != -1,] 
after_exclusions <- nrow(masterDataExclusions_all_switchers)

before_exclusions - after_exclusions
rm(before_exclusions)
rm(after_exclusions)
#Good - only one exclusion.

# - ii) Calculate z scores
masterDataExclusions_all_switchers$overall_avg_speed_z <- (masterDataExclusions_all_switchers$overall_avg_speed - mean(masterDataExclusions_all_switchers$overall_avg_speed)) /sd(masterDataExclusions_all_switchers$overall_avg_speed)

# -- d) Get position of switch - checkpoint

# - i) Create a vector of positions of switch via for loops.
position_of_switch_vector <- 0
for(counter in c(1:nrow(masterDataExclusions_all_switchers))){ #Get every single mover's progress string
  progress_string <- toString(masterDataExclusions_all_switchers$computer_progress_string[counter])
  progress_string_stamps <- unlist(strsplit(progress_string, "\\W;")) #Split progress string by character ';'
  time_of_switch <- masterDataExclusions_all_switchers$computer_elapsed_time[counter]  
  
  for(stamp_counter in c(1:length(progress_string_stamps))){ #Loops through every stamp in progress string
    current_stamp <- unlist(strsplit(progress_string_stamps[stamp_counter], ","))
    
    if(stamp_counter == 1) #removes the ';' that comes before the first timestamp
      current_stamp[1] <- substr(current_stamp[1], 2, nchar(current_stamp[1])) 
    
    if(as.numeric(current_stamp[1]) >= time_of_switch){
      position_of_switch <- as.numeric(current_stamp[2])
      if(position_of_switch != 0)
        position_of_switch <- position_of_switch-1
      break
    }
  }#end of for(stamp_counter)
  position_of_switch_vector[counter] <- position_of_switch
}#end of for(masterDataExclusions)

# - ii) Add positions of switch to the dataset
masterDataExclusions_all_switchers$position_of_switch <- position_of_switch_vector

# - iii) Variable Cleanup
rm(current_stamp)
rm(progress_string)
rm(progress_string_stamps)
rm(position_of_switch)
rm(position_of_switch_vector)
rm(counter)
rm(time_of_switch)
rm(stamp_counter)

#****************************************************************************************************
#                     Analysis I: How did people perform on the maze?
#****************************************************************************************************

# -- a) Movers vs. Monitors

# - i) Graphs
par(mfrow = c(2,2))

#Movers (those paired up with a partner)
boxplot(masterDataExclusions$computer_solo_time/100, main = "Dist of Mover Completion Time",
        ylab = "Completion Time (s)")
hist(masterDataExclusions$computer_solo_time/100, main = "Freq of Mover Completion Time",
     xlim=c(0,200), xlab = "Completion Time (s)")
abline(v=mean(masterDataExclusions$computer_solo_time/100), col="Red")

#Monitors
boxplot(masterDataExclusions$completion_time, main = "Dist of monitor Completion Time",
        ylab = "Completion Time (s)")
hist(masterDataExclusions$completion_time, main = "Freq of monitor completion Time",
     xlim=c(0,200), xlab = "Completion Time (s)")
abline(v=mean(masterDataExclusions$completion_time), col = "Red")

par(mfrow = c(1,1))


#How intruiging! Both graphs looks extremely similar, both in terms of mean and in terms of distributions. Switching must not really have much of an impac ton performance. Let's take a closer look at the mean.


# - ii) Diff of Means
mover_mean <- mean(masterDataExclusions$computer_solo_time/100)
monitor_mean <- mean(masterDataExclusions$completion_time)

paste("Represented Mover Mean =", mover_mean, "Monitor Mean =", monitor_mean, "Dif of means = ", monitor_mean - mover_mean,sep = " ")
rm(mover_mean)
rm(monitor_mean)

#As confirmed by the graph, the mean completion times are extreme similar; it seems that switching doesn't really effect performance. 


# - iii) T Test just to make sure there's no significance. Completoin Time (s)
holder_monitor <- data.frame("Completion Time " = masterDataExclusions$completion_time,
                             "Condition" = "monitor")
holder_mover <- data.frame("Completion Time " = masterDataExclusions$computer_solo_time/100,
                           "Condition" = "mover")
holder_final <- rbind(holder_monitor, holder_mover)

t.test(holder_final$Completion.Time. ~ as.factor(holder_final$Condition))
# No significant difference. Let's look at Z scores to get a better understanding.


# - iv) T Test just to make sure there's no significance. Completoin Time (Z scores)

holder_final$completion_time_z <- (holder_final$Completion.Time. - mean(holder_final$Completion.Time.))/sd(holder_final$Completion.Time.)

t.test(holder_final$completion_time_z ~ as.factor(holder_final$Condition))

rm(holder_monitor)
rm(holder_mover)
rm(holder_final)

# Yep - no statistical signifiance in z scores either


# -- b) How does the performance of switchers compare to non-switchers?

# - i) Plots
par(mfrow=c(2,2))
barplot(prop.table(table(masterDataExclusions$switch)), ylab = "Proportion", main = "Did the switch happen?")
boxplot(masterDataExclusions$completion_time ~ masterDataExclusions$switch, main = "Switch vs. no-switch distributions", ylab = "Completion time (s)")

hist(masterDataExclusions_non_switchers$completion_time, 
     main = "Freq of non-switcher completion Time", xlim=c(0,200), xlab = "Completion Time (s)")
abline(v=mean(masterDataExclusions_non_switchers$completion_time), col = "Red")

hist(masterDataExclusions_all_switchers$completion_time, 
     main = "Freq of switcher completion Time", xlim=c(0,200), xlab = "Completion Time (s)")
abline(v=mean(masterDataExclusions_all_switchers$completion_time), col = "Red")

par(mfrow=c(1,1))

# Ok - most people (~60%) switched, as confirmed by the quantatative data.

# It seemes like the mean completion time of switchers is higher than that of non-switchers, so when looking at the aggregate, pressing the siwtch button will make one do worse on performance. Let's confirm this


# - ii) Diff of means - completoin time (seconds)
switch_mean <- round(mean(masterDataExclusions_all_switchers$completion_time),2)
no_switchmean <- round(mean(masterDataExclusions_non_switchers$completion_time),2)

paste("Switch Mean =", switch_mean, "No Switch Mean =", no_switchmean, 
      "Diff of means = ", I(switch_mean - no_switchmean) ,sep = " ")
rm(switch_mean)
rm(no_switchmean)

# Switching makes you perform worse by around 23 seconds, when looking at all of monitors. Let's control for this nested data however.


# - iii) Diff of means - completoin time (zs)
switch_mean <- round(mean(masterDataExclusions_all_switchers$monitor_completion_time_z),2)
no_switchmean <- round(mean(masterDataExclusions_non_switchers$monitor_completion_time_z),2)

paste("Switch Mean =", switch_mean, "No Switch Mean =", no_switchmean, 
      "Diff of means = ", I(switch_mean - no_switchmean) ,sep = " ")
rm(switch_mean)
rm(no_switchmean)

# Switching makes you do worse by .6 SDs! 


# - iv) Mixed linear effects - completion time SECONDS
mlreg = lme(completion_time ~ switch, data = masterDataExclusions, random=~1|computer_id)
summary(mlreg)

#Incredible! Even when controlling for the nested nature of the data, SWITCHERS DO WORSE THAN NON-SWITCHERS BY A WHOLE 21 SECONDS! I know this because when switchHappened = 0, people complete the maze in around 89.6 seconds, but when switchHappened = 1, this value goes up by 21.56 seconds. Because the longer someone takes, the less his bonus will be, the time increase means that switching may be related to performing worse overall.

# It would be interesting to prove this relationship is causal by running another condition where we force people to switch roles half way through the maze...


# - v) Mixed linear effects - completion time Zs
mlreg = lme(monitor_completion_time_z ~ switch, data = masterDataExclusions, random=~1|computer_id)
summary(mlreg)


# -- C) Effect of the swtich on the mover's performance.

# Note - we use absolute value because we care abotu the magnitude of the effect, not the direction of the effect

# - i) Graphs & Normality

# Effects of switch on intiial movers performance
par(mfrow=c(1,2))
hist(masterDataExclusions_all_switchers$effects_of_switch/100,
     main = "Effect of switch histogram Seconds", breaks = 100,
     xlab = "Effect of Switch (s)")
abline(v=mean(masterDataExclusions_all_switchers$effects_of_switch/100), col = "red")

hist(masterDataExclusions_all_switchers$effects_of_switch_z,
     main = "Effect of switch histogram Zs", breaks = 100,
     xlab = "Effect of Switch (z)")
abline(v=mean(masterDataExclusions_all_switchers$effects_of_switch_z), col = "red")
par(mfrow=c(1,1))

shapiro.test(masterDataExclusions_all_switchers$effects_of_switch_z)
# The graph appears pretty close to norally distributed. This is very surprising: why is there such a wide distribution regarding whether the switch is optimal?



#****************************************************************************************************
#       Analysis II: Why is there a skill performance in switchers? (and also non-switchers?) 
#****************************************************************************************************

# H-1) Maybe the good switchers switched on the bad movers & the bad switchers switched on good movers? In other words, maybe bad switchers switched on the wrong people.

# H-2) Maybe there's a skill difference. In other words, the monitors whose switch HELPED performance the most may have simply been the most skilled.


# -- 1) H-1: Incorrect Switching Hypothosis

# -- A) Incorrect switching hypothosis: Switchers vs. Non Switchers. 

# In other words, is there really a difference between those who are switched on vs. those who are not?

# - i) Graphs

par(mfrow=c(2,2))
#ALL Switchers - intiial mover performance
hist(masterDataExclusions_all_switchers$computer_solo_time/100, 
     main = "Mover Performance, ALL Sw", xlim=c(0,200), 
     xlab = "Completion Time (s)")
abline(v=mean(masterDataExclusions_all_switchers$computer_solo_time/100), col = "Red")

# ALL siwtchers - inital mover performance Zs
hist(masterDataExclusions_all_switchers$computer_solo_time_z, 
     main = "Mover Performance, ALL Sw", 
     xlab = "Completion Time (z)")
abline(v=mean(masterDataExclusions_all_switchers$computer_solo_time_z), col = "Red")

#Non Switchers - intiial mover performance
hist(masterDataExclusions_non_switchers$computer_solo_time/100, 
     main = "Mover Performance, Non Sw", xlim=c(0,200), 
     xlab = "Completion Time (s)")
abline(v=mean(masterDataExclusions_non_switchers$computer_solo_time/100), col = "Red")

# Non siwtchers - inital mover performance Zs
hist(masterDataExclusions_non_switchers$computer_solo_time_z, 
     main = "Mover Performance, Non Sw", 
     xlab = "Completion Time (z)")
abline(v=mean(masterDataExclusions_non_switchers$computer_solo_time_z), col = "Red")
par(mfrow=c(1,1))


#Ok, so it looks like the movers of non switchers did noticeably better than the movers of switchers. Non switchers must choose not to switch on the really good people, or so the graphs suggest.


# - ii) T-tests

#initial mover time - SECONDS
t.test(I(computer_solo_time/100) ~ as.factor(game_mode-2), data = masterDataExclusions)

#initial mover time - Zs
t.test(I(computer_solo_time_z) ~ as.factor(game_mode-2), data = masterDataExclusions)


# Ok - movers who DON'T get switched on tend to have an average completion time of 88.6 seconds, which is around 13 seconds BETTER than the completion times of those who are switched on (101.8s). 
# Z-scores are a similar conclusion: people who AREN'T switched on are 1/4th a standard deviation BETTER than those who are switched on (.23 sd)!

# Take away: Movers who aren't switched on are only 1/4th a SD better than those who are switched on, which isn't that much. (i.e. the average mover who ISN'T switched on is only 8% worse than the average mover who is switched on)


# - iii) Regression (to control for something)
reg <- glm( (game_mode-2) ~ computer_solo_time_z, 
            data = masterDataExclusions, family = binomial(link='logit'))
summary(reg)


# I forgot what this controls for, but it holds the relationship. 


# H-1: SUPPORTED (?) for Switchers vs. No Switchers: no switchers only refuse to switch on people marginally better than the average Joe, so they're basically switching on the same people.

#Ok, so non switchers vs. switchers don't know who to switch on.if the incorrect switching hyptohosis is true for switchers, then we should expect people who switch to be hurting the mover's performane


# -- B) Incorrect switch hypothosis for SWITCHERS

# - i) Graphs

#Seconds
plot(I(effects_of_switch/100) ~ I(computer_solo_time/100), data = masterDataExclusions_all_switchers, 
     xlab = "Mover (computer) completion time WITHOUT switch (s)", ylab = "Effect of switch (s)",
     main = "Effects of switch from monitor vs. Initial Mover completion time")
abline(h = 0, col = "black")

#Plot reg
reg_s <- lm(I(effects_of_switch/100) ~ I(computer_solo_time/100), 
            data = masterDataExclusions_all_switchers)
abline(reg_s, col = "blue")

#Plot average lines.
#This is the mean of ALL Movers, not just the movers who were switchd on
abline(v = mean(masterDataExclusions$computer_solo_time/100), col = "pink") 
#This is the mean of the movers who were switched on 
abline(v = mean(masterDataExclusions_all_switchers$computer_solo_time/100), col = "red")
#This is the mean of the movers who were NOT switched on
abline(v = mean(masterDataExclusions_non_switchers$computer_solo_time/100), col = "purple") 

# The graph shows the following: the WORSE an initial mover does overall, the BETTER the switch would be (i.e. the effects of switch are negative & make the team do better).

# However, what's striking about the graph is the following: most people who are switched on visually appear to be above average (i.e. there are many dots to the left of the pink line, the mean completion time of movers.) AND, the movers who are CLOSE TO AVERAGE, these guys, if they're switched on, WILL HAVE THEIR PERFORMANCE WORSENED! It seems that you shouldn't switch on the AVERAGE JOE, since you will HURT his performance.

# - ii) Regression - Zs
reg_z <- lm(I(effects_of_switch_z) ~ I(computer_solo_time_z), 
            data = masterDataExclusions_all_switchers)
summary(reg_z) #computer_solo_time = b * effects_of_switch + a
rm(reg_z)

# For every one standard deviation WORSE that the intial mover was, the MONITOR HELPED performance by 1/2 a standard deviation (-.49 zs)

# - iii) Regression - SECONDS
mlreg = lme(I(effects_of_switch/100) ~ I(computer_solo_time/100), 
            data = masterDataExclusions, random=~1|computer_id)
summary(mlreg) #computer_solo_time = b * effects_of_switch + a

# For every one second WORSE the initial mover was, the MORE the switch HELPED performance by .43 seconds. In other words, it's just as we would expect: the worse the mover is, the more the act of switching will help him. 


# - iv) Seconds regression 1: What happens if the monitor switched on the perfectly average mover?
#y = b * x + a

fixed.effects(mlreg)[2] * mean(masterDataExclusions$computer_solo_time/100) + fixed.effects(mlreg)[1]

# However, as expected visually from the graph, if you were to switch on the perfectly average participant, your act of switching will hurt his performance by 6.32 seconds!


# - v) Seconds regression 2: the average mover didn't switch on the average Joe; they switched on someone insignificantly worse than the average Joe (roughly 2-3 seconds worse). What happens if they switch on him?

#y = b * x + a
fixed.effects(mlreg)[2] * mean(masterDataExclusions_all_switchers$computer_solo_time/100) + fixed.effects(mlreg)[1]

# There's not much of a difference here: the average switcher hurt the barely-less-than-average Joe's performance by roughly 3.94 seconds.


# - vi) Seconds regression 3: How bad must someone be in order for the act of switching to be beneficial?

#y = b * x + a. If y (effects of switch) = 0 (break even point), then 0 = bx+a; x = -a/b
break_even_point <- -fixed.effects(mlreg)[1]/fixed.effects(mlreg)[2] #111.34s
(break_even_point - mean(moverData$completion_time))/sd(moverData$completion_time)# Z-score equivlence: .31zs
rm(break_even_point)

# Woah: you really need to suck at the maze in order for switching ot help performance: you need to do the maze in 111.34s, which is 11.82 seconds WORSE THAN AVERAGE! In other words, you neeed to be 1/3 a standard deviation WORSE THAN THE AVERAGE JOE for swtiching to take effect (i.e. you need to be in the bottom 40% (68/2 * 1/3 SD ~= .1) ! The problem is though that most people switch ont he average Joe, which produces the adverse effects


# - vii) Seconds regression 4: What would've happened if the average NON-SWITCHER switched on the average SWITCHER

#y = b * x + a
fixed.effects(mlreg)[2] * mean(masterDataExclusions_non_switchers$computer_solo_time/100) + fixed.effects(mlreg)[1]
rm(mlreg)
rm(reg_z)
rm(reg_s) 

#Amazing: If the average NON-SWITCHER would have switched on their partner, they would've hurt their teams performance by 9.99 seconds, which is ~8.8 seconds more damaging than the average switcher. To be hoenst though, this 10 seconds isn't that much more.

### H-1 : Supported by data! People really don't know when to switch: you need to switch on someone 1/3 a SD WORSE than the average Joe for switching to be optimal, but the problem is that people usually switch on the average Joe, which produces the suboptimal effects!




# --- 3) H-2 Testing: SKill Differences in SWITCHERS


# -- a) Overall Speed

# - i) Raw results: seconds (effect of switch)
reg_s <- lme(I(overall_avg_speed_z) ~ I(effects_of_switch/100), 
            data = masterDataExclusions_all_switchers, random=~1|computer_id)
summary(reg_s)

# Ok. So for every second that the monitor HURT a mover's performance, his speed was lower by .01 standard deviation. This doesn't seem that big.


# - ii) Raw results: z-scores (effect of switch)
reg_z <- lme(I(overall_avg_speed_z) ~ I(effects_of_switch_z), 
           data = masterDataExclusions_all_switchers, random=~1|computer_id)
summary(reg_z)

# This is a bit more intelligable: for every standard deviation that the switch HURT performance, the switcher's speed was -.6 standard deviations WORSE


# iii) Plots
par(mfrow=c(1,2))
plot(I(overall_avg_speed_z) ~ I(effects_of_switch/100), data = masterDataExclusions_all_switchers,
     ylab = "Overall Avg Speed (z)", xlab = "Effects of switch (s)", ylim = c(-5,5),
     main = "Overall Speed vs. Effect of Switch (s)")
reg <- lm(I(overall_avg_speed_z) ~ I(effects_of_switch/100), data = masterDataExclusions_all_switchers)
abline(reg, col = "blue")
abline(v = 0, col = "gray")
abline(h = 0, col = "gray")

plot(I(overall_avg_speed_z) ~ I(effects_of_switch_z), data = masterDataExclusions_all_switchers,
     ylab = "Overall Avg Speed (z)", xlab = "Effects of switch (z)", ylim = c(-5,5),
     main = "Overall Speed vs. Effect of Switch (z)")
reg <- lm(I(overall_avg_speed_z) ~ I(effects_of_switch_z), data = masterDataExclusions_all_switchers)
abline(reg, col = "blue")
abline(v = 0, col = "gray")
abline(h = 0, col = "gray")
par(mfrow=c(1,1))

#Ok -  so it looks like a slight decrease in speed leads to a a pretty severe PENTALTY when you're looking at the effect of switching. In other words, people who switched who really hurt performance tended to have worse speeds.

# iv) Are the least skilled people also the same as those who don't knwo when to switch?
reg <- lme(I(overall_avg_speed_z) ~ I(computer_solo_time_z), 
             data = masterDataExclusions_all_switchers, random=~1|computer_id)
summary(reg)

#Wow! There's NO CORRELATION between those who are the least skilled vs. those who don't know when to switch! It seems that H-2 is also supported: people who's switch HURTS performance ten dto be LESS SKILLED, as evident by overall average speed


# -- b) Improvment Index

# - i) Raw results: seconds (effect of switchh
reg_s <- lme(I(improvement_index) ~ I(effects_of_switch/100), 
             data = masterDataExclusions_all_switchers, random=~1|computer_id)
summary(reg_s)

# No significant/meaningful difference in imrpovement index: people who were great at switching improved just as much as people who were bad at swiching



# -- c) Reset Count (wall rams)

# - i) Raw results: seconds (effect of switchh
reg_s <- lme(human_reset_counter ~ I(effects_of_switch/100), data = masterDataExclusions_all_switchers, 
             random=~1|computer_id)
summary(reg_s)

#Ok, for every second longer that someone hurts one's switch, they ram into the wall .078 more times

# - ii) Raw results: seconds (effect of switchh
reg_z <- lme(human_reset_counter ~ I(effects_of_switch_z), data = masterDataExclusions_all_switchers, 
             random=~1|computer_id)
summary(reg_z)

#Very good - for every standard deviation WORSE a monitor makes a mover's eprformance, the monitor will ram into the wall 3.72 more times.


# - iii) Plot
plot(human_reset_counter ~ I(effects_of_switch/100), data = masterDataExclusions_all_switchers,
     ylab = "Monitor Wall Rams (# of rams)", xlab = "Effects of switch (s)", 
     main = "Wall Rams vs. Effects of Switch", ylim = c(0,15))
abline(v = 0, col = "black")
abline(h = mean(masterDataExclusions_all_switchers$human_reset_counter), col = "red")

# No significant/meaningful difference in imrpovement index: people who were great at switching improved just as much as people who were bad at swiching


# - iv) Are the people who ram into the most walls also the people who don't know when to switch?
reg_s <- lme(human_reset_counter ~ I(computer_solo_time/100), data = masterDataExclusions_all_switchers, 
             random=~1|computer_id)
summary(reg_s)

#No correlation again (p=.59). 


### H-2: Supported. Monitors who make movers worse off are less skilled, as evident by overall average speed and wall rams, but NOT improvement (the learning metric)! Furthermore, this is NOT CORRELATED with mover skill (i.e. computer overall time), meaning the monitors who are bad at switching are also NOT those who are the least skilled



#*****************************************************************************************************
#       Analysis III: Does waiting have anything to do with effectiveness of switch? 
#*****************************************************************************************************

#I don't think H-1 and H-2 are mutually exclusive: maybe there's something about a switching strategy that's leading the monitor to overall becoming less skilled. Let's find out...

### H-1) Early Switch Hyptohosis: Maybe bad switchers switch too early, and that's why they don't get an accurate measure of performance?


# --- 1) Test Early Switch Hypothosis: Maybe those who are better at switching just wait later?

# -- A) Create two datasets
center_data <- masterDataExclusions_all_switchers[masterDataExclusions_all_switchers$start_from_center,]
edge_data <- masterDataExclusions_all_switchers[!masterDataExclusions_all_switchers$start_from_center,]

nrow(center_data) - nrow(edge_data)
#6 more people started from the edge than from the center.


# -- B) Use Elapsed Time

# - i) Graphs
par(mfrow=c(1,2))
plot(I(effects_of_switch/100) ~ I(computer_elapsed_time/100), data = center_data,
     xlab = "Computer Elapsed Time (s)", ylab = "Effects of switch", main = "Center")
abline(v = mean(center_data$computer_elapsed_time/100), col = "Red")
abline(h = 0, col = "black")
plot(I(effects_of_switch/100) ~ I(computer_elapsed_time/100), data = edge_data,
     xlab = "Computer Elapsed Time (s)", ylab = "Effects of switch", main = "Edge")
abline(v = mean(edge_data$computer_elapsed_time/100), col = "Red")
abline(h = 0, col = "black")
par(mfrow=c(1,1))

# There doesn't seem to be any correlation - why's that? In other words, it doesn't appear to matter how long one waits before they press the switch button: people who press the switch button early on are just as likely to hurt the mover's performance as those who wait 100 seconds.

# - ii) mixed linear reg - CENTER
mlreg <- lme(I(effects_of_switch/100) ~ I(computer_elapsed_time/100), data = center_data, 
             random=~1|computer_id)
summary(mlreg)

# No significant relationship.

# - iii) mixed linear reg - EDGE
mlreg <- lme(I(effects_of_switch/100) ~ I(computer_elapsed_time/100), data = edge_data, 
             random=~1|computer_id)
summary(mlreg)

# No significant relationship.


# -- B) Use % of total performance

# - i) Graphs
par(mfrow=c(1,2))
plot(I(effects_of_switch/100) ~ I(timing_of_switch_mover), data = center_data,
     xlab = "Timing of Switch, Mover (% complete)", ylab = "Effects of switch", main = "Center")
abline(v = mean(center_data$timing_of_switch_mover), col = "Red")
abline(h = 0, col = "black")
plot(I(effects_of_switch/100) ~ I(timing_of_switch_mover), data = edge_data,
     xlab = "Timing of Switch, Mover (% complete)", ylab = "Effects of switch", main = "Edge")
abline(v = mean(edge_data$timing_of_switch_mover), col = "Red")
abline(h = 0, col = "black")
par(mfrow=c(1,1))

# How fascinating! When we look at the % complete the intial mover is, NOW there seems to be a significant relationship: the longer one waits, the more likely he is to foul up the switch

# - ii) mixed linear reg - CENTER
mlreg <- lme(I(effects_of_switch/100) ~ I(timing_of_switch_mover), data = center_data, 
             random=~1|computer_id)
summary(mlreg)

# - iii) mixed linear reg - EDGE
mlreg <- lme(I(effects_of_switch/100) ~ I(timing_of_switch_mover), data = edge_data, 
             random=~1|computer_id)
summary(mlreg)

# Nope - this relationship is insignificant (again) for both the enter and edge


# -- C) Use Position of switch (checkpoint)

# - i) Graphs
par(mfrow = c(1,2))
plot(I(effects_of_switch/100) ~ position_of_switch, xlim = c(0,120),
     data = center_data, xlab = "Position of Switch (checkpoint)", ylab = "Effects of switch",  main = "Center")
abline(v = mean(center_data$position_of_switch), col = "Red")
abline(h = 0, col = "black")
plot(I(effects_of_switch/100) ~ position_of_switch, xlim=c(0,120),
     data = edge_data, xlab = "Position of Switch (checkpoint)", ylab = "Effects of switch",  main = "Edge")
abline(v = mean(edge_data$position_of_switch), col = "Red")
abline(h = 0, col = "black")
par(mfrow=c(1,1))

# How fascinating! When we look at the % complete the intial mover is, NOW there seems to be a significant relationship: the longer one waits, the more likely he is to foul up the switch

# - ii) mixed linear reg - center
mlreg <- lme(I(effects_of_switch/100) ~ position_of_switch, data = center_data, 
             random=~1|computer_id)
summary(mlreg)

# - ii) mixed linear reg - edge
mlreg <- lme(I(effects_of_switch/100) ~ position_of_switch, data = edge_data, 
             random=~1|computer_id)
summary(mlreg)

# Nope - this is insignificant again


# -- D) Let's look at switchers who wait a while: give me a dataset of people who waited LONGER THAN THE MEAN!

# - i) New dataset - draw from both center & edge, since tehre's no glaring differences
test <- masterDataExclusions_all_switchers[masterDataExclusions_all_switchers$computer_elapsed_time > mean(masterDataExclusions_all_switchers$computer_elapsed_time),]

# - ii) Graphs
par(mfrow=c(1,2))
plot(I(effects_of_switch/100) ~ I(timing_of_switch_mover), data = test,
     xlab = "Percentage of mover performance", ylab = "Effects of switch", main = "Mover Performance %")
abline(h = 0, col = "black")
plot(I(effects_of_switch/100) ~ I(position_of_switch), data = test,
     xlab = "Position of switch (checkpoint)", ylab = "Effects of switch", main = "Position of switch")
abline(h = 0, col = "black")
par(mfrow=c(1,1))


# iii) LME - Mover Performance
mlreg <- lme(I(effects_of_switch/100) ~ timing_of_switch_mover, data = test, 
             random=~1|computer_id)
summary(mlreg)

# - ii) LME - Checkpoint
mlreg <- lme(I(effects_of_switch/100) ~ position_of_switch, data = test, 
             random=~1|computer_id)
summary(mlreg)

#Both insignificant


# -- D) Cleanup
rm(edge_data)
rm(center_data)
rm(test)



#*****************************************************************************************************
#       Analysis IV: How do switchers determine when to switch? Is it even a good metric?
#*****************************************************************************************************

# --- 1) What do monitors rely on when they decide whether they should switch?

# -- A) Does speed prior to switch predict whent hey switch?

#

# -i) Regression
mlreg <- lm( I(position_of_switch) ~ I(computer_avg_speed_before_switch_z),
             data = masterDataExclusions_all_switchers)
summary(mlreg)

#Wow! Foor every standard deviation increase in speed, the monitors will let the mover complete roughly 30 checkpoints (27)! That's around a quarter of the maze!

# - ii) Graph
plot(position_of_switch ~ computer_avg_speed_before_switch_z, data = masterDataExclusions_all_switchers,
     ylab = "Position of switch (checkpoint)", xlab = ("Avg Mover speed before switch (z)"),
     main = "How does the mover's speed affect when the switch occurs?")
abline(I(lm(position_of_switch ~ computer_avg_speed_before_switch_z, data = masterDataExclusions_all_switchers)))

# Ok - so it really looks like that one's speed prior to the switch predicts when monitor would switch. In other words, the faster the mover is, the more of the maze the monitor will let them complete.


# -- b) Is speed prior to switch a good metric?


# - i) Regression: Does speed BEFORE the switch predict how good a mover is? Use MOVER COMPLETION TIME
mlreg <- lme( I(computer_solo_time/100) ~ I(computer_avg_speed_before_switch_z),
              data = masterDataExclusions_all_switchers,
              random=~1|computer_id)
summary(mlreg)

#NO!!!! Completely insignificant (p=.39)! The speed prior to a switch does NOT PREDICT the computer's completion timie!

# - ii) Regression II: Does speed BEFORE the switch predict how good a mover is? Use MOVER COMPLETION TIME
mlreg <- lme( I(computer_overall_speed) ~ I(computer_avg_speed_before_switch_z),
              data = masterDataExclusions_all_switchers,
              random=~1|computer_id)
summary(mlreg)

#NO!!!! Insignificant (p=.14)! The speed prior to the switch does NOT predict OVERALL SPEED!!!!

# - iii) Plots
par(mfrow=c(1,2))
plot(I(computer_solo_time/100) ~ I(computer_avg_speed_before_switch_z), data = masterDataExclusions_all_switchers,
     ylab = "Computer Solo Time (s)", xlab = "Computer Avg Speed before switch (z)", 
     main = "Speed vs. mover completion Time")
plot(I(computer_overall_speed) ~ I(computer_avg_speed_before_switch_z), data = masterDataExclusions_all_switchers,
     ylab = "Overall Avg Speed (checkpoints/second)", xlab = "Computer avg speed before switch (z)",
     main = "Speed vs. Mover Overall Speed", ylim = c(0.005, 0.025))
par(mfrow=c(1,1))



# Great! So we know the following: (1) Switchers rely on their partner's speed to determine wehther they should switch, but (2) this is a bad metric because your speed before the switch (i.e. like 25-30s in) does NOT predict how well you'll do afterwards!



# --- C) Does this mean that the computer improves way more than the human?

# - i) Significance testing: t-test with improvement indexes
holder_monitor <- data.frame("Improvement Index" = masterDataExclusions_all_switchers$improvement_index,
                             "Condition" = "Monitor")
holder_mover <-  data.frame("Improvement Index" =  masterDataExclusions_all_switchers$computer_post_switch_improvement_index, "Condition" = "Mover")
holder_final <- rbind(holder_monitor, holder_mover)

t.test(holder_final$Improvement.Index ~ as.factor(holder_final$Condition))

#YES!!! If the mover would've allowed the computer to continue, then the computer would've improved MUCH MORE than the monitor! In other wrods, the computer improves by roughly 30% more than the monitor!

# - ii) Graphs
plot(improvement_index ~ computer_post_switch_improvement_index, data = masterDataExclusions_all_switchers,
     xlim = c(-.005, .015), ylim = c(-.005, .015), ylab = "Improvement Index (Monitor)",
     xlab = "Post-Switch Improvement Index (mover)", main = "Who improved more?")
abline(a = 0, b = 1)

#YES!! The computer alLMOST ALWAYS improves more, even visually.




# TODO - Figure out WHAT FACTORS CAUSE PEOPLE TO SWITCH (If they switch) vs. when they swich
# To do this, take the first 30 seconds of mover data an dlook at something like wall rams or average speed,
# and compare that to the first 30 seconds of the switchers data, then see if they differ from eachother.




#*******************************************************************************************************************
#                                            Qualtrics Data
#*******************************************************************************************************************


#************************************************************
#                     Data Preperation
#*************************************************************

# --- 1) "Compared to most people, how skilled do you think you are?"
# -i) Monitor
masterDataExclusions$self_skill_avg_q <- NA
masterDataExclusions$self_skill_avg_q[masterDataExclusions$self_skill_avg == "I am less skilled than most people"] <- -1
masterDataExclusions$self_skill_avg_q[masterDataExclusions$self_skill_avg == "I am about as skilled as most people"] <- 0
masterDataExclusions$self_skill_avg_q[masterDataExclusions$self_skill_avg == "I am more skilled than most people"] <- 1
# - ii) Mover
moverData$self_skill_avg_q <- NA
moverData$self_skill_avg_q[moverData$self_skill_avg == "I am less skilled than most people"] <- -1
moverData$self_skill_avg_q[moverData$self_skill_avg == "I am about as skilled as most people"] <- 0
moverData$self_skill_avg_q[moverData$self_skill_avg == "I am more skilled than most people"] <- 1



# "Compared to MOST PEOPLE, how skilled do you think your partner is?"
masterDataExclusions$partner_skill_avg_q <- NA
masterDataExclusions$partner_skill_avg_q[masterDataExclusions$partner_skill_avg == "They are less skilled than most people"] <- -1
masterDataExclusions$partner_skill_avg_q[masterDataExclusions$partner_skill_avg == "They are about as skilled as most people"] <- 0
masterDataExclusions$partner_skill_avg_q[masterDataExclusions$partner_skill_avg == "They are more skilled than most people"] <- 1

# "Compared to YOU, how skilled do you think your partner is?
masterDataExclusions$partner_skill_you_q <- NA
masterDataExclusions$partner_skill_you_q[masterDataExclusions$partner_skill_you == "They are less skilled than me"] <- -1
masterDataExclusions$partner_skill_you_q[masterDataExclusions$partner_skill_you == "They are about as skilled as me"] <- 0
masterDataExclusions$partner_skill_you_q[masterDataExclusions$partner_skill_you == "They are more skilled than me"] <- 1

# "How often were you thinking about switching?"
masterDataExclusions$switch_thinking_q <- NA
masterDataExclusions$switch_thinking_q[masterDataExclusions$switch_thinking == "Never"] <- 0
masterDataExclusions$switch_thinking_q[masterDataExclusions$switch_thinking == "Some of the time"] <- 1
masterDataExclusions$switch_thinking_q[masterDataExclusions$switch_thinking == "About half the time"] <- 2
masterDataExclusions$switch_thinking_q[masterDataExclusions$switch_thinking == "Most of the time"] <- 3
masterDataExclusions$switch_thinking_q[masterDataExclusions$switch_thinking == "The entire time"] <- 4


#better_on_own: How much better would you be on your own (METRIC)? Absolute
masterDataExclusions$effect_of_having_partner <- NA
masterDataExclusions$effect_of_having_partner <- 
  masterDataExclusions$completion_time - masterDataExclusions$perf_self_solo


#better_on_own: How much better would you be on your own (METRIC)? Absolute & percent
masterDataExclusions$myself_average_comparison_seconds <- NA
masterDataExclusions$myself_average_comparison_percent <- NA
masterDataExclusions$myself_average_comparison_seconds <- 
  masterDataExclusions$perf_avg_pair - masterDataExclusions$completion_time
masterDataExclusions$myself_average_comparison_percent <- 
  round(( masterDataExclusions$perf_avg_pair/masterDataExclusions$completion_time - 1) * 100, 2)

#When should the switch have happened?



#Create new datasets to reflect new variables, and remove the old ones
switchers <- masterDataExclusions[masterDataExclusions$game_mode == 3,]
non_switchers <- masterDataExclusions[masterDataExclusions$game_mode == 2,]
rm(masterDataExclusions_all_switchers); rm(masterDataExclusions_non_switchers);



#************************************************************
#Analysis 1: Do people want to be switched on?
#************************************************************

# --- 1) What do participants think of the switch?

# i) Graph

par(mfrow = c(1,3))
table <- round(prop.table(table(non_switchers$switch_goodbad)), 2)
plot <- barplot(table, main = "Non Switchers (n=53)", ylab = "Proportion", xlab = "Should we have switched?",
                names.arg = c("No", "Yes"))
text(x = plot, y = table, label = table, pos = 1, cex = 1, col = "red")
plot
table <- round(prop.table(table(switchers$switch_goodbad)), 2)
plot <- barplot(table, main = "Switchers (n=91)", ylab = "Proportion", xlab = "Should we have switched?", 
                names.arg = c("No", "Yes"))
text(x = plot, y = table, label = table, pos = 1, cex = 1, col = "red")
plot
table <- round(prop.table(table(moverData$switch_goodbad)), 2)
plot <- barplot(table, main = "Movers (n=49)", ylab = "Proportion", xlab = "Should we have switched?", 
                names.arg = c("No", "Yes"))
text(x = plot, y = table, label = table, pos = 1, cex = 1, col = "red")
plot
par(mfrow=c(1,1))


#Wow! First of all, non-switchers are VERY happy with their decision not to switch, as evident by 95% agreeing that theys houldn't have swithced!
# Secondly, most switchers (70%) think they should've switched. HOWEVER, 75% of movers don't want to be switched on!


# --- 2) Do the movers who are switched on want to be switched on?

par(mfrow=c(1,2))
table<-round(prop.table(
  table(masterDataExclusions[masterDataExclusions$computer_switch_goodbad == "Partner wanted switch",]$switch)),2)
plot <- barplot(table, main = "Movers who wanted switch", 
                xlab = "Were they switched on?", ylab = "Proportion", names = c("No", "Yes"))
text(x = plot, y = table, label = table, pos = 1, cex = 1, col = "red")
plot
table<-round(prop.table(
  table(masterDataExclusions[masterDataExclusions$computer_switch_goodbad == "Partner didn't want switch",]$switch)),2)
plot <- barplot(table, main = "Movers who did NOT want switch", 
                xlab = "Were they switched on?", ylab = "Proportion", names = c("No", "Yes"))
text(x = plot, y = table, label = table, pos = 1, cex = 1, col = "red")
plot
par(mfrow=c(1,1))

#Wow. I guess it doesn't matter whether or not you want to be switched on: regardless of what you think, you'll be switched on 70% of the itme


# ii) Among ALL movers, how many are happy with the monitor's decision?


t1 <- table(masterDataExclusions[masterDataExclusions$computer_switch_goodbad == "Partner wanted switch",]$switch)
names(t1) <- c("Dissatisfied", "Satisfied")
t2 <- table(masterDataExclusions[masterDataExclusions$computer_switch_goodbad == "Partner didn't want switch",]$switch)
names(t2) <- c("Satisfied", "Dissatisfied")
n <- intersect(names(t1), names(t2))
combined_table <- t1[n] + t2[n]

plot <- barplot(prop.table(combined_table), main = "How many are satisfied with the switch decision?", 
                xlab = "Satisfied with switch decision?", ylab = "Proportion")
text(x = plot, y = prop.table(combined_table), 
     label = round(prop.table(combined_table),2), pos = 1, cex = 1, col = "red")
plot

#Wow - so it's roughly a 50/50 split in that half are dissatisfied and the other half are satisfied

# iii) Different view of switching

ggplot(masterDataExclusions, aes(x = computer_name, y = switch, color = effects_of_switch > 0)) + geom_point(position = position_jitter(w = 0, h = .1)) + facet_wrap(~computer_switch_goodbad, scales="free") + scale_color_manual(values=c("green","red")) + labs(colour = "Switching Hurt Performance?") + xlab("Partner Name") + ylab("Did the switch happen?")


# --- 2) What motivates switchers/movers into wanting/not wanting to switch? Is it objective skill?

# - i) Graphs

par(mfrow=c(1,2))
boxplot(completion_time ~ switch_goodbad, data = moverData, main = "Objective Skill vs. Switch Desire (Mover)",
     xlab = "Should we have switched roles?", names = c("No", "Yes"), ylab = "Completion Time (s)")
boxplot(I(effects_of_switch/100) ~ switch_goodbad, data = switchers, 
        main = "Objective Skill vs. Switch Desire (Switcher)", xlab = "Should we have switched roles?", 
        names  = c("No", "Yes"), ylab = "Effects of Switch (s)")
par(mfrow=c(1,1))

#Wow! There doesn't appear to be any OBJECTIVE skill difference: in both the mover and monitor condition, the people who said yes/no are roughly equally skilled! 

#However, in the mover condition, people who were objectively bad usually wanted to be switched on, as evident by the greater distribution under the 'yes' catagory. 
#Within switchers, there's a HUGE distribution of people who are happy with switching roles: regardless of whether ot not someone was outrageously bad or good, people all over the spectrum were happy with the switch, as evident from the distribution under the 'yes'c atagory. However, people who didn't like the switch were usually a bit below average, as evident by the smaller distribution 



# --- 3) Are switchers perceptions of the switch accurate?

plot(effect_of_having_partner ~ I(effects_of_switch/100), switchers, 
     xlab = "Effect of switch (s): how much my switch HURT partner", 
     ylab = "Perception of how much partner hurt me (s)",
     main = "Are people good judges of the impact of switch?",
     xlim = c(-150,150), ylim = c(-150, 150))
abline(v = 0, col = "black")
abline(h = 0, col = "black")

# effect_of_having_partner = your_completion_time - your_completion_time_without_partner
# 60s - 100 s = -40s; not having a partner would improve your performance by 40s
# 120s - 60s = 60s; having a partner hurt you by 60s






#************************************************************
#Analysis 2: How does switching affect likeability?
#************************************************************

# --- 1) How does switching affect likeability? Non switchers vs- switchers

par(mfrow=c(1,3))
boxplot(Partner_Likeability ~ I(game_mode == 2), data = masterDataExclusions, 
        main = "Partner Likeability",
        xlab = "Grou[p", names = c("Switchers", "Non Switchers"), ylab = "Completion Time (s)")

table <- round(prop.table(table(switchers$Partner_Likeability)), 2)
plot <- barplot(table, main = "Switchers", ylab = "Proportion", xlab = "Partner Likeability")
text(x = plot, y = table, label = table, pos = 1, cex = 1, col = "red")

table <- round(prop.table(table(non_switchers$Partner_Likeability)), 2)
plot <- barplot(table, main = "Non Switchers", ylab = "Proportion", xlab = "Partner Likeability")
text(x = plot, y = table, label = table, pos = 1, cex = 1, col = "red")
par(mfrow=c(1,1))

#Wow: non switchers usually liked their partner A LOT (i.e. nearly all of them gave their partner a likeability rating of 4+). Switchers, however, were a bit more variable: they tended to like their partner a lot less


# --- 2) Does likeability have to do with the timing of the switch?

par(mfrow=c(1,3))
table <- round(table(switchers$Partner_Likeability), 2)
plot <- barplot(table, main = "Sample Size of Likeability bins", ylab = "Sample Size", xlab = "Partner Likeability")
text(x = plot, y = table, label = table, pos = 1, cex = 1, col = "red")
boxplot(position_of_switch ~ Partner_Likeability, data = masterDataExclusions_all_switchers, 
        xlab = "Likeability", ylab = "Position of switch (checkpoint)",
        main = "Does position of switch affect likeability?")
plot(computer_avg_speed_before_switch_z ~ position_of_switch, data = masterDataExclusions_all_switchers,
     main = "Speed at time of switch", xlab = "Mover speed before switch")
reg <- lm(computer_avg_speed_before_switch_z ~ position_of_switch, data = masterDataExclusions_all_switchers)
abline(reg)
par(mfrow=c(1,1))

#Wow! It looks the longer people wait on a switch, the more they like them! This is likely because the longer someone waits, the more skill their partner becomes, and the more the monitor sympathiszes with them before they switch.

# - ii) Do movers get more skilled as time goes on, which affects the likeability rating?
reg <- lme(computer_avg_speed_before_switch_z ~ position_of_switch, data = masterDataExclusions_all_switchers
          , random=~1|computer_id)
summary(reg)

#Yep - effect is significant.




#************************************************************
#Analysis 3: Are people accurate judges of their performance?
#************************************************************

#
