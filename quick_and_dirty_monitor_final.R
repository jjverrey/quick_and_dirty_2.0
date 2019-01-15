#install.packages("nlme")

library(ggplot2)
library(nlme)

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

#And now let's also seperate the switchers who hurt performance from those who helped
masterDataExclusions_good_switchers <- 
  subset(masterDataExclusions_all_switchers, !switch_hurt_performance)
masterDataExclusions_bad_switchers <- 
  subset(masterDataExclusions_all_switchers, switch_hurt_performance)

nrow(masterDataExclusions_good_switchers) - nrow(masterDataExclusions_bad_switchers)


# Huh - There wer 91 total switchers. This means that 32 were good switchers and 59 were bad switchers.
# People must usually be bad at switching roles then...


# --- 6) Subset-specific variable creation

# -- a) Effects of Switch Zs among those who switched ONLY
masterDataExclusions_all_switchers$effects_of_switch_z <- (masterDataExclusions_all_switchers$effects_of_switch - mean(masterDataExclusions_all_switchers$effects_of_switch))/sd(masterDataExclusions_all_switchers$effects_of_switch)


# -- b) First 30s z-scores for switchers ONLY (non-switchers speed values of -1)

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
     main = "Effect of switch histogram Seconds", breaks = 10,
     xlab = "Effect of Switch (s)")
abline(v=mean(masterDataExclusions_all_switchers$effects_of_switch/100), col = "red")

hist(masterDataExclusions_all_switchers$effects_of_switch_z,
     main = "Effect of switch histogram Zs", breaks = 10,
     xlab = "Effect of Switch (z)")
abline(v=mean(masterDataExclusions_all_switchers$effects_of_switch_z), col = "red")
par(mfrow=c(1,1))

# The graph appears pretty close to norally distributed. This is very surprising: why is there such a wide distribution regarding whether the switch is optimal?



#****************************************************************************************************
#       Analysis II: Why is there a skill performance in switchers? (and also non-switchers?) 
#****************************************************************************************************

# H-1) Maybe the good switchers switched on the bad movers & the bad switchers switched on good movers? In other words, maybe bad switchers switched on the wrong people.

# H-2) Maybe there's a skill difference. In other words, the monitors whose switch HELPED performance the most may have simply been the most skilled.

# --- 1) H-1: Incorrect switch hypothosis

# -- A) Incorrect switch hypothosis for SWITCHERS

# - i) Graphs

#Seconds
plot(I(effects_of_switch/100) ~ I(computer_solo_time/100), data = masterDataExclusions_all_switchers, 
     xlab = "Mover (computer) completion time WITHOUT switch (s)", ylab = "Effect of switch (s)",
     main = "Effects of switch from monitor vs. Initial Mover completion time")
abline(h = 0, col = "black")
#This is the mean of ALL Movers, not just the movers who were switchd on
abline(v = mean(masterDataExclusions$computer_solo_time/100), col = "pink") 
#This is the mean of the movers who were switched on 
abline(v = mean(masterDataExclusions_all_switchers$computer_solo_time/100), col = "red")
#This is the mean of the movers who were NOT switched on
abline(v = mean(masterDataExclusions_non_switchers$computer_solo_time/100), col = "purple") 

#Plot reg
reg_s <- lm(I(effects_of_switch/100) ~ I(computer_solo_time/100), 
          data = masterDataExclusions_all_switchers)
abline(reg_s, col = "blue")


# The graph shows the following: the WORSE an initial mover does overall, the BETTER the switch would be (i.e. the effects of switch are negative & make the team do better).

# However, what's striking about the graph is the following: most people who are switched on visually appear to be above average (i.e. there are many dots to the left of the pink line, the mean completion time of movers.) AND, the movers who are CLOSE TO AVERAGE, these guys, if they're switched on, WILL HAVE THEIR PERFORMANCE WORSENED! It seems that you shouldn't switch on the AVERAGE JOE, since you will HURT his performance.

# - ii) Regression - Zs
reg_z <- lm(I(effects_of_switch_z) ~ I(computer_solo_time_z), 
            data = masterDataExclusions_all_switchers)
summary(reg_z) #computer_solo_time = b * effects_of_switch + a
rm(reg_z)

# For every one standard deviation WORSE that the intial mover was, the MONITOR HELPED performance by 1/2 a standard deviation (-.49 zs)

# - iii) Regression - SECONDS
summary(reg_s) #computer_solo_time = b * effects_of_switch + a

# For every one second WORSE the initial mover was, the MORE the switch HELPED performance by .6 seconds. In other words, it's just as we would expect: the worse the mover is, the more the act of switching will help him. 


# - iv) Seconds regression 1: What happens if the monitor switched on the perfectly average mover?

#y = b * x + a
reg_s$coefficients[2] * mean(masterDataExclusions$computer_solo_time/100) + reg_s$coefficients[1]

# However, as expected visually from the graph, if you were to switch on the perfectly average participant, your act of switching will hurt his performance by 13 seconds!


# - v) Seconds regression 2: the average mover didn't switch on the average Joe; they switched on someone insignificantly worse than the average Joe (roughly 2-3 seconds worse). What happens if they switch on him?

#y = b * x + a
reg_s$coefficients[2] * mean(masterDataExclusions_all_switchers$computer_solo_time/100) + reg_s$coefficients[1]

# There's not much of a difference here: the average switcher hurt the barely-less-than-average Joe's performance by roughly 10 seconds.


# - vi) Seconds regression 3: How bad must someone be in order for the act of switching to be beneficial?

#y = b * x + a. If y (effects of switch) = 0 (break even point), then 0 = bx+a; x = -a/b
break_even_point <- -reg_s$coefficients[1]/reg_s$coefficients[2] #118.71s
(break_even_point - mean(moverData$completion_time))/sd(moverData$completion_time)# Z-score equivlence: .5zs
rm(break_even_point)

# Woah: you really need to suck at the maze in order for switching ot help performance: you need to do the maze in 118.7s, which is 19.2 seconds WORSE THAN AVERAGE! In other words, you neeed to be 1/2 a standard deviation WORSE THAN THE AVERAGE JOE for swtiching to take effect! The problem is though that most people switch ont he average Joe, which produces the adverse effects


# - vii) Seconds regression 4: What would've happened if the average NON-SWITCHER switched on the average SWITCHER

#y = b * x + a
reg_s$coefficients[2] * mean(masterDataExclusions_non_switchers$computer_solo_time/100) + reg_s$coefficients[1]
rm(reg_s) 

#Amazing: If the average NON-SWITCHER would have switched on their partner, they would've hurt their teams performance by 18.78 seconds, which is ~8.8 seconds more damaging than the average switcher. To be hoenst though, this 8.8 seconds isn't that much more.

### H-1 : Supported by data! People really don't know when to switch: you need to switch on someone 1/2 a SD WORSE than the average Joe for switching to be optimal, but the problem is that people usually switch on the average Joe, which produces the suboptimal effects!


# -- b) Incorrect switching hypothosis: Switchers vs. Non Switchers

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

# Take away: Movers who aren't switched on are only 1/4th a SD better than those who are switched on, which isn't that much.


# - iii) Regression (to control for something)
reg <- glm( (game_mode-2) ~ computer_solo_time_z, 
            data = masterDataExclusions, family = binomial(link='logit'))
summary(reg)


# I forgot what this controls for, but it holds the relationship. 


# H-1: NOT SUPPORTED for Switchers vs. No Switchers: no switchers only refuse to switch on people marginally better than the average Joe


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

plot(I(overall_avg_speed_z) ~ I(effects_of_switch/100), data = masterDataExclusions_all_switchers,
     ylab = "Overall Avg Speed (z)", xlab = "Effects of switch (s)", ylim = c(-5,5),
     main = "Overall Speed vs. Effect of Switch (z)")
reg <- lm(I(overall_avg_speed_z) ~ I(effects_of_switch/100), data = masterDataExclusions_all_switchers)
abline(reg, col = "blue")
abline(v = 0, col = "gray")
abline(h = 0, col = "gray")
par(mfrow=c(1,1))

#Ok -  so it looks like a slight decrease in speed leads to a a pretty severe PENTALTY when you're looking at the effect of switching. In other words, people who switched who really hurt performance tended to have worse speeds.

#TODO - do same thing for imrpovment Index & other measures.

# iv) Reg: What does a 1 standard deviation increas
sd(masterDataExclusions_all_switchers$effects_of_switch/100)

# -- a) Improvement index
mlreg = lme(I(improvement_index * 1000) ~ effects_of_switch, 
data = masterDataExclusions_all_switchers, random=~1|computer_id)
summary(mlreg)

#The 

```{r}
mlreg = lme(completion_time ~ I(improvement_index * 1000), 
data = masterDataExclusions_all_switchers, random=~1|computer_id)
summary(mlreg)

```

```{r}
summary(masterDataExclusions_all_switchers$improvement_index * 1000)
```


Avg Speed


```{r}
mlreg = lme(switch_hurt_performance ~ I(overall_avg_speed * 1000), 
data = masterDataExclusions_all_switchers, random=~1|computer_id)
summary(mlreg)
```

```{r}
mlreg = lme(completion_time ~ I(overall_avg_speed * 1000), 
data = masterDataExclusions_all_switchers, random=~1|computer_id)
summary(mlreg)

```

```{r}
summary(masterDataExclusions_all_switchers$overall_avg_speed * 1000)
```


Wall Rams

```{r}
mlreg = lme(switch_hurt_performance ~ human_reset_counter, 
data = masterDataExclusions_all_switchers, random=~1|computer_id)
summary(mlreg)
```

```{r}
mlreg = lme(completion_time ~ human_reset_counter, 
data = masterDataExclusions_all_switchers, random=~1|computer_id)
summary(mlreg)

```

```{r}
summary(masterDataExclusions_all_switchers$human_reset_counter)
```


### H-2: Not supported: There is no skill difference





### H-1 Follow Up: What causes this difference in who is switched on? Maybe bad switchers & good switchers have different switching strategies?

### H-1-A) Maybe bad switchers switch too early, and that's why they don't get an accurate measure of performance?

```{r}
par(mfrow=c(2,2))

#Do not graph GROUP eprformance - see note below

#Timing of Switch - Good Sw - MOVER PERF (timing_of_switch/mover_total_completion_time)
hist(masterDataExclusions_good_switchers$timing_of_switch_mover, 
     xlab = "Timing of switch (%) (MOVER)", ylab = "Freq", 
     main = "Timing of switch. Good Sw.", xlim=c(0,100), breaks = 10)
abline(v = mean(masterDataExclusions_good_switchers$timing_of_switch_mover), col = "red")

#Timing of Switch - bad Sw - MOVER PERF (timing_of_switch/mover_total_completion_time)
hist(masterDataExclusions_bad_switchers$timing_of_switch_mover, 
     xlab = "Timing of switch (%) (MOVER)", ylab = "Freq", 
     main = "Timing of switch. Bad Sw.", xlim=c(0,100), breaks = 10)
abline(v = mean(masterDataExclusions_bad_switchers$timing_of_switch_mover), col = "red")

boxplot(timing_of_switch_mover ~ switch_hurt_performance, main = "Dist of Timing of Switch",
        data = masterDataExclusions_all_switchers, ylab = "Timing of switch (%) (mover)")
par(mfrow = c(1,1))
```

Ok - this might look confusing, but I think the mover's performanc eis more reliable: we want to see when the monitor switched RELATIVE TO THE MOVER'S performance because that's a good proxy for what % of the maze the mover was done with. We don't want to see when the timing of the switch for the group performance because the monitor's performancfe confounds the initial mover's performance.

The distribution also appears a lot different between good vs. bad switchers: bad switchers have a much larger distribution than good switchers, as evident by the box plot. Perhaps this means that bad switchers are a lot more ambivalent as to whether they should switch on their good partner, meaning tghey hesitate more while switching?
  
  With that in mind, it seems that the bad switchers actually wait LONGER to switch! Let's confirm this via mixed linaer effects

```{r}
mlreg = lme(timing_of_switch_mover ~ switch_hurt_performance, 
data = masterDataExclusions_all_switchers, random=~1|computer_id)
summary(mlreg)
```

Ok - so good switchers usually wait until the mover is 22% done, and then they hit the switch button. The mean completion time of movers who were switched on for good swithcers is 122 seconds, so the average good switcher waits 26.89 seconds, on average, before deciding to hit the switch button.

Bad switchers, however wait a bit longer to hit the switch button: they wait until their partner is roughly 35% done with their performance (22 + 13 = 35. The mean completion time of movers who were switched on for bad switchers is 90.76 seconds, so this translates onto 31.77 seconds.

In short, because the bad switchers wait longer than good switchers when deciding whether or not to switch, this could indicate that bad switchers are a bit more ambivalent as to whether or not to swtich. Nevertheless, they still choose to switch despite teh ambiguity


### H-1-A COnfirmed: People who are bad at switching switch later than those who are good at switching, perhaps because they are more ambivalent & hesitant as to wheteher to switch.

### H-2: Maybe there's a skill difference?
  


## 2) Why cuases someone to press the "switch" button? For ALL switchers


### A) Let's look at all the maze metrics together.

```{r}
mlreg <- lme( (game_mode-2) ~ computer_total_pixel_movement +  computer_overall_speed + computer_improvement_index + I(computer_solo_time/100) + I(computer_avg_speed_before_switch * 1000) + computer_reset_counter_at_time_of_switch + computer_improvement_index, data = masterDataExclusions, random=~1|computer_id)

summary(mlreg)
```



```{r}
mlreg <- lme( (game_mode-2) ~ I(computer_avg_speed_before_switch * 1000), data = masterDataExclusions, random=~1|computer_id)

summary(mlreg)
```


```{r}
mean(masterDataExclusions$computer_total_reset_counter)
```

How disappointing: NONE of these values explain why people chose to switch versus not swtich. The only thing that sort of explains it is the computer's total reset counter, but even then, running into a wall only makes one 3.6% more likely to switch roles PER HIT. The average person htis a wall 3-4 times, so that translates to a 10.8 to 14.4% increase in likeliness for switching.


### B) Is there any difference for good vs. bad switchers?

#### B-1) Good Switchers

```{r}
mlreg <- lme(switch_hurt_performance ~ I(computer_solo_time/100) + computer_total_reset_counter + computer_total_pixel_movement +  computer_overall_speed + computer_improvement_index, data = masterDataExclusions_all_switchers, random=~1|computer_id)

summary(mlreg)
```

No difference.

## 3) WITHIN the switch condition, what are the effects of switching?

### First of all, when do people switch?



```{r}
plot(completion_time ~ timing_of_switch, data = masterDataExclusions_switchers, xlab = "Timing of switch (%)", ylab = "Completion Time (s)")
abline(lm(completion_time ~ timing_of_switch, data = masterDataExclusions_switchers), col = "red")
```

```{r}
mlreg <- lme(completion_time ~ timing_of_switch, data = masterDataExclusions_switchers, random=~1|computer_id)

summary(mlreg)
```

Wow - even when you wait, you're stil not gonna be good at switching

### Let's use improvement indexes. First, we need to establish what it means.
```{r}
#First of all, let's convert these 'improvement indexes' into meaningful units
masterDataExclusions_switchers$improvement_index <- 
  masterDataExclusions_switchers$improvement_index * 1000
masterDataExclusions_switchers$computer_improvement_index <- 
  masterDataExclusions_switchers$computer_improvement_index * 1000
masterDataExclusions_switchers$computer_post_switch_improvement_index<- 
  masterDataExclusions_switchers$computer_post_switch_improvement_index * 1000

#Next, let's see how this relates to completion_time
```

```{r}
par(mfrow=c(1,3))
#Monitors
eq <- completion_time ~ improvement_index
plot(eq , data = masterDataExclusions_switchers, ylab = "Completion Time (s)", xlab = "Improvement Index", main = "Monitors")
mon_impr_ind_reg <- lm(eq, data = masterDataExclusions_switchers)
abline(mon_impr_ind_reg, col = "red")

#Movers (Improvement)
eq <- I(computer_solo_time/100) ~ computer_improvement_index
plot(eq , data = masterDataExclusions_switchers, ylab = "Completion Time (s)", xlab = "Improvement Index", main = "Movers - Improvement Index")
mov_impr_ind_reg <- lm(eq, data = masterDataExclusions_switchers)
abline(mov_impr_ind_reg, col = "red")

#Movers (post-switch)
eq <- I(computer_solo_time/100) ~ computer_post_switch_improvement_index
plot(eq , data = masterDataExclusions_switchers, ylab = "Completion Time (s)", xlab = "Post Switch Improvement Index", main = "Movers - Post Switch Improvement Index")
mov_post_switch_impr_ind_reg <- lm(eq, data = masterDataExclusions_switchers)
abline(mov_post_switch_impr_ind_reg, col = "red")
par(mfrow=c(1,1))
```

Ok - so it looks like that the more you improve, the better your time gets. Let's seeexactly how much

```{r}
summary(mon_impr_ind_reg)
```

```{r}
#For computer improvement index
summary(lme(I(computer_solo_time/100) ~ computer_improvement_index, data = masterDataExclusions_switchers, random=~1|computer_id))
```

Good - a 1 unit increase in the IMPROVEMENT INDEX leads to a ~3 second decrease in completion time




# Part IV) Qualtrics

### How do you feel about switching?
```{r}
par(mfrow=c(1,3))

#Non switchers
barplot(prop.table(table(masterDataExclusions_non_switchers$switch_goodbad)), main = "Non switchers - Should you switch?", ylab = "Proportion", names.arg = c("No","Yes"), ylim=c(0,1))

#Good switchers (helped performance)
barplot(prop.table(table(subset(masterDataExclusions_switchers, effects_of_switch < 0 )$switch_goodbad)), main = "Switchers, improved performance", ylab = "Proportion", names.arg = c("No","Yes"), ylim=c(0,1))

#Bad switchers (hurt performance)
barplot(prop.table(table(subset(masterDataExclusions_switchers, effects_of_switch >= 0 )$switch_goodbad)), main = "Switchers, hurt performance", ylab = "Proportion", names.arg = c("No","Yes"), ylim=c(0,1))

par(mfrow=c(1,1))
```

Switching is a lot more ambigious; no dif between non-switchers & switchers.

### Did the switch happen at the right time?
```{r}
par(mfrow=c(1,2))

#Good switchers (helped performance)
barplot(prop.table(table(subset(masterDataExclusions_switchers, effects_of_switch < 0 )$timing_real))[2:4], main = "Switchers, improved performance", ylab = "Proportion", ylim=c(0,1), names.arg = c("Earlier", "Later", "Just Right"))

#Bad switchers (hurt performance)
barplot(prop.table(table(subset(masterDataExclusions_switchers, effects_of_switch >= 0  )$timing_real))[2:4], main = "Switchers, hurt performance", ylab = "Proportion", ylim=c(0,1), names.arg = c("Earlier", "Later", "Just Right"))


par(mfrow=c(1,1))
```

Most switchers think they switched at the right moment, but the people who hurt performance think the switch should've been later. It's still remarkable that they're niave to the fact that they hurt performance.


### Do you like your partner?
```{r}
par(mfrow=c(2,2))

#Non switchers
barplot(prop.table(table(masterDataExclusions_non_switchers$Partner_Likeability)), main = "Non switchers - like partner?", ylab = "Proportion", ylim=c(0,.5))

#All switchers (helped performance)
barplot(prop.table(table(masterDataExclusions_switchers$Partner_Likeability)), main = "All Switchers", ylab = "Proportion", ylim=c(0,.5))

#Good switchers (helped performance)
barplot(prop.table(table(subset(masterDataExclusions_switchers, effects_of_switch < 0 )$Partner_Likeability)), main = "Switchers - helped performance", ylab = "Proportion", ylim=c(0,.5))

#Bad switchers (hurt performance)
barplot(prop.table(table(subset(masterDataExclusions_switchers, effects_of_switch >= 0 )$Partner_Likeability)), main = "Non switchers - hurt performance", ylab = "Proportion", ylim=c(0,.5))

par(mfrow=c(2,2))
```





#### Do partners wanna be switched on?

```{r}
ggplot(masterDataExclusions, aes(x = computer_name, y = switch, color = effects_of_switch > 0)) + geom_point(position = position_jitter(w = 0, h = .1)) + facet_wrap(~computer_switch_goodbad, scales="free") + scale_color_manual(values=c("green","red")) + labs(colour = "Switching Hurt Performance?") + xlab("Partner Name") + ylab("Did the switch happen?")

