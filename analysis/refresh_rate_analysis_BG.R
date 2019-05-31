library(readr)
library(dplyr)
library(jsonlite)
library(ggplot2)
library(tidyr)

all.data <- read_csv2('data/refresh_rate_data.csv')

## 

refresh.data <- all.data %>% filter(trial_type == "estimate-refresh-rate")
subjects <- unique(refresh.data$prolific_pid)
combined.data <- NA
for(s in subjects){
  frames.diff <- fromJSON((refresh.data %>% filter(prolific_pid == s))$frames_diff[1])
  frames.raw.inside <- fromJSON((refresh.data %>% filter(prolific_pid == s))$frames_raw_inside[1])
  frames.raw.fn <- fromJSON((refresh.data %>% filter(prolific_pid == s))$frames_raw_fn_param[1])
  frame.type <- c(
    rep('frames.diff', length(frames.diff)),
    rep('frames.raw.inside', length(frames.raw.inside)),
    rep('frames.raw.fn', length(frames.raw.fn))
  )
  subject.data <- data.frame(
    frame.type,
    index=c(1:length(frames.diff), 1:length(frames.raw.inside), 1:length(frames.raw.fn)),
    value=c(frames.diff, frames.raw.fn, frames.raw.inside)
  )
  subject.data$subject <- s
  if(is.na(combined.data)){
    combined.data <- subject.data
  } else {
    combined.data <- rbind(combined.data, subject.data)
  }
}

## final estimate

final.estimate <- combined.data %>% filter(frame.type=="frames.diff") %>% group_by(subject) %>% summarize(frame.rate = 1000/mean(value))
ggplot(final.estimate, aes(x=frame.rate))+geom_histogram(binwidth=1)

## cumulative estimate

cumulative.estimate <- combined.data %>% filter(frame.type=="frames.diff") %>% group_by(subject) %>% mutate(estimated.frame.rate = 1000/cummean(value))
ggplot(cumulative.estimate, aes(x=index, y=estimated.frame.rate, group=subject))+
  geom_line()+
  labs(x="Number of Frames Recorded", y="Estimated Frame Rate (Hz)")+
  theme_bw()

frame.diff.2 <- combined.data %>% filter(frame.type=="frames.raw.fn") %>% mutate(diff=value-lag(value)) %>% filter(index > 1) %>%
  group_by(subject) %>% mutate(estimated.frame.rate = 1000/cummean(diff))
ggplot(frame.diff.2, aes(x=index, y=estimated.frame.rate, group=subject))+
  geom_line()+
  labs(x="Number of Frames Recorded", y="Estimated Frame Rate (Hz)")+
  theme_bw()

## Procedure for estimating variance

# 1. Sample *n* random time points, calculate estimated frame rate.
# 2. Repeat *t* times.
# 3. Calculate variance in estimated frame rates among *t* samples.
# 4. Repeat for *n* = 1 to *max*
# 5. Repeat for all subjects

t.reps <- 1000

generate.variance.estimate <- function(frames, n.min, n.max, t.reps){
  output <- data.frame(number.of.frames=n.min:n.max)
  variance.estimate <- sapply(output$number.of.frames, function(n){
    var(replicate(t.reps, {mean(sample(frames,n, replace=T))}))
  })
  output$variance <- variance.estimate
  return(output)
}

combined.data$subject <- as.factor(combined.data$subject)
all.subjects <- as.data.frame(levels(combined.data$subject))
names(all.subjects) <- "subjID"

# for (i in 1:nrow(all.subjects)) {
#   test.frames <- (combined.data %>% filter(frame.type=="frames.diff", subject==all.subjects$subjID[i]))$value
#   subject.var.est <- generate.variance.estimate(test.frames, 1, 500, 1000)
#   png(paste("variance_over_trials_subj_", all.subjects$subjID[i], ".png", sep=""))
#   print(plot(subject.var.est$variance, main = paste("Subject ID: ", all.subjects$subjID[i], sep="")))
#   dev.off()
# }

## Frame rate histogram by device type
# does the variability in frame rate estimates come from a certain type of device?
all.device.type <- all.data %>% filter(trial_type=="survey-multi-choice")
ppt.device.types <- NA
# remove subject in row 64 - no response, JSON isn't valid 
all.device.type <- all.device.type[-64,]
subjects.device.type <- subjects[-63]
for (j in subjects.device.type) {
  device.type <- fromJSON((all.device.type %>% filter(prolific_pid == j))$responses[1])$Q0 
  subj.device.data <- data.frame(subjID=j, device = device.type)
  if(is.na(ppt.device.types)){
    ppt.device.types <- subj.device.data
  } else {
    ppt.device.types <- rbind(ppt.device.types, subj.device.data)
  }
}

ppt.estimates.devices <- merge(final.estimate, ppt.device.types, by.x = "subject", by.y = "subjID")
ppt.estimates.devices$device <- as.factor(ppt.estimates.devices$device)

ggplot(ppt.estimates.devices, aes(x=frame.rate)) +
  geom_histogram(binwidth=1) +
  facet_wrap(~device, scales = "free_y")
# it looks like the variability in frame rates occurs across all device types

## check the by-participant distributions 
# are there clear outliers that could be strongly influencing the frame rate estimates?
combined.data.devices <- merge(combined.data, ppt.device.types, by.x = "subject", by.y = "subjID")
combined.data.devices.frame.diff <- subset(combined.data.devices, frame.type=="frames.diff")
ggplot(combined.data.devices.frame.diff, aes(x=reorder(subject,value,FUN=median), y=value)) +
  geom_boxplot(outlier.colour = "red") +
  scale_y_continuous(limits = c(0,140)) +
  facet_wrap(~device)
# there are definitely outlier values so some sort of trimming is needed

for (k in subjects.device.type) {
  subj.data <- subset(combined.data.devices.frame.diff, subject == k)
  subj.data <- droplevels(subj.data)
  png(paste("frame_rate_boxplot_", k, ".png", sep = ""))
  print(ggplot(subj.data, aes(x=subject, y=value)) +
    geom_boxplot(outlier.colour = "red"))
  dev.off()
}

## summary of by-participant variability
ppt.variability <- combined.data.devices.frame.diff %>% group_by(subject) %>% summarize(
  frame.diff.min = min(value),
  frame.diff.max = max(value),
  frame.diff.mean = mean(value),
  frame.diff.SD = sd(value),
  frame.diff.N = length(value))
ppt.variability.min.summary <- summary(ppt.variability$frame.diff.min)
ppt.variability.max.summary <- summary(ppt.variability$frame.diff.max)
ppt.variability.mean.summary <- summary(ppt.variability$frame.diff.mean)
ppt.variability.sd.summary <- summary(ppt.variability$frame.diff.SD)
ppt.variability.n.summary <- summary(ppt.variability$frame.diff.N)

## try removing outliers and recompute the final estimate
# SD-based
combined.data.devices.frame.diff.no.outliers.sd <- 
  combined.data.devices.frame.diff %>%
  group_by(subject) %>%
  filter(!(abs(value - median(value)) > 2*sd(value))) 
# trimmed mean: remove top/bottom 10%
combined.data.devices.frame.diff.no.outliers.10pc <- 
  combined.data.devices.frame.diff %>%
  group_by(subject) %>%
  arrange(subject, value) %>%
  filter(row_number() / n() >= .1 & row_number() / n() <= .9) 
  
# calculate estimated rate in Hz
final.estimate.no.outliers.sd <- combined.data.devices.frame.diff.no.outliers.sd %>% group_by(subject) %>% summarise(frame.rate = 1000/mean(value), device=unique(device))
final.estimate.no.outliers.10pc <- combined.data.devices.frame.diff.no.outliers.10pc %>% group_by(subject) %>% summarise(frame.rate = 1000/mean(value), device=unique(device))
# merge with final estimate data (no trimming) for comparison
final.estimates.combined <- merge(final.estimate, final.estimate.no.outliers.sd, by = "subject")
names(final.estimates.combined) <- c("subject", "frame.rate.original", "frame.rate.no.outliers.sd", "device")
final.estimates.combined.all <- merge(final.estimates.combined, final.estimate.no.outliers.10pc, by = c("subject","device"))
names(final.estimates.combined.all)[which(names(final.estimates.combined.all) == "frame.rate")] <- "frame.rate.no.outliers.10pc"
#write.csv(final.estimates.combined.all,"data/refresh_rate_data_estimates.csv",row.names = FALSE)

# compare histograms
ggplot(final.estimates.combined.all, aes(x=frame.rate.original)) +
  geom_histogram(binwidth=1) +
  scale_x_continuous(limits=c(0,160),name="Estimate without trimming") +
  scale_y_continuous(limits=c(0,300))
ggplot(final.estimates.combined.all, aes(x=frame.rate.no.outliers.sd)) +
  geom_histogram(binwidth=1) +
  scale_x_continuous(limits=c(0,160),name="Estimate with outliers +/- 2 SDs trimmed") +
  scale_y_continuous(limits=c(0,300))
ggplot(final.estimates.combined.all, aes(x=frame.rate.no.outliers.10pc)) +
  geom_histogram(binwidth=1) +
  scale_x_continuous(limits=c(0,160),name="Estimate with top/bottom 10% trimmed") +
  scale_y_continuous(limits=c(0,300))

final.estimates.combined.long <- gather(final.estimates.combined.all, estimate.type, estimate, frame.rate.original:frame.rate.no.outliers.10pc)
ppts.ordered.by.original <- final.estimate$subject[order(final.estimate$frame.rate)]
ggplot(final.estimates.combined.long, aes(x=subject, 
                                          y=estimate, colour=estimate.type)) + 
  geom_point() +
  scale_x_discrete(limits=ppts.ordered.by.original) +
  scale_colour_manual(values = c("frame.rate.original" = "blue", "frame.rate.no.outliers.sd" = "red", "frame.rate.no.outliers.10pc" = "green"),
                      breaks = c("frame.rate.original", "frame.rate.no.outliers.sd", "frame.rate.no.outliers.10pc"),
                      labels = c("mean of all data","SD-based trimming", "top/bottom 10% trimmed"))

# separate plots by device
ggplot(final.estimates.combined.long, aes(x=subject, #reorder(subject,estimate,mean)
                                          y=estimate, colour=estimate.type)) + 
  geom_point() +
  scale_x_discrete(limits=ppts.ordered.by.original) +
  scale_colour_manual(values = c("frame.rate.original" = "blue", "frame.rate.no.outliers.sd" = "red", "frame.rate.no.outliers.10pc" = "green"),
                      breaks = c("frame.rate.original", "frame.rate.no.outliers.sd", "frame.rate.no.outliers.10pc"),
                      labels = c("mean of all data","SD-based trimming", "top/bottom 10% trimmed")) +
  facet_wrap(~device)

# counts of estimates after rounding to nearest integer
counts.by.estimate.type <- final.estimates.combined.long %>%
  mutate(estimate.rounded = round(estimate)) %>%
  count(estimate.type, estimate.rounded, sort=FALSE)
counts.60hz.by.estimate.type <- final.estimates.combined.long %>% 
  group_by(estimate.type) %>%
  summarise(n=length(estimate[estimate >= 59 & estimate <= 61]),p=n/length(estimate))

## investigate rates lower than ~60 Hz
ppts.lower.than.60hz <- final.estimates.combined.all %>%
  filter(frame.rate.no.outliers.10pc < 58) %>%
  arrange(frame.rate.no.outliers.10pc)

# histograms 
for (l in 1:length(ppts.lower.than.60hz$subject)) {
  png(paste("hist_trimmed_10pc_", ppts.lower.than.60hz$subject[l], ".png", sep = ""))
  print(ggplot(combined.data.devices.frame.diff.no.outliers.10pc[combined.data.devices.frame.diff.no.outliers.10pc$subject==ppts.lower.than.60hz$subject[l],], 
         aes(x=value)) +
    geom_histogram(binwidth=1))
  dev.off()
}