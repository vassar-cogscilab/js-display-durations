library(ggplot2)

#setwd("U:/Documents/rAF visual stim testing/non_jsPsych_rAF_version/js-display-durations/analysis")
visual_stim_durations_500 <- read.csv("combined_500ms.csv", stringsAsFactors = FALSE)

# number of trials with correct/incorrect true frame counts
table(visual_stim_durations_500$frames)
# 29 30 
# 16 84 

adj_target_dur <- 16.67 * 30
# target duration when frame count >= intended frame count -1
adj_target_dur_buffer <- adj_target_dur - 5 
true_target_dur <- mean(visual_stim_durations_500$obs.duration[visual_stim_durations_500$frames==30])
true_dur_missed_frame <- mean(visual_stim_durations_500$obs.duration[visual_stim_durations_500$frames==29])
# new target duration when frame count >= intended frame count 
target_dur_thresh_at_frame_count_new <- true_dur_missed_frame + ((adj_target_dur_buffer - true_dur_missed_frame)/2)
# old target duration when frame count >= intended frame count 
target_dur_thresh_at_frame_count_old <- adj_target_dur_buffer - 16.67


# check characteristics of trials with incorrect (missed) frames: can these be differentiated reliably?
png("500ms_trials_browser_durations_by_frame_error.png", width = 1200, height = 1000)
ggplot(visual_stim_durations_500, aes(x = curr_delay, fill=factor(frames))) +
  geom_histogram(binwidth = 0.5, color = "black", alpha=.4, position="identity") +
  #geom_density(alpha=.3) + 
  scale_x_continuous(limits = c(475,510), name = "Browser-recorded duration (ms)") + 
  scale_y_continuous(limits = c(0,9.6), breaks = seq(0,10,2), name = "Frequency") +
  scale_fill_manual(breaks = c(29,30), 
                    labels = c("-1"," 0"), 
                    name = "Frame error",
                    values = c("red","gray30")) +
  geom_vline(aes(xintercept = adj_target_dur_buffer), color="blue", size=2) +
  geom_vline(aes(xintercept = target_dur_thresh_at_frame_count_new), color="forestgreen", size=2) +
  geom_vline(aes(xintercept = true_target_dur), color="gray30", size=2, alpha=.4) +
  geom_vline(aes(xintercept = true_dur_missed_frame), color="red", size=2, alpha=.4) +
  geom_vline(aes(xintercept = target_dur_thresh_at_frame_count_old), color="purple", size=2) +
  annotate("text", label = "Threshold when\nframe count is\n>= intended frame\ncount minus 1", 
           x = adj_target_dur_buffer+0.5, y = 9.5, color = "blue", 
           hjust=0, vjust=1, size=6) +
  annotate("text", label = "True duration\nfor red trials", 
           x = true_dur_missed_frame+0.5, y = 6, color = "red", 
           hjust=0, vjust=1, size=6) +
  annotate("text", label = "New threshold\nfor when\nframe count\n= intended\nframe count", 
           x = target_dur_thresh_at_frame_count_new+0.5, y = 9.5, color = "forestgreen", 
           hjust=0, vjust=1, size=6) +
  annotate("text", label = "True duration\nfor gray trials", 
           x = true_target_dur+0.5, y = 6, color = "gray30", 
           hjust=0, vjust=1, size=6) +
  annotate("text", label = "Old threshold\nfor when\nframe count\n= intended\nframe count\n(too low)", 
           x = target_dur_thresh_at_frame_count_old+0.5, y = 9.5, color = "purple", 
           hjust=0, vjust=1, size=6) +
  theme_minimal(base_size = 30) + 
  theme(legend.text=element_text(size=20),
        legend.key.size=unit(1.5,"cm"),
        axis.line.y = element_line(color="black", size = 1),
        axis.line.x = element_line(color="black", size = 1))
dev.off()