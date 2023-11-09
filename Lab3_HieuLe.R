# Load in tidyverse
library('tidyverse')
# Import Stroop
stroop = read.csv("Stroop.csv")

stroop_id_congruency = stroop %>%
  group_by(id, congruency) %>%
  summarize(rt_mean = mean(rt))

stroop_by_congruency = stroop_id_congruency %>%
  group_by(congruency) %>%
  summarize(rt_mean = mean(rt_mean),
            rt_se = sd(rt_mean) / sqrt(n()))

library(ggplot2)

ggplot(stroop_by_congruency, aes(x = congruency, y = rt_mean, fill = congruency)) +
  geom_col(position = position_dodge(width = 0.7)) +
  
  geom_errorbar(aes(ymin = rt_mean - rt_se, ymax = rt_mean + rt_se), 
                position = position_dodge(width = 0.7), width = 0.7) +
  
  scale_fill_manual(values = c("congruent" = "blue", "incongruent" = "red")) +
  
  theme(legend.position = "top") +
  
  theme(legend.position = "top") +
  
  geom_col(color = "black", width = 1.0) +
  
  labs(title = "Stroop Test Results") +
  
  labs(x = NULL, y = "Average RT (milliseconds)")

ggsave("stroop_barplot.png")







  
  

  
  
  