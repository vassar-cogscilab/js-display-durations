library(runjags)
library(coda)
jags.data <- list(
  y = data.all$frame.error+3,
  method = as.numeric(factor(data.all$method)),
  ndata = length(data.all$frame.error)
)
result <- run.jags('jags-model.txt', data=jags.data, n.chains=3, sample = 1000, monitor = c('pr'))

mc.data <- as.matrix(as.mcmc(result))

means <- apply(mc.data, 2, mean)
up.95 <- apply(mc.data, 2, function(x){quantile(x, .975)})               
low.95 <- apply(mc.data, 2, function(x){quantile(x, .025)})

means.empirical <- data.all %>% 
  group_by(method, frame.error) %>% 
  summarize(proportion = n() / 700)

plot.data <- expand.grid(method=c('cssAnimation', 'rAF', 'rAFframecounting', 'setTimeout'),frame.error= -2:2)

plot.data <- plot.data %>% left_join(means.empirical)

plot.data$high <- up.95
plot.data$low <- low.95
  
ggplot(plot.data, aes(x=frame.error, y=proportion, ymax=high, ymin=low))+
  geom_bar(stat='identity')+
  geom_linerange()+
  theme_minimal()+
  facet_wrap(~method)
