# This script produces all the graphics used in the paper and presentations
library(tidyverse)
library(latex2exp)
library(broom)
load("AnalysisOutput/AnalysisOutput.RData")



#### Explaining Event Study Graphics ####

set.seed <- 15
y <- 100 + rnorm(41, 0, 1)
y.terror <- 98 + rnorm(10, 0 , 1)
mu.bar <- mean(y[1:20])
es.df <- tibble(y, mu.bar)
es.df$Event.Date <- seq(-30, 10)
es.df$y.obs <- y
es.df$y.obs[37:41] <- y.terror


event.study.explanation.plot <- ggplot(es.df, aes(Event.Date, y.obs)) +
  geom_smooth(se = FALSE, method = 'loess') +
  geom_line(aes(Event.Date, mu.bar), linetype = 'longdash', alpha = 0.3) +
  geom_vline(xintercept = 0) +
  ylim(96, 102) +
  geom_segment( x = -30, xend = -10, y = 101, yend = 101,
           colour = "black", size = 1, linetype = 'dotted') +
  annotate("text", x = -20, y = 101.25, label = "Estimation Window") +
  geom_segment(x = 0, xend = 10,y = 101, yend = 101,
               colour = "black", size = 1, linetype = 'dotted' ) +
  annotate('text', x = 5, y = 101.25, label = 'Event Window') +
  annotate('text', x = 5, y = 100.35, label=TeX("$E\\[R_{i,\\tau}|\\Omega_{i,\\tau}\\]$", output='character'), parse=TRUE) + 
  geom_segment( x = 10, xend = 10, y = 98.2, yend = mu.bar, linetype = 'longdash') +
  annotate('text', x = 10.8, y = 99, label = TeX('$AR_{i,\\tau = 10}$', output = 'character'), parse = TRUE, angle = 270) +
  xlab(label = TeX('$\\tau$')) +
  ylab(TeX('$\\R_{it}')) +
  ggtitle('An Event Study') +
  theme_minimal() 

event.study.explanation.plot
# save(event.study.explanation.plot, file = 'Event Study Explanation ggplot.Rdata')
# #ggsave(filename = 'Event Study Explanation.png') Don't redo this one, random seed makes it messy

#### Summary Statistics Graphics ####

## Histograms ##


histogram.wounded.small <- ggplot(terror.data[(terror.data$nwound > 0), ], aes(nwound, fill = cut(nwound, 100))) +
  geom_histogram(show.legend = FALSE) +
  xlim(0, 100) +
  xlab('Number of wounded | at least one person is wounded') +
  ggtitle('Number of wounded from UK Terror Attacks, 1970-2016', subtitle = 'xlim(0, 100)') +
  theme_minimal()

histogram.wounded.large <- ggplot(terror.data[(terror.data$nwound > 0), ], aes(nwound, fill = cut(nwound, 100))) +
  geom_histogram(show.legend = FALSE, binwidth = 5) +
  xlab('Number of wounded | at least one person is wounded') +
  ggtitle('Number of wounded from UK Terror Attacks, 1970-2016') +
  theme_minimal()

histogram.killed.at.least.1 <- ggplot(terror.data[(terror.data$nkill > 0), ], aes(nkill, fill = cut(nkill, 100))) +
  geom_histogram(show.legend = FALSE, binwidth = 5) +
  xlab('Number of fatalities | at least one person is killed') +
  ggtitle('Number of fatalities from UK Terror Attacks, 1970-2016') +
  theme_minimal()

histogram.killed <- ggplot(terror.data, aes(nkill, fill = cut(nkill, 100))) +
  geom_histogram(show.legend = FALSE, binwidth = 5) +
  xlab('Number of fatalities') +
  annotate('text', x = 150, y = 1570, label = 'N.B. scale has doubled', colour = 'orange', size = 8) +
  ggtitle('Number of fatalities from UK Terror Attacks, 1970-2016') +
  theme_minimal()

histogram.prop.damage.at.least.1 <- ggplot(terror.data[(terror.data$propvalue > 0), ], aes(propvalue)) +
  geom_histogram(show.legend = FALSE) +
  xlab('Recorded Property Damage | Property Damage > 0') +
  ggtitle('Property Damage from UK Terror Attacks, 1970-2016') +
  annotate('text', x = 2.5*10^9, y = 20, label = '1992 Manchester \n Bombing', colour = 'red') +
  annotate('text', x = 1.2*10^9, y = 15, label = '1996 Manchester Bombing', colour = 'red') +
  theme_minimal()

histogram.terror.intensity <- ggplot(terror.data, aes(log(terror.intensity), fill = cut(log(terror.intensity), 1000))) +
  geom_histogram(show.legend = FALSE) +
  ggtitle('Log(Terror Intensity) from 1983-2016 in the UK') +
  theme_minimal()


# bar.5.largest.events <- ggplot(events.top5, aes(event.name, terror.intensity)) +
#   geom_col(aes(event.name, terror.intensity, fill = -terror.intensity), show.legend = FALSE) +
#   xlab('Event') +
#   ylab('Terror Intensity') +
#   ggtitle('Terror Intensity, \nTop 5 Events') +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.ticks.y = element_blank()) 


## Scatter Graphs ##

scatter.fatalities.over.time <- ggplot(terror.data, aes(Date, nkill, colour = cut(nkill, 10000))) +
  geom_point(size = 1, show.legend = FALSE, aes(size = terror.intensity)) +
  xlab('Year of Attack') +
  ylab('Number of Fatalities') +
  ggtitle('Deaths Attributed to Terror in the UK, 1970-2016') +
  theme_minimal()

scatter.log.fatalities.over.time <- ggplot(terror.data, aes(Date, nkill, colour = cut(nkill, 10000))) +
  geom_point( aes(size = terror.intensity), show.legend = FALSE) +
  scale_y_log10() +
  xlab('Year of Attack') +
  ylab('Number of Fatalities, \n logarithmic scale') +
  ggtitle('Deaths Attributed to Terror in the UK, 1970-2016') + 
  geom_vline(aes(xintercept = as.Date('1998-01-01')), linetype = 'longdash', colour = 'green', size = 1) +
  annotate('text', x = as.Date('2000-01-01'), y = 5, label = 'End of The Troubles', angle = 270) +
  theme_minimal()

scatter.wounded.over.time <- ggplot(terror.data, aes(Date, nwound, Terror.intensity,  colour = cut(nwound, 100))) +
  geom_point(show.legend = FALSE, aes(size = terror.intensity)) +
  ylab('Number of wounded') +
  xlab('Year of Attack') +
  ggtitle('Injuries Attributed to Terror in the UK, 1970-2016') + 
  theme_minimal()

scatter.log.wounded.over.time <- ggplot(terror.data, aes(Date, log(nwound), colour = cut(nwound, 100))) +
  geom_point(show.legend = FALSE, aes(size = terror.intensity)) +
  ylab('Log Number of wounded') +
  xlab('Year of Attack') +
  ggtitle('Injuries Attributed to Terror in the UK, 1970-2016') + 
  geom_vline(aes(xintercept = as.Date('1998-01-01')), linetype = 'longdash', colour = 'green', size = 1) +
  annotate('text', x = as.Date('2000-01-01'), y = 5, label = 'End of The Troubles', angle = 270) +
  theme_minimal()

scatter.terror.intensity.over.time <- ggplot(terror.data, aes(Date, terror.intensity, colour = cut(terror.intensity, 100))) +
  geom_point(show.legend = FALSE) +
  ylab('Terror Intensity') +
  xlab('Year of Attack') +
  ggtitle('Terror Intensity, UK 1970-2016') + 
  theme_minimal()

scatter.log.terror.intensity.over.time <- ggplot(terror.data, aes(as.Date(Date), log(terror.intensity), colour = cut(log(terror.intensity), 100))) +
  geom_point(show.legend = FALSE) +
  ylab('Log Terror Intensity') +
  xlab('Year of Attack') +
  ggtitle('Terror Intensity, UK 1970-2016') +
  geom_vline(aes(xintercept = as.Date('1998-01-01')), linetype = 'longdash', colour = 'green', size = 1) +
  annotate('text', x = as.Date('2000-01-01'), y = 5, label = 'End of The Troubles', angle = 270) +
  theme_minimal()

## Index Returns Plots

line.ALLSHARE.time <- ggplot(na.omit(index.data.UK.ALLSHARE), aes(Date, FTSE.ALL.SHARE...PRICE.INDEX)) +
  geom_line() +
  geom_vline(data = events.top5, aes(xintercept = Date), linetype = 'dotted') +
  ylab('FTSE All-Share Price') +
  xlab('Time') +
  ggtitle('FTSE All-Share Price', subtitle = 'Five largest terror attacks shown') +
  theme_minimal()






#### Largest Events Graphics ####

## @knitr lockerbie.plot
lockerbie.plot <- ggplot(lockerbie.bombing.event.study, aes(time.delta, event.car)) +
  geom_line(size = 2, colour = 'pink') +
  geom_line(aes(time.delta, event.car + event.confidence.interval), linetype = 'longdash', alpha = 0.3) +
  geom_line(aes(time.delta, event.car - event.confidence.interval), linetype = 'longdash', alpha = 0.3) +
  geom_hline(aes(yintercept = 0), linetype = 'longdash', size = 1, colour = 'red', alpha = 0.4) +
  xlab('Days Since Attack') +
  ylab('Cumulate Abnormal Returns (%)')+
  ggtitle('Lockerbie Bombing, Cumulative Abnormal Returns', subtitle = 'FTSE ALL SHARE Price Index, log differenced - 21 December 1988') +
  ylim(-3, 5) +
  theme_minimal()
#ggsave('lockerbie_plot.png', path = '~/R Working Directory/EME-Dissertation/Presentation and Plots/R/plots/Script1 plots/')

## @knitr london.7.7.plot
london.7.7.plot <-ggplot(london.7.7.bombing.event.study, aes(time.delta, event.car)) +
  geom_line(size = 2, colour = 'red') +
  geom_line(aes(time.delta, event.car + event.confidence.interval), linetype = 'longdash', alpha = 0.3) +
  geom_line(aes(time.delta, event.car - event.confidence.interval), linetype = 'longdash', alpha = 0.3) +
  geom_hline(aes(yintercept = 0), linetype = 'longdash', size = 1, colour = 'red', alpha = 0.4) +
  xlab('Days Since Attack') +
  ylab('Cumulate Abnormal Returns (%)')+
  ylim(-3, 3.5) +
  scale_x_discrete(limit = seq(11) - 1) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())
london.7.7.plot
# ggsave('London_plot.pdf', path = 'Figures/')
## @knitr omagh.plot
omagh.plot <- ggplot(omagh.bombing.event.study, aes(time.delta, event.car)) +
  geom_line(size = 2, colour = 'pink') +
  geom_line(aes(time.delta, event.car + event.confidence.interval), linetype = 'longdash', alpha = 0.3) +
  geom_line(aes(time.delta, event.car - event.confidence.interval), linetype = 'longdash', alpha = 0.3) +
  geom_hline(aes(yintercept = 0), linetype = 'longdash', size = 1, colour = 'red', alpha = 0.4) +
  xlab('Days Since Attack') +
  ylab('Cumulate Abnormal Returns (%)')+
  ggtitle('Omagh Bombing, Cumulative Abnormal Returns', subtitle = 'FTSE ALL SHARE Price Index, log differenced - 15 August 1998') +
  theme_minimal()
#ggsave('Omagh_plot.png', path = '~/R Working Directory/EME-Dissertation/Presentation and Plots/R/plots/Script1 plots/')
## @knitr manchester.plot
manchester.plot <- ggplot(manchester.bombing.1996.event.study, aes(time.delta, event.car)) +
  geom_line(size = 2, colour = 'pink') +
  geom_line(aes(time.delta, event.car + event.confidence.interval), linetype = 'longdash', alpha = 0.3) +
  geom_line(aes(time.delta, event.car - event.confidence.interval), linetype = 'longdash', alpha = 0.3) +
  geom_hline(aes(yintercept = 0), linetype = 'longdash', size = 1, colour = 'red', alpha = 0.4) +
  xlab('Days Since Attack') +
  ylab('Cumulate Abnormal Returns (%)')+
  ggtitle('1996 Manchester Bombing, Cumulative Abnormal Returns', subtitle = 'FTSE ALL SHARE Price Index, log differenced - 15 June 1996') +
  ylim(-3, 3) +
  theme_minimal()
#ggsave('Manchester_plot.png', path = '~/R Working Directory/EME-Dissertation/Presentation and Plots/R/plots/Script1 plots/')
## @knitr droppin.well.plot
droppin.well.plot <- ggplot(droppin.well.bombing.event.study, aes(time.delta, event.car)) +
  geom_line(size = 2, colour = 'pink') +
  geom_line(aes(time.delta, event.car + event.confidence.interval), linetype = 'longdash', alpha = 0.3) +
  geom_line(aes(time.delta, event.car - event.confidence.interval), linetype = 'longdash', alpha = 0.3) +
  geom_hline(aes(yintercept = 0), linetype = 'longdash', size = 1, colour = 'red', alpha = 0.4) +
  xlab('Days Since Attack') +
  ylab('Cumulate Abnormal Returns (%)')+
  ggtitle('Droppin Well Disco Bombing, Cumulative Abnormal Returns', subtitle = 'FTSE ALL SHARE Price Index, log differenced - 6 December 1982') +
  ylim(-5, 3) +
  theme_minimal()
#ggsave('Droppin_plot.png', path = '~/R Working Directory/EME-Dissertation/Presentation and Plots/R/plots/Script1 plots/')
# lockerbie.plot
# london.7.7.plot
# omagh.plot
# manchester.plot
# droppin.well.plot


## @knitr rolling.CAAR.plot
# Now graphing rolling CAAR
rolling.CAAR.plot <- ggplot(all.CAR.10.day.ALLSHARE, aes(n, rolling.CAAR))+
  geom_point(shape = 16, size = 1, colour = "#fdafee", show.legend = FALSE) +
  geom_line(aes(n, rolling.CAAR - rolling.ci), alpha = 0.3, linetype = 'longdash') +
  geom_line(aes(n, rolling.CAAR + rolling.ci), alpha = 0.3, linetype = 'longdash') +
  ylim(-5, 5) +
  xlab('Largest N attacks') +
  ylab('Rolling Cumulative Average Abnormal Return (%)') +
  ggtitle('Rolling mean of Cumulative Abnormal Returns', subtitle = 'UK Terror Attacks with FTSE ALLSHARE data, 1980-2016') +
  theme_minimal()
rolling.CAAR.plot
#ggsave('All_CAAR_plot.png', path = '~/R Working Directory/EME-Dissertation/Presentation and Plots/R/plots/Script1 plots/')

## Rolling CAAR plot but with filtered data
rolling.CAAR.filtered.plot <-  ggplot(all.CAR.10.day.ALLSHARE.no.overlap, aes(n, rolling.CAAR)) +
  geom_point(shape = 16, size = 3, colour = "#fdafee", show.legend = FALSE, alpha = 0.6) +
  geom_line(aes(n, rolling.CAAR - rolling.ci), alpha = 0.3, linetype = 'longdash') +
  geom_line(aes(n, rolling.CAAR + rolling.ci), alpha = 0.3, linetype = 'longdash') +
  geom_smooth(se = FALSE) +
  ylim(-5, 5) +
  xlab('Largest N attacks') +
  ylab('Rolling Cumulative Average Abnormal Return (%)') +
  ggtitle('Rolling mean of Cumulative Abnormal Returns, screened', subtitle = 'UK Terror Attacks with FTSE ALLSHARE data, 1980-2016') +
  theme_minimal()
#ggsave('All_CAAR_filtered.png', path = '~/R Working Directory/EME-Dissertation/Presentation and Plots/R/plots/Script1 plots/')
rolling.CAAR.filtered.plot

# Plotting conditional probability results





large.cp.subset <- subset(select(large.cp.results, -c(model)))

large.cp.results.plot <- ggplot(large.cp.results, aes(event, estimate)) +
  geom_point(data = large.cp.subset,colour = 'grey', alpha = 0.2, size = 3, shape = 22, fill = 'grey') +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),  size=1, color="blue", fill="white", shape=22, linetype = 'longdash') +
  geom_hline(yintercept = 0.05, linetype = 'longdash', alpha = 0.2)+
  facet_wrap(~ model) +
  guides(colour = FALSE)+
  ylab('Probability of observing a market movement as bad or worse than return observed') +
  xlab('Event number') +
  ggtitle('Conditional Probability of observing more extreme market return on day of attack',
          subtitle = '5 largest attacks') +
  theme_bw()
large.cp.results.plot

#ggsave('large_cp_plot.png', path = '~/R Working Directory/EME-Dissertation/Presentation and Plots/R/plots/Script1 plots/')


# CAAR for top 5 events plot
top5.CAAR.plot <- ggplot(largest.5.events.CAAR.table, aes(day.CAAR, CAAR.allshare)) +
  geom_line() +
  geom_ribbon(aes(day.CAAR, ymin = boot.ci.lower.allshare, ymax = boot.ci.upper.allshare), linetype = 'longdash', alpha = 0.1) + 
  geom_line(aes(day.CAAR, CAAR.allshare + `CI width.allshare`), linetype = 'longdash', alpha = 0.3) +
  geom_line(aes(day.CAAR, CAAR.allshare - `CI width.allshare`), linetype = 'longdash', alpha = 0.3) +
  scale_x_discrete(limits = seq(11) - 1) +
  xlab('Days since attack') +
  theme_minimal()
top5.CAAR.plot


#### Decade Graphics ####


# Need to recode some variables as factors for plot to be ordered by event.car size
decade.event.study.CAR10$market.date <- factor(decade.event.study.CAR10$market.date)
decade.event.study.CAR10$market.date <- factor(decade.event.study.CAR10$market.date,
                                               levels = decade.event.study.CAR10$market.date[order(-decade.event.study.CAR10$event.car)])
## @knitr bar.chart.decade.event.study.by.CAR
bar.chart.decade.event.study.by.CAR <- ggplot(decade.event.study.CAR10, aes(market.date, event.car, event.p.value, alpha = -event.p.value)) +
  geom_col(fill = 'red') +
  coord_flip() +
  ylab('10 Day Cumulative Abnormal Return') +
  xlab('Market Date of Attack') + 
  theme_minimal() +
  ggtitle('10 Day Cumulative Abnormal Returns in Response to Terror Event', subtitle = 'Top 5 Events per Decade') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = 'none')


#ggsave('bar_chart_decade_CAR_plot.png', path = '~/R Working Directory/EME-Dissertation/Presentation and Plots/R/plots/Script1 plots/')

# This time ordering temporally
decade.event.study.CAR10$market.date <- factor(decade.event.study.CAR10$market.date)
decade.event.study.CAR10$market.date <- factor(decade.event.study.CAR10$market.date,
                                               levels = decade.event.study.CAR10$market.date[order(decade.event.study.CAR10$Date)])
## @knitr bar.chart.decade.event.study.by.time
bar.chart.decade.event.study.by.time <- ggplot(decade.event.study.CAR10, aes(market.date, event.car, event.p.value, alpha = -event.p.value)) +
  geom_col(fill = 'red') +
  ylab('10 Day Cumulative Abnormal Return') +
  xlab('Market Date of Attack') + 
  theme_minimal() +
  ggtitle('10 Day Cumulative Abnormal Returns in Response to Terror Event', subtitle = 'Top 5 Events per Decade') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = 'none',
        axis.text.x = element_text(angle = 270, hjust = 1))

#ggsave('bar_chart_decade_time.png', path = '~/R Working Directory/EME-Dissertation/Presentation and Plots/R/plots/Script1 plots/')

## Decade Event studies with CAR4
# Need to recode some variables as factors for plot to be ordered by event.car size
decade.event.study.CAR4$market.date <- factor(decade.event.study.CAR4$market.date)
decade.event.study.CAR4$market.date <- factor(decade.event.study.CAR4$market.date,
                                               levels = decade.event.study.CAR4$market.date[order(-decade.event.study.CAR4$event.car)])
## @knitr bar.chart.decade.event.study.by.CAR
bar.chart.decade.event.study.by.CAR4 <- ggplot(decade.event.study.CAR4, aes(market.date, event.car, event.p.value, alpha = -event.p.value)) +
  geom_col(fill = 'red') +
  coord_flip() +
  ylab('4 Day Cumulative Abnormal Return') +
  xlab('Market Date of Attack') + 
  theme_minimal() +
  ggtitle('4 Day Cumulative Abnormal Returns in Response to Terror Event', subtitle = 'Top 5 Events per Decade') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = 'none')

#ggsave('decade_event_study_CAR4.png', path = '~/R Working Directory/EME-Dissertation/Presentation and Plots/R/plots/Script1 plots/')

## CAAR by decade


# #ggsave('bar_chart_time_plot.png')
decade.cp.subset <- subset(select(decade.cp.results, -c(decade)))

decade.cp.results.plot <- ggplot(decade.cp.results, aes(event, estimate, colour = decade, shape = model)) +
  geom_point(data = decade.cp.subset, colour = 'grey', alpha = 0.2, size = 3) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0.05, linetype = 'longdash', alpha = 0.2)+
  facet_wrap(~ decade) +
  guides(colour = FALSE)+
  ylab('Probability of observing market movement as bad or worse than return observed') +
  xlab('Event number') +
  ggtitle('Conditional Probability of observing more extreme return on day of attack',
          subtitle = '5 largest attacks per decade') +
  theme_bw()
decade.cp.results.plot
#ggsave('decade_cp_plot.png', path = '~/R Working Directory/EME-Dissertation/Presentation and Plots/R/plots/Script1 plots/')
results.hfit.y_hat.subset <-  subset(select(results.hfit.y_hat, -c(decade)))


decade.cp.results.plot.hierarchical <- ggplot(results.hfit.y_hat, aes(event, estimate, colour = decade)) +
  geom_point(data = results.hfit.y_hat.subset, colour = 'grey', alpha = 0.2, size = 3) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),  size=1, color="blue", fill="white", shape=22, linetype = 'longdash') +
  geom_hline(yintercept = 0.05, linetype = 'longdash', alpha = 0.2)+
  facet_wrap(~ decade) +
  guides(colour = FALSE)+
  ylab('Probability of observing a market movement as bad or worse than return observed') +
  xlab('Event number') +
  ggtitle('Conditional Probability of observing more extreme market return on day of attack',
          subtitle = '5 largest attacks per decade - hierarchical model only') +
  theme_bw()
#ggsave('decade_cp_hierarchical_plot.png',path = '~/R Working Directory/EME-Dissertation/Presentation and Plots/R/plots/Script1 plots/')


####################################################################
#### Largest Event Tables ####
largest.5.events.CAAR.table <- as.tibble(largest.5.events.CAAR.table)


