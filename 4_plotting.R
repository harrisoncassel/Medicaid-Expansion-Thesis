##### Setup #####
# Load final BRFSS comparison data
SOUTH.MEANS <- read.csv('results/final south means.csv')
SOUTH.SE <- read.csv('results/final south se.csv')
COHORT.MEANS <- read.csv('results/final cohort means.csv')
COHORT.SE <- read.csv('results/final cohort se.csv')

# FUNCTION: general_comparison_plot
## Automatically generates south vs. cohort comparison plot for one variable
### south.mean.vec: Vector of observed means to plot for the south
### cohort.mean.vec: Vector of observed means to plot for the cohort
### south.se.vec: Vector of standard errors to create error band
### cohort.se.vec: Vector of standard errors to create error band
### plot.title: Title for the plot
### ylab: y-axis label
### years: Years for which data is supplied (will be used as x-axis); default = c(2014, 2018, 2022)
### se.band.width: How many SEs wide to make the standard error bands; default = 2
### plot.subtitle: Subtitle for the plot; default = '' (no subtitle)
### xlab: x-axis label; default = 'Year'
### south.col: Color in which to plot south data; default = 'red'
### cohort.col: Color in which to plot cohort data; default = 'blue'
### pch vector: of ints; position [1] is pch for Cohort & [2] is for Southeast; default = c(15, 16)
### point.size: Point size for plotting all observed means; default = 4
### main:.line.width Line width for plotting all observed means; default = 1.2
### se.fill.alpha: Alpha for filling in standard error region; default
## Returns a ggplot2 plot object
general_comparison_plot <- function(south.mean.vec, cohort.mean.vec, south.se.vec, cohort.se.vec,
                                    plot.title, ylab, years=c(2014, 2018, 2022), se.band.width=2,
                                    plot.subtitle='', xlab='Year',  south.col='red',
                                    cohort.col='blue', pch=c(15, 16), point.size=4, main.line.width=1.2,
                                    se.lines.width=.8, se.fill.alpha=.25) {
  
  # Define data.frame to pass to ggplot2
  plot.mean.vec <- c(south.mean.vec, cohort.mean.vec)
  plot.se.vec <- c(south.se.vec, cohort.se.vec)
  plot.lab.vec <- c(rep('Southeast', length(south.mean.vec)), rep('Cohort', length(cohort.mean.vec)))
  
  PLOT.DF <- data.frame(Year=years, Mean=plot.mean.vec, SE=plot.se.vec, lab=plot.lab.vec) %>%
    mutate(
      Upper = Mean + (se.band.width * SE),
      Lower = Mean - (se.band.width * SE)
    )
  
  # Craft plot using passed arguments
  plt <- ggplot(data=PLOT.DF, aes(x=Year, y=Mean, color=lab, pch=lab, linetype=lab)) +
    geom_point(size=point.size) +
    geom_line(linewidth=main.line.width) +
    geom_line(linewidth=se.lines.width) +
    geom_line(linewidth=se.lines.width) +
    geom_ribbon(aes(ymin=Lower, ymax=Upper, fill=lab), alpha=se.fill.alpha) +
    scale_color_manual(name='Region:', labels = c('Peer', 'Southeast'), values = c(cohort.col, south.col),
                       aesthetics=c('color', 'fill')) +
    scale_shape_manual(name='Region:', labels = c('Peer', 'Southeast'), values = c(pch[1], pch[2])) +
    guides(fill='none', linetype='none') +
    theme(legend.position='bottom')
  
  # Add labels dynamically, according to definition/ommission of a subtitle
  if (plot.subtitle == '') {
    plt <- plt +
      labs(
        title = plot.title,
        x = xlab,
        y = ylab
      )
  } else {
    plt <- plt +
      labs(
        title = plot.title,
        subtitle = plot.subtitle,
        x = xlab,
        y = ylab
      )
  }
  
  # Adjust x-axis to only include years of observation
  plt <- plt +
    scale_x_continuous(breaks=c(2014,2018,2022))
  
  return(plt)
}

##### Plotting #####
# Flu True (respondents saying they've had flu shot in past 12 mos.)
## rates * 100 to report %s
flu.plt <- general_comparison_plot(SOUTH.MEANS$fluTRUE*100, COHORT.MEANS$fluTRUE*100,
                                   SOUTH.SE$fluTRUE*100, COHORT.SE$fluTRUE*100,
                                   plot.title='Flu Vaccination Rate',
                                   plot.subtitle='% of respondents who reported a flu shot in the past 12 months',
                                   ylab='% with Vaccination'); flu.plt
ggsave('plots/flu.png', width=7, height=7)

# Foregone care due to cost -- *100 to get %s
foregone.plt <- general_comparison_plot(SOUTH.MEANS$foregone.due.to.costTRUE*100,
                                        COHORT.MEANS$foregone.due.to.costTRUE*100,
                                        SOUTH.SE$foregone.due.to.costTRUE*100,
                                        COHORT.SE$foregone.due.to.costTRUE*100,
                                        plot.title='Foregoing Care Due to Cost',
                                        plot.subtitle='% of respondents who reported foregoing care due to an\ninability to afford it in the past 12 months',
                                        ylab='% Foregone Care'); foregone.plt
ggsave('plots/foregone.png', width=7, height=7)

# HIV test -- *100 to get %s
hiv.plt <- general_comparison_plot(SOUTH.MEANS$hiv.testTRUE*100, COHORT.MEANS$hiv.testTRUE*100,
                                   SOUTH.SE$hiv.testTRUE*100, COHORT.SE$hiv.testTRUE*100,
                                   plot.title='HIV Testing Rate',
                                   plot.subtitle='% of respondents who reported ever having taken an HIV test\n(not including those taken as part of blood donation)',
                                   ylab='% Tested'); hiv.plt
ggsave('plots/hiv.png', width=7, height=7)

# Medicaid as primary source of insurance -- *100 to get %s
medicaid.plt <- general_comparison_plot(SOUTH.MEANS$medicaid.primary.insTRUE*100,
                                        COHORT.MEANS$medicaid.primary.insTRUE*100,
                                        SOUTH.SE$medicaid.primary.insTRUE*100,
                                        COHORT.SE$medicaid.primary.insTRUE*100,
                                        plot.title='Medicaid as a Primary Source of Insurance',
                                        plot.subtitle='% of respondents who reported Medicaid as their primary source of insurance',
                                        ylab='% with Medicaid as Primary Coverage'); medicaid.plt
ggsave('plots/medicaid.png', width=7, height=7)

# Number days mentally unwell
mental.hlth.plt <- general_comparison_plot(SOUTH.MEANS$menthlth.clean,
                                           COHORT.MEANS$menthlth.clean,
                                           SOUTH.SE$menthlth.clean,
                                           COHORT.SE$menthlth.clean,
                                           plot.title='Number of Days Mentally Unwell',
                                           plot.subtitle='Number of days in the past 30 that respondents reported being mentally unwell',
                                           ylab='Number of Days'); mental.hlth.plt
ggsave('plots/mental_health.png', width=7, height=7)

# Uninsured -- *100 to get %s
uninsured.plt <- general_comparison_plot(SOUTH.MEANS$no.coverageTRUE*100,
                                         COHORT.MEANS$no.coverageTRUE*100,
                                         SOUTH.SE$no.coverageTRUE*100,
                                         COHORT.SE$no.coverageTRUE*100,
                                         plot.title='Uninsured Rate',
                                         plot.subtitle='% of respondents who did not report having any health coverage',
                                         ylab='% without Health Coverage'); uninsured.plt
ggsave('plots/uninsured.png', width=7, height=7)

# % with personal provider -- *100 to get %s
provider.plt <- general_comparison_plot(SOUTH.MEANS$personal.providerTRUE*100,
                                        COHORT.MEANS$personal.providerTRUE*100,
                                        SOUTH.SE$personal.providerTRUE*100,
                                        COHORT.SE$personal.providerTRUE*100,
                                        plot.title='Connection with Personal Provider',
                                        plot.subtitle='% of respondents who reported having at least one personal health care provider',
                                        ylab='% with Provider'); provider.plt
ggsave('plots/provider.png', width=7, height=7)

# Number days physically unwell
phys.hlth.plt <- general_comparison_plot(SOUTH.MEANS$physhlth.clean,
                                         COHORT.MEANS$physhlth.clean,
                                         SOUTH.SE$physhlth.clean,
                                         COHORT.SE$physhlth.clean,
                                         plot.title='Number of Days Physically Unwell',
                                         plot.subtitle='Number of days in the past 30 that respondents reported being physically unwell',
                                         ylab='Number of Days'); phys.hlth.plt
ggsave('plots/phys_health.png', width=7, height=7)

# Days obstructed from activity

poor.hlth.plt <- general_comparison_plot(SOUTH.MEANS$poorhlth.clean,
                                         COHORT.MEANS$poorhlth.clean,
                                         SOUTH.SE$poorhlth.clean,
                                         COHORT.SE$poorhlth.clean,
                                         plot.title='Obstruction from Daily Activities',
                                         plot.subtitle='Number of days in the past 30 that respondents reported being unable to\nparticipate in their normal activities to do physical or mental unwellness',
                                         ylab='Number of Days'); poor.hlth.plt
ggsave('plots/poor_health.png', width=7, height=7)
