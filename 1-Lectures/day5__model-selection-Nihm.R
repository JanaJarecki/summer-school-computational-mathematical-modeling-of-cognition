dev.off() # clear the graphs

#Some psychophysics stimuli (stimulus magnitudes) for the input
stimuli<-c(2, 4, 6, 8, 10, 12)

#points representing some experimental data (observations of perceived intensity)
observations <-c(1.1, 1.5, 2.1, 2.5, 2.7, 3.2)
n <- length(observations)

#plot the stimulus magnitudes and the associated observations
plot(stimuli,observations, pch=3, las=1,
     xlab='Physical Intensity',
     ylab='Perceived Intensity')

#fit Nihm's law and superimpose in graph
lm_n <- lm(observations ~ poly(stimuli,n-1))
lines(stimuli,predict(lm_n))

# Exercise: generate observations from a variety of functions (with variability), and fit polynomial as above
# Example:
observations <- log(stimuli) + rnorm(n,0,1)
# Things to try: different amounts of noise, linear fn, exponential, decreasing function, quadratic, sin...