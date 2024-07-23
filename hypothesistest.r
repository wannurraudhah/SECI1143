################################################################
#                                                                TESTING                                                                     #
################################################################

# HYPOTHESIS TESTING
# MU OF BPM > 120?
# z test used

library(BSDA)
z.test(x = spotify_2023$bpm, y = NULL,
       alternative = "two.sided",
       mu = 120,
       conf.level = 0.95,
       sigma.x = sd(spotify_2023$bpm))

z.test(x = spotify_2023$bpm, y = NULL,
       alternative = "two.sided",
       mu = 122,
       conf.level = 0.95,
       sigma.x = sd(spotify_2023$bpm))

qnorm(0.004803)

ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  stat_function(fun = dnorm) +
  geom_vline(xintercept = 2.82, color = "steelblue") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = -1.96, color = "red") + 
  geom_vline(xintercept = 1.96, color = "red") +
  labs(title = "Z-test on Mean BPM of Top Spotify Songs") +
  xlab("Z-score") + 
  ylab("Probability") +
  theme_bw()
