library(tidyverse)  # load the package containing both ggplot2 and dplyr
library(lme4)
library(ggeffects)  # install the package first if you haven't already, then load it
library(sjPlot)
library(stargazer)

load("dragons.RData")
head(dragons)

hist(dragons$testScore)  # seems close to a normal distribution - good!

dragons$bodyLength2 <- scale(dragons$bodyLength, center = TRUE, scale = TRUE)

basic.lm <- lm(testScore ~ bodyLength2, data = dragons)
summary(basic.lm)

(prelim_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore)) +
    geom_point() +
    geom_smooth(method = "lm"))

plot(basic.lm, which = 1)  # not perfect... 
## but since this is a fictional example we will go with it
## for your own data be careful:
## the bigger the sample size, the less of a trend you'd expect to see

plot(basic.lm, which = 2)  # a bit off at the extremes, but that's often the case; again doesn't look too bad


boxplot(testScore ~ mountainRange, data = dragons)  # certainly looks like something is going on here

(colour_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = mountainRange)) +
    geom_point(size = 2) +
    theme_classic() +
    theme(legend.position = "none"))

(split_plot <- ggplot(aes(bodyLength, testScore), data = dragons) + 
    geom_point() + 
    facet_wrap(~ mountainRange) + # create a facet for each mountain range
    xlab("length") + 
    ylab("test score"))

mountain.lm <- lm(testScore ~ bodyLength2 + mountainRange, data = dragons)
summary(mountain.lm)

test <- 1 + 3

test


mixed.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange), data = dragons)
summary(mixed.lmer)

anova(mixed.lmer)

plot(mixed.lmer)  # looks alright, no patterns evident

qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))  # points fall nicely onto the line - good!

head(dragons)  # we have site and mountainRange
str(dragons)  # we took samples from three sites per mountain range and eight mountain ranges in total

dragons <- within(dragons, sample <- factor(mountainRange:site))

mixed.WRONG <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|site), data = dragons)  # treats the two random effects as if they are crossed
summary(mixed.WRONG)

anova(mixed.WRONG)

mixed.lmer2 <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|sample), data = dragons)  # the syntax stays the same, but now the nesting is taken into account
summary(mixed.lmer2)

(mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
    facet_wrap(~mountainRange, nrow=2) +   # a panel for each mountain range
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(dragons, pred = predict(mixed.lmer2)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)


mixed.ranslope <- lmer(testScore ~ bodyLength2 + (1 + bodyLength2|mountainRange/site), data = dragons) 

summary(mixed.ranslope)


### plot
(mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
    facet_wrap(~mountainRange, nrow=2) +   # a panel for each mountain range
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(dragons, pred = predict(mixed.ranslope)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)

# Extract the prediction data frame
pred.mm <- ggpredict(mixed.lmer2, terms = c("bodyLength2"))  # this gives overall predictions for the model

# Plot the predictions 

(ggplot(pred.mm) + 
    geom_line(aes(x = x, y = predicted)) +          # slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_point(data = dragons,                      # adding the raw data (scaled values)
               aes(x = bodyLength2, y = testScore, colour = mountainRange)) + 
    labs(x = "Body Length (indexed)", y = "Test Score", 
         title = "Body length does not affect intelligence in dragons") + 
    theme_minimal()
)


ggpredict(mixed.lmer2, terms = c("bodyLength2", "mountainRange"), type = "random") %>% 
  plot() +
  labs(x = "Body Length", y = "Test Score", title = "Effect of body size on intelligence in dragons") + 
  theme_minimal()

# Visualise random effects 
(re.effects <- plot_model(mixed.ranslope, type = "re", show.values = TRUE))

# show summary
summary(mixed.ranslope)

stargazer(mixed.lmer2, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

full.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|sample), 
                  data = dragons, REML = FALSE)

reduced.lmer <- lmer(testScore ~ 1 + (1|mountainRange) + (1|sample), 
                     data = dragons, REML = FALSE)

anova(reduced.lmer, full.lmer)  # the two models are not significantly different



