##################### Transplant experiment R script #####################

setwd("~/Dropbox/PhD/Thesis components/Transplant study/Data/")
x <- read.csv("transplant.csv", stringsAsFactors = FALSE)


########################## Mortality #####################################

m <- x[2:5]
m$survived195 <- match(m$survived195, c("N", "Y")) - 1
m$survived374 <- match(m$survived374, c("N", "Y")) - 1

c195 <- rbind(aggregate(m[c("survived195")], by = m["transplant_site"], sum),
              aggregate(m[c("survived195")], by = m["transplant_site"], function(z) sum(!z)))

c374 <- rbind(aggregate(m[c("survived374")], by = m["transplant_site"], sum, na.rm = TRUE),
              aggregate(m[c("survived374")], by = m["transplant_site"], function(z) sum(!z, na.rm = TRUE)))
count <- c(c195$survived195, c374$survived374)
site <- c(c195$transplant_site, c374$transplant_site)
duration <- rep(c("195", "374"), each = 4)
status <- rep(c("survived", "survived", "died", "died"), times = 2)

## test for three way interaction
xtab <- xtabs(count ~ site + status + duration)
MASS::loglm(~status + site + duration + site*status + site*duration + status*duration,  data = xtab)
# X^2 df  P(> X^2)
# Likelihood Ratio 0.4775432  1 0.4895373
# Pearson          0.5687043  1 0.4507743

## test for conditional independence in each stratum 
mantelhaen.test(xtabs(count ~ site + status + duration))
## Mantel-Haenszel X-squared = 44.029, df = 1, p-value = 3.236e-11
## common odds ratio 0.0426202 
## site affect on mortality (duration = strata)
mantelhaen.test(xtabs(count ~ duration + status + site))
## Mantel-Haenszel X-squared = 41.72, df = 1, p-value = 1.053e-10
## common odds ratio 0.05857741 
## duration effect on mortality (site = strata)


z <- x[x$transplant_site == "The Arch", ]
z$survived195 <- match(z$survived195, c("N", "Y")) - 1

# ## first test for orig-site effect
# m <- m[m$transplant_site == "The Arch", ]
# c195 <- rbind(aggregate(m[c("survived195")], by = m["original_site"], sum),
#               aggregate(m[c("survived195")], by = m["original_site"], function(z) sum(!z)))
# 
# c374 <- rbind(aggregate(m[c("survived374")], by = m["original_site"], sum, na.rm = TRUE),
#               aggregate(m[c("survived374")], by = m["original_site"], function(z) sum(!z, na.rm = TRUE)))
# count <- c(c195$survived195, c374$survived374)
# site <- c(c195$original_site, c374$original_site)
# duration <- rep(c("195", "374"), each = 4)
# status <- rep(c("survived", "survived", "died", "died"), times = 2)
# mantelhaen.test(xtabs(count ~ site + status + duration))
# # Mantel-Haenszel chi-squared test with continuity correction
# # 
# # data:  xtabs(count ~ site + status + duration)
# # Mantel-Haenszel X-squared = 0.59766, df = 1, p-value = 0.4395
# # alternative hypothesis: true common odds ratio is not equal to 1
# # 95 percent confidence interval:
# #   0.2038013 1.6362293
# # sample estimates:
# #   common odds ratio 
# # 0.5774648 


## full model
model <- glm(survived195 ~  dry_weight_initial + original_site + original_SIGV.STOTAL, 
             family=binomial(link='logit'), data=z)
summary(model)
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.7789   0.2778   0.5101   0.6876   0.9995  
# 
# Coefficients:
#                       Estimate Std. Error z value Pr(>|z|)  
# (Intercept)            0.34217    1.70759   0.200   0.8412  
# original_siteThe Arch  0.02029    0.78714   0.026   0.9794  
# dry_weight_initial     0.28446    0.15391   1.848   0.0646 .
# original_SIGV.STOTAL  -1.03811    2.32712  -0.446   0.6555  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 57.169  on 59  degrees of freedom
# Residual deviance: 51.556  on 56  degrees of freedom
# AIC: 59.556
# 
# Number of Fisher Scoring iterations: 5

exp(0.28446) # 1.329
## so a 1g increase in dry weight corresponds to a 33% increase in the odds of survival


###################### Growth Rates ################################

y <- x[!is.na(x$time_of_retrieval), ] # get rid of dead ones

y$time_of_retrieval <- as.factor(y$time_of_retrieval) 
modela <- lm(growth_rate_x.10.3.per_day_compounding ~  dry_weight_initial + original_site + 
               transplant_site + time_of_retrieval + SIGV.STOTAL + time_of_retrieval*transplant_site, data=y)
summary(modela)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.89360 -0.41887 -0.02071  0.31647  2.18189 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                   1.73994    0.46507   3.741 0.000356 ***
# dry_weight_initial                           -0.03918    0.03081  -1.272 0.207462    
# original_siteThe Arch                        -0.06037    0.19773  -0.305 0.760989    
# transplant_siteThe Arch                       0.10239    0.24181   0.423 0.673183    
# time_of_retrieval374                          0.21864    0.21496   1.017 0.312382    
# SIGV.STOTAL                                   0.47256    0.57814   0.817 0.416298    
# transplant_siteThe Arch:time_of_retrieval374 -1.18575    0.39925  -2.970 0.003999 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7644 on 75 degrees of freedom
# Multiple R-squared:  0.1545,	Adjusted R-squared:  0.08683 
# F-statistic: 2.284 on 6 and 75 DF,  p-value: 0.04444

## interaction between time of retrieval and transplant site - 
## so proceed to simple effects tests at each level of deployment duration


y195 <- y[y$time_of_retrieval == 195, ]
y374 <- y[y$time_of_retrieval == 374, ]

model195 <- lm(growth_rate_x.10.3.per_day_compounding ~  dry_weight_initial + original_site + 
                 transplant_site + SIGV.STOTAL, data=y195)
summary(model195)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.73518 -0.49179 -0.05046  0.39890  1.45263 
# 
# Coefficients:
#                         Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              2.32319    0.63469   3.660 0.000823 ***
# original_siteThe Arch   -0.39311    0.27150  -1.448 0.156535    
# dry_weight_initial      -0.10126    0.04210  -2.405 0.021575 *  
# transplant_siteThe Arch  0.09402    0.23001   0.409 0.685204    
# SIGV.STOTAL              0.49305    0.78620   0.627 0.534648    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7268 on 35 degrees of freedom
# Multiple R-squared:  0.1647,	Adjusted R-squared:  0.06928 
# F-statistic: 1.726 on 4 and 35 DF,  p-value: 0.1664
car::qqPlot(model195)
plot(model195$fitted.values, model195$residuals)
summary(lm(growth_rate_x.10.3.per_day_compounding ~ 1, data=y195))
## 1.7540 +/- 0.1191 (SE)


model374 <- lm(growth_rate_x.10.3.per_day_compounding ~  dry_weight_initial + original_site + 
                 transplant_site + SIGV.STOTAL, data=y374)
summary(model374)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.5553 -0.3753 -0.0945  0.3851  1.9494 
# 
# Coefficients:
#                         Estimate Std. Error t value Pr(>|t|)   
# (Intercept)              1.49377    0.63119   2.367  0.02330 * 
# original_siteThe Arch    0.25506    0.28379   0.899  0.37458   
# dry_weight_initial       0.01980    0.04417   0.448  0.65661   
# transplant_siteThe Arch -1.01852    0.32779  -3.107  0.00362 **
# SIGV.STOTAL              0.25514    0.83740   0.305  0.76232   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7839 on 37 degrees of freedom
# Multiple R-squared:  0.234,	Adjusted R-squared:  0.1512 
# F-statistic: 2.825 on 4 and 37 DF,  p-value: 0.03846
car::qqPlot(model374)
plot(model374$fitted.values, model374$residuals)
summary(lm(growth_rate_x.10.3.per_day_compounding ~ transplant_site, data=y374))
## 1.903 +/- 0.129 (SE) for North Bay
## 1.903-1.043 = 0.86 +/- 0.374 for The Arch
y374$transplant_site <- factor(y374$transplant_site, levels = c("The Arch", "North Bay"))
summary(lm(growth_rate_x.10.3.per_day_compounding ~ transplant_site, data=y374))
# 0.8600  +/- 0.2887 for the Arch 



###################### IGV Cladocopium abundance ########################### 

modelb <- lm(SIGV.STOTAL ~ original_SIGV.STOTAL + dry_weight_initial + 
               original_site + transplant_site + time_of_retrieval + 
               time_of_retrieval*transplant_site,  data=y)
car::qqPlot(modelb)
plot(modelb$fitted.values, modelb$residuals)
summary(modelb)
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.122140 -0.025559  0.002113  0.029712  0.194621 
# 
# Coefficients:
#                                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                   0.071782   0.033403   2.149   0.0349 *  
# original_SIGV.STOTAL                          0.869730   0.040046  21.718   <2e-16 ***
# dry_weight_initial                            0.000191   0.002281   0.084   0.9335    
# original_siteThe Arch                        -0.024953   0.014697  -1.698   0.0937 .  
# transplant_siteThe Arch                      -0.007974   0.017885  -0.446   0.6570    
# time_of_retrieval374                          0.008854   0.015896   0.557   0.5792    
# transplant_siteThe Arch:time_of_retrieval374  0.020781   0.029535   0.704   0.4839    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.05655 on 75 degrees of freedom
# Multiple R-squared:  0.8642,	Adjusted R-squared:  0.8533 
# F-statistic: 79.52 on 6 and 75 DF,  p-value: < 2.2e-16
summary(lm(SIGV.STOTAL ~ original_SIGV.STOTAL , data=y))
## Adjusted R-squared:  0.853
## residual variance component = 0.147

t.test(y$original_SIGV.STOTAL, y$SIGV.STOTAL, paired = TRUE)
# Paired t-test
# 
# data:  y$original_SIGV.STOTAL and y$SIGV.STOTAL
# t = 1.3189, df = 81, p-value = 0.1909
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.004527391  0.022332269
# sample estimates:
#   mean of the differences 
# 0.008902439 



######################### Plots ################################
library(ggplot2)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

z$survived195 <- factor(c("Perished", "Survived")[z$survived195 + 1])


p1 <- ggplot(z, aes(x = survived195, y = original_SIGV.STOTAL)) +
  geom_boxplot(fill = "grey") +
  #theme(legend.title=element_blank(), legend.text=element_text(size=6)) +
  ylab(expression(S[IGV]:S[TOTAL])) +
  xlab("") +
  ylim(0, 1) +
  theme_bw() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.x = element_text(size = 15, margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  coord_flip() 

p1

setEPS()
postscript("~/Dropbox/PhD/Thesis components/Transplant study/Figures/transplant_fig1.eps", 
           width = 5, height = 5, family = "Times")
p1
dev.off()




ytw <- summarySE(y, "growth_rate_x.10.3.per_day_compounding", 
                 c("time_of_retrieval", "transplant_site"))
ytw$transplant_site <- as.factor(ytw$transplant_site)
ytw$time_of_retrieval <- as.factor(ytw$time_of_retrieval)

## y$growth_rate_x.10.3.per_day_compounding[y$transplant_site == "The Arch" & y$time_of_retrieval == "374"]
#y$transplant_site <- factor(y$transplant_site)


p2 <- ggplot(ytw, aes(x = time_of_retrieval, y = growth_rate_x.10.3.per_day_compounding, 
                    fill = transplant_site)) +
  geom_bar(position = position_dodge(0.91), stat = "identity") +
  geom_errorbar(aes(ymin=growth_rate_x.10.3.per_day_compounding - ci, 
                    ymax=growth_rate_x.10.3.per_day_compounding + ci), 
                width = 0.1, position = position_dodge(0.91)) +
  ylab(expression("Growth rate ("%*%" 10"^"-3"*" % per day)")) +
  xlab("Duration of deployment (days)") +
  #annotate("text", x = 1, y = 2.5, label = "ns", hjust = 0.5, size = 4, alpha = 1) +
  annotate("text", x = 2, y = 2.5, label = "**", hjust = 0.5, vjust = 1, size = 7, alpha = 1) +
  annotate("text", x = 1, y = 0.03, label = "n = 20", hjust = 2, vjust = 0, size = 4, alpha = 1) +
  annotate("text", x = 1, y = 0.03, label = "n = 20", hjust = -0.2, vjust = 0, size = 4, alpha = 1) +
  annotate("text", x = 2, y = 0.03, label = "n = 35", hjust = 2, vjust = 0, size = 4, alpha = 1) +
  annotate("text", x = 2, y = 0.03, label = "n = 7", hjust = -0.2, vjust = 0, size = 4, alpha = 1) +
  theme_bw() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.x = element_text(size = 14, margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
        legend.position = "none")
        # legend.text = element_text(size = 12),
        # legend.title = element_blank())
  #geom_point(position=pd) +
  #geom_hline(yintercept = 0,colour = "grey90")

p2

setEPS()
postscript("~/Dropbox/PhD/Thesis components/Transplant study/Figures/transplant_fig2.eps", 
           width = 5, height = 5, family = "Times")
p2
dev.off()


p3 <- ggplot(y, aes(x = original_SIGV.STOTAL, y = SIGV.STOTAL, shape = time_of_retrieval, color = transplant_site)) +
  geom_point(size = 4) +
  geom_abline(intercept = 0.073, slope = 0.856, size = 0.1) +
  xlab(expression('Initial S'[IGV]*':S'[TOTAL])) +
  ylab(expression('Final S'[IGV]*':S'[TOTAL])) +
  annotate("text", x = 0.6, y = 0.15, label = expression(''*italic(y)*' = 0.07'* italic(x)*' + 0.86'), 
           hjust = 0, vjust = 0, size = 5, alpha = 0.6) +
  annotate("text", x = 0.6, y = 0.1, label = expression('Adjusted R'^2*' = 0.853'), 
           hjust = 0, vjust = 0, size = 5, alpha = 0.6) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.x = element_text(size = 14, margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
        #legend.text = element_text(size = 12),
        #legend.title = element_blank())
        legend.position = "none")

p3

setEPS()
postscript("~/Dropbox/PhD/Thesis components/Transplant study/Figures/transplant_fig3.eps", 
           width = 5, height = 5, family = "Times")
p3
dev.off()

# library(gridExtra)
# gridExtra::grid.arrange(p2, p3, nrow = 1)
####################################################################################



