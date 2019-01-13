######## Analyze subject fixationS #########  #########  #########  #########  #########  ######### 


require(lme4)
require(lmerTest)
require(afex)
require("numDeriv")
library(aod)
library(Rcpp)
library(multcomp)
library(effects)
library(sjPlot)



########### read the data #########################################################################

binofix <-  read.csv('binomial_fixations_all_var.csv')
summary(binofix$group)
binofix$group <- factor(binofix$group, levels = c("IMM", "IE", "IS", "ISP"))

# substitute pro with null so martin is happy
binofix$condition <- factor(binofix$condition, levels = c("pro", "overt"))
levels(binofix$condition) <- sub("pro", "null", levels(binofix$condition))
summary(binofix$condition)


binofix$window <- factor(binofix$window, levels = c("verb", "adv", "adj"))
summary(binofix$window)

sard <- binofix[binofix$group == "IS", ]
sardpass <- binofix[binofix$group == "ISP", ]
eng <- binofix[binofix$group == "IE", ]
itami <- binofix[binofix$group == "IMM", ]



########### sardinian #########################################################################

sard_null <- glmer(fixsubj ~ (1+condition|subject) + (1+condition|trial),
                   data = sard, family = binomial,
                   control = glmerControl(optimizer = "bobyqa"))
summary(sard_null)

sard_cond <- glmer(fixsubj ~ condition 
                   + (1+condition|subject) + (1+condition|trial),
                   data = sard, family = binomial,
                   control = glmerControl(optimizer = "bobyqa"))
summary(sard_cond)

sard_condewind <- glmer(fixsubj ~ condition + window
                      + (1+condition|subject) + (1+condition|trial),
                   data = sard, family = binomial,
                   control = glmerControl(optimizer = "bobyqa"))
summary(sard_condewind)

sard_condwind <- glmer(fixsubj ~ condition * window
                        + (1+condition|subject) + (1+condition|trial),
                        data = sard, family = binomial,
                        control = glmerControl(optCtrl=list(maxfun=6e4)))
ss <- getME(sard_condwind,c("theta","fixef"))
sard_condwind <- update(sard_condwind, start=ss, control=glmerControl(optimizer = "bobyqa"))
summary(sard_condwind)

anova(sard_null, sard_cond)
anova(sard_cond, sard_condewind)
anova(sard_condewind, sard_condwind)

## plot sard
plot_sard <- plot(effect('condition: window', sard_condwind,
                     xlevels=NULL),
              type="response",
              ylim = c(.12, 0.45),
              multiline=TRUE, ci.style="bars",
              ylab = 'prob of subject fixations',
              main = 'Subject fixations (fitted values): Italian-Sardinian',
              symbols=1:2, lines=1:2, colors=c(1, 4))
plot_sard



########### sardinian  passive ####################################################################

sardpass_null <- glmer(fixsubj ~ (1+condition|subject) + (1+condition|trial),
                       data = sardpass, family = binomial,
                       control = glmerControl(optimizer = "bobyqa"))
summary(sardpass_null)

sardpass_cond <- glmer(fixsubj ~ condition 
                       + (1+condition|subject) + (1+condition|trial),
                       data = sardpass, family = binomial,
                       control = glmerControl(optimizer = "bobyqa"))
summary(sardpass_cond)

sardpass_condewind <- glmer(fixsubj ~ condition + window
                            + (1+condition|subject) + (1+condition|trial),
                            data = sardpass, family = binomial,
                            control = glmerControl(optimizer = "bobyqa"))
summary(sardpass_condewind)

sardpass_condwind <- glmer(fixsubj ~ condition * window
                           + (1+condition|subject) + (1+condition|trial),
                           data = sardpass, family = binomial,
                           control = glmerControl(optCtrl=list(maxfun=6e4)))
ss <- getME(sardpass_condwind,c("theta","fixef"))
sardpass_condwind <- update(sardpass_condwind, start=ss, control=glmerControl(optimizer = "bobyqa"))
summary(sardpass_condwind)

anova(sardpass_null, sardpass_cond)
anova(sardpass_cond, sardpass_condewind)
anova(sardpass_condewind, sardpass_condwind)

## plot sardpass
plot_sardpass <- plot(effect('condition: window', sardpass_condwind,
                             xlevels=NULL),
                      type="response",
                      ylim = c(.12, 0.45),
                      multiline=TRUE, ci.style="bars",
                      ylab = 'prob of subject fixations',
                      main = 'Subject fixations (fitted values): Italian-sardinian Passive',
                      symbols=1:2, lines=1:2, colors=c(1, 4))
plot_sardpass



########### ita- english #########################################################################

eng_null <- glmer(fixsubj ~ (1+condition|subject) + (1+condition|trial),
                  data = eng, family = binomial,
                  control = glmerControl(optimizer = "bobyqa"))
summary(eng_null)

eng_cond <- glmer(fixsubj ~ condition 
                  + (1+condition|subject) + (1+condition|trial),
                  data = eng, family = binomial,
                  control = glmerControl(optimizer = "bobyqa"))
summary(eng_cond)

eng_condewind <- glmer(fixsubj ~ condition + window
                       + (1+condition|subject) + (1+condition|trial),
                       data = eng, family = binomial,
                       control = glmerControl(optimizer = "bobyqa"))
ss <- getME(eng_condewind,c("theta","fixef"))
eng_condewind <- update(eng_condewind, start=ss, control=glmerControl(optimizer = "bobyqa"))
summary(eng_condewind)

eng_condwind <- glmer(fixsubj ~ condition * window
                      + (1+condition|subject) + (1+condition|trial),
                      data = eng, family = binomial,
                      control = glmerControl(optCtrl=list(maxfun=6e4)))
summary(eng_condwind)

anova(eng_null, eng_cond)
anova(eng_cond, eng_condewind)
anova(eng_condewind, eng_condwind)

## plot eng
plot_eng <- plot(effect('condition: window', eng_condwind,
                        xlevels=NULL),
                 type="response",
                 ylim = c(.12, 0.45),
                 multiline=TRUE, ci.style="bars",
                 ylab = 'prob of subject fixations',
                 main = 'Subject fixations (fitted values): Italian-English',
                 symbols=1:2, lines=1:2, colors=c(1, 4))
plot_eng



########### italian #########################################################################

itami_null <- glmer(fixsubj ~ (1+condition|subject) + (1+condition|trial),
                    data = itami, family = binomial,
                    control = glmerControl(optimizer = "bobyqa"))
summary(itami_null)

itami_cond <- glmer(fixsubj ~ condition 
                    + (1+condition|subject) + (1+condition|trial),
                    data = itami, family = binomial,
                    control = glmerControl(optimizer = "bobyqa"))
summary(itami_cond)

itami_condewind <- glmer(fixsubj ~ condition + window
                         + (1+condition|subject) + (1+condition|trial),
                         data = itami, family = binomial,
                         control = glmerControl(optimizer = "bobyqa"))
summary(itami_condewind)

itami_condwind <- glmer(fixsubj ~ condition * window
                        + (1+condition|subject) + (1+condition|trial),
                        data = itami, family = binomial,
                        control = glmerControl(optCtrl=list(maxfun=6e4)))
summary(itami_condwind)

anova(itami_null, itami_cond)
anova(itami_cond, itami_condewind)
anova(itami_condewind, itami_condwind)

## plot itami
plot_itami <- plot(effect('condition: window', itami_condwind,
                          xlevels=NULL),
                   type="response",
                   ylim = c(.12, 0.45),
                   multiline=TRUE, ci.style="bars",
                   ylab = 'prob of subject fixations',
                   main = 'Subject fixations (fitted values): Italian Dominant',
                   symbols=1:2, lines=1:2, colors=c(1, 4))
plot_itami




########### across groups ######################################################################

subj_null <- glmer(fixsubj ~ (1+condition|subject) + (1+condition|trial),
                    data = binofix, family = binomial,
                    control = glmerControl(optCtrl=list(maxfun=7e4)))
summary(subj_null)

subj_cond <- glmer(fixsubj ~ condition 
                    + (1+condition|subject) + (1+condition|trial),
                    data = binofix, family = binomial,
                    control = glmerControl(optCtrl=list(maxfun=7e4)))
#optCtrl=list(maxfun=7e4)
summary(subj_cond)

subj_condegroup <- glmer(fixsubj ~ condition + group
                         + (1+condition|subject) + (1+condition|trial),
                         data = binofix, family = binomial,
                         control = glmerControl(optCtrl=list(maxfun=7e4)))
summary(subj_condegroup)

subj_condgroup <- glmer(fixsubj ~ condition * group
                         + (1+condition|subject) + (1+condition|trial),
                         data = binofix, family = binomial,
                         control = glmerControl(optCtrl=list(maxfun=7e4)))
ss <- getME(subj_condgroup,c("theta","fixef"))
subj_condgroup <- update(subj_condgroup, start=ss, control=glmerControl(optimizer = "bobyqa"))
summary(subj_condgroup)

anova(subj_null, subj_cond) # 
anova(subj_cond, subj_condegroup) # 
anova(subj_condegroup, subj_condgroup) # 


## plot subject fixations across groups
plot_subjfix <- plot(effect('condition: group', subj_condgroup,
                          xlevels=NULL),
                   type="response",
                   ylim = c(.12, 0.45),
                   multiline=TRUE, ci.style="bars",
                   ylab = 'prob of subject fixations',
                   main = 'Subject fixations across groups (fitted values)',
                   symbols=1:2, lines=1:2, colors=c(1, 4))
plot_subjfix



# try further adding window
subj_condewind <- glmer(fixsubj ~ condition + window
                             + (1+condition|subject) + (1+condition|trial),
                             data = binofix, family = binomial,
                             control = glmerControl(optimizer = "bobyqa"),
                             verbose = TRUE)
summary(subj_condewind)

subj_condwind <- glmer(fixsubj ~ condition * window
                        + (1+condition|subject) + (1+condition|trial),
                        data = binofix, family = binomial,
                        control = glmerControl(optimizer = "bobyqa"),
                        verbose = TRUE)

summary(subj_condwind)

anova(subj_cond, subj_condewind)
anova(subj_condewind, subj_condwind)


# plot itd
plot_sujfixw <- plot(effect('condition: window', subj_condwind,
                            xlevels=NULL),
                     type="response",
                     ylim = c(.12, 0.45),
                     multiline=TRUE, ci.style="bars",
                     ylab = 'prob of subject fixations',
                     main = 'Subject fixations across time windows (fitted values)',
                     symbols=1:2, lines=1:2, colors=c(1, 4))
plot_sujfixw

# is adverb different from verb? pairwise
binofix$condwind<-interaction(binofix$condition,binofix$window)
subj_condwind_inter<-glmer(fixsubj ~ condwind
                          + (1+condition|subject) + (1+condition|trial),
                          data = binofix, family = binomial,
                          control = glmerControl(optCtrl=list(maxfun=12e4)),
                          verbose = TRUE)

comp_subj <- glht(subj_condwind_inter, linfct=mcp(condwind="Tukey")) 
summary(comp_subj)