######## Analyze object fixationS #########  #########  #########  #########  #########  ######### 


require(lme4)
require(lmerTest)
require(afex)
require("numDeriv")
library(aod)
library(Rcpp)
library (multcomp)
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

sard_o_null <- glmer(fixobj ~ (1+condition|subject) + (1+condition|trial),
                   data = sard, family = binomial,
                   control = glmerControl(optimizer = "bobyqa"))
summary(sard_o_null)

sard_o_cond <- glmer(fixobj ~ condition 
                   + (1+condition|subject) + (1+condition|trial),
                   data = sard, family = binomial,
                   control = glmerControl(optimizer = "bobyqa"))
summary(sard_o_cond)

sard_o_condewind <- glmer(fixobj ~ condition + window
                        + (1+condition|subject) + (1+condition|trial),
                        data = sard, family = binomial,
                        control = glmerControl(optimizer = "bobyqa"))
ss <- getME(sard_o_condewind,c("theta","fixef"))
sard_o_condewind <- update(sard_o_condewind, start=ss, control=glmerControl(optimizer = "bobyqa"))
summary(sard_o_condewind)

sard_o_condwind <- glmer(fixobj ~ condition * window
                       + (1+condition|subject) + (1+condition|trial),
                       data = sard, family = binomial,
                       control = glmerControl(optCtrl=list(maxfun=6e4)))
summary(sard_o_condwind)

anova(sard_o_null, sard_o_cond)
anova(sard_o_cond, sard_o_condewind)
anova(sard_o_condewind, sard_o_condwind)

## plot sard
plot_sard_o <- plot(effect('condition: window', sard_o_condwind,
                         xlevels=NULL),
                  type="response",
                  ylim = c(.12, 0.45),
                  multiline=TRUE, ci.style="bars",
                  ylab = 'prob of object fixations',
                  main = 'Object fixations (fitted values): Italian-Sardinian',
                  symbols=1:2, lines=1:2, colors=c(1, 4))
plot_sard_o



########### sardinian  passive ####################################################################

sardpass_o_null <- glmer(fixobj ~ (1+condition|subject) + (1+condition|trial),
                       data = sardpass, family = binomial,
                       control = glmerControl(optimizer = "bobyqa"))
summary(sardpass_o_null)

sardpass_o_cond <- glmer(fixobj ~ condition 
                       + (1+condition|subject) + (1+condition|trial),
                       data = sardpass, family = binomial,
                       control = glmerControl(optimizer = "bobyqa"))
summary(sardpass_o_cond)

sardpass_o_condewind <- glmer(fixobj ~ condition + window
                            + (1+condition|subject) + (1+condition|trial),
                            data = sardpass, family = binomial,
                            control = glmerControl(optimizer = "bobyqa"))
summary(sardpass_o_condewind)

sardpass_o_condwind <- glmer(fixobj ~ condition * window
                           + (1+condition|subject) + (1+condition|trial),
                           data = sardpass, family = binomial,
                           control = glmerControl(optCtrl=list(maxfun=6e4)))
summary(sardpass_o_condwind)

anova(sardpass_o_null, sardpass_o_cond)
anova(sardpass_o_cond, sardpass_o_condewind)
anova(sardpass_o_condewind, sardpass_o_condwind)

## plot sardpass
plot_sardpass_o <- plot(effect('condition: window', sardpass_o_condwind,
                             xlevels=NULL),
                      type="response",
                      ylim = c(.12, 0.45),
                      multiline=TRUE, ci.style="bars",
                      ylab = 'prob of object fixations',
                      main = 'Object fixations (fitted values): Italian-sardinian Passive',
                      symbols=1:2, lines=1:2, colors=c(1, 4))
plot_sardpass_o


# pairwise
sardpass$condwind<-interaction(sardpass$condition,sardpass$window)
sardpass_condwind_inter<-glmer(fixsubj ~ condwind
                           + (1+condition|subject) + (1+condition|trial),
                           data = sardpass, family = binomial,
                           control = glmerControl(optimizer = "bobyqa"),
                           verbose = TRUE)

comp_sardpass_o <- glht(sardpass_condwind_inter, linfct=mcp(condwind="Tukey")) 
summary(comp_sardpass_o)

########### ita- english #########################################################################

eng_o_null <- glmer(fixobj ~ (1+condition|subject) + (1+condition|trial),
                  data = eng, family = binomial,
                  control = glmerControl(optimizer = "bobyqa"))
summary(eng_o_null)

eng_o_cond <- glmer(fixobj ~ condition 
                  + (1+condition|subject) + (1+condition|trial),
                  data = eng, family = binomial,
                  control = glmerControl(optimizer = "bobyqa"))
summary(eng_o_cond)

eng_o_condewind <- glmer(fixobj ~ condition + window
                       + (1+condition|subject) + (1+condition|trial),
                       data = eng, family = binomial,
                       control = glmerControl(optimizer = "bobyqa"))
ss <- getME(eng_o_condewind,c("theta","fixef"))
eng_o_condewind <- update(eng_o_condewind, start=ss, control=glmerControl(optimizer = "bobyqa"))
summary(eng_o_condewind)

eng_o_condwind <- glmer(fixobj ~ condition * window
                      + (1+condition|subject) + (1+condition|trial),
                      data = eng, family = binomial,
                      control = glmerControl(optCtrl=list(maxfun=6e4)))
summary(eng_o_condwind)

anova(eng_o_null, eng_o_cond)
anova(eng_o_cond, eng_o_condewind)
anova(eng_o_condewind, eng_o_condwind)

## plot eng
plot_eng_o <- plot(effect('condition: window', eng_o_condwind,
                        xlevels=NULL),
                 type="response",
                 ylim = c(.12, 0.45),
                 multiline=TRUE, ci.style="bars",
                 ylab = 'prob of object fixations',
                 main = 'Object fixations (fitted values): Italian-English',
                 symbols=1:2, lines=1:2, colors=c(1, 4))
plot_eng_o



########### italian #########################################################################

itami_o_null <- glmer(fixobj ~ (1+condition|subject) + (1+condition|trial),
                    data = itami, family = binomial,
                    control = glmerControl(optimizer = "bobyqa"))
summary(itami_o_null)

itami_o_cond <- glmer(fixobj ~ condition 
                    + (1+condition|subject) + (1+condition|trial),
                    data = itami, family = binomial,
                    control = glmerControl(optimizer = "bobyqa"))
summary(itami_o_cond)

itami_o_condewind <- glmer(fixobj ~ condition + window
                         + (1+condition|subject) + (1+condition|trial),
                         data = itami, family = binomial,
                         control = glmerControl(optimizer = "bobyqa"))
summary(itami_o_condewind)

itami_o_condwind <- glmer(fixobj ~ condition * window
                        + (1+condition|subject) + (1+condition|trial),
                        data = itami, family = binomial,
                        control = glmerControl(optCtrl=list(maxfun=6e4)))
summary(itami_o_condwind)

anova(itami_o_null, itami_o_cond)
anova(itami_o_cond, itami_o_condewind)
anova(itami_o_condewind, itami_o_condwind)

## plot itami
plot_itami_o <- plot(effect('condition: window', itami_o_condwind,
                          xlevels=NULL),
                   type="response",
                   ylim = c(.12, 0.45),
                   multiline=TRUE, ci.style="bars",
                   ylab = 'prob of object fixations',
                   main = 'Object fixations (fitted values): Italian Dominant',
                   symbols=1:2, lines=1:2, colors=c(1, 4))
plot_itami_o




########### across groups ######################################################################

obj_null <- glmer(fixobj ~ (1+condition|subject) + (1+condition|trial),
                  data = binofix, family = binomial,
                  control = glmerControl(optCtrl=list(maxfun=7e4)))
summary(obj_null)

obj_cond <- glmer(fixobj ~ condition 
                  + (1+condition|subject) + (1+condition|trial),
                  data = binofix, family = binomial,
                  control = glmerControl(optCtrl=list(maxfun=7e4)))
#optCtrl=list(maxfun=7e4)
summary(obj_cond)

obj_condegroup <- glmer(fixobj ~ condition + group
                        + (1+condition|subject) + (1+condition|trial),
                        data = binofix, family = binomial,
                        control = glmerControl(optCtrl=list(maxfun=7e4)))
summary(obj_condegroup)

obj_condgroup <- glmer(fixobj ~ condition * group
                       + (1+condition|subject) + (1+condition|trial),
                       data = binofix, family = binomial,
                       control = glmerControl(optCtrl=list(maxfun=7e4)))
ss <- getME(obj_condgroup,c("theta","fixef"))
obj_condgroup <- update(obj_condgroup, start=ss, control=glmerControl(optimizer = "bobyqa"))
summary(obj_condgroup)

anova(obj_null, obj_cond) # 
anova(obj_cond, obj_condegroup) # 
anova(obj_condegroup, obj_condgroup) # 


## plot subject fixations across groups
plot_objfix <- plot(effect('condition: group', obj_condgroup,
                           xlevels=NULL),
                    type="response",
                    ylim = c(.12, 0.45),
                    multiline=TRUE, ci.style="bars",
                    ylab = 'prob of objectfixations',
                    main = 'Object fixations across groups (fitted values)',
                    symbols=1:2, lines=1:2, colors=c(1, 4))
plot_objfix


# try adding window and taking group away since it is n.s. Evaluate interactions to see
# "when" the two pronouns are interpreted as object: same or different?
obj_condewind <- glmer(fixobj ~ condition + window
                      + (1+condition|subject) + (1+condition|trial),
                      data = binofix, family = binomial,
                      control = glmerControl(optCtrl=list(maxfun=8e4)),
                      verbose = TRUE)
ss <- getME(obj_condewind,c("theta","fixef"))
obj_condewind <- update(obj_condewind, start=ss,
                              control=glmerControl(optimizer = "bobyqa"),
                             verbose = TRUE)
summary(obj_condewind)

obj_condwind <- glmer(fixobj ~ condition * window
                       + (1+condition|subject) + (1+condition|trial),
                       data = binofix, family = binomial,
                       control = glmerControl(optCtrl=list(maxfun=12e4)),
                       verbose = TRUE)
summary(obj_condwind)

anova(obj_cond, obj_condewind) # wind < . 000
anova(obj_condewind, obj_condwind) # interaction < .000


# plot it
plot_objfixw <- plot(effect('condition: window', obj_condwind,
                           xlevels=NULL),
                    type="response",
                    ylim = c(.12, 0.45),
                    multiline=TRUE, ci.style="bars",
                    ylab = 'prob of object fixations',
                    main = 'Object fixations across time windows (fitted values)',
                    symbols=1:2, lines=1:2, colors=c(1, 4))
plot_objfixw

# is adverb different from verb? pairwise
binofix$condwind<-interaction(binofix$condition,binofix$window)
obj_condwind_inter<-glmer(fixobj ~ condwind
                          + (1+condition|subject) + (1+condition|trial),
                          data = binofix, family = binomial,
                          control = glmerControl(optCtrl=list(maxfun=12e4)),
                          verbose = TRUE)

comp <- glht(obj_condwind_inter, linfct=mcp(condwind="Tukey")) 
summary(comp)

