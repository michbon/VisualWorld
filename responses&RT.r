################# Binomial regression on keypress responses ###################################################
#### this new script analyses responses and RT by group and across groups
### w.r.t previous analysis, the random structure is simplified


require(lme4)
require(lmerTest)
require(afex)
require("numDeriv")
library(aod)
library(Rcpp)
library (multcomp)
library(effects)
library(sjPlot)



###############################################################################################################
resp<- read.csv('binomial_responses_all_var.csv')
allresp <- resp[resp$response != 'null', ]


# check factors

# substitute pro with null so martin is happy
allresp$condition <- factor(allresp$condition, levels = c("pro", "overt"))
levels(allresp$condition) <- sub("pro", "null", levels(allresp$condition))
summary(allresp$condition)
summary(allresp$resp)
allresp$group <- factor(allresp$group, levels = c("IMM", "IE", "IS", "ISP"))
summary(allresp$group) 



# for the choice of the random effect structure, see relative script


########################## by group analysis #####################################################################


# # # sard

sard <- allresp[allresp$group == "IS", ]


# on subject resp
sard_subj_null <- glmer(resp~ (1+condition|subject),
                   data = sard, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(sard_subj_null)

sard_subj_cond <- glmer(resp~ condition+(1+condition|subject),
                   data = sard, family = binomial , control = glmerControl(optCtrl=list(maxfun=4e4)))
summary(sard_subj_cond)

anova(sard_subj_null, sard_subj_cond) # effect of condition is IS: .022

# RT
sard_rt_null <-lmer(RT~ (1+condition|subject),
                        data = sard, control = lmerControl(optimizer = "bobyqa"))
summary(sard_rt_null)
sard_rt_cond <- lmer(RT~ condition+(1+condition|subject),
                        data = sard, control = lmerControl(optCtrl=list(maxfun=4e4)))
summary(sard_rt_cond)
anova(sard_rt_null, sard_rt_cond) # nothing





# # # sardpass

sardpass <- allresp[allresp$group == "ISP", ]


# on subject resp
sardpass_subj_null <- glmer(resp~ (1+condition|subject),
                            #+(1+condition|trial),
                            data = sardpass, family = binomial, control = glmerControl(optimizer = "bobyqa"))
#ss <- getME(sardpass_subj_null,c("theta","fixef"))
#sardpass_subj_null <- update(sardpass_subj_null, start=ss, control=glmerControl(optimizer = "bobyqa"))
summary(sardpass_subj_null)

sardpass_subj_cond <- glmer(resp~ condition+(1+condition|subject),
                            #+(1+condition|trial),
                            data = sardpass, family = binomial, control = glmerControl(optCtrl=list(maxfun=4e4)))
summary(sardpass_subj_cond)

anova(sardpass_subj_null, sardpass_subj_cond) # condition .013


# RT
sardp_rt_null <-lmer(RT~ (1+condition|subject),
                    data = sardpass, control = lmerControl(optimizer = "bobyqa"))
summary(sardp_rt_null)
sardp_rt_cond <- lmer(RT~ condition+(1+condition|subject),
                     data = sardpass, control = lmerControl(optCtrl=list(maxfun=4e4)))
summary(sardp_rt_cond)
anova(sardp_rt_null, sardp_rt_cond) # .026




# # # eng

eng <- allresp[allresp$group == "IE", ]


# on subject resp
eng_subj_null <- glmer(subjresp~ (1+condition|subject),
                       #(1+condition|trial),
                       data = eng, family = binomial, control = glmerControl(optCtrl=list(maxfun=6e4)))
#ss <- getME(eng_subj_null,c("theta","fixef"))
#eng_subj_null <- update(eng_subj_null, start=ss, control=glmerControl(optimizer = "bobyqa"))
summary(eng_subj_null)


eng_subj_cond <- glmer(subjresp~ condition+(1+condition|subject),
                       #+(1+condition|trial),
                       data = eng, family = binomial, control = glmerControl(optCtrl=list(maxfun=6e4)))
summary(eng_subj_cond)

anova(eng_subj_null, eng_subj_cond) # .0008


# RT
eng_rt_null <-lmer(RT~ (1+condition|subject),
                     data = eng, control = lmerControl(optimizer = "bobyqa"))
summary(eng_rt_null)
eng_rt_cond <- lmer(RT~ condition+(1+condition|subject),
                      data = eng, control = lmerControl(optCtrl=list(maxfun=4e4)))
summary(eng_rt_cond)
anova(eng_rt_null, eng_rt_cond) # .026


# # # ita mono

itami <- allresp[allresp$group == "IMM", ]


# on subject resp
itami_subj_null <- glmer(subjresp~ (1+condition|subject),
                         #+(1+condition|trial),
                         data = itami, family = binomial, control = glmerControl(optCtrl=list(maxfun=6e4)))
summary(itami_subj_null)

itami_subj_cond <- glmer(subjresp~ condition+(1+condition|subject),
                         #+(1+condition|trial),
                         data = itami, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(itami_subj_cond)

anova(itami_subj_null, itami_subj_cond) # p < .000


# RT
imm_rt_null <-lmer(RT~ (1+condition|subject),
                   data = itami, control = lmerControl(optimizer = "bobyqa"))
summary(imm_rt_null)
imm_rt_cond <- lmer(RT~ condition+(1+condition|subject),
                    data = itami, control = lmerControl(optCtrl=list(maxfun=4e4)))
summary(imm_rt_cond)
anova(imm_rt_null, imm_rt_cond) # .001




###### across-group analysis on subject response ##############################################################



subj_null <- glmer(resp~ (1+condition|subject),
                   #+(1+condition|trial),
                    data = allresp, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(subj_null)

subj_cond <- glmer(resp~ condition+(1+condition|subject),
                   #+(1+condition|trial),
                    data = allresp, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(subj_cond)


subj_condegroup <-glmer(resp~ condition+group+(1+condition|subject),
                        #+(1+condition|trial),
                    data = allresp, family = binomial, control = glmerControl(optCtrl=list(maxfun=6e4)))
#ss <- getME(subj_condegroup,c("theta","fixef"))
#subj_condegroup <- update(subj_condegroup, start=ss, control=glmerControl(optimizer = "bobyqa"))
summary(subj_condegroup)

subj_condgroup <-glmer(resp~ condition*group+(1+condition|subject),
                       #+(1+condition|trial),
                    data = allresp, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(subj_condgroup)

anova(subj_null, subj_cond)
anova(subj_cond, subj_condegroup)
anova(subj_condegroup, subj_condgroup)




### plot across groups

### plot 1
sjp.int(subj_condgroup, type = "eff", vars = c("group", "condition"), swap.pred = FALSE,
        show.ci = TRUE, jitter.ci = TRUE, facet.grid = FALSE,
        geom.size = 1,
        ylim = c(0, 1),
        #legend.labels = c("Italian-Sardinian", "Italian-English", 
         #                 "Italian-Sardinian Passive", "Italian monolingual"),
        title = ("title"))

## plot2
plot2 <- plot(effect('condition: group', subj_condgroup,
                       xlevels=NULL),
                type="response",
                #ylim = c(0, 0.5),
                multiline=TRUE, ci.style="bars",
                ylab = 'prob of subject response',
                main = 'Proportion Responses (fitted values)',
                symbols=1:2, lines=1:2, colors=c(1, 4))
plot2


 
#### RT across

rt_null <- lmer(RT~ (1+condition|subject),
                   #+(1+condition|trial),
                   data = allresp, control = lmerControl(optimizer = "bobyqa"))
summary(rt_null)

  rt_cond <- lmer(RT~ condition+(1+condition|subject),
                     #+(1+condition|trial),
                     data = allresp, control = lmerControl(optimizer = "bobyqa"))
  summary(rt_cond)


rt_condegroup <-lmer(RT ~ condition+group+(1+condition|subject),
                        #+(1+condition|trial),
                        data = allresp, control = lmerControl(optCtrl=list(maxfun=6e4)))
summary(rt_condegroup)

rt_condgroup <- lmer(RT~ condition*group+(1+condition|subject),
                       #+(1+condition|trial),
                       data = allresp, control = lmerControl(optimizer = "bobyqa"))
summary(rt_condgroup)

anova(rt_null, rt_cond)
anova(rt_cond, rt_condegroup)
anova(rt_condegroup, rt_condgroup)




### plot RT

# ### plot 1 RT
# sjp.int(rt_condgroup, type = "eff", vars = c("group", "condition"), swap.pred = FALSE,
#         show.ci = TRUE, jitter.ci = TRUE, facet.grid = FALSE,
#         geom.size = 1,
#         #ylim = c(0, 1),
#         #legend.labels = c("Italian-Sardinian", "Italian-English", 
#         #                 "Italian-Sardinian Passive", "Italian monolingual"),
#         title = ("RT"))

## plot2 RT
  plot2rt <- plot(effect('condition: group', rt_condgroup,
                       xlevels=NULL),
                type="response",
                #ylim = c(1500, 3000),
                multiline=TRUE, ci.style="bars",
                #ylab = 'p of object fixations',
                main = 'RT (fitted values)',
                symbols=1:2, lines=1:2, colors=c(1, 4))
  plot2rt


 

 
 
 
 
 
 
 