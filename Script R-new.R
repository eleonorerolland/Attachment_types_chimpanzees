
save.image("Attachment_types.RData")

load("Attachment_types.RData")




#install and load packages

packages <- c(
  "colorspace", "viridis", "ggeffects", "stringr", "factoextra", "car", "devtools", 
  "FactoMineR", "corrr", "ggcorrplot", "RColorBrewer", "ggpubr", "ggbiplot", "psych", 
  "dplyr", "magrittr", "tidyr", "ggplot2", "lubridate", "tidyverse", "brms", "cowplot", 
  "parallel", "rstan", "zoo", "scatterplot3d", "lme4", "arm", "MuMIn", "plyr", "broom", 
  "coda", "grid", "gridExtra", "HDInterval", "knitr", "kableExtra", "loo"
)

install.packages(packages)
install.packages("svglite")

library(svglite)
library(colorspace)
library(viridis)
library(ggeffects)
library(stringr)
library(factoextra)
library(car)
library(devtools)
library(FactoMineR)
library(corrr)
library(ggcorrplot)
library(RColorBrewer)
library(ggpubr)
library(ggbiplot)
library(psych)
library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(brms)
library(cowplot)
library(parallel)
library(rstan)
library(zoo)
library(scatterplot3d)
library(lme4)
library(arm)
library(MuMIn)
library(plyr)
library(broom)
library(coda)
library(grid)
library(gridExtra)
library(HDInterval)
library(knitr)
library(kableExtra)
library(loo)


# A.1. Comfort-seeking during distress - Model 1 ####

UTE.data<-read.csv("UTE-disorganized.csv",sep=";",dec=",") #load file

UTE.data$Group  <- factor(UTE.data$Group  , levels=c('NORD', 'SUD','EST'))
UTE.data$Sex  <- factor(UTE.data$Sex  , levels=c('F', 'M'))

#check model collinearity
Model.1.vif=lm(Approach.mother.offspring ~ Whimper.or.scream + Age.z + Sex + Group+Party.size.adult.z,data = UTE.data)
vif(Model.1.vif)

#set up priors
prior.Model.1 = get_prior( Approach.mother.offspring ~ Whimper.or.scream + Age.z + Sex + Group+Party.size.adult.z+ (1+Age.z+Party.size.adult.z+Whimper.or.scream|Focal),
                           data = UTE.data, family = bernoulli())

prior.Model.1
prior.Model.1$prior[2:7] <- "normal(0,1)"# so here change the indexing to the rows containing your fixed effects
prior.Model.1

#checking if there are issues in model 1
make_stancode(Approach.mother.offspring ~ Whimper.or.scream +Age.z + Sex + Group+Party.size.adult.z+ (1+Age.z+Party.size.adult.z+Whimper.or.scream|Focal),
              data = UTE.data, family = bernoulli(), prior = prior.Model.1)

#running the model
Model.1= brm(Approach.mother.offspring ~ Whimper.or.scream +Age.z + Sex + Group+Party.size.adult.z+ (1+Age.z+Party.size.adult.z+Whimper.or.scream|Focal),
             data = UTE.data, family = bernoulli(), 
             prior = prior.Model.1, 
             chains = 12,
             cores = 12, 
             iter = 2000,
             warmup = 1000, 
             control = list(adapt_delta = 0.985), 
             sample_prior = "yes")


#result of model 1
summary(Model.1)
summary(Model.1, prob=0.89)


#cond.r2
bayes_R2(Model.1, re_formula = NULL, summary = T)
#marginal.r2
bayes_R2(Model.1, re_formula = NA, summary = T)

conditional_effects(Model.1) # check conditional effect of model 1
plot(Model.1) #check assumptions of model 1
pp_check(Model.1,nsamples=1000) # check posterior predictive of model 1


#Figure S1: Individual differences in vocal distress predicting approaches: Credible intervals of Model 1

disorganized.xx = ranef(Model.1,
                        summary = TRUE)
disorganized <- as.data.frame(disorganized.xx$Focal[, , "Whimper.or.scream"])
disorganized$Focal <- rownames(disorganized)

plot1 <- ggplot(disorganized, aes(x = Estimate, y = Focal)) +
  geom_point() +  # Add points for each individual
  geom_errorbarh(aes(xmin = Q2.5, xmax = Q97.5), height = 0.2) +  # Add horizontal error bars
  labs(x = "Estimate", y = "Individual") +  # Label axes
  theme_minimal() +  # Apply a minimal theme
  geom_vline(xintercept = c(0.19, 0.52), linetype = "dashed", color = "red")  # Add vertical lines

# Print the plot
print(plot1)




# A.2. Aggression between mother and offspring ####

Mother.offspring.aggressions.data<- read.csv("mother-offspring-aggressions.csv",sep=";",dec=",")#load file

Mother.offspring.aggressions.data$Dyads <- paste(Mother.offspring.aggressions.data$Mother, Mother.offspring.aggressions.data$Offspring, sep = ".") #create dyads

Mother.offspring.aggressions.data$Rate<-round(Mother.offspring.aggressions.data$Rate,2) #round rtes


# Count number of dyads for each unique combination of MO.ag and Rate
dyad_counts <- aggregate(Dyads ~ MO.ag + Rate, data = Mother.offspring.aggressions.data, FUN = length)

dyad_counts$Contact <- ifelse(grepl("non contact", dyad_counts$MO.ag), "non contact", "contact")

#plot
ggplot(dyad_counts, aes(x = MO.ag, y = Rate, size = Dyads, color = Contact)) +
  geom_point() +
  labs(x = "Aggressions", y = "Rate of aggressions") +
  guides(size = guide_legend(title = "Number of dyads"),  
         color = guide_legend(title = "Aggression type", 
                              override.aes = list(size = 5))) + # Adjusting symbol size in color legend
  scale_size_continuous(range = c(3, 12), breaks = c(1, 5, 10,50)) +  # Adjusting breaks for size legend
  theme(axis.title.y = element_text(size = 18),            
        axis.text.y = element_text(size = 18),             
        legend.text = element_text(size = 18),             
        legend.title = element_text(size = 18),
        legend.key.size = unit(2, "lines"))  # Adjusting legend key size




# B.1. Existence of different organized attachment types ####

offspring.reaction.UTE.data = read.csv("offspring-reaction-UTE.csv", header = T, sep =";", dec =",")

#Approach mother
prior.Approaching.mother = get_prior(Approaching.mother ~ Age.z + older.siblings.presence+Party.size.adult.z +  Sex + (1+Age.z+Party.size.adult.z|Focal), data =  offspring.reaction.UTE.data,
                                     family = bernoulli)

prior.Approaching.mother
prior.Approaching.mother$prior[2:5] <- "normal(0,1)"
prior.Approaching.mother

make_stancode(Approaching.mother ~ Age.z + older.siblings.presence+Party.size.adult.z +  Sex + (1+Age.z+Party.size.adult.z|Focal), data =  offspring.reaction.UTE.data,
              family = bernoulli, prior = prior.Approaching.mother)

b.Approaching.mother <- brm(Approaching.mother ~ Age.z + older.siblings.presence+Party.size.adult.z +  Sex + (1+Age.z+Party.size.adult.z|Focal), data =  offspring.reaction.UTE.data,
                            family = bernoulli,
                            prior = prior.Approaching.mother, 
                            chains = 12, 
                            cores = 12, 
                            iter = 2000, 
                            warmup = 1000, 
                            thin = 2, 
                            control = list(adapt_delta = 0.98), 
                            sample_prior = "yes")

summary(b.Approaching.mother)

Approaching.mother.xx = ranef(b.Approaching.mother,
                              summary = TRUE)
Approaching.mother <- as.data.frame(Approaching.mother.xx$Focal[, , "Intercept"])
Approaching.mother$Focal <- rownames(Approaching.mother)
colnames(Approaching.mother)<-c("Approaching.mother.Intercept","q1","q2","q3","Focal")


#looking towards mother
prior.Looking.towards.mother = get_prior(Looking.towards.mother ~ Age.z + older.siblings.presence+Party.size.adult.z +  Sex + (1+Age.z+Party.size.adult.z|Focal), data =  offspring.reaction.UTE.data,
                                         family = bernoulli)

prior.Looking.towards.mother
prior.Looking.towards.mother$prior[2:5] <- "normal(0,1)"# so here change the indexing to the rows containing your fixed effects
prior.Looking.towards.mother

make_stancode(Looking.towards.mother ~ Age.z + older.siblings.presence+Party.size.adult.z +  Sex + (1+Age.z+Party.size.adult.z|Focal), data =  offspring.reaction.UTE.data,
              family = bernoulli, prior = prior.Looking.towards.mother)

b.Looking.towards.mother <- brm(Looking.towards.mother ~ Age.z + older.siblings.presence+Party.size.adult.z +  Sex + (1+Age.z+Party.size.adult.z|Focal), data =  offspring.reaction.UTE.data,
                                family = bernoulli,
                                prior = prior.Looking.towards.mother, 
                                chains = 12,
                                cores = 12, 
                                iter = 2000,
                                warmup = 1000, 
                                thin = 2,
                                control = list(adapt_delta = 0.98),
                                sample_prior = "yes")
summary(b.Looking.towards.mother)

Looking.towards.mother.xx = ranef(b.Looking.towards.mother,
                                  summary = TRUE)
Looking.towards.mother <- as.data.frame(Looking.towards.mother.xx$Focal[, , "Intercept"])
Looking.towards.mother$Focal <- rownames(Looking.towards.mother)
colnames(Looking.towards.mother)<-c("Looking.towards.mother.Intercept","q1","q2","q3","Focal")

#Approaching another individual
prior.Approaching.another.individual = get_prior(Approaching.another.individual ~ Age.z + older.siblings.presence+Party.size.adult.z +  Sex + (1+Age.z+Party.size.adult.z|Focal), data =  offspring.reaction.UTE.data,
                                                 family = bernoulli)

prior.Approaching.another.individual
prior.Approaching.another.individual$prior[2:5] <- "normal(0,1)"
prior.Approaching.another.individual

make_stancode(Approaching.another.individual ~ Age.z + older.siblings.presence+Party.size.adult.z +  Sex + (1+Age.z+Party.size.adult.z|Focal), data =  offspring.reaction.UTE.data,
              family = bernoulli, prior = prior.Approaching.another.individual)

b.Approaching.another.individual <- brm(Approaching.another.individual ~ Age.z + older.siblings.presence+Party.size.adult.z +  Sex + (1+Age.z+Party.size.adult.z|Focal), data =  offspring.reaction.UTE.data,
                                        family = bernoulli,
                                        prior = prior.Approaching.another.individual, 
                                        chains = 12, 
                                        cores = 12,  
                                        iter = 2000, 
                                        warmup = 1000, 
                                        thin = 2, 
                                        control = list(adapt_delta = 0.98), 
                                        sample_prior = "yes")

summary(b.Approaching.another.individual)

Approaching.another.individual.xx = ranef(b.Approaching.another.individual,
                                          summary = TRUE)
Approaching.another.individual <- as.data.frame(Approaching.another.individual.xx$Focal[, , "Intercept"])
Approaching.another.individual$Focal <- rownames(Approaching.another.individual)
colnames(Approaching.another.individual)<-c("Approaching.another.individual.Intercept","q1","q2","q3","Focal")


#Climbing up a tree
prior.Climbing.up.a.tree = get_prior(Climbing.up.a.tree ~ Age.z + older.siblings.presence+Party.size.adult.z +  Sex + (1+Age.z+Party.size.adult.z|Focal), data =  offspring.reaction.UTE.data,
                                     family = bernoulli)

prior.Climbing.up.a.tree
prior.Climbing.up.a.tree$prior[2:5] <- "normal(0,1)"
prior.Climbing.up.a.tree

make_stancode(Climbing.up.a.tree ~ Age.z + older.siblings.presence+Party.size.adult.z +  Sex + (1+Age.z+Party.size.adult.z|Focal), data =  offspring.reaction.UTE.data,
              family = bernoulli, prior = prior.Climbing.up.a.tree)

b.Climbing.up.a.tree <- brm(Climbing.up.a.tree ~ Age.z + older.siblings.presence+Party.size.adult.z +  Sex + (1+Age.z+Party.size.adult.z|Focal), data =  offspring.reaction.UTE.data,
                            family = bernoulli,
                            prior = prior.Climbing.up.a.tree, 
                            chains = 12, 
                            cores = 12, 
                            iter = 2000, 
                            warmup = 1000, 
                            thin = 2, 
                            control = list(adapt_delta = 0.98),
                            sample_prior = "yes")

summary(b.Climbing.up.a.tree)

Climbing.up.a.tree.xx = ranef(b.Climbing.up.a.tree,
                              summary = TRUE)
Climbing.up.a.tree <- as.data.frame(Climbing.up.a.tree.xx$Focal[, , "Intercept"])
Climbing.up.a.tree$Focal <- rownames(Climbing.up.a.tree)
colnames(Climbing.up.a.tree)<-c("Climbing.up.a.tree.Intercept","q1","q2","q3","Focal")


#Whimpering
prior.Whimpering = get_prior(Whimpering ~ Age.z + older.siblings.presence+Party.size.adult.z +  Sex + (1+Age.z+Party.size.adult.z|Focal), data =  offspring.reaction.UTE.data,
                             family = bernoulli)

prior.Whimpering
prior.Whimpering$prior[2:5] <- "normal(0,1)"
prior.Whimpering

make_stancode(Whimpering ~ Age.z + older.siblings.presence+Party.size.adult.z +  Sex + (1+Age.z+Party.size.adult.z|Focal), data =  offspring.reaction.UTE.data,
              family = bernoulli, prior = prior.Whimpering)

b.Whimpering <- brm(Whimpering ~ Age.z + older.siblings.presence+Party.size.adult.z +  Sex + (1+Age.z+Party.size.adult.z|Focal), data =  offspring.reaction.UTE.data,
                    family = bernoulli,
                    prior = prior.Whimpering, 
                    chains = 12, 
                    cores = 12,
                    iter = 2000,
                    warmup = 1000, 
                    thin = 2, 
                    control = list(adapt_delta = 0.98), 
                    sample_prior = "yes")

summary(b.Whimpering)

Whimpering.xx = ranef(b.Whimpering,
                      summary = TRUE)
Whimpering <- as.data.frame(Whimpering.xx$Focal[, , "Intercept"])
Whimpering$Focal <- rownames(Whimpering)
colnames(Whimpering)<-c("Whimpering.Intercept","q1","q2","q3","Focal")


#Attack or arm wave
prior.Attacking.or.arm.waving = get_prior(Attacking.or.arm.waving ~ Age.z + older.siblings.presence+Party.size.adult.z +  Sex + (1+Age.z+Party.size.adult.z|Focal), data =  offspring.reaction.UTE.data,
                                          family = bernoulli)

prior.Attacking.or.arm.waving
prior.Attacking.or.arm.waving$prior[2:5] <- "normal(0,1)"
prior.Attacking.or.arm.waving

make_stancode(Attacking.or.arm.waving ~ Age.z + older.siblings.presence+Party.size.adult.z +  Sex + (1+Age.z+Party.size.adult.z|Focal), data =  offspring.reaction.UTE.data,
              family = bernoulli, prior = prior.Attacking.or.arm.waving)

b.Attacking.or.arm.waving <- brm(Attacking.or.arm.waving ~ Age.z + older.siblings.presence+Party.size.adult.z +  Sex + (1+Age.z+Party.size.adult.z|Focal), data =  offspring.reaction.UTE.data,
                                 family = bernoulli,
                                 prior = prior.Attacking.or.arm.waving, 
                                 chains = 12, 
                                 cores = 12,
                                 iter = 2000, 
                                 warmup = 1000, 
                                 thin = 2, 
                                 control = list(adapt_delta = 0.98), 
                                 sample_prior = "yes")

summary(b.Attacking.or.arm.waving)


Attacking.or.arm.waving.xx = ranef(b.Attacking.or.arm.waving,
                                   summary = TRUE)
Attacking.or.arm.waving<- as.data.frame(Attacking.or.arm.waving.xx$Focal[, , "Intercept"])
Attacking.or.arm.waving$Focal <- rownames(Attacking.or.arm.waving)
colnames(Attacking.or.arm.waving)<-c("Attacking.or.arm.waving.Intercept","q1","q2","q3","Focal")



#Screaming
prior.Screaming = get_prior(Screaming ~ Age.z + older.siblings.presence+Party.size.adult.z +  Sex + (1+Age.z+Party.size.adult.z|Focal), data =  offspring.reaction.UTE.data,
                            family = bernoulli)

prior.Screaming
prior.Screaming$prior[2:5] <- "normal(0,1)"
prior.Screaming

make_stancode(Screaming ~ Age.z + older.siblings.presence+Party.size.adult.z +  Sex + (1+Age.z+Party.size.adult.z|Focal), data =  offspring.reaction.UTE.data,
              family = bernoulli, prior = prior.Screaming)

b.Screaming <- brm(Screaming ~ Age.z + older.siblings.presence+Party.size.adult.z +  Sex + (1+Age.z+Party.size.adult.z|Focal), data =  offspring.reaction.UTE.data,
                   family = bernoulli,
                   prior = prior.Screaming, 
                   chains = 12, 
                   cores = 12, 
                   iter = 2000, 
                   warmup = 1000, 
                   thin = 2, 
                   control = list(adapt_delta = 0.98), 
                   sample_prior = "yes")


summary(b.Screaming)

Screaming.xx = ranef(b.Screaming,
                     summary = TRUE)
Screaming<- as.data.frame(Screaming.xx$Focal[, , "Intercept"])
Screaming$Focal <- rownames(Screaming)
colnames(Screaming)<-c("Screaming.Intercept","q1","q2","q3","Focal")


#Running away
prior.Running.away = get_prior(Running.away ~ Age.z + older.siblings.presence+Party.size.adult.z +  Sex + (1+Age.z+Party.size.adult.z|Focal), data =  offspring.reaction.UTE.data,
                               family = bernoulli)

prior.Running.away
prior.Running.away$prior[2:5] <- "normal(0,1)"
prior.Running.away

make_stancode(Running.away ~ Age.z + older.siblings.presence+Party.size.adult.z +  Sex + (1+Age.z+Party.size.adult.z|Focal), data =  offspring.reaction.UTE.data,
              family = bernoulli, prior = prior.Running.away)

b.Running.away <- brm(Running.away ~ Age.z + older.siblings.presence+Party.size.adult.z +  Sex + (1+Age.z+Party.size.adult.z|Focal), data =  offspring.reaction.UTE.data,
                      family = bernoulli,
                      prior = prior.Running.away, 
                      chains = 12, 
                      cores = 12, 
                      iter = 2000, 
                      warmup = 1000, 
                      thin = 2, 
                      control = list(adapt_delta = 0.98), 
                      sample_prior = "yes")


summary(b.Running.away)

Running.away.xx = ranef(b.Running.away,
                        summary = TRUE)
Running.away<- as.data.frame(Running.away.xx$Focal[, , "Intercept"])
Running.away$Focal <- rownames(Running.away)
colnames(Running.away)<-c("Running.away.Intercept","q1","q2","q3","Focal")


#Not reacting
prior.Not.reacting = get_prior(Not.reacting ~ Age.z + older.siblings.presence+Party.size.adult.z +  Sex + (1+Age.z+Party.size.adult.z|Focal), data =  offspring.reaction.UTE.data,
                               family = bernoulli)

prior.Not.reacting
prior.Not.reacting$prior[2:5] <- "normal(0,1)"
prior.Not.reacting

make_stancode(Not.reacting ~ Age.z + older.siblings.presence+Party.size.adult.z +  Sex + (1+Age.z+Party.size.adult.z|Focal), data =  offspring.reaction.UTE.data,
              family = bernoulli, prior = prior.Not.reacting)

b.Not.reacting <- brm(Not.reacting ~ Age.z + older.siblings.presence+Party.size.adult.z +  Sex + (1+Age.z+Party.size.adult.z|Focal), data =  offspring.reaction.UTE.data,
                      family = bernoulli,
                      prior = prior.Not.reacting, 
                      chains = 12, 
                      cores = 12, 
                      iter = 2000, 
                      warmup = 1000, 
                      thin = 2, 
                      control = list(adapt_delta = 0.98), 
                      sample_prior = "yes")


summary(b.Not.reacting)

Not.reacting.xx = ranef(b.Not.reacting,
                        summary = TRUE)
Not.reacting<- as.data.frame(Not.reacting.xx$Focal[, , "Intercept"])
Not.reacting$Focal <- rownames(Not.reacting)
colnames(Not.reacting)<-c("Not.reacting.Intercept","q1","q2","q3","Focal")


#ppcheck
ppcheck_Not_reacting <- pp_check(b.Not.reacting, nsamples = 1000) + ggtitle("Not reacting")
ppcheck_looking_towards_mother <- pp_check(b.Looking.towards.mother, nsamples = 1000) + ggtitle("Looking towards the mother")
ppcheck_approaching_mother <- pp_check(b.Approaching.mother, nsamples = 1000) + ggtitle("Approaching the mother")
ppcheck_running_away <- pp_check(b.Running.away, nsamples = 1000) + ggtitle("Running away")
ppcheck_Screaming <- pp_check(b.Screaming, nsamples = 1000) + ggtitle("Screaming")
ppcheck_Attacking_or_arm_waving <- pp_check(b.Attacking.or.arm.waving, nsamples = 1000) + ggtitle("Attacking or arm waving")
ppcheck_whimpering <- pp_check(b.Whimpering, nsamples = 1000) + ggtitle("Whimpering")
ppcheck_climbing_up_a_tree <- pp_check(b.Climbing.up.a.tree, nsamples = 1000) + ggtitle("Climbing up a tree")
ppcheck_approaching_another_individual <- pp_check(b.Approaching.another.individual, nsamples = 1000) + ggtitle("Approaching another individual")

# Create a list of PPC plots
plot_list <- list(ppcheck_Not_reacting, ppcheck_looking_towards_mother, ppcheck_approaching_mother, 
                  ppcheck_running_away, ppcheck_Screaming, ppcheck_Attacking_or_arm_waving, 
                  ppcheck_whimpering, ppcheck_climbing_up_a_tree, ppcheck_approaching_another_individual)

# Ensure plot_list is not empty before calling grid.arrange
if (length(plot_list) > 0) {
  # Arrange the plots in a grid
  grid.arrange(grobs = plot_list, nrow = 3)  # Adjust the number of rows as needed
} else {
  cat("No PPC plots generated.\n")
}
plot_list


behaviours.intercept<-cbind(Not.reacting,Approaching.mother,Looking.towards.mother,Screaming,Whimpering,Attacking.or.arm.waving,Running.away,Approaching.another.individual,Climbing.up.a.tree)
behaviours.intercept<-data.frame(behaviours.intercept$Focal,behaviours.intercept$Not.reacting.Intercept,behaviours.intercept$Looking.towards.mother.Intercept,behaviours.intercept$Approaching.mother.Intercept,behaviours.intercept$Approaching.another.individual.Intercept,behaviours.intercept$Running.away.Intercept,behaviours.intercept$Climbing.up.a.tree.Intercept,behaviours.intercept$Whimpering.Intercept,behaviours.intercept$Screaming.Intercept,behaviours.intercept$Attacking.or.arm.waving.Intercept)
colnames(behaviours.intercept)<-c("Focal","Not.reacting","Looking.towards.mother","Approaching.mother","Approaching.another.individual","Running.away","Climbing.up.a.tree","Whimpering","Screaming","Attacking.or.arm.waving")

write.csv2(behaviours.intercept,"offspring-reaction-intercept.csv")



#matrix Model 2
Ymat2 = as.matrix(offspring.reaction.UTE.data[,c("Not.reacting", "Approaching.mother",  "Looking.towards.mother", "Whimpering",
                                                 "Climbing.up.a.tree", "Screaming", "Running.away", "Attacking.or.arm.waving",
                                                 "Approaching.another.individual")]) 

offspring.reaction.UTE.data$Ymat = Ymat2
offspring.reaction.UTE.data$Ytrials = rowSums(Ymat2)

offspring.reaction.UTE.data$Focal<-as.factor(offspring.reaction.UTE.data$Focal)

colnames(offspring.reaction.UTE.data)

prior.Model.2 = get_prior(Ymat | trials(Ytrials) ~ Age.z+ older.siblings.presence+Party.size.adult.z +  Sex + (1+Age.z+Party.size.adult.z|Focal), data =  offspring.reaction.UTE.data,
                          family = multinomial())

prior.Model.2
prior.Model.2$prior[6:9] <- "normal(0,1)"
prior.Model.2$prior[17:20] <- "normal(0,1)"
prior.Model.2$prior[28:31] <- "normal(0,1)"
prior.Model.2$prior[39:42] <- "normal(0,1)"
prior.Model.2$prior[50:53] <- "normal(0,1)"
prior.Model.2$prior[61:64] <- "normal(0,1)"
prior.Model.2$prior[72:75] <- "normal(0,1)"
prior.Model.2$prior[83:86] <- "normal(0,1)"
prior.Model.2

Model.2 <- brm(Ymat | trials(Ytrials) ~ Age.z + older.siblings.presence+Party.size.adult.z +  Sex + (1+Age.z+Party.size.adult.z|Focal), data =  offspring.reaction.UTE.data,
               family = multinomial(), cores = 12, chains =12, 
               control = list(adapt_delta = 0.98), prior=prior.Model.2,
               refresh = 100, warmup = 1000, iter = 2000)


pp_check(Model.2,nsamples=1000)


#graph with all behaviours and age unscaled Figure 2

c_eff_test1 <- conditional_effects(Model.2, effects = "Age.z", re_formula = NA, 
                                   robust = TRUE, prob = 0.95, method = "fitted",
                                   spaghetti = FALSE, surface = FALSE, resolution = 100,
                                   categorical = TRUE)

# Extract the plot without rendering
plot_obj <- plot(c_eff_test1, plot = FALSE)[[1]]

summary(Model.2)

# Customize x-axis labels
# Estimate the mean and SD from Age.z and whatever raw Age ranges you expect
# For example, if you *know* that Age.z ranges from ~-1.5 to 1.5
# and that corresponds to raw ages from 0 to 120 months:

# Back-calculate approximate mean and SD
age_min <- 0     # assume 0 months ~ lowest Age.z
age_max <- 120   # assume 120 months ~ highest Age.z
z_min <- min(offspring.reaction.UTE.data$Age.z, na.rm = TRUE)
z_max <- max(offspring.reaction.UTE.data$Age.z, na.rm = TRUE)

# Approximate SD and mean from linear relationship
approx_sd <- (age_max - age_min) / (z_max - z_min)
approx_mean <- age_min - z_min * approx_sd

# Now compute unscaled axis breaks
unscaled_breaks <- seq(0, 120, by = 12)
scaled_breaks <- (unscaled_breaks - approx_mean) / approx_sd
x_labels <- as.character(unscaled_breaks)


# Plot with customized x-axis labels and axis name


colorblind_palette <- c("#1F78B4", "black", "#E31A1C", "gold", "darkorchid1", "#A6CEE3", "darkgreen", "coral","gray")

Figure2a<-plot_obj +
  scale_x_continuous(
    name = "Age (in months)",
    limits = NULL,
    breaks = scaled_breaks,
    labels = x_labels
  ) +
  scale_y_continuous(
    name = "Proportion of events",
    limits = c(0, 1),  # Set the limits of the y-axis from 0 to 1
    expand = c(0, 0)   # Ensure the y-axis does not expand beyond the specified limits
  ) +
  scale_color_manual(
    name = "Offspring behavior",  # Change the legend title
    values = c("#1F78B4", "white", "#E31A1C", "gold", "darkorchid1", "#A6CEE3", "chartreuse2", "coral","black"),
    labels = c(
      "Not reacting", "Approaching the mother", "Looking towards the mother",
      "Whimpering", "Climbing up a tree", "Screaming", "Running away",
      "Attacking or arm waving", "Approaching another individual"
    )
  ) +
  scale_fill_manual(
    name = "Offspring behavior",  # Change the legend title
    values = colorblind_palette,
    labels = c(
      "Not reacting", "Approaching the mother", "Looking towards the mother",
      "Whimpering", "Climbing up a tree", "Screaming", "Running away",
      "Attacking or arm waving", "Approaching another individual"
    )
  ) +
  guides(color = guide_legend(override.aes = list(fill = NA))) +
  theme(
    legend.text = element_text(size = 18),
    legend.title = element_text(
      size = 19, vjust = 0.5,  # Adjust vjust to move title to the top
      face = "bold", hjust = 0.5
    ),  # Adjust margin to move title to the top
    legend.key.height = unit(1, "cm"),
    legend.key.width = unit(1, "cm"),
    axis.text = element_text(size = 17, color="black"),
    axis.title = element_text(size = 18),
    legend.position = "top",  # Move legend to the top
    legend.box.spacing = unit(0.5, "cm"),  # Adjust spacing between legend rows
    legend.justification = "center"  # Center the legend horizontally
  )+ guides(colour = guide_legend(title.position = "top"))+
  guides(
    color = guide_legend(
      override.aes = list(fill = colorblind_palette),
      nrow = 3,
      title.position = "top",
      title.hjust = 0.5,
      title.vjust = 1
    ),
    fill = guide_legend(
      nrow = 3,
      title.position = "top",
      title.hjust = 0.5,
      title.vjust = 1
    )
  )


Figure2a




#save
ggsave("Figure2a.svg", plot = Figure2a, width = 12, height = 10, dpi = 300)
ggsave("Figure2a.pdf", plot = Figure2a, width = 12, height = 10, dpi = 300)


#graph with behaviours and deviation Figure 3

behaviours.intercept<-read.csv("offspring-reaction-intercept.csv",sep=";",dec=",")

combined_df <- behaviours.intercept %>%
  pivot_longer(cols = c("Climbing.up.a.tree","Whimpering","Approaching.another.individual","Approaching.mother","Running.away","Screaming","Looking.towards.mother","Attacking.or.arm.waving","Not.reacting"),names_to = "Behaviour", values_to = "Probability")

#violin plot

modified_palette <- c( "gray36","gray", "coral", "darkorchid1","#E31A1C","#1F78B4", "darkgreen", "#A6CEE3",  "gold")



Figure2b<-ggplot(combined_df, aes(x = Behaviour, y = Probability, fill = Behaviour)) +
  geom_violin(trim = FALSE, scale = "width") +
  geom_point(position = position_jitter(width = 0.2, height = 0), size = 2, alpha = 0.5, color = "black") +
  stat_summary(fun = "mean", geom = "point", shape = 17, size = 2, color = "black", fill = "red", show.legend = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(title = "Deviation of behaviors according to age and other variables\n during dangerous events",  # Two lines title
       x = NULL,  # Remove x-axis label
       y = "Deviation") +
  scale_fill_manual(values = modified_palette) +
  guides(
    fill = guide_legend(title = "",
                        nrow = 3),
    color = FALSE,
    x = guide_axis(title = "Offspring Behavior")
  ) +
  scale_fill_manual(values = modified_palette, labels = c("Approaching the mother", "Approaching another individual", "Attacking or arm waving", "Climbing up a tree", "Looking towards the mother", "Not reacting", "Running away", "Screaming", "Whimpering")) +
  
  scale_x_discrete(labels = c("Approaching the mother", "Approaching  another individual", "Attacking or arm waving", "Climbing up a tree", "Looking towards the mother","Not reacting","Running away","Screaming","Whimpering")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 20),  # Increase the size of the legend text
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(size = 20, hjust = 0.5),  # Adjust hjust for the title
    legend.key.width = unit(1, "cm"),  # Adjust the width of legend key
    legend.key.height = unit(1.5, "cm")  # Adjust the height of legend key
  )

#save
ggsave("Figure2b.svg", plot = Figure2b, width = 12, height = 10, dpi = 300)


#Principal Component Analysis (PCA)

offspring.reaction.intercept.1.6.data<-read.csv("offspring-reaction-intercept-1-6.csv",sep=';',dec=",")


offspring.reaction.intercept.1.6.data <- offspring.reaction.intercept.1.6.data %>%
  rename(
    "Not reacting" = "Not.reacting",
    "Approaching the mother" = "Approaching.mother",
    "Running away" = "Running.away",
    "Looking at the mother" = "Looking.towards.mother",
    "Screaming" = "Screaming",
    "Whimpering" = "Whimpering",
    "Climbing up a tree" = "Climbing.up.a.tree",
    "Attacking or arm waving" = "Attacking.or.arm.waving",
    "Approaching another individual" = "Approaching.another.individual"
  )

#correlation test because data not normal and frequency

# Filter out the character columns
numeric_data <- offspring.reaction.intercept.1.6.data %>% select_if(is.numeric)

for (col in 1:ncol(numeric_data)) {
  numeric_data[, col] <- numeric_data[, col] * 100
}

# Calculate Kendall correlation matrix
kendall_cor_matrix <- numeric_data %>% correlate(method = "kendall")

kendall_cor_matrix


#parallel analysis
result <- fa.parallel(numeric_data, fm = 'ml', fa = 'fa', n.iter = 1000, show.legend = FALSE)
result

# check for missing values
colSums(is.na(offspring.reaction.intercept.1.6.data))
#names rows with offspring name
rownames(offspring.reaction.intercept.1.6.data)<-offspring.reaction.intercept.1.6.data$Focal

# normalizing the data
numerical.offspring.reaction.intercept.1.6.data <- offspring.reaction.intercept.1.6.data[,2:10]


#prcomp method
PCA.results <- prcomp(numerical.offspring.reaction.intercept.1.6.data, scale = F)

# Eigenvalues
eig.val <- get_eigenvalue(PCA.results)
eig.val

summary(PCA.results)


#attribute attachment types given by the UMAP
offspring.reaction.intercept.1.6.data$attachment.type[offspring.reaction.intercept.1.6.data$Focal == "SM1"] <- "Attachment type 1"
offspring.reaction.intercept.1.6.data$attachment.type[offspring.reaction.intercept.1.6.data$Focal == "EM9"] <- "Attachment type 3"
offspring.reaction.intercept.1.6.data$attachment.type[offspring.reaction.intercept.1.6.data$Focal == "EM10"] <- "Attachment type 3"
offspring.reaction.intercept.1.6.data$attachment.type[offspring.reaction.intercept.1.6.data$Focal == "NM15"] <- "Attachment type 3"
offspring.reaction.intercept.1.6.data$attachment.type[offspring.reaction.intercept.1.6.data$Focal == "SM22"] <- "Attachment type 1"
offspring.reaction.intercept.1.6.data$attachment.type[offspring.reaction.intercept.1.6.data$Focal == "NF23"] <- "Attachment type 1"
offspring.reaction.intercept.1.6.data$attachment.type[offspring.reaction.intercept.1.6.data$Focal == "EM20"] <- "Attachment type 2"
offspring.reaction.intercept.1.6.data$attachment.type[offspring.reaction.intercept.1.6.data$Focal == "SM19"] <- "Attachment type 1"
offspring.reaction.intercept.1.6.data$attachment.type[offspring.reaction.intercept.1.6.data$Focal == "EF7"] <- "Attachment type 1"
offspring.reaction.intercept.1.6.data$attachment.type[offspring.reaction.intercept.1.6.data$Focal == "SM8"] <- "Attachment type 3"
offspring.reaction.intercept.1.6.data$attachment.type[offspring.reaction.intercept.1.6.data$Focal == "SF27"] <- "Attachment type 1"
offspring.reaction.intercept.1.6.data$attachment.type[offspring.reaction.intercept.1.6.data$Focal == "NM16"] <- "Attachment type 1"
offspring.reaction.intercept.1.6.data$attachment.type[offspring.reaction.intercept.1.6.data$Focal == "SM28"] <- "Attachment type 2"
offspring.reaction.intercept.1.6.data$attachment.type[offspring.reaction.intercept.1.6.data$Focal == "SF6"] <- "Attachment type 2"
offspring.reaction.intercept.1.6.data$attachment.type[offspring.reaction.intercept.1.6.data$Focal == "EM5"] <- "Attachment type 1"
offspring.reaction.intercept.1.6.data$attachment.type[offspring.reaction.intercept.1.6.data$Focal == "SM26"] <- "Attachment type 2"
offspring.reaction.intercept.1.6.data$attachment.type[offspring.reaction.intercept.1.6.data$Focal == "EM30"] <- "Attachment type 2"
offspring.reaction.intercept.1.6.data$attachment.type[offspring.reaction.intercept.1.6.data$Focal == "SM25"] <- "Attachment type 2"
offspring.reaction.intercept.1.6.data$attachment.type[offspring.reaction.intercept.1.6.data$Focal == "NF23"] <- "Attachment type 1"


#graph of PCA Figure 5

attachment  <-  offspring.reaction.intercept.1.6.data$attachment.type


Figure3b<-fviz_pca_biplot(
  PCA.results,
  col.var = "black",
  col.ind = attachment,  # Color individuals by attachment type
  palette = c("brown", "dodgerblue2", "orange"),
  pointshape = 16,
  legend = "top",
  pointsize = 7,
  labelsize = 6,
  geom = "point"
) +
  labs(
    x = "Dim 1 (65.3%) explained by Approaching the mother",  # Change X-axis title
    y = "Dim 2 (13.8%) explained by Approaching another individual"   # Change Y-axis title
  ) +
  theme(
    legend.text = element_text(size = 16),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 18)
  ) +
  guides(col = guide_legend(title = NULL))  # Remove "col." in the legend title


#save
ggsave("Figure3b.svg", plot = Figure3b, width = 12, height = 10, dpi = 300)


# B.2. Mother-offspring proximity during social exploration Model 3 ####

mother.offspring.mean.distance.social.exploration.data<-read.csv("mother-offspring-mean-distance-social-exploration.csv",sep=";",dec=",") #load dataset


mother.offspring.mean.distance.social.exploration.data$attachment.type  <- factor(mother.offspring.mean.distance.social.exploration.data$attachment.type  , levels=c('3','1','2'))
mother.offspring.mean.distance.social.exploration.data$Group  <- factor(mother.offspring.mean.distance.social.exploration.data$Group  , levels=c('NORD', 'SUD','EST'))
mother.offspring.mean.distance.social.exploration.data$Sex  <- factor(mother.offspring.mean.distance.social.exploration.data$Sex  , levels=c('F', 'M'))

mother.offspring.mean.distance.social.exploration.data$Focal  <- factor(mother.offspring.mean.distance.social.exploration.data$Focal)

sort(unique(mother.offspring.mean.distance.social.exploration.data$attachment.type))
sort(unique(mother.offspring.mean.distance.social.exploration.data$Group))
sort(unique(mother.offspring.mean.distance.social.exploration.data$Sex))

nrow(mother.offspring.mean.distance.social.exploration.data)

range(mother.offspring.mean.distance.social.exploration.data$mean.distance.date)

Model.3.vif=lm(mean.distance.date ~ attachment.type+Age.z+ Age.mother.year.z + percentage.oestrus.z+ Sex + Group +rank.z +party.size.z ,
               data = mother.offspring.mean.distance.social.exploration.data)
vif(Model.3.vif)

prior.Model.3 = get_prior(mean.distance.date ~ attachment.type*Age.z+ Age.mother.year.z + percentage.oestrus.z+ Sex + Group +rank.z +party.size.z  +(1+party.size.z+Age.z+percentage.oestrus.z|Focal),
                          data = mother.offspring.mean.distance.social.exploration.data, family = gaussian)


prior.Model.3
prior.Model.3$prior[2:13] <- "normal(0,1)"
prior.Model.3

make_stancode(mean.distance.date ~ attachment.type*Age.z+ Age.mother.year.z + percentage.oestrus.z+ Sex + Group +rank.z +party.size.z  +(1+party.size.z+Age.z+percentage.oestrus.z|Focal),
              data = mother.offspring.mean.distance.social.exploration.data, family = gaussian, prior = prior.Model.3)

Model.3 = brm(mean.distance.date ~ attachment.type*Age.z+ Age.mother.year.z + percentage.oestrus.z+ Sex + Group +rank.z +party.size.z  +(1+party.size.z+Age.z+percentage.oestrus.z|Focal),
              data = mother.offspring.mean.distance.social.exploration.data, family = gaussian, 
              prior = prior.Model.3, 
              chains = 12, 
              cores = 12, 
              iter = 2000, 
              warmup = 1000,  
              thin = 2, 
              control = list(adapt_delta = 0.975), 
              sample_prior = "yes")



summary(Model.3)
summary(Model.3, prob=0.89)

hypothesis(Model.3, "attachment.type1=attachment.type2", class = "b", group = "",
           scope = c("standard", "ranef", "coef"), alpha = 0.05, seed = NULL)

hypothesis(Model.3, "attachment.type1:Age.z=attachment.type2", class = "b", group = "",
           scope = c("standard", "ranef", "coef"), alpha = 0.05, seed = NULL)

hypothesis(Model.3, "attachment.type1=attachment.type2", class = "b", group = "",
           scope = c("standard", "ranef", "coef"), alpha = 0.11, seed = NULL)

hypothesis(Model.3, "attachment.type1:Age.z=attachment.type2", class = "b", group = "",
           scope = c("standard", "ranef", "coef"), alpha = 0.11, seed = NULL)

#cond.r2
bayes_R2(Model.3, re_formula = NULL, summary = T)
#marginal.r2
bayes_R2(Model.3, re_formula = NA, summary = T)

conditional_effects(Model.3)
plot(Model.3)
pp_check(Model.3,nsamples=1000)



#boxplot conditional effects Figure 5

new_data <- expand.grid(
  Focal = unique(mother.offspring.mean.distance.social.exploration.data$Focal),
  attachment.type = unique(mother.offspring.mean.distance.social.exploration.data$attachment.type),
  Age.z = mean(mother.offspring.mean.distance.social.exploration.data$Age.z),
  Age.mother.year.z = mean(mother.offspring.mean.distance.social.exploration.data$Age.mother.year.z),
  percentage.oestrus.z = mean(mother.offspring.mean.distance.social.exploration.data$percentage.oestrus.z),
  Sex = unique(mother.offspring.mean.distance.social.exploration.data$Sex),
  Group = unique(mother.offspring.mean.distance.social.exploration.data$Group),
  rank.z = mean(mother.offspring.mean.distance.social.exploration.data$rank.z),
  party.size.z = mean(mother.offspring.mean.distance.social.exploration.data$party.size.z)
)

# Extract posterior samples for predictions
posterior_samples <- tidybayes::tidy_draws(predict(Model.3, newdata = new_data, draws = 1000))

# Add the attachment.type variable to the posterior_samples dataframe
posterior_samples$attachment.type <- rep(new_data$attachment.type, each = nrow(posterior_samples) / nrow(new_data))

# Plot boxplot with datapoints Figure 6

desired_order <- c("1", "2", "3")

# Reorder the levels of attachment.type
posterior_samples$attachment.type <- factor(
  posterior_samples$attachment.type,
  levels = desired_order
)

attachment_colors <- c("brown","dodgerblue2","orange")

Figure4<-ggplot(posterior_samples, aes(x = as.factor(attachment.type), y = Estimate, fill = as.factor(attachment.type))) +
  geom_violin() +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.7) +  # Add box plot inside
  geom_jitter(width = 0.1, alpha = 0.5, color = "black") +
  scale_fill_manual(values = attachment_colors) +
  labs(
    x = "Attachment type",
    y = "Mean distance in m during social exploration"
  ) +
  scale_x_discrete(
    breaks = c("1", "2", "3"),
    labels = c("1", "2", "3")
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 23),
    axis.title.y = element_text(size = 23),
    axis.text.x = element_text(size = 28),
    axis.text.y = element_text(size = 28),
    plot.title = element_text(size = 28)
  ) +
  guides(fill = FALSE)


#save
ggsave("Figure4.svg", plot = Figure4, width = 12, height = 10, dpi = 300)





# B.3. Receiving comfort and being comforted after a threatening event: the distinction between secure and insecure-resistant ####

solicitation.after.threat<-read.csv("solicitation-after-threat.csv",sep=";",dec=",")

Figure5<-ggplot(solicitation.after.threat, aes(x = MA.OA, fill = soli2)) +
  geom_bar(position = "stack") +
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
            size = 5, color = "black") +
  labs(x = "", y = "Number of events") +
  scale_fill_manual(values = c("#0072B2", "#D55E00"), name = "Offspring continuing or renewing \nvocal distress within 30s after\n the threatening event") +
  theme_minimal() +
  theme(
    legend.position = c(0.35, 0.85),  # Adjust the first value (x-coordinate) to move the legend box to the right
    legend.title = element_text(size = 17, color = "black", hjust = 0.5, margin = margin(b = 10), face = "bold"),
    legend.box = "horizontal",
    legend.background = element_rect(color = "black"),
    legend.box.margin = margin(6, 6, 6, 6),
    axis.text.x = element_text(size = 16, color = "black", angle = 0, hjust = 0.5, vjust = 0.5, lineheight = 0.9),
    axis.text.y = element_text(size = 15, color = "black"),
    axis.title.y = element_text(size = 16, color = "black"),
    legend.text = element_text(size = 17, color = "black"),
    plot.title = element_text(face = "bold", size = 16, color = "black", hjust = 0.5)
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  ylim(0, 40)  # Limit y-axis labels to 0 to 40

#save
ggsave("Figure5.svg", plot = Figure5, width = 12, height = 10, dpi = 300)


