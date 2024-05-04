library(brms)
library(bayestestR)
# Define function to summarise hypothesis with Bayes factors in both directions (10 & 01)
hypothesis_bf <- function (model, x) {
  hyp <- hypothesis(model, x)
  hyp$hypothesis$BF01 <- hyp$hypothesis$Evid.Ratio # BF in favour of H0 over H1
  hyp$hypothesis$BF10 <- 1/hyp$hypothesis$BF01 # BF in favour of H1 over H0
  cbind(hyp$hypothesis, hdi(hyp$samples))[, c("Hypothesis", "BF10", "BF01", "Estimate","CI_low", "CI_h")]
}

#Load and prepare data
xdata=read.table(file="degreea.txt",header=T,sep="\t")
xdata
str(xdata)

xdata$Degree<-as.numeric(xdata$Degree)
xperiod<-subset(xdata,Group=="Before"|Group=="During"|Group=="After")
str(xperiod)

### BRMS models
## fit model 1.1

form<- bf(Degree ~ 0 + Group + (1|ID))
get_prior(form, data=xperiod)
pri<- c(
  prior(student_t(5, 0, 5), class="b"),
  prior(student_t(3,0,2.5), class="sd"),
  prior(student_t(3,0,2.5), class="sigma")
)
chains<-4
cores<-chains
iter<-1000
m1.1<-brm(formula=form,
          data=xperiod,
          family=gaussian(),
          chains=chains,
          cores=cores,
          iter=iter,
          prior=pri,
          backend = "cmdstanr",
          sample_prior = "yes") # remove if we are not using bayes factor

#check that it is a good fit
plot(m1.1)
pp1= pp_check(m1.1, ndraw=100)
pp1

#results
summary(m1.1)
hdi(m1.1)
hypothesis_bf(m1.1, c(
  "GroupBefore = GroupDuring",
  "GroupDuring = GroupAfter",
  "GroupBefore = GroupAfter"))

## fit model 1.2

xdistance<-subset(xdata,Group=="L3m"|Group=="M3m")
str(xdistance)

form<- bf(Degree ~ 0 + Group + (1|ID))
get_prior(form, data=xdistance)
pri<- c(
  prior(student_t(5, 0, 5), class="b"),
  prior(student_t(3, 0, 2.5), class="sd"),
  prior(student_t(3,0,2.5), class="sigma")
)
chains<-4
cores<-chains
iter<-5000
m1.2<-brm(formula=form,
          data=xdistance,
          family=gaussian(),
          chains=chains,
          cores=cores,
          iter=iter,
          prior=pri,
          backend = "cmdstanr",
          sample_prior = "yes")

#check that it is a good fit
plot(m1.2)
pp1.2= pp_check(m1.2, ndraw=100)
pp1.2

#results
summary(m1.2)
hypothesis_bf(m1.2, "GroupL3m = GroupM3m")

## fit model 2.1
xdata=read.table(file="strengthb.txt",header=T,sep="\t")
xdata
str(xdata)
xdata$Strength<-as.numeric(xdata$Strength)

xperiod<-subset(xdata,Group=="Before"|Group=="During"|Group=="After")
str(xperiod)

form<- bf(Strength ~ 0 + Group + (1|ID))
get_prior(form, data=xperiod)

pri<- c(
  prior(student_t(5, 0, 5), class="b"),
  prior(student_t(3, 0, 2.5), class="sd"),
  prior(student_t(3, 0, 2.5), class="sigma")
)
chains<-4
cores<-chains
iter<-5000
m2.1<-brm(formula=form,
          data=xperiod,
          family=gaussian(),
          chains=chains,
          cores=cores,
          iter=iter,
          prior=pri,
          backend = "cmdstanr",
          sample_prior = "yes")
                                  
#check that it is a good fit
plot(m2.1)
pp2.1= pp_check(m2.1, ndraw=100)
pp2.1

#results
summary(m2.1)
hypothesis_bf(m2.1, c(
  "GroupBefore = GroupDuring",
  "GroupDuring = GroupAfter",
  "GroupBefore = GroupAfter"
))

### fit model 2.2
xdistance<-subset(xdata,Group=="L3m"|Group=="M3m")
str(xdistance)

form<- bf(Strength ~ 0 + Group + (1|ID))
get_prior(form, data=xdistance)

pri<- c(
  prior(student_t(5, 0, 5), class="b"),
  prior(student_t(3, 0, 2.5), class="sd"),
  prior(student_t(3, 0, 2.5), class="sigma")
)
chains<-4
cores<-chains
iter<-5000
m2.2<-brm(formula=form,
          data=xdistance,
          family=gaussian(),
          chains=chains,
          cores=cores,
          iter=iter,
          prior=pri,
          backend = "cmdstanr",
          sample_prior="yes")

#check that it is a good fit
plot(m2.2)
pp2.2= pp_check(m2.2, ndraw=100)
pp2.2

#results
summary(m2.2)
hypothesis_bf(m2.2, c("GroupL3m = GroupM3m"))

## fit model 3.1
xdata=read.table(file="closeness.txt",header=T,sep="\t")
xdata
str(xdata)
xdata$Closeness<-as.numeric(xdata$Closeness)
xperiod<-subset(xdata,Group=="Before"|Group=="During"|Group=="After")
str(xperiod)

form<- bf(Closeness ~ 0 + Group + (1|ID))
get_prior(form, data=xperiod)

pri<- c(
  prior(student_t(5, 0, 0.05), class="b"),
  prior(student_t(3, 0, 2.5), class="sd"),
  prior(student_t(3, 0, 2.5), class="sigma")
)
chains<-4
cores<-chains
iter<-5000
m3.1<-brm(formula=form,
          data=xperiod,
          family=gaussian(),
          chains=chains,
          cores=cores,
          iter=iter,
          prior=pri,
          backend = "cmdstanr",
          sample_prior="yes")

#check that it is a good fit
plot(m3.1)
pp3.1= pp_check(m3.1, ndraw=100)
pp3.1

#results
summary(m3.1)
hypothesis_bf(m3.1, c(
  "GroupBefore = GroupDuring",
  "GroupDuring = GroupAfter",
  "GroupBefore = GroupAfter"
))

## fit model 3.2
xdistance<-subset(xdata,Group=="L3m"|Group=="M3m")
str(xdistance)

form<- bf(Closeness ~ 0 + Group + (1|ID))
get_prior(form, data=xdistance)

pri<- c(
  prior(student_t(5, 0, 0.05), class="b"),
  prior(student_t(3, 0, 2.5), class="sd"),
  prior(student_t(3, 0, 2.5), class="sigma")
)
chains<-4
cores<-chains
iter<-5000
m3.2<-brm(formula=form,
          data=xdistance,
          family=gaussian(),
          chains=chains,
          cores=cores,
          iter=iter,
          prior=pri,
          backend = "cmdstanr",
          sample_prior = "yes")

#check that it is a good fit
plot(m3.2)
pp3.2= pp_check(m3.2, ndraw=100)
pp3.2

#results
summary(m3.2)
hypothesis_bf(m3.2, c("GroupL3m = GroupM3m"))
