make an error   # do not source this file
##################################################################################################################################################################
# linear models of the pupilometry data, 21 March 2013.
##################################################################################################################################################################
# get the residuals from fitting a linear model to all the data (ignoring the old/new, valence labels) for each person.

rm(list=ls());  # clear R's memory

path <- "/Users/kchiew/Documents/RWorkspace_Diss/";   # directory where the files should be read in from

tbl <- read.csv(paste(path, "passiveview_indlsubj_mixedmodel.csv", sep=""));
#> head(tbl)
#  Subject ImageFile Luminosity OldNew Valence   PUPILMAG USABLE
#1       2      1440     139.70      0       1  -2.604524      1
#2       2      1463     105.43      0       1 -14.094040      1
#3       2      1600     102.72      1       1   6.829717      1

# set the columns that should not be interpreted as numbers to factors
tbl$Subject <- factor(tbl$Subject);
tbl$ImageFile <- factor(tbl$ImageFile);
tbl$OldNew <- factor(tbl$OldNew);
tbl$Valence <- factor(tbl$Valence);
tbl$USABLE <- factor(tbl$USABLE);

# do a bit of data cleaning.
# first, recode the OldNew and Valence for subject 35 to match everyone else
inds <- which(tbl$Subject == 35 & tbl$OldNew == 1);
tbl$OldNew[inds] <- 0;
inds <- which(tbl$Subject == 35 & tbl$Valence == 1);
tbl$Valence[inds] <- 0;
inds <- which(tbl$Subject == 35 & tbl$OldNew == 2);
tbl$OldNew[inds] <- 1;
inds <- which(tbl$Subject == 35 & tbl$Valence == 2);
tbl$Valence[inds] <- 1;

# second, get rid of rows that are not USABLE or have too large PUPILMAG
tbl <- tbl[which(tbl$USABLE == 1),];   # delete the rows with USABLE of 0
tbl <- tbl[which(tbl$PUPILMAG <= 100),];  # delete rows that are too big.

sub.ids <- unique(tbl$Subject);  # set this variable so we can easily loop through the subjects

# calculate the residuals, adding them as a column to tbl.
resids <- rep(NA, nrow(tbl));
for (i in 1:length(sub.ids)) {    # i <- 1;
  inds <- which(tbl$Subject == sub.ids[i]);  # the rows of tbl corresponding to this person
  lm.out <- lm(tbl$PUPILMAG[inds] ~ tbl$Luminosity[inds]);  # linear regression. summary(lm.out) shows the formatted results table
  resids[inds] <- lm.out$residuals;  # put the residuals into resids
}
if (length(which(is.na(resids))) > 0) { stop("a residual is missing"); }
tbl <- data.frame(tbl, resids);   # add the resids column to tbl

# turn the factor columns into strings so we don't have to keep converting them.
####### KIM: ARE THESE MAPPINGS CORRECT????
tbl$Subject <- paste("sub", tbl$Subject, sep="")
tbl$ImageFile <- paste("img", tbl$ImageFile, sep="")
# since OldNew and Valence are factors, you need to add to the levels for the mapping to work properly.
levels(tbl$OldNew) <- c(levels(tbl$OldNew), "new", "old")
inds <- which(tbl$OldNew == 1);
tbl$OldNew[inds] <- "new";
inds <- which(tbl$OldNew == 0);
tbl$OldNew[inds] <- "old";

levels(tbl$Valence) <- c(levels(tbl$Valence), "positive", "neutral")
inds <- which(tbl$Valence == 1);
tbl$Valence[inds] <- "positive";
inds <- which(tbl$Valence == 0);
tbl$Valence[inds] <- "neutral";

tbl <- subset(tbl, select=-USABLE);  # drop the USABLE column since we've only kept "usable" rows.

write.table(tbl, paste(path, "withResiduals.txt", sep=""));
# write.csv(tbl, paste(path, "withResiduals.csv", sep=""));   # easier to read into excel

##################################################################################################################################################################
##################################################################################################################################################################
# plot the residuals and fit the linear models.

rm(list=ls());  # clear R's memory

path <- "/Users/kchiew/Documents/RWorkspace_Diss/";   # directory where the files should be read in from

tbl <- read.table(paste(path, "withResiduals.txt", sep=""));  # from above
sub.ids <- unique(tbl$Subject)

y.lim <- c(min(tbl$resids), max(tbl$resids));  # the maximum and minimum y-axis values so can set the plot scale
yttl <- "residuals"

#pdf(file="d:/temp/resids_byValence.pdf", paper='US', width=8, height=10)  
#layout(matrix(1:20, c(4,5), byrow=TRUE));
par(mar=c(3, 4, 3, 2)) # sets the bottom, left, top and right plotting margins, in number of lines of text.
layout(matrix(1:16, c(4,4), byrow=TRUE));
for (i in 1:length(sub.ids)) {    #for (i in 1:20) {    #  i <- 1;   #i <- 1;
  stbl <- subset(tbl, Subject == sub.ids[i]);
  ttl <- paste("\n", sub.ids[i], " (", nrow(stbl), " points)", sep="");
  boxplot(stbl$resids~stbl$Valence, col=c('cornsilk', 'lightblue'), main=ttl, ylab=yttl, notch=TRUE)  #, ylim=y.lim
  lines(x=c(-1,6), y=c(0,0), col='grey');  # zero line
  boxplot(stbl$resids~stbl$Valence, col=c('cornsilk', 'lightblue'), notch=TRUE, add=TRUE);  # plot again so boxes *over* zero line (stupid R tricks)
}
#dev.off() 


#pdf(file="d:/temp/resids_byOldNew.pdf", paper='US', width=8, height=10)  
#layout(matrix(1:20, c(4,5), byrow=TRUE));
par(mar=c(3, 4, 3, 2)) # sets the bottom, left, top and right plotting margins, in number of lines of text.
layout(matrix(1:16, c(4,4), byrow=TRUE));
for (i in 1:length(sub.ids)) { #for (i in 1:20) {    #  i <- 1;   #i <- 1;
  stbl <- subset(tbl, Subject == sub.ids[i]);
  ttl <- paste("\n", sub.ids[i], " (", nrow(stbl), " points)", sep="");
  boxplot(stbl$resids~stbl$OldNew, col=c('cornsilk', 'lightblue'), main=ttl, ylab=yttl, notch=TRUE)  #, ylim=y.lim
  lines(x=c(-1,6), y=c(0,0), col='grey');  # zero line
  boxplot(stbl$resids~stbl$OldNew, col=c('cornsilk', 'lightblue'), notch=TRUE, add=TRUE);  # plot again so boxes *over* zero line (stupid R tricks)
}
#dev.off() 

##################################################################################################################################################################
##################################################################################################################################################################
# mixed models. do the residuals vary with valence or oldnew? each person saw many images, but the images are the same across people.

# note from D:\svnFiles\ToddFaceWord\crossDayFinalizing\clusters_20subs\correlations\graphs_ToddFiles.docx  -
# " about the mixed models result tables - I show the anova (fitted.lme) for the two ways of ordering the classification accuracy and trialType factors.
# These tables are for sequential F-tests: the "model enhancement" of including the second factor on top of the first. So if both factors are quite important
# the one listed first will have the better p-value (so the p-values will change a lot between the two models); if one is much better its p-value will be much
# better regardless of position. The other results tables are "marginal", so the order doesn't matter in the same way. (see my "mixed modeling" page in OneNote) "


library(nlme);
library(multcomp);

rm(list=ls());  # clear R's memory

path <- "/Users/kchiew/Documents/RWorkspace_Diss/";   # directory where the files should be read in from

tbl <- read.table(paste(path, "withResiduals.txt", sep=""));  # from above

#> head(tbl)
#  Subject ImageFile Luminosity OldNew  Valence   PUPILMAG     resids
#1    sub2   img1440     139.70    old positive  -2.604524   1.499900
#2    sub2   img1463     105.43    old positive -14.094040 -14.355600


# linear mixed models (follow rat brain example in chapter 5 of practical mixed models pdf book 
lme1 <- lme(fixed=resids~Valence*OldNew, random=~1|Subject, data=tbl); 
summary(lme1)
#       AIC      BIC    logLik
#  25745.26 25781.78 -12866.63
anova(lme1)  # order matters ...
#               numDF denDF   F-value p-value
#(Intercept)        1  3163  0.000000  1.0000
#Valence            1  3163  0.481672  0.4877
#OldNew             1  3163 22.532087  <.0001
#Valence:OldNew     1  3163  1.452233  0.2283

anova(lme(fixed=resids~OldNew*Valence, random=~1|Subject, data=tbl))
#               numDF denDF   F-value p-value
#Intercept)         1  3163  0.000000  1.0000
#OldNew             1  3163 22.517798  <.0001
#Valence            1  3163  0.495961  0.4813
#OldNew:Valence     1  3163  1.452233  0.2283
# pretty close to the other factor ordering; looks like it's see a much larger oldnew than valence effect.


# pdf pg 20: random effect of Valence associated with each person
lme2 <- lme(fixed=resids~Valence*OldNew, random=~Valence|Subject, data=tbl);  # did not converge.

lme2 <- lme(fixed=resids~Valence*OldNew, random=~OldNew|Subject, data=tbl);
summary(lme2)
#       AIC      BIC    logLik
#  25749.26 25797.96 -12866.63    # smaller is better. these are a bit bigger.

anova(lme1, lme2)
#     Model df      AIC      BIC    logLik   Test    L.Ratio p-value
#lme1     1  6 25745.26 25781.78 -12866.63                          
#lme2     2  8 25749.26 25797.96 -12866.63 1 vs 2 1.7028e-06       1
# so stick with lme1; lme2 is worse.


# residual variances differ for the treatments?
lme3 <- lme(fixed=resids~Valence*OldNew, random=~1|Subject, data=tbl, weights=varIdent(form=~1|Valence)); 
summary(lme3)
#    AIC      BIC   logLik
#  25745 25787.61 -12865.5   # smaller is better; these are practically identical

anova(lme1, lme3)
#     Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#lme1     1  6 25745.26 25781.78 -12866.63                        
#lme3     2  7 25745.01 25787.61 -12865.50 1 vs 2 2.258568  0.1329
# not sig difference, so keep lme1 as the best (it is simpler)


# residual variances differ for the treatments?
lme4 <- lme(fixed=resids~Valence*OldNew, random=~1|Subject, data=tbl, weights=varIdent(form=~1|OldNew)); 
summary(lme4)
#    AIC      BIC   logLik
#  25742.56 25785.17 -12864.28   # smaller is better; these are about the same

anova(lme1, lme4)
#     Model df      AIC      BIC    logLik   Test L.Ratio p-value
#lme1     1  6 25745.26 25781.78 -12866.63                       
#lme4     2  7 25742.56 25785.17 -12864.28 1 vs 2 4.70338  0.0301  
# but this is significant, so we use lme4.

anova(lme4)
#               numDF denDF   F-value p-value
#(Intercept)        1  3163  0.065224  0.7984
#Valence            1  3163  0.577479  0.4474
#OldNew             1  3163 22.526710  <.0001
#Valence:OldNew     1  3163  1.451801  0.2283
plot(lme4)   # four vertical lines!!! ideally this would just plot a blobby mess (no structure). so maybe there is some more fitting to do.


# the interaction is not significant, so take it out for the final version:
lme4b <- lme(fixed=resids~Valence+OldNew, random=~1|Subject, data=tbl, weights=varIdent(form=~1|OldNew)); 
anova(lme4b)
#            numDF denDF   F-value p-value
#(Intercept)     1  3164  0.065235  0.7984
#Valence         1  3164  0.577414  0.4474
#OldNew          1  3164 22.523570  <.0001

summary(glht(lme4b, mcp(OldNew="Tukey")))
#Linear Hypotheses:
#               Estimate Std. Error z value Pr(>|z|)    
#old - new == 0   2.0988     0.4422   4.746 2.08e-06 ***
# estimate is positive, so significant effect is in the direction of higher residuals for old than for new.




#### #### extra

# try with a random effect for ImageFile as well?
lme5 <- lme(fixed=resids~Valence*OldNew, random=~1|Subject/ImageFile, data=tbl, weights=varIdent(form=~1|OldNew)); 
summary(lme5)
#       AIC      BIC    logLik
#  25744.56 25793.25 -12864.28
anova(lme5)
#               numDF denDF   F-value p-value
#(Intercept)        1  3163  0.065204  0.7985
#Valence            1  3163  0.577464  0.4474
#OldNew             1  3163 22.526721  <.0001
#Valence:OldNew     1  3163  1.451802  0.2283


lme6 <- lme(fixed=resids~Valence*OldNew, random=~1|Subject/ImageFile, data=tbl); 
summary(lme6)



lme7 <- lme(fixed=resids~Valence*OldNew, random=~ImageFile|Subject, data=tbl);   # fails




##################################################################################################################################################################

























































#
