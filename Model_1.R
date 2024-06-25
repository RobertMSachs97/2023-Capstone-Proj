# COLUMN GUIDE #################################################################################################################################

#Generic columns: 7:12 and 19:24
names(out.df)[1:6]
names(out.df)[7:12] 
names(out.df)[13:18]
names(out.df)[19:24]
genericNSP.df = out.df[ ,7:12]
genericEUTRx.df = out.df[ ,19:24]

# DISCRETE DDX EXPERIMENT ######################################################################################################################

allGeneric.ddx.df = cbind(genericNSP.df, genericEUTRx.df)
for(i in 1:12){
     copy.vec = c(allGeneric.ddx.df[ ,i], allGeneric.ddx.df[ ,i][70]) # length is 71
     if(copy.vec[1] == 0 & copy.vec[2] == 0){
          copy.vec = c(copy.vec[3:71], NA, NA)
     }
     copy2.vec = vector(length = 70)
     for(j in 1:70){
          copy2.vec[j] = copy.vec[j+1] / copy.vec[j]
     }
     allGeneric.ddx.df[ ,i] = copy2.vec
}
plot(c(1:70)/4, col = 'white', main = 'DDX Experiment 1')
for(i in 1:12){
     lines(allGeneric.ddx.df[ ,i], lwd =.7, col = 586 + i)
}

# DISCRETE DDX MODEL ###########################################################################################################################

allGeneric.ddx.df = allGeneric.ddx.df[1:9,]
plot(c(1:10)*1.5, col = 'white', main = 'DDX Experiment 1 Zoom')
for(i in 1:12){
     lines(allGeneric.ddx.df[ ,i], lwd =.7, col = 586 + i)
}

test = vector(length = 9)
for(i in 1:9){
     test[i] = mean(unlist(allGeneric.ddx.df[i, ]))
}
lines(test, col = 'red', lwd = 1)
summary(glm(test ~ c(1:9)))
summary(glm(test ~ (exp(c(1:9)))))
            
# AUTOPLOTTING #################################################################################################################################

max.int = 0
for(i in 1:24){
     hold.int = max(out.indexed.df[ ,i], na.rm = TRUE)
     if(hold.int > max.int){
          max.int = hold.int
     }
}
plot(c(1:70), ylim = c(0, max.int), main = "All Data (Indexed)", col = 'white')
for(i in 1:24){
     col.str = ''
     if(i %in% 1:6){     col.str = 'violetred'
     }
     if(i %in% 7:12){    col.str = 'slateblue'
     }
     if(i %in% 13:18){   col.str = 'maroon'
     }
     if(i %in% 19:24){   col.str = 'palegreen'
     }
     lines(out.indexed.df[ ,i], col = col.str, lwd = 1.5)
}

# MODEL ONE ####################################################################################################################################
#Generic columns: 7:12 and 19:24
# log(p / 1 - p)
logOdds <- function(negResponse, posResponse){
     q = length(negResponse)
     out.vec = vector(length = q)
     p.db = 0
     for(i in 1:q){
          if(is.na(posResponse[i])){
               out.vec[i] = NA
          }
          else{
               p.db = posResponse[i]/(negResponse[i] + posResponse[i])
               out.vec[i] = log(p.db/(1-p.db))
          }
     }
     return(out.vec)
}
prob <- function(negResponse, posResponse){
     q = length(negResponse)
     out.vec = vector(length = q)
     for(i in 1:q){
          if(is.na(posResponse[i])){
               out.vec[i] = NA
          }
          else{
               out.vec[i] = posResponse[i]/(negResponse[i] + posResponse[i])
          }
     }
     return(out.vec)
}

edited.df = cbind(out.indexed.df[ ,2:3], out.indexed.df[ ,5:9], out.indexed.df[ ,11:15], out.indexed.df[ ,17:21], out.indexed.df[ ,23:25])
genericLogOdd.mat = matrix(nrow = 70, ncol = 10)
genericProb.mat = matrix(nrow = 70, ncol = 10)

for(i in 1:10){
     if(i %in% 1:5){
          genericLogOdd.mat[ ,i] = logOdds(negResponse = edited.df[ ,i], posResponse = edited.df[ ,i+5])
          genericProb.mat[ ,i] = prob(negResponse = edited.df[ ,i], posResponse = edited.df[ ,i+5])
     }
     else{
          genericLogOdd.mat[, i] = logOdds(negResponse = edited.df[ ,i+5], posResponse = edited.df[ ,i+10])
          genericProb.mat[ ,i] = prob(negResponse = edited.df[ ,i+5], posResponse = edited.df[ ,i+10])
     }
     
}

col.vec = c('royalblue', 'salmon', 'plum1', 'slateblue', 'turquoise')
plot((1:70), col = 'white', ylim = c(-5, 15), main = 'Log Odds')
for(i in 1:10){
     if(i %in% 1:5){col.str = col.vec[i]
     }
     else{col.str = col.vec[i - 5]
     }
     lines(genericLogOdd.mat[ ,i], col = col.str)
}

plot((1:70)/70, col = 'white', ylab = "Proportion of Market", xlab = "Monthly Rolling Average", main = "Generic Market Share Growth")
for(i in 1:10){
     if(i %in% 1:5){col.str = col.vec[i]
     }
     else{col.str = col.vec[i - 5]
     }
     
     lines(genericProb.mat[ ,i], col = col.str, lwd = 2)
}

latestIndex.int = 0
for(i in 1:10){
     hold.int = sum(is.na(genericProb.mat[ ,i]))
     if(hold.int > latestIndex.int){
          latestIndex.int = hold.int
     }
}
latestIndex.int = 70 - latestIndex.int
genericProbTwo.mat = matrix(nrow = latestIndex.int, ncol = 10)
for(i in 1:10){
     if(i == 2 || i == 7){
          index.int = 43
     }
     else{
          index.int = sum(is.na(genericProb.mat[ ,i])) + 1
     }
     hold.vec = genericProb.mat[ ,i]
     genericProbTwo.mat[ ,i] = hold.vec[index.int:(index.int + latestIndex.int - 1)]
     
}

plot((1:12)/12, col = 'white', ylab = "Proportion of Market", xlab = "Quarterly Rolling Average", main = "Generic Market Share Growth")
for(i in 1:10){
     if(i %in% 1:5){col.str = col.vec[i]
     }
     else{col.str = col.vec[i - 5]
     }
     
     lines(genericProbTwo.mat[ ,i], col = col.str, lwd = 2)
}

# MODEL TWO ####################################################################################################################################

probMean.vec = vector(length = 12)
for(i in 1:12){ probMean.vec[i] = mean(genericProbTwo.mat[i, ])
}

     # Stuff
probMean.exp.vec = exp(probMean.vec)
plot(probMean.exp.vec)
test1 = lm(probMean.vec ~ log(sqrt(c(1:12))))
summary(test1)
test3 = lm(probMean.vec ~ log(sqrt(c(1:12)) + 1))
summary(test3)

     # Variance
var.vec = vector(length = 12)
for(i in 1:12){
     var.vec[i] = var(genericProbTwo.mat[i, ])
}
var.vec 
plot(var.vec, main = "Proportion Variance", cex = .5)

plot((1:12)/12, col = 'white', ylab = "Proportion of Market", xlab = "Monthly Rolling Average", main = "Generic Market Share Growth",
     ylim = c(-.1, 1))
for(i in 6:10){
     lines(genericProbTwo.mat[ ,i], lwd = .7, col = 76 + i)
}
lines(probMean.vec, col = 'red', lwd = 2)

lines(probMean.vec - (1.96*sqrt(var.vec)), col = 'blue'); lines(probMean.vec + (1.96*sqrt(var.vec)), col = 'blue')

# MULTICOLINEARITY #############################################################################################################################
# genericProbTwo.mat
sum.db = 0
count.int = 0
for(i in 5:10){
     for(j in 1:5){
          sum.db = sum.db + cor(genericProbTwo.mat[ ,i], genericProbTwo.mat[ ,j])
          # sum.db = sum.db + cor(genericLogOdd.two.mat[ ,i], genericLogOdd.two.mat[ ,j])
          count.int = count.int + 1
     }
}
     # Demonstration
count.int; sum.db; sum.db/count.int
summary(glm(c(1:12) ~ (
     (exp((genericProbTwo.mat[, 6]))^2) + 
          (exp((genericProbTwo.mat[, 7]))^2) +
     (exp((genericProbTwo.mat[, 8]))^2) + (exp((genericProbTwo.mat[, 9]))^2) + (exp((genericProbTwo.mat[, 10])))^2)))

summary(glm(c(1:12) ~ (exp((genericProbTwo.mat[, 6]))^2) + (exp((genericProbTwo.mat[, 5]))^2)))

summary(glm(c(1:12) ~ (exp((genericProbTwo.mat[, 6]))^2) + (exp((genericProbTwo.mat[, 7]))^2) 
        ))


     # Multicolinearity effects are severe
# MODEL THREE ##################################################################################################################################
latestIndex.int = 0
for(i in 1:10){
     hold.int = sum(is.na(genericLogOdd.mat[ ,i]))
     if(hold.int > latestIndex.int){
          latestIndex.int = hold.int
     }
}

latestIndex.int = 70 - latestIndex.int
genericLogOdd.two.mat = matrix(nrow = latestIndex.int, ncol = 10)
for(i in 1:10){
     if(i == 2 || i == 7){
          index.int = 43
     }
     else{
          index.int = sum(is.na(genericLogOdd.mat[ ,i])) + 1
     }
     hold.vec = genericLogOdd.mat[ ,i]
     genericLogOdd.two.mat[ ,i] = hold.vec[index.int:(index.int + latestIndex.int - 1)]
}

plot(1:12, ylim = c(min(genericLogOdd.two.mat), max(genericLogOdd.two.mat)), col = 'white')
for(i in 1:10){
     lines(genericLogOdd.two.mat[ ,i])
}

varLog.vec = vector(length = 12)
for(i in 1:12){
     varLog.vec[i] = var(genericLogOdd.two.mat[i, ])
}
plot(varLog.vec, main = "Log Odds Variance", cex = .5)

# var.vec, varLog.vec
pVarVar.db = var(var.vec); logOddVarVar.db = var(varLog.vec)

var.std.vec = (var.vec - mean(var.vec)) / (sqrt(pVarVar.db))

varLog.std.vec = (varLog.vec - mean(varLog.vec)) / (sqrt(logOddVarVar.db))

plot(varLog.std.vec, main = "Variance Comparison (Standardized)", cex = .5)
     points(var.std.vec , cex = .5, col = 'red')

logOddVarVar.db
var(var.vec)

# MODEL FOUR ###################################################################################################################################
varTwo.vec = vector(length = 12)
probMeans.testdata.vec = vector(length = 12)
for(i in 1:12){
     varTwo.vec[i] = var(genericProbTwo.mat[i, 5:10])
     probMeans.testdata.vec[i] = mean(genericProbTwo.mat[i,5:10])
}
plot(varTwo.vec, main = "Variance Change over Time ", ylab = "Variance", xlab = "Time Since Market Entry")
plot(ts(probMeans.testdata.vec))

summary(lm(probMeans.testdata.vec ~ log(sqrt(1:12))))

# MODEL FIVE ~ STAGE ONE #######################################################################################################################
  
q.vec = out.indexed.df$formoterol_generic.EUTRx.ts
w.vec = out.indexed.df$formoterol_branded.EUTRx.ts
index.int = sum(is.na(q.vec)) + 3
q.vec = q.vec[index.int:(index.int + 11)]; w.vec = w.vec[index.int:(index.int + 11)]
p.vec = q.vec/(q.vec + w.vec)
masterProb.mat = cbind(genericProbTwo.mat[ ,6:10], newProbs.mat, p.vec)
rm(q.vec, w.vec, p.vec, index.int)

plot(1:12, ylim = c(-.1, 1), col = 'white')
for(i in 1:18){
     hold.vec = masterProb.mat[ ,i]
     
     if(i < 6){col.ref = 'red'
     }
     else if(i %in% 6:17){col.ref = 'blue'
     }
     else if(i == 18){col.ref = 'green'
     }
     lines(hold.vec, col = col.ref)
     rm(col.ref, hold.vec)
}
combMean.vec = vector(length = 12)
for(i in 1:12){
     combMean.vec[i] = mean(masterProb.mat[i, ])
}
lines(combMean.vec, lwd = 2)

combVar.vec = vector(length = 12)
for(i in 1:12){
     combVar.vec[i] = var(masterProb.mat[i, ])
}
lines(combMean.vec + 1.96*combVar.vec)


plot(1:11,ylim = c(-.3, .3), col = 'white')
zero.int = 0; one.int = 0; three.int = 0; two.int = 0
for(i in 1:18){
     hold.vec = diff(masterProb.mat[ ,i], 1)
     if(sum(hold.vec < 0) == 0){
          col.ref = 'slateblue'
          zero.int = zero.int + 1
     }
     else if(sum(hold.vec < 0) == 1){
          col.ref = 'turquoise'
          one.int = one.int + 1
     }
     else if(sum(hold.vec <0) == 2){
          col.ref = 'salmon'
          two.int = two.int + 1
     }
     else{
          col.ref = 'red'
          three.int = three.int + 1     
               print(i)
     }
     lines(hold.vec, col = col.ref)
     rm(col.ref, hold.vec)
}

     # OBSERVATIONS 
          # 9 and 18 seem to be the weird ones. They are now trimmed off for atypical behavior. In other words, they are not included in the 
          # model because there patters are observably rare and indicative of extenuating circumstances. 
     
zero.int; one.int; two.int; three.int
(8 + 5) / (8 + 5 + 4 + 1) # <- 72% of drugs have 1 or fewer negative growth periods. 
(8 + 4) / (8 + 5 + 4) # <- When we trim off the the atypical drugs, that number is 70%. 
          # In summary, generic markets generally have 1 or fewer months where the rolling average does not increase. 

masterProb.edited.mat = masterProb.mat[ ,1:17]

# MODEL FIVE ~ STAGE TWO #######################################################################################################################
combMean.edited.vec = vector(length = 12)
classif.tags = vector(length = 17)
for(i in 1:17){
     hold.vec = diff(masterProb.edited.mat[ ,i], 1)
     if(sum(hold.vec < 0) < 2){
          classif.tags[i] = TRUE
     }
     else if(sum(hold.vec <0) == 2){
          classif.tags[i] = FALSE
     }
     rm(hold.vec)
}
plot(1:12, ylim = c(-.1, 1), col = 'white')
for(i in 1:17){
     #if(i < 13){
      #    combMean.edited.vec[i] = mean(masterProb.edited.mat[i, ])
     #}
     bool = classif.tags[i]
     print(bool)
     if(bool){
          col.ref = 'slateblue'
     }
     else{
          col.ref = 'salmon'
     }
     lines(masterProb.edited.mat[ ,i], col = col.ref)
     rm(col.ref)
}
lines(combMean.edited.vec, lwd = 2, col = 'red')


combVar.edited.vec = vector(length = 12)
for(i in 1:12){
     combVar.edited.vec[i] = var(masterProb.edited.mat[i, ])
}
plot(combVar.edited.vec, ylim = c(0, .1))
points(combVar.vec, col = 'red')
     # It appears that the removal of the unusual vectors decreases variance in the beginning of the market entry, but the final value it 
          # converges to is roughly similar. It also seems that in the first six months, your three month rolling average is going to demonstrate
          # it's general behavior. 

for(i in 1:17){
     plot(masterProb.edited.mat[ ,i], main = i)
     acf(masterProb.edited.mat[ ,i], main = i)
}

plot(1:12, ylim = c(-.1, 1), col = 'white')
ref.vec = c(9, 17, 18)
for(i in 1:18){
     col.ref = 'slateblue'
     if(i %in% ref.vec){
          col.ref = 'red'
     }
     lines(masterProb.mat[ ,i], col = col.ref)
     rm(col.ref)
}

# MODEL SIX ####################################################################################################################################
typical.mat = cbind(masterProb.mat[ ,1:8], masterProb.mat[ ,10:16])
atypical.mat = cbind(masterProb.mat[ ,9], masterProb.mat[ ,17:18])

typicalMeans.vec = vector(length = 12)
typicalVar.vec = vector(length = 12)
for(i in 1:12){
     typicalMeans.vec[i] = mean(typical.mat[i, ])
     typicalVar.vec[i] = var(typical.mat[i, ])
}
lines(typicalMeans.vec, lwd = 2)
plot(typicalVar.vec, ylim = c(0, .075), cex = .6)                            #### <<< USE <<<
     points(combVar.vec, col = 'red', cex = .6)
     points(combVar.edited.vec, col = 'slateblue', cex = .6)
     # OBSERVATIONS - With all three, there was pretty high or peak variance around month five. If we reference the growth charts, this should be
          # unsurprising. It's around month five that the biggest changes are occurring, and the shape of the curve is being established. If you
          # are doing well by month five, you will likely continue to do well. If there are bad months, don't despair, growth will still happen. 
classif.tags[9] = FALSE
typicalClassif.tags = c(classif.tags[1:8], classif.tags[10:16])

plot(1:12, ylim = c(-.1, 1), col = 'white', main = "Market Share Through Time", xlab = "Months Since Market Entry", 
     ylab = "Proportion of Market")
for(i in 1:15){
     col.ref = 'salmon'
     bool = typicalClassif.tags[i]
     if(bool){
          col.ref = 'black'
     }
     lines(typical.mat[ ,i], col = col.ref)
     rm(bool, col.ref)
}
lines(typicalMeans.vec, col = 'red', lwd = 2)
lines(typicalMeans.vec-1.96*(sqrt(typicalVar.vec)), col = 'blue', lwd = 2)
lines((1/(1 + exp(-1*(typicalMeans.vec-1.96*(sqrt(typicalVar.vec)))))), col = 'green', lwd = 2)

typical.sigmoid.mat = 1/(1 + (exp(-1*(typical.mat))))
plot(1:12, ylim = c(0, 1), col = 'white')
for(i in 1:15){
     lines(typical.sigmoid.mat[ ,i])
}
typicalVar.sigmoid.vec = vector(length = 12)
for(i in 1:12){
     typicalVar.sigmoid.vec[i] = var(typical.sigmoid.mat[i, ])
}


# FEATURE TESTING <<< USE THESE ################################################################################################################
nodip.df = NULL; dip.df = NULL

     # TESTS A DIP WITHIN t = [1:5]
for(i in 1:15){
     hold.vec = diff(typical.mat[ ,i][1:5], 1)
     bool = (sum(hold.vec < 0) > 0)
     if(bool){
          dip.df = cbind(dip.df, typical.mat[ ,i])
     }
     else{
          nodip.df = cbind(nodip.df, typical.mat[ ,i])
     }
     rm(hold.vec, bool)
}

shapiro.test(dip.df[12, ])
shapiro.test(nodip.df[12, ])
var.test(dip.df[12, ], nodip.df[12, ], alternative = "two.sided")
     # Both fail to reject (H0: Data ~ N()). We also fail to reject (HO: Var1 = Var2)
t.test(dip.df[12, ], nodip.df[12, ], alternative = 'less', var.equal = TRUE)
     # There is a difference, but it is NOT statistically significant. This means that there is no provable link between a rolling average dip 
          # and a lower expected market share. 

rm(dip.df, nodip.df)

     # TESTS A SUM OF PROPORTIONS WITHIN t = [1:3]
sub.df = NULL; over.df = NULL
meanSum.db = sum(typicalMeans.vec[1:3])
for(i in 1:15){
     hold.vec = typical.mat[ ,i]
     test.db = sum(hold.vec[1:3])
     bool = test.db > meanSum.db

     if(bool){
          over.df = cbind(over.df, hold.vec)
     }
     else{
          sub.df = cbind(sub.df, hold.vec)
     }
     rm(bool, hold.vec, test.db)
}

mean(sub.df[12, ]); mean(over.df[12, ])
shapiro.test(sub.df[12, ])
shapiro.test(over.df[12, ])
var.test(sub.df[12, ], over.df[12, ], alternative = "two.sided")
t.test(sub.df[12, ], over.df[12, ], alternative = 'less', var.equal = FALSE)
     # Difference was not provably significant at most standard alpha levels. However, the p value is not crazy high. In other words, there is a 
          # decent chance the final result will be lower. 

steeper.df = NULL; shallow.df = NULL
meanSlope.db = typicalMeans.vec[5] / typicalMeans.vec[1]
for(i in 1:15){
     hold.vec = typical.mat[ ,i]
     test.db  = hold.vec[5] / hold.vec[1]
     bool = (meanSlope.db < test.db)
     if(bool){
          steeper.df = cbind(steeper.df, hold.vec)
     }
     else{
          shallow.df = cbind(shallow.df, hold.vec)
     }
     rm(hold.vec, test.db, bool)
}
mean(steeper.df[12, ]); mean(shallow.df[12, ])
shapiro.test(steeper.df[12, ])
shapiro.test(shallow.df[12, ])
var.test(steeper.df[12, ], shallow.df[12, ], alternative = "two.sided")
t.test(steeper.df[12, ], shallow.df[12, ], alternative = 'greater', var.equal = TRUE)


# STUFF ########################################################################################################################################
plot(0.009258, main = "Error", col = 'red', ylim = c(-.1, max(typical.mat[1, ])), lwd = 2, cex = 2.)
for(i in 1:15){
     point.db = typical.mat[1, i]
     points(point.db, cex = .5)
     
}
summary(lm(typicalMeans.vec ~ log(sqrt(1:12))))

plot(1:12, lwd = 3, main = "Market Share Through Time", xlab = "Months Since Market Entry", 
     ylab = "Rolling Average Market Share", ylim = c(0, 1), col = 'white')
for(i in 1:15){
     lines(typical.mat[ ,i], lwd = .5)
}
lines(log(sqrt(c(1:12)))*.616535, lwd = 2, col = 'red')
lines(typicalMeans.vec, col = 'blue', lwd = 2)
plot(ts(typicalVar.vec), main = "Dispersion at Time t", lwd = 1.5, ylab = "Variance", xlab = "Months Since Market Entry")

plot(1:12, col = 'white', ylim = c(-.1, 1), main = "Atypical Adoption Curves",  xlab = "Months Since Market Entry", 
     ylab = "Rolling Average Market Share")

for(i in 1:3){
     lines(atypical.mat[ ,i], col = col.vec[i + 2], lwd = 2)
}
