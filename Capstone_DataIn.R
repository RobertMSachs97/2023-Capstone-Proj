# DATA IN ######################################################################################################################################

rawDataFrames.list = list(arformoterol_branded, asenapine_branded, formoterol_branded, lacosamide_branded, nebivolol_branded, pregabalin_branded,
                          arformoterol_generic, asenapine_generic, formoterol_generic, lacosamide_generic, nebivolol_generic, pregabalin_generic)
     # Note: These data frames contain both NSP and EUTRx data, alternating by columns. This means that R is reading these with each date being 
     # a new variable and each variable being a new row. 
dataNames.NSP.vec = c("arformoterol_branded.NSP.ts", "asenapine_branded.NSP.ts", "formoterol_branded.NSP.ts", "lacosamide_branded.NSP.ts", 
                       "nebivolol_branded.NSP.ts", "pregabalin_branded.NSP.ts", "arformoterol_generic.NSP.ts", "asenapine_generic.NSP.ts", 
                       "formoterol_generic.NSP.ts", "lacosamide_generic.NSP.ts", "nebivolol_generic.NSP.ts", "pregabalin_generic.NSP.ts")
dataNames.EUTrx.vec = c("arformoterol_branded.EUTRx.ts", "asenapine_branded.EUTRx.ts", "formoterol_branded.EUTRx.ts", 
                        "lacosamide_branded.EUTRx.ts", "nebivolol_branded.EUTRx.ts", "pregabalin_branded.EUTRx.ts", 
                        "arformoterol_generic.EUTRx.ts", "asenapine_generic.EUTRx.ts", "formoterol_generic.EUTRx.ts", 
                        "lacosamide_generic.EUTRx.ts", "nebivolol_generic.EUTRx.ts", "pregabalin_generic.EUTRx.ts")

for(i in 1:12){
     hold.df = as.data.frame(rawDataFrames.list[i])         
     hold.2.ts = iqviaToTimeSeries2(hold.df)                     # NSP      
     hold.3.ts = iqviaToTimeSeries2(hold.df, select = 'odd')     # EUTRx
     
     assign(dataNames.NSP.vec[i], hold.2.ts)
     assign(dataNames.EUTrx.vec[i], hold.3.ts)
}

# REFFERENCE LISTS #############################################################################################################################

allNSPSeries.list = list(arformoterol_branded.NSP.ts, asenapine_branded.NSP.ts, formoterol_branded.NSP.ts, lacosamide_branded.NSP.ts, 
                         nebivolol_branded.NSP.ts, pregabalin_branded.NSP.ts, arformoterol_generic.NSP.ts, asenapine_generic.NSP.ts, 
                         formoterol_generic.NSP.ts, lacosamide_generic.NSP.ts, nebivolol_generic.NSP.ts, pregabalin_generic.NSP.ts)
allEUTRxSeries.list = list(arformoterol_branded.EUTRx.ts, asenapine_branded.EUTRx.ts, formoterol_branded.EUTRx.ts, 
                            lacosamide_branded.EUTRx.ts, nebivolol_branded.EUTRx.ts, pregabalin_branded.EUTRx.ts, 
                            arformoterol_generic.EUTRx.ts, asenapine_generic.EUTRx.ts, formoterol_generic.EUTRx.ts, 
                            lacosamide_generic.EUTRx.ts, nebivolol_generic.EUTRx.ts, pregabalin_generic.EUTRx.ts)
masterRef.list = allNSPSeries.list           
masterRef.list = append(masterRef.list, allEUTRxSeries.list)
masterNames.list = c(dataNames.NSP.vec, dataNames.EUTrx.vec)

# AUTOPLOTTING  ################################################################################################################################

for(i in 1:12){
     plot(ts(unlist(allNSPSeries.list[i])), main = dataNames.NSP.vec[i])
}
for(i in 1:12){
     plot(ts(unlist(allEUTRxSeries.list[i])), main = dataNames.EUTrx.vec[i])
}
for(i in 1:12){
     if(i <= 6){
          col.NSP.ref = 'red'
          col.EUTRx.ref = 'orange'
     }
     else{
          col.NSP.ref = 'blue'
          col.EUTRx.ref = 'green'
     }
     if(i == 1){
          plot(ts(unlist(allNSPSeries.list[i])),ylim = c(0, 25000000), col = col.NSP.ref)
          lines(ts(unlist(allEUTRxSeries.list[i])), col = col.EUTRx.ref)
     }
     else{
          lines(ts(unlist(allNSPSeries.list[i])), col = col.NSP.ref)
          lines(ts(unlist(allEUTRxSeries.list[i])), col = col.EUTRx.ref)
     }
}

# DATA OUT CREATION ############################################################################################################################
 
maxLength.int = 0 
for(i in 1:24){
     hold.ts = ts(unlist(masterRef.list[i]))
     hold.int = length(hold.ts)
     if(hold.int > maxLength.int){
          maxLength.int = hold.int
     }
}    # maxLength.int = 70
dummy.vec = vector(length = 70)
out.df = as.data.frame(dummy.vec)

for(i in 1:24){
     hold.ts = ts(unlist(masterRef.list[i]))
     hold.int = length(hold.ts)
     if(hold.int < 70){
          na.vec = c(rep(NA, (70 - hold.int)))
          hold.ts = c(hold.ts, na.vec)
          out.df = cbind(out.df, hold.ts)
     }
     else{
          out.df = cbind(out.df, hold.ts)
     }
}

out.df = out.df[ ,2:25]
names(out.df) = masterNames.list

# DATA CORRUPTION CHECK ########################################################################################################################

sumMistmatch.int = 0
for(i in 1:24){
     out.ref = out.df[ ,i]
     inner.ref = unlist(masterRef.list[i])
     for(j in 1:length(inner.ref)){
          if(inner.ref[j] != out.ref[j]){
               sumMistmatch.int = sumMistmatch.int + 1
          }
     }
}
sumMistmatch.int # If this does not equal zero, there is a problem. 

# INDEX SYNC ###################################################################################################################################

out.indexed.df = out.df
startIndex.vec = vector(length = 24)

# allDateRef.vec = paste0(substr(names(arformoterol_branded), 11, 13)[2:141], substr(names(arformoterol_branded), 15, 18)[2:141])[odd.vec]
for(i in 1:24){
     startIndex.vec[i] = 70 - sum(is.na(out.df[ ,i]))                                     
     if(startIndex.vec[i] < 70){                                                         
          out.indexed.df[ ,i] = c(rep(NA, 70))                                           
          out.indexed.df[ ,i][(71 - (startIndex.vec[i])):70] = out.df[ ,i][1:startIndex.vec[i]];
     }
     print(sum(is.na(out.df[ ,i])) == sum(is.na(out.indexed.df[ ,i])))
}
startIndex.vec

tail(out.indexed.df)
ncol(out.df)

# EXPORT #######################################################################################################################################

write.csv(out.df, file = '/Users/robert/Capstone/out_df_export')
write.csv(out.indexed.df, file = '/Users/robert/Capstone/out_indexed_df_export')

head(read.csv('/Users/robert/Capstone/out_indexed_df_export'))

# DATA IN TWO ##################################################################################################################################

rawDataFrames.two.list = list(test_book_brovana, truvada_book, bystolic_book, atripla_book, selzentry_book, amitiza_book, saphris_book, 
                              chantix_book, lyrica_book, uloric_book, vesicare_book, symbicort_book) # <- length = 12
test_book_brovana <- assign('brovana_book', test_book_brovana)
newProbs.mat = matrix(nrow = 12, ncol = 12)

charScaleToInt <- function(vec.out, vec.ref){
     for(i in 1:length(vec.out)){
               if(is.na(vec.ref[i])){
                    vec.ref[i] = 'NA'
               }
               if(vec.ref[i] == 'M'){
                    vec.out[i] = 1000000
               }
               else if(vec.ref[i] == 'K'){
                    vec.out[i] = 1000
               }
               else if(vec.ref[i] == 'A'){
                    vec.out[i] = 1
               }
          }
     return(vec.out)
}

for(i in 1:12){
     hold.df = as.data.frame(rawDataFrames.two.list[i])                              # copy referenced df
     ref.obj = hold.df[1, 3]                                                         # copy first obs on 3rd col
     if(i != 8){                                                                     # one of them is NA so it must be handled differently
          index.int = sum(hold.df[ ,4] == ref.obj) + 1
     }
     else{
          index.int = sum(is.na(hold.df[ ,4])) + 1
     }
     brand.vec    = hold.df[ ,2]                                
     gen.vec      = hold.df[ ,4]
     for(q in 1:72){
          bool = (gen.vec[q] == '#N/A') | is.na(gen.vec[q])
          if(bool){
               gen.vec[i] = NA
          }
     }
     
     brandScale.char = substr(brand.vec, nchar(brand.vec), nchar(brand.vec))
     genScale.char = substr(gen.vec, nchar(gen.vec), nchar(gen.vec))
     
     brandScale.vec = vector(length = 72)
     genScale.vec = vector(length = 72)
     
     brandScale.vec = charScaleToInt(brandScale.vec, brandScale.char) 
     genScale.vec = charScaleToInt(genScale.vec, genScale.char)
     
     brandVol.vec = as.numeric(gsub('\\.', '', substr(brand.vec, 1, nchar(brand.vec) - 1)))
     genVol.vec = as.numeric(gsub('\\.', '', substr(gen.vec, 1, nchar(gen.vec) - 1)))
     
     brandVol.vec = brandVol.vec*brandScale.vec
     genVol.vec = genVol.vec*genScale.vec
     
     for(j in 4:72){
          brandVol.vec[j] = mean(brandVol.vec[(j - 2):j], na.rm = TRUE)
          genVol.vec[j] = mean(genVol.vec[(j - 2):j], na.rm = TRUE)
     }
     out.vec = genVol.vec/(genVol.vec + brandVol.vec)
     out.vec = out.vec[index.int:(index.int + 11)]
     newProbs.mat[ ,i] = out.vec
}


plot(1:12, ylim = c(-.1, 1), col = 'white')
test = vector(length = 12)
for(i in 1:12){
     lines(newProbs.mat[ ,i])
     test[i] = mean(newProbs.mat[i, ])
}
lines(test, lwd = 2, col = 'red')
