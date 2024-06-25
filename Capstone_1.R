# DATA IN ######################################################################################################################################

vol.df = as.data.frame(read.csv("/Users/robert/Downloads/Integrated_View_01_2023.csv"))

NSP.df = vol.df
NSP.df = NSP.df[ ,(c(1:141) %% 2) == 0]
NSP.df = as.data.frame(t(NSP.df))
names(NSP.df) = vol.df[ ,1]

for(i in 1:14){
    NSP.df[ ,i] = as.numeric(gsub(',', '', NSP.df[ ,i]))
}

NSP.df = cbind(NSP.df[ ,1:12], NSP.df[ ,14])

NSP.ts = vector(length = 70)
for(i in 1:70){
    NSP.ts[i] = sum(NSP.df[i, ])
}
NSP.ts = ts(NSP.ts)
plot(NSP.ts)

vyvanceDates.str = paste(substr(names(vol.df), 12, 14), substr(names(vol.df), 16, 19))
vyvanceDates.str = vyvanceDates.str[2:length(vyvanceDates.str)]
vyvanceDates.str = vyvanceDates.str[odd.vec]

# SECTION ONE  #################################################################################################################################

library(TSA)
sectOne.ts = ts(NSP.ts[1:40]); sectTwo.ts = ts(NSP.ts[40:length(NSP.ts)])

seasonMu.vec = vector(length = 12)
count.vec = vector(length = 12)
index.int = 1

for(i in 1:40){
    seasonMu.vec[index.int] = seasonMu.vec[index.int] + sectOne.ts[i] # Add data point at index.
    count.vec[index.int] = count.vec[index.int] + 1     # Add to the count of how many data points have been added 
                                                        # to that month.
    index.int = index.int + 1                           # Advance index. 
    if(index.int == 13){                                # Reset index so that it keeps adding the same months        
        index.int = 1                                   # together. 
    }
}
seasonMu.vec = seasonMu.vec / count.vec                 # Devide by the number of observations to find seasonal 
                                                        # means. 
test.ts = rep(1, 40)
index.int = 1
for(i in 1:40){
    test.ts[i] = test.ts[i]*seasonMu.vec[index.int]
    index.int = index.int + 1                           # Advance index. 
    if(index.int == 13){                                # Reset index so that it keeps adding the same months        
        index.int = 1                                   # together. 
    }
}
plot(ts(test.ts))
plot(ts(sectOne.ts - test.ts))

# SECTION TWO ##################################################################################################################################
plot(sectTwo.ts)
sectTwo.lm = lm(sectTwo.ts ~ time(sectTwo.ts), data = sectTwo.ts)
lines(sectTwo.lm$coefficients[2]*c(1:31) + sectTwo.lm$coefficients[1], col = 'red')
plot(sectTwo.ts - sectTwo.lm$coefficients[2]*c(1:31) + sectTwo.lm$coefficients[1])

# FUNCTIONS #######################################################################################################

iqviaToTimeSeries2 <- function(q, select = 'even'){
    hold.df = q          
    out.df = hold.df                                             # Two copies of the raw data frame. 
    
    if(select == 'odd'){
        out.df = out.df[ ,(c(1:ncol(out.df)) %% 2) == 1]         # Selects odd columns.
        out.df = as.data.frame(t(out.df))
        names(out.df) = hold.df[ ,1]
    }
    else{
        out.df = out.df[ ,(c(1:ncol(out.df)) %% 2) == 0]         # Selects even columns.
        out.df = as.data.frame(t(out.df))
        names(out.df) = hold.df[ ,1]
    }
    
    out.ts = vector(length = nrow(out.df))                       # Creates a vector with the length matching the number of rows. 
    
    for(i in 1:ncol(out.df)){                                    # Selects each column and coerces it to numeric. 
        out.df[ ,i] = as.numeric(gsub(',', '', out.df[ ,i]))
    }
    for(i in 1:nrow(out.df)){
        out.ts[i] = sum(out.df[i, ], na.rm = TRUE)
    }
    
    out.ts = ts(out.ts)
    if(select == 'odd'){
         out.ts = out.ts[2:length(out.ts)]
    }
    return(out.ts)
    #return(out.df)
}

# EXPORT #######################################################################################################################################

vyvanceNSP.vec = NSP.ts
vyvanceEUTRx.vec = iqviaToTimeSeries2(vol.df, select = 'odd')
vyvanceOut.df = cbind(vyvanceDates.str, vyvanceEUTRx.vec, vyvanceNSP.vec)

write.csv(vyvanceOut.df, file = '/Users/robert/Capstone/vyvance_df_export')
read_csv("/Users/robert/Downloads/Amitiza-Brand.xlsx")

