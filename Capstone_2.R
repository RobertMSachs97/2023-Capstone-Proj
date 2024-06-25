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

