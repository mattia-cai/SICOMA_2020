# Illustrate the logic of the permanent shock 
# Prepared for the steerging group meeting of Sep 8th, 2020
rm( list = ls() )

# Dependencies
require( "data.table" )

# Auxiliary functions
flist <- list.files( "code/funlib", pattern = "*.R", full.names = TRUE )
for ( f in flist ){ source( f ) }

# Competition case dataset
load( file = paste0( "outputData/compCases.RData"), verbose = T )

# Pick one industry for the example
i <- "C29"

# All cases in this indutry
( cases.i <- dta.cases[ nace2_2d_a64 == i, 
                        .( case_id, type, year, duration, mktT, delta_p ) ] )

# Affected market size and avoided price increase in this industry, by year
cases.i[, count := 1 ]
dt.all <- cases.i[ ,
                    .( count = sum( count ),
                       mktT = sum( mktT ),
                       delta_p = weighted.mean( delta_p, mktT ) ), 
                    by = year ]

# Expand with missing years
dt.all <- merge( data.table( year = 2012:2019 ), dt.all, by = "year", all = T )
dt.all[ is.na( count ), count := 0 ]
dt.all[ is.na( mktT ), mktT := 0 ]
dt.all[ is.na( delta_p ), delta_p := 0 ]
dt.all

# In an average year, these are the values of mktT and delta_p that you can expect
dt.all <- dt.all[ , .( count = mean( count ), 
                      mktT = mean( mktT),
                      delta_p = mean( delta_p ) ) ]
( dt.all <- cbind( d = "ALL", dt.all ) )

# In an average year, there will also be cases carried over from the earlier year
( dt2 <- cases.i[ duration == 2, ] ) # cases with 2-year duration
dt2 <- dt2[,.( count = sum( count ),
               mktT = sum( mktT ),
               delta_p = weighted.mean( delta_p, mktT ) ), 
           by = year ]
dt2 <- merge( data.table( year = 2012:2019 ), dt2, by = "year", all = T )
dt2[ is.na( count ), count := 0 ]
dt2[ is.na( mktT ), mktT := 0 ]
dt2[ is.na( delta_p ), delta_p := 0 ]
dt2
dt2 <- dt2[ , .( count = mean( count ), 
                 mktT = mean( mktT),
                 delta_p = mean( delta_p ) ) ]
( dt2 <- cbind( d = "2Y", dt2 ) )

# cases with 3-year duration
( dt3 <- cases.i[ duration == 3, ] ) 
dt3 <- dt3[,.( count = sum( count ),
               mktT = sum( mktT ),
               delta_p = weighted.mean( delta_p, mktT ) ), 
           by = year ]
dt3 <- merge( data.table( year = 2012:2019 ), dt3, by = "year", all = T )
dt3[ is.na( count ), count := 0 ]
dt3[ is.na( mktT ), mktT := 0 ]
dt3[ is.na( delta_p ), delta_p := 0 ]
dt3
dt3 <- dt3[ , .( count = mean( count ), 
                 mktT = mean( mktT),
                 delta_p = mean( delta_p ) ) ]
( dt3 <- cbind( d = "3Y", dt3 ) )

# No cases with 4- or 5- year duration
cases.i[ duration == 4, ]
cases.i[ duration == 5, ]

# Cases with 6-year duration
( dt6 <- cases.i[ duration == 6, ] ) 
dt6 <- dt6[,.( count = sum( count ),
               mktT = sum( mktT ),
               delta_p = weighted.mean( delta_p, mktT ) ), 
           by = year ]
dt6 <- merge( data.table( year = 2012:2019 ), dt6, by = "year", all = T )
dt6[ is.na( count ), count := 0 ]
dt6[ is.na( mktT ), mktT := 0 ]
dt6[ is.na( delta_p ), delta_p := 0 ]
dt6
dt6 <- dt6[ , .( count = mean( count ), 
                 mktT = mean( mktT),
                 delta_p = mean( delta_p ) ) ]
( dt6 <- cbind( d = "6Y", dt6 ) )

# Pull it all together
( dt <- rbind( dt.all, dt2, dt3, dt6 ) )

# What should we do?
dt[ , .( count = sum( count ),
         mktT = sum( mktT ),
         delta_p = weighted.mean( delta_p, mktT ) ) ]
