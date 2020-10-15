rm( list = ls( ) )

# Average shock 2012-19 : with/without duration

# Dependencies
require( "data.table" )

# Auxiliary functions
flist <- list.files( "code/funlib", pattern = "*.R", full.names = TRUE )
for ( f in flist ){ source( f ) }

# EU28 supply and use system (2014)
load( "intmData/IO_EU28_2014.RData" )

# Short industry labels
labs0 <- read.csv( "inputData/SUTs/nace2_short_labels.csv", header = T )
nace2 <- as.character( labs0$nace_2 )
tmp <- nace2
tmp[ tmp %in% c( "L68A", "L68B" ) ] <- "L68"
labs0 <- as.character( labs0$short )
labs0 <- paste0( tmp, " - ", labs0 )
names( labs0 ) <- nace2

# Years relevant for the mean
yr.rng <- 2012:2019 

# price impacts without duration
load( file = "outputData/price_impacts_no_duration.RData", verbose = T )
dta1 <- dta[ year %in% yr.rng,.( case.count = mean( case.count ),
                                 mktT = mean( mktT ),
                                 go2_a64 = mean( go2_a64 ),
                                 w = mean( w ),
                                 dp = mean( dp ) ), by = nace2_2d_a64 ]
dta1[ , year := 8888 ]

# price impacts with duration
load( file = "outputData/price_impacts_duration.RData", verbose = T )
dta2 <- dta[ year %in% yr.rng,.( case.count = mean( case.count ),
                                 mktT = mean( mktT ),
                                 go2_a64 = mean( go2_a64 ),
                                 w = mean( w ),
                                 dp = mean( dp ) ), by = nace2_2d_a64 ]
dta2[ , year := 9999 ]

# Compute the spillover matrices listwise
dta.lst <- list( no_dur = dta1, dur = dta2 )
mat.lst <- lapply( dta.lst, spilloverCalculations )
# If necessary, these can saved for further manipulation

# Within-industry, spillover and total effects
for ( t in names( dta.lst ) ) {
  dta.lst[[ t ]][ , within := - ( w * dp ) * 100 ]
  dta.lst[[ t ]][ , spill := rowSums( mat.lst[[ t ]] ) * 100 ]
  dta.lst[[ t ]][ , total := within + spill ]
}
dta <- do.call( "rbind", dta.lst )

# Grand averages
dta[, .( within = weighted.mean( within, go2_a64 ),
         spill = weighted.mean( spill, go2_a64 ),
         total = weighted.mean( total, go2_a64 ) ),
    by = year ]


# Plot (selected industries)
cutoff1 <- dta[ year == 8888, quantile( abs( total ), probs = .75 ) ]
ind1 <- dta[ year == 8888 & abs(total) > cutoff1, nace2_2d_a64 ]
cutoff2 <- dta[ year == 9999, quantile( abs( total ), probs = .75 ) ]
ind2 <- dta[ year == 9999 & abs(total) > cutoff2, nace2_2d_a64 ]
ind <- sort( unique( c( ind1, ind2 ) ) )
dta[ year == 8888, d := "Without duration" ]
dta[ year == 9999, d := "With duration" ]
dta[ , d := factor( d, levels = c( "Without duration", "With duration" ) ) ]
dta <- dta[, .( nace2_2d_a64, d, within, spill, total ) ]
dta <- melt( dta, id.vars = c( "nace2_2d_a64", "d" ) )
dta <- dta[ variable != "total" ]
dta[ , variable := factor( variable, levels = c( "within", "spill" ) ) ]
dta[, lab := labs0[ dta[ , nace2_2d_a64 ] ] ]
dta1 <- dta[ nace2_2d_a64 %in% ind ]
p <- ggplot( data = dta1, aes( x = lab, y = abs( value ), fill = variable ) ) 
p <- p + geom_bar( stat = "identity", position = "stack" )
p <- p + facet_wrap( ~ d )
p <- p + labs( y = "Percent", x = "" )
p <- p + theme_bw()
#p <- p + theme( text = element_text( size = 30 ) )
p <- p + theme(axis.text.x = element_text( angle = 90 ) )
p <- p + theme( legend.title = element_blank() )
p