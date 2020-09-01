rm( list = ls( ) )

# Dependencies
require( "data.table" )
library( "ggplot2" )
library( "scales" )

# dataset -- (single Nace 2 assignment)
load( file = paste0( "outputData/compCases.RData"), verbose = T )
str( dta.cases )


# Average case size and duration
dta.cases[ , count := 1 ]
vars <- c( "count", "duration", "mkt", "mkt_t_a64_log" )
dta <- dta.cases[ , lapply( .SD, sum ), by = .( year, type ), .SDcols = vars ]
dta[ , dur := duration/count ]        # average duration
dta[ , duration := NULL ]
dta
dta <- melt( dta, id.vars = c( "year", "type", "count", "dur" ), value.name = "mkt" ) 
dta[ variable == "mkt", det := "nodet" ]
dta[ variable != "mkt", det := "det" ]
dta[ , variable := NULL ]
dta <- melt( dta, id.vars = c( "year", "type", "det" )  )
dta <- dta[ !( ( variable == "count" | variable == "dur" ) & det == "nodet" ) ] # simplify table
dta <- dcast( dta, year ~ type + variable + det , value.var = "value" )
dta
old.nms <- c( "merger_count_det", "cartel_count_det", "merger_dur_det", "cartel_dur_det" )
new.nms <- c( "merger_count", "cartel_count", "merger_dur_avg", "cartel_dur_avg" )
setnames( dta, old.nms, new.nms)
dta

# Actually, this is what I am going to put in the report 
dta[ , merger_mkt_tot := merger_mkt_nodet ] 
dta[ , merger_mkt_avg := merger_mkt_tot / merger_count] 
dta[ , cartel_mkt_tot := cartel_mkt_nodet ] 
dta[ , cartel_mkt_avg := cartel_mkt_tot / cartel_count] 
dta <- dta[, .( year, merger_count, merger_dur_avg, merger_mkt_tot, merger_mkt_avg,
         cartel_count, cartel_dur_avg, cartel_mkt_tot, cartel_mkt_avg ) ] 
dta
write.csv( dta , paste0( "results/main_case_des.csv" ), row.names = F )





##########################
### Plot - by industry ###
##########################

# Simple industries
dta.cases[ substr( nace2_2d_a64, 1, 1 ) == "C", ind := "(C) Manufacturing" ]
dta.cases[ substr( nace2_2d_a64, 1, 1 ) == "D" | substr( nace2_2d_a64, 1, 1 ) == "E", ind := "(D-E) Utilities" ]
dta.cases[ substr( nace2_2d_a64, 1, 1 ) == "G", ind := "(G) Trade" ]
dta.cases[ substr( nace2_2d_a64, 1, 1 ) == "H", ind := "(H) Transport" ]
dta.cases[ substr( nace2_2d_a64, 1, 1 ) == "J", ind := "(J) Communication" ]
dta.cases[ substr( nace2_2d_a64, 1, 1 ) == "K", ind := "(K) Finance" ]
dta.cases[ is.na( ind ), table( substr( nace2_2d_a64, 1, 1 ) ) ]
dta.cases[ is.na( ind ), ind := "Other" ]

dta <- dta.cases[ , .( "mkt" = sum( mkt ) ), by = .( year, type, ind ) ]
dta[ type == "cartel", type := "Cartels" ]
dta[ type == "merger", type := "Mergers" ]
ggplot( data = dta, aes( x = as.factor( year ), y = mkt / 1000, fill = ind ) ) + 
  geom_bar( stat = "identity", position = "stack" ) +
  facet_wrap( ~ type ) +
  labs( y = "Affected market size (Bln EUR)", x = "Year" ) +
  theme_bw() +
  theme( text = element_text( size = 20 ),
         axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5 ),
         plot.title = element_text( hjust = 0.5 ),
         legend.text = element_text( size = 20 ),
         legend.title = element_blank(), 
         legend.key = element_rect( size = 10 ) ) 

  





##########################
### Plot - by industry ###
##########################

# Mergers only
ggplot( data = dta[ type == "Mergers" ], aes( x = as.factor( year ), y = mkt / 1000, fill = ind ) ) + 
  geom_bar( stat = "identity", position = "stack" ) +
  labs( y = "Affected market size (Bln EUR)", x = "Year" ) +
  theme_bw() +
  theme( text = element_text( size = 20 ),
         axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5 ),
         plot.title = element_text( hjust = 0.5 ),
         legend.text = element_text( size = 20 ),
         legend.title = element_blank(), 
         legend.key = element_rect( size = 10 ) ) 


# Cartels only
ggplot( data = dta[ type == "Cartels" ], aes( x = as.factor( year ), y = mkt / 1000, fill = ind ) ) + 
  geom_bar( stat = "identity", position = "stack" ) +
  labs( y = "Affected market size (Bln EUR)", x = "Year" ) +
  theme_bw() +
  theme( text = element_text( size = 20 ),
         axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5 ),
         plot.title = element_text( hjust = 0.5 ),
         legend.text = element_text( size = 20 ),
         legend.title = element_blank(), 
         legend.key = element_rect( size = 10 ) ) 


