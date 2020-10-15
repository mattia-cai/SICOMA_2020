rm( list = ls( ) )

# Count 

# Dependencies
require( "data.table" )
require( "data.table" )

# dataset -- (single Nace 2 assignment)
load( file = "outputData/compCases.RData", verbose = T )


# Aggregate industries
dta.cases[ , l := substr( nace2_2d_a64, 1, 1 ) ]
dta.cases[ l == "C", ind := "Manufacturing (C)" ]
dta.cases[ l %in% c( "D", "E"), ind := "Utilities (D-E)" ]
dta.cases[ l == "G", ind := "Trade (G)" ]
dta.cases[ l == "H", ind := "Transport (H)" ]
dta.cases[ l == "J", ind := "Communication (J)" ]
dta.cases[ l == "K", ind := "Finance (K)" ]
dta.cases[ is.na( ind ), table( l ) ]
dta.cases[ is.na( ind ), ind := "Other" ]

# Factor order
lab <- dta.cases[ , levels( as.factor( ind ) ) ]
index <- order( substr( lab, nchar( lab ) - 1, nchar( lab ) - 1 ) )
lab <- c( setdiff( lab[ index ], "Other" ), "Other" )
dta.cases[ ,ind := factor( ind, levels = lab ) ]

# Add up - by type
dta <- dta.cases[ , .( "mkt" = sum( mkt ) ), by = .( year, type, ind ) ]

# All cases
dta1 <- dta.cases[ , .( "mkt" = sum( mkt ) ), by = .( year, ind ) ]
dta1[, type := "All" ]
dta <- rbind( dta, dta1 )

# Some formatting
dta[ type == "cartel", type := "Cartels" ]
dta[ type == "merger", type := "Mergers" ]
dta[ , mkt := mkt / 1000 ]

# Long table
dta[, total := sum( mkt ) , by = .( type, year ) ]
dta[ , pct := ( mkt / total ) * 100 ]
tab <- dcast( dta,
              year + type + total ~ ind,
              value.var = "pct" )
setkeyv( tab, c( "type", "year" ) )
write.csv( tab, "results/case_descriptives/cases_by_industry_table_long.csv", na = "", row.names = F )

# Wide table
dta <- dta[ type != "All" ]
dta[ , total := sum( mkt ) , by = year ]
dta[ , pct := ( mkt / total ) * 100 ]
tab <- dcast( dta,
              year + total ~ ind + type,
              value.var = "pct" )
setkeyv( tab, "year" )
write.csv( tab, "results/case_descriptives/cases_by_industry_table_wide.csv", na = "", row.names = F )


# Transposed table
tab <- dcast( dta,
              ind + type ~ year,
              value.var = "pct" )
write.csv( tab, "results/case_descriptives/cases_by_industry_table_t.csv", na = "", row.names = F )
