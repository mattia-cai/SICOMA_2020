rm( list = ls( ) )

# Recast the DG COMP case data into a more computer-friendly format
# author: mattia cai



######################
### Auxiliary data ###
######################

# Dependencies 
require( "readxl" )
require( "data.table" )

# 4-digit nace2 codes from the SBS
load( file = "intmData/SBS_data_wide.RData" ) 
sbs <- dta.sbs[ , sort( as.character ( unique( nace2 ) ) ) ]

# A64 nace 2 codes
nace2 <- read.csv( "inputData/SUTs/nace_2.csv", header = T, stringsAsFactors = F )
a64 <- sort( as.character( unique( nace2[, "nace_2"] ) ) )
a64[ a64 == "L68B" ] <- "L68"
a64 <- a64[ a64 != "L68A" ]



##################################
### Basic clean up of the data ###
##################################

# Process the excel file one sheet at a time
sheetCleanUp <- function( yr ){
  
  # Read the dataset
  s <- paste0( "Customer savings ", yr )
  pathname <- "inputData/DgComp/JRC_v3 input data 2012-2019.xlsx"
  dta <- read_excel( path = pathname, sheet = s, skip = 1, col_names = T )
  dta <- as.data.frame( dta )
  
  # Use standardized column names (modified in Sep 20 to reflect changes in the underlying file structure)
  cb  <- read.csv( "inputData/DgComp/comp_data_codebook.csv", header = T, stringsAsFactors = F )
  dta <- dta[ , cb$column ]
  names( dta ) <- cb[ , 1 ]
  
  # year
  dta$year <- yr
  
  # merger/cartel identifier (even though I guess it could be recovered from the case id...)
  dta$type <- "merger"
  # tag <- which( grepl( pattern = "cartel", x = tolower( dta$id ) ) )
  tag <- trimws( tolower( dta$id ) ) 
  tag <- grep( paste0( "cartels ", yr ), tag )
  dta[ tag : nrow( dta ) , "type" ] <- "cartel"
  
  # Drop all unnecessary rows
  dta <- dta[ c( 1 : ( tag - 1 ), ( tag + 2 ) : nrow( dta ) ), ] 
  dta <- dta[ apply( dta[ , 1 : 5 ], 1, function( x ) sum( is.na( x ) ) ) != 5, ]
  rownames( dta ) <- NULL
  
  # Other useless rows
  tag <- which( ! dta[ , "name" ] %in% c( "mergers", "cartels" ) )
  dta <- dta[ tag, ]
  
  # The 2015 and 2017 sheets have a whole bunch of notes at the bottom...
  tag <- which( grepl( pattern = "(*)", dta$id, fixed = T ) )
  if( length( tag ) > 0 ){ dta <- dta[ 1 : ( tag - 1 ), ] }
  
  # Drop some totals
  tag1 <- grepl( pattern = "GDP", dta$mkt, fixed = T )
  tag2 <- grepl( pattern = "savings", dta$mkt, fixed = T )
  tag3 <- grepl( pattern = "provisional", dta$mkt, fixed = T )
  tag <- which( tag1 | tag2 | tag3 )
  dta <- dta[ -tag, ]
  
  # Sometimes there is a dash for missing values
  dta[ dta == "-" ] <- NA
  
  # Also, I want to get rid of any thousand delimiters and 
  # express all figures in a proper numeric format
  cols <- c( "mkt", "duration" )
  fun <- function( z ){ as.numeric( gsub( pattern = ",", replacement = "", x = z ) ) }
  dta[ , cols ] <- lapply( dta[ , cols ], fun )
  dta$mkt <- round( dta$mkt, 2 )
  
  # output
  dta <- dta[ , c( "year", "type", cb$variable ) ]
  return( dta )
}

# Apply listwise and stack
df.list <- lapply( 2012:2019, sheetCleanUp )
dta.cases <- do.call( "rbind", df.list)
dta.cases <- as.data.table( dta.cases )
dta.cases$type <- factor( dta.cases$type, levels = c( "merger", "cartel" ) )
str( dta.cases )



###################################
### Annoying multidecision case ###
###################################

# A legitimate multirow  case
dta.cases[ , table( is.na( id ) ) ]
dta.cases[ is.na( id ) ]

# It is a component of this case (check the underlying excel file) 
# You have two decisions under the same case number
dta.cases[ id =="AT.39924*" ]

# Fix it
dta.cases[ id =="AT.39924*", id := "AT.39924a" ]
dta.cases[ is.na( id ), id := "AT.39924b" ]
dta.cases[ id == "AT.39924b", name := "Libor" ]
dta.cases[ is.na( nace2 ), nace2 := "K.64.30" ]



###########################
### Typo in Nace 2 code ###
###########################

# H.77.11 does not exist. It's N.77.11
dta.cases[ year == 2018 & id == "M. 8744" & nace2 == "H.77.11" ]
dta.cases[ year == 2018 & id == "M. 8744" & nace2 == "H.77.11", nace2 := "N.77.11" ]



#################################
### 4-digit NACE2 identifiers ###
#################################

# 4 digit codes to match SBS the identifiers

# Get all the codes that appear in the dataset
id <- dta.cases[ , nace2 ]
#id <- sort( unique( unlist( id0 ) ) ) 

# remove the dots
id4 <- strsplit( x = id, split = ".", fixed = T ) 

# In 2017, we have some 6-digit codes (drop the last part)
tag <- which( sapply( id4, length ) > 3 )
dta.cases[ tag ]
id4[ tag ] <- lapply( id4[ tag ], function( x ) x[ 1 : 3 ] )
id4 <- sapply( id4, function( x ) paste( x, collapse = "" ) )

# Do these codes match nicely with SBS?
setdiff( id4, sbs )
# Nope :(
# These cases, however, are in sectors outside the scope of the SBS,

# Now it is better: these are all out of SBS scope
setdiff( id4, sbs )

# replace the codes
dta.cases[ , nace2_4d := id4 ]
dta.cases[ , any( is.na( id4 ) ) ]   # SHould be F



######################################
### Extract the A64 industry codes ###
######################################

# Nace 2 divisions
id2 <- strsplit( x = id, split = ".", fixed = T ) 
id2 <- sapply( id2, function( x ) paste( x[ 1 : 2 ], collapse = "" ) )

# Make it A64
agg <- list( "B" = paste0( "B0", 5:9 ), 
             "C10-C12" = paste0( "C", 10:12 ),
             "C31_C32" = paste0( "C", 31:32 ),
             "E37-E39" = paste0( "E", 37:39 ),
             "J59_J60" = paste0( "J", 59:60 ),
             "J62_J63" = paste0( "J", 62:63 ), 
             "L68" = "L",
             "N80-N82" = paste0( "N", 80:82 ),
             "R90-R92" = paste0( "R", 90:92 ) )
for( i in seq_along( agg ) ){ id2[ id2 %in% agg[[ i ]] ] <- names( agg )[ i ] }

# Check
all( id2 %in% a64 ) # Should be T

# replace the single codes
dta.cases[ , nace2_2d_a64 := id2 ]



#########################
### Save this dataset ###
#########################

# eventually, this will be dropped 
dta.cases[ is.na( mkt ) ]

# Reorder columns
dta.cases <- dta.cases[ , c( "year", "type", "id", "name",
                             "nace2_2d_a64", "nace2_4d", "nace2_desc", 
                             "mkt", "duration",
                             "dp_min", "dp_max",
                             "save_min", "save_max" ), with = F ] 
save( dta.cases, file = "intmData/compCaseData.RData" ) 
write.csv( dta.cases, file = "intmData/compCaseData.csv", row.names = F, na = "" ) 


