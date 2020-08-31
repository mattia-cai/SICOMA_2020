rm( list = ls( ) )


##################
### Input data ###
##################

# 4 levels of NACE2
nace2 <- read.csv( "inputData/NACE_REV2_20161007_172920.csv", header = T, stringsAsFactors = F )
nace2 <- nace2[ , c( "Level", "Code" ) ]

# from NACE2 divisions to A64 in IOT
a64 <- read.csv( "inputData/SUTs/nace_2.csv", header = T, stringsAsFactors = F )
a64 <- unique( a64[ , 1 : 2] )



#######################################################
### Count how many 4-digit aggregates in a division ###
#######################################################

nace2 <- as.data.table( nace2 ) 
nace2 <- nace2[ Level == 4 ]
nace2[ , div := substr( x = Code, start = 1, stop = 2 ) ]
nace2[ , n4 := 1 ]
count <- nace2[ , .( "n4" = sum( n4 ) ), by = div ] 


###########################################
### Match divisions to A64 aggregations ###
###########################################


count[ , div := as.numeric( div ) ]
vec <- a64[ match( count[ , div ], a64$nace_2_detail ), "nace_2" ]
count[ , nace2 := vec  ]
count[ vec %in% c( "L68A" , "L68B" ), nace2 := "L68B" ]
count <- count[ , .( "n4"  = sum( n4 ) ), by = "nace2" ]
save( count, file = "intmData/N4_subsector_count.RData" )
write.csv( count, file = "intmData/N4_subsector_count.csv", row.names = F, na = "" )