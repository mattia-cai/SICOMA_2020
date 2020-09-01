rm( list = ls() )

# Main SICOMA script (started in September 2020)
# Trying to use Git for version control


##########################
### Auxiliary datasets ###
##########################

# Structural Business Statistics (2012-2016 data from file downloaded in Nov 2018)
source( "code/SBS_data.R" )

# Input-Output (IO) system
source( "code/IO_EU28_2014.R" )

# A64 gross output data from the national accounts
source( "code/nama10_a64.R" )       # national accounts from eurostat bulk download
source( "code/grossOutputA64.R" )   # extract gross output and aggregate for EU28

# Markup (MUP) and gross output (GO) data from Ecfin file
source( "code/EcfinMupGo.R" )

# Markup (MUP) and gross output (GO) data from Ecfin file. Updated with the most recent data on Dec 11th, 2018

# # Compare different sources of gross output data
# source( "code/GrossOutputComparisons.R" )



################################
### Competition case dataset ###
################################

# Make the data from DG COMP usable :)
source( "code/cleanCompData.R" )

# Count how many 4-digit aggregates in each NACE2 industry (needed for imputations in industries out of SBS scope)
source( "code/count_aggregates.R" )

# Merge the various datasets
source( "code/compCaseData.R" )



################
### Modeling ###
################

# Key sector analysis (standardized price shock)
source( "code/key_sectors.R" )

# Compute within- and cross-industry price effects (Expanded with duration analysis in Nov 19)
source( "code/priceImpacts.R" )

# # Compute within- and cross-industry price effects for a couple of significant cases
# source( "code/singleDecisions.R" )  # Could be probably eliminated

# Price level changes: main tables
source( "code/tables_like_book_chapter.R" )

# Numerical example for report
source( "code/numerical_example.R" )



#############################
### More stuff for report ###
#############################

# Main case descriptives
source( "code/main_case_des.R" )

# Industry-level price changes
source( "code/price_by_industry_charts.R" )
