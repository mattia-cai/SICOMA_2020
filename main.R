rm( list = ls() )

# Main SICOMA script (started in September 2020)
# Trying to use Git for version control


##########################
### Auxiliary datasets ###
##########################

# Structural Business Statistics (2012-2016 data from file downloaded in Nov 2018)
source( "code/SBS_data.R" )         # Read from bulk dataset

# Input-Output (IO) system
source( "code/IO_EU28_2014.R" )

# A64 gross output data from the national accounts
source( "code/nama10_a64.R" )       # national accounts from eurostat bulk download
source( "code/grossOutputA64.R" )   # extract gross output and aggregate for EU28

# Markup (MUP) and gross output (GO) data from Ecfin file
source( "code/EcfinMupGo.R" )

# Make the data from DG COMP usable :)
source( "code/cleanCompData.R" )    # Has J59_J60 been fixed?

# Count how many 4-digit aggregates in each NACE2 industry (needed for imputations in industries out of SBS scope)
source( "code/count_aggregates.R" )



################################
### Competition case dataset ###
################################

# # In earlier versions, all these tasks were carried out in a single file
# source( "code/compCaseData.R" )

# Merge the various datasets
source( "code/full_dataset.R" )

# Estimate GO4 for the relevant sectors
source( "code/go4_calculations.R" )

# Logistic deterrence
source( "code/logistic_deterrence.R" )

# Avoided price increase assumptions + some dataset formatting
source( "code/compCases.R" )





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
