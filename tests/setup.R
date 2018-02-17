if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)

library(lodown)
this_sample_break <- Sys.getenv( "this_sample_break" )
nls_cat <- get_catalog( "nls" , output_dir = file.path( getwd() ) )
record_categories <- ceiling( seq( nrow( nls_cat ) ) / ceiling( nrow( nls_cat ) / 2 ) )
nls_cat <- nls_cat[ record_categories == this_sample_break , ]
nls_cat <- lodown( "nls" , nls_cat )
if( any( grepl( "nlsy97" , nls_cat$full_url ) ) ){











options( survey.lonely.psu = "adjust" )

library(survey)

nlsy_files <- 
	list.files(
		file.path( getwd() ) ,
		full.names = TRUE ,
		recursive = TRUE
	)

# read in the R loading script
nlsy97_r <- 
	readLines( nlsy_files[ grepl( "nlsy97(.*)R$" , basename( nlsy_files ) ) ] )

# find all instances of "data$" in the r script
data_dollar <- grep( "data\\$" , nlsy97_r )
	
# extract the column names from the R script
first_line <- grep( "names(new_data) <-" , nlsy97_r , fixed = TRUE )
close_parentheses <- grep( ")" , nlsy97_r , fixed = TRUE )
last_line <- min( close_parentheses[ close_parentheses > first_line ] )
column_names_lines <- nlsy97_r[ seq( first_line , last_line ) ]
column_names_lines <- 
	gsub( 'names(new_data) <-' , 'column_names <-' , column_names_lines , fixed = TRUE )
eval( parse( text = column_names_lines ) )

# choose which columns to import
columns_to_import <-
	c( 'R0000100' , 'T5206900' , 'R9829600' , 'R0536300' , 
	'Z9061800' , 'T6657200' , 'R1205300' , 'T7545600' )

	
# for each column to import, look for a recoding block
find_recoding_block <-
	function( w ){
		
		this_block_start <- min( grep( paste0( "data\\$" , w ) , nlsy97_r ) )
		
		recode_lines <- 
			seq( 
				this_block_start , 
				min( data_dollar[ data_dollar > this_block_start ] ) - 1 
			)
			
		paste( nlsy97_r[ recode_lines ] , collapse = '' )
		
	}

recodes_to_run <- unlist( lapply( columns_to_import , find_recoding_block ) )
	
# readr::read_delim() columns must match their order in the csv file
columns_to_import <-
	columns_to_import[ order( match( columns_to_import , column_names ) ) ]

	
# confirm all column names are available
stopifnot( all( columns_to_import %in% column_names ) )

# identify the .dat file
nlsy97_dat <- nlsy_files[ grepl( "nlsy97(.*)dat$" , basename( nlsy_files ) ) ]

nls_variables_df <- 
	data.frame( 
		readr::read_delim( 
			nlsy97_dat , 
			col_names = columns_to_import , 
			col_types = 
				paste0( 
					ifelse( column_names %in% columns_to_import , 'n' , '_' ) , 
					collapse = "" 
				) ,
			delim = ' '
		) 
	)

# remove all missings
nls_variables_df[ nls_variables_df < 0 ] <- NA

recodes_to_run <- 
	gsub( "data\\$" , "nls_variables_df$" , recodes_to_run )

# align the main variables with what the R script says	
for( this_recode in recodes_to_run ) eval( parse( text = this_recode ) )

# cluster and strata variables
nls_psustr_df <-
	readRDS( grep( "strpsu\\.rds$" , nlsy_files , value = TRUE ) )
	
# you can read more about longitudinal weights here
# http://www.nlsinfo.org/weights

# the lodown:::get_nlsy_weights function returns a data.frame object
# containing the unique person identifiers and also a column of weights.

# view which points-in-time are available for a particular study
# lodown:::get_nlsy_selections( "nlsy97" )

# download weights for respondents in 1997
w <- lodown:::nls_get_weights( "nlsy97" , 'YES' , 'SURV1997' )

# download weights for respondents who were in any of the 1997, 2002, or 2007 surveys
# w <- 
# 	lodown:::nls_get_weights( "nlsy97" , 'YES' , c( 'SURV1997' , 'SURV2002' , 'SURV2007' ) )

# download weights for respondents who were in all of the 1997, 2002, and 2007 surveys
# w <- 
# 	lodown:::nls_get_weights( "nlsy97" , 'NO' , c( 'SURV1997' , 'SURV2002' , 'SURV2007' ) )

# download weights for respondents who are in all available surveys
# w <- 
# 	lodown:::nls_get_weights( "nlsy97" , "NO" , lodown:::nls_get_selections( "nlsy97" ) )

# merge weights with cluster and strata variables
nls_survey_df <- merge( nls_psustr_df , w )

# merge variables onto survey design
nls_df <- merge( nls_variables_df , nls_survey_df )

nls_design <- 
	svydesign( 
		~ R1489800 , 
		strata = ~ R1489700 , 
		data = nls_df ,
		weights = ~ weight ,
		nest = TRUE
	)
nls_design <- 
	update( 
		nls_design , 
		one = 1 ,
		bachelors_degree_or_higher = 
			as.numeric( as.numeric( T6657200 ) >= 5 )
	)
sum( weights( nls_design , "sampling" ) != 0 )

svyby( ~ one , ~ R1205300 , nls_design , unwtd.count )
svytotal( ~ one , nls_design )

svyby( ~ one , ~ R1205300 , nls_design , svytotal )
svymean( ~ T7545600 , nls_design , na.rm = TRUE )

svyby( ~ T7545600 , ~ R1205300 , nls_design , svymean , na.rm = TRUE )
svymean( ~ T6657200 , nls_design , na.rm = TRUE )

svyby( ~ T6657200 , ~ R1205300 , nls_design , svymean , na.rm = TRUE )
svytotal( ~ T7545600 , nls_design , na.rm = TRUE )

svyby( ~ T7545600 , ~ R1205300 , nls_design , svytotal , na.rm = TRUE )
svytotal( ~ T6657200 , nls_design , na.rm = TRUE )

svyby( ~ T6657200 , ~ R1205300 , nls_design , svytotal , na.rm = TRUE )
svyquantile( ~ T7545600 , nls_design , 0.5 , na.rm = TRUE )

svyby( 
	~ T7545600 , 
	~ R1205300 , 
	nls_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ R9829600 , 
	denominator = ~ T7545600 , 
	nls_design ,
	na.rm = TRUE
)
sub_nls_design <- subset( nls_design , R1205300 %in% 4:5 )
svymean( ~ T7545600 , sub_nls_design , na.rm = TRUE )
this_result <- svymean( ~ T7545600 , nls_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ T7545600 , 
		~ R1205300 , 
		nls_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( nls_design )
svyvar( ~ T7545600 , nls_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ T7545600 , nls_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ T7545600 , nls_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ bachelors_degree_or_higher , nls_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( T7545600 ~ bachelors_degree_or_higher , nls_design )
svychisq( 
	~ bachelors_degree_or_higher + T6657200 , 
	nls_design 
)
glm_result <- 
	svyglm( 
		T7545600 ~ bachelors_degree_or_higher + T6657200 , 
		nls_design 
	)

summary( glm_result )
library(srvyr)
nls_srvyr_design <- as_survey( nls_design )
nls_srvyr_design %>%
	summarize( mean = survey_mean( T7545600 , na.rm = TRUE ) )

nls_srvyr_design %>%
	group_by( R1205300 ) %>%
	summarize( mean = survey_mean( T7545600 , na.rm = TRUE ) )
}
