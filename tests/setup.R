if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)

this_sample_break <- Sys.getenv( "this_sample_break" )

library(lodown)

nhanes_cat <-
	get_catalog( "nhanes" ,
		output_dir = file.path( getwd() ) )

# skip the two physical activity files because ci fails for some undetermined reason
nhanes_cat <- subset( nhanes_cat , !grepl( "paxraw" , output_filename ) )

nhanes_cat <- nhanes_cat[ split( seq( nrow( nhanes_cat ) ) , 1 + sort( seq( nrow( nhanes_cat ) ) %% 10 ) )[[ this_sample_break ]] , ]

nhanes_cat <- lodown( "nhanes" , nhanes_cat )
if( any( nhanes_cat$years == "2015-2016" ) ){











options( survey.lonely.psu = "adjust" )

library(survey)

nhanes_demo_df <- 
	readRDS( file.path( getwd() , "2015-2016/demo_i.rds" ) )

nhanes_tchol_df <- 
	readRDS( file.path( getwd() , "2015-2016/tchol_i.rds" ) )

nhanes_df <- merge( nhanes_demo_df , nhanes_tchol_df , all = TRUE )

stopifnot( nrow( nhanes_df ) == nrow( nhanes_demo_df ) )

# keep only individuals who took the "mobile examination center" component
nhanes_df <- subset( nhanes_df , ridstatr %in% 2 )

nhanes_design <- 
	svydesign(
		id = ~sdmvpsu , 
		strata = ~sdmvstra ,
		nest = TRUE ,
		weights = ~wtmec2yr ,
		data = nhanes_df
	)
nhanes_design <- 
	update( 
		nhanes_design , 
		
		one = 1 ,
		
		pregnant_at_interview = 
			ifelse( ridexprg %in% 1:2 , as.numeric( ridexprg == 1 ) , NA ) ,
		
		race_ethnicity = 
			factor( 
				c( 3 , 3 , 1 , 2 , 4 )[ ridreth1 ] ,
				levels = 1:4 , 
				labels = 
					c( 'non-hispanic white' , 'non-hispanic black' , 
						'hispanic' , 'other' )
			) ,
		
		age_category =
			factor(
				findInterval( ridageyr , c( 20 , 40 , 60 ) ) ,
				labels = c( "0-19" , "20-39" , "40-59" , "60+" )
			)
	)
sum( weights( nhanes_design , "sampling" ) != 0 )

svyby( ~ one , ~ race_ethnicity , nhanes_design , unwtd.count )
svytotal( ~ one , nhanes_design )

svyby( ~ one , ~ race_ethnicity , nhanes_design , svytotal )
svymean( ~ lbxtc , nhanes_design , na.rm = TRUE )

svyby( ~ lbxtc , ~ race_ethnicity , nhanes_design , svymean , na.rm = TRUE )
svymean( ~ riagendr , nhanes_design )

svyby( ~ riagendr , ~ race_ethnicity , nhanes_design , svymean )
svytotal( ~ lbxtc , nhanes_design , na.rm = TRUE )

svyby( ~ lbxtc , ~ race_ethnicity , nhanes_design , svytotal , na.rm = TRUE )
svytotal( ~ riagendr , nhanes_design )

svyby( ~ riagendr , ~ race_ethnicity , nhanes_design , svytotal )
svyquantile( ~ lbxtc , nhanes_design , 0.5 , na.rm = TRUE )

svyby( 
	~ lbxtc , 
	~ race_ethnicity , 
	nhanes_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ lbxtc , 
	denominator = ~ ridageyr , 
	nhanes_design ,
	na.rm = TRUE
)
sub_nhanes_design <- subset( nhanes_design , age_category == "60+" )
svymean( ~ lbxtc , sub_nhanes_design , na.rm = TRUE )
this_result <- svymean( ~ lbxtc , nhanes_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ lbxtc , 
		~ race_ethnicity , 
		nhanes_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( nhanes_design )
svyvar( ~ lbxtc , nhanes_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ lbxtc , nhanes_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ lbxtc , nhanes_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ pregnant_at_interview , nhanes_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( lbxtc ~ pregnant_at_interview , nhanes_design )
svychisq( 
	~ pregnant_at_interview + riagendr , 
	nhanes_design 
)
glm_result <- 
	svyglm( 
		lbxtc ~ pregnant_at_interview + riagendr , 
		nhanes_design 
	)

summary( glm_result )
library(srvyr)
nhanes_srvyr_design <- as_survey( nhanes_design )
nhanes_srvyr_design %>%
	summarize( mean = survey_mean( lbxtc , na.rm = TRUE ) )

nhanes_srvyr_design %>%
	group_by( race_ethnicity ) %>%
	summarize( mean = survey_mean( lbxtc , na.rm = TRUE ) )

}
