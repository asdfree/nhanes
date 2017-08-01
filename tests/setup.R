if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)

nhanes_cat <-
	get_catalog( "nhanes" ,
		output_dir = file.path( getwd() ) )

# sample 25% of the records
which_records <- sample( seq( nrow( nhanes_cat ) ) , round( nrow( nhanes_cat ) * 0.25 ) )

# always sample years == "2013-2014"
nhanes_cat <- unique( rbind( nhanes_cat[ which_records , ] , subset( nhanes_cat , years == "2013-2014" ) ) )

lodown( "nhanes" , nhanes_cat )
