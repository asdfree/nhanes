if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)
lodown( "nhanes" , output_dir = file.path( getwd() ) )
