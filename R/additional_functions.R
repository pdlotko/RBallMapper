stdev_in_balls <- function( BM , values )
{
   stdev_balls <- numeric(length(BM$points_covered_by_landmarks))
	 for ( i in 1 : length(BM$points_covered_by_landmarks) )
	 {
	   v <- BM$points_covered_by_landmarks[[i]]
	   values_of_function_in_this_ball <- numeric( length(v) ) 
     for ( j in 1:length(v) )
     {
       values_of_function_in_this_ball[j] <- values[ v[j] ,1]
     }
     stdev_balls[i] <- sd( values_of_function_in_this_ball )
     #print(i)
     #print( values_of_function_in_this_ball )
     #readline(prompt="Press [enter] to continue")
	 }
   return_list <- stdev_balls
}

relative_std_in_balls <- function( BM , values )
{
  relative_std_in_balls <- numeric(length(BM$points_covered_by_landmarks))
  for ( i in 1 : length(BM$points_covered_by_landmarks) )
  {
    v <- BM$points_covered_by_landmarks[[i]]
    values_of_function_in_this_ball <- numeric( length(v) ) 
    for ( j in 1:length(v) )
    {
      values_of_function_in_this_ball[j] <- values[ v[j] ,1]
    }
    relative_std_in_balls[i] <- 
    sd( values_of_function_in_this_ball )/mean( values_of_function_in_this_ball )
  }
  return_list <- relative_std_in_balls
}

# 
