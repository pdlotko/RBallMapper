#include <Rcpp.h>
#include <vector>
#include <iostream>
#include <cmath>
using namespace Rcpp;
using namespace std;

/*
library(Rcpp)
sourceCpp('BallMapperClassifier.cpp')

training <- as.data.frame( read.csv('../circle') )
values = pts[,1]

test <- as.data.frame( read.csv('../perturbed_circle') )


v <- BMClassify( training,test,values,0.3 )


*/


inline double compute_distance_standard_points
  ( const std::vector< double >& pt1 , const std::vector< double >& pt2 , double p = 2 )
{
  double result = 0;
  for ( size_t i = 0 ; i != pt1.size() ; ++i )
  {
     result += pow( ( pt1[i]-pt2[i] ) , p );
  }
  return pow(result,1/p);
}//compute_distance_standard_points
//
//
//
//
//
//
//
//
//
//
//
std::vector< std::vector<double> > transpose_points_from_R( const DataFrame& points_df )
{
    std::vector< NumericVector > points_transposed( points_df.size() );
    for ( int i = 0 ; i != points_df.size() ; ++i )
    {
        points_transposed[i] = points_df[i];
    }

    std::vector< std::vector<double> > points( points_transposed[0].size() );

    size_t dim_of_points = points_transposed.size();
    for ( int i = 0 ; i != points_transposed[0].size() ; ++i )
    {
        std::vector< double > pt( dim_of_points );
        for ( size_t j = 0 ; j != dim_of_points ; ++j )
        {
         pt[ j ] = points_transposed[j][i];
        }
        points[i] = pt;
    }
    return points;
}


// [[Rcpp::export]]
std::vector< std::vector< int > > ballsInPoints( const DataFrame& points_df , double epsilon  )
{
  bool dbg = false;
  if ( points_df.size() == 0 )
  {
    Rcerr << "No points in the BallMapperCpp procedure, the program will now terminate";
    throw "No points in the BallMapperCpp procedure, the program will now terminate";
  }

  //the points we obtain from R are unnaturally transposed, so we transpose them back to the situation when we
  //have one point per row.
  //First we need to store them as vector of NumericVectorS:
  std::vector< std::vector<double> > points = transpose_points_from_R( points_df );

  int number_of_points = points.size();
  if (dbg) Rcerr << "Number of points : " << number_of_points << endl;

  std::vector< std::vector< int > > points_in_epsi_neigh( points.size() );

  for ( size_t i = 0 ; i != points.size() ; ++i )
  {
      if ( dbg )
      {
          Rcerr << std::endl << "Consider point : " << i << std::endl;
      }
      for ( size_t j = 0 ; j != points.size() ; ++j )
      {
          if ( compute_distance_standard_points( points[i],points[j] ) <= epsilon )
          {
             points_in_epsi_neigh[i].push_back( j );
             if ( dbg )
             {
                 //Rcerr << "Point : " << j << "is a neighbour. ";
             }
          }
       }
       if ( dbg )
       {
           Rcerr << "Number of neigbors : " << points_in_epsi_neigh[i].size() << std::endl;
       }
  }
  return points_in_epsi_neigh;
}

// [[Rcpp::export]]
std::vector<double> BMClassify( const DataFrame& points_training_set_df , const DataFrame& points_test_set_df , const DataFrame& values_df , double epsilon_train , double epsilon_test )
{
    std::vector< std::vector< int > > balls = ballsInPoints( points_training_set_df , epsilon_train  );
    std::vector< std::vector<double> > points_training = transpose_points_from_R( points_training_set_df );
    std::vector< std::vector<double> > points_test = transpose_points_from_R( points_test_set_df );
    NumericVector training_values = values_df[0];

    //Rcerr << "training_values.size() : " << training_values.size() << std::endl; //150
    //Rcerr << "balls.size() : " << balls.size() << endl;

    //Now, for every training point compute the value:
    std::vector< double > values_of_points( balls.size() );
    for ( size_t i = 0 ; i != balls.size() ; ++i )
    {
        double av = 0;
        for ( size_t j = 0 ; j != balls[i].size() ; ++j )
        {
            av += training_values[ balls[i][j] ];
        }
        av /= balls[i].size();
        values_of_points[i] = av;
    }

    //now, for every test point:
    std::vector< double > classification_result( points_test.size() );
    for ( size_t i = 0 ; i != points_test.size() ; ++i )
    {
        //find all the points in points_training no further away than epsilon_test:
        double av = 0;

        double counter = 0;
        for ( size_t j = 0 ; j != points_training.size() ; ++j )
        {
            double d = compute_distance_standard_points( points_test[i] , points_training[j] );
            if ( d <= epsilon_test )
            {
                //without distance weighting
                counter++;//change
                av += values_of_points[ j ];//weighting by the distance??

                //with distance weighting
                //counter += 1/(d);
                //av += values_of_points[ j ]/(d);
            }
        }
        if ( counter )
        {
            //Rcout << "av vs. counter " <<  av << " " << counter << endl;
            classification_result[i] = av/counter;
        }
        else
        {
            //Rcerr << "Point not found !!! \n";
            classification_result[i] = std::numeric_limits<double>::infinity();
        }
    }

    return classification_result;
}//BMClassify










// [[Rcpp::export]]
std::vector<double> BMClassify_with_distance_weighting
( const DataFrame& points_training_set_df , const DataFrame& points_test_set_df , const DataFrame& values_df , double epsilon_train , double epsilon_test )
{
    std::vector< std::vector< int > > balls = ballsInPoints( points_training_set_df , epsilon_train  );
    std::vector< std::vector<double> > points_training = transpose_points_from_R( points_training_set_df );
    std::vector< std::vector<double> > points_test = transpose_points_from_R( points_test_set_df );
    NumericVector training_values = values_df[0];

    //Rcerr << "training_values.size() : " << training_values.size() << std::endl; //150
    //Rcerr << "balls.size() : " << balls.size() << endl;

    //Now, for every training point compute the value:
    std::vector< double > values_of_points( balls.size() );
    for ( size_t i = 0 ; i != balls.size() ; ++i )
    {
        double av = 0;
        for ( size_t j = 0 ; j != balls[i].size() ; ++j )
        {
            av += training_values[ balls[i][j] ];
        }
        av /= balls[i].size();
        values_of_points[i] = av;
    }

    //now, for every test point:
    std::vector< double > classification_result( points_test.size() );
    for ( size_t i = 0 ; i != points_test.size() ; ++i )
    {
        //find all the points in points_training no further away than epsilon_test:
        double av = 0;

        double counter = 0;
        for ( size_t j = 0 ; j != points_training.size() ; ++j )
        {
            double d = compute_distance_standard_points( points_test[i] , points_training[j] );
            if ( d <= epsilon_test )
            {
                //without distance weighting
                //counter++;//change
                //av += values_of_points[ j ];//weighting by the distance??

                //with distance weighting
                counter += 1/(d+1);
                av += values_of_points[ j ]/(d+1);
            }
        }
        if ( counter )
        {
            //Rcout << "av vs. counter " <<  av << " " << counter << endl;
            classification_result[i] = av/counter;
        }
        else
        {
            //Rcerr << "Point not found !!! \n";
            classification_result[i] = std::numeric_limits<double>::infinity();
        }
    }

    return classification_result;
}//BMClassify

