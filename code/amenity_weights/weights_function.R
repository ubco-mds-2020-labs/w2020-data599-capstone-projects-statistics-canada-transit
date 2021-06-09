
  

weights<-function(data,Amenity="museuem"){
  data%>%filter(type==Amenity)->poi_type
  # select relevent features 
  poi_type%>%select(poi_name,open_days,Total_hours,Rating,Total_Review)->poi_type
  # check number of missing data
  poi_type[poi_type == 0] <- NA
  missing_percentage<-colMeans(is.na(poi_type))
  
  # fill NA with column mean 
  poi_type<-na_mean(poi_type)
  
  # normalize the features 
  normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  }
  
  Norm_poi<-poi_type%>%mutate_if(is.numeric, normalize)
  
  # Navie weighted index 
  Norm_poi%>%mutate(Index=(open_days+Total_hours+Rating+Total_Review)/4)->Norm_poi
  Norm_poi%>%select(poi_name,Index)->return_df
  return(return_df)
  
  
}

