import requests
import json
import pandas as pd
class GooglePlaces(object):
    def __init__(self,apiKey):
        super(GooglePlaces).__init__()
        self.apiKey=apiKey
        
    def search_place_by_coordinate(self,location):
        endpoint_url= "https://maps.googleapis.com/maps/api/place/nearbysearch/json"
        places = []
        params = {
            'location':location,
            'key':self.apiKey
            
        }
        
        res=requests.get(endpoint_url,params=params)
        results = json.load(res.content)
        return results
    

  
api=GooglePlaces("AIzaSyA_O9wNiYYALLuROYMDBUkGTuyr0DlEzU0")
places= api.search_place_by_coordinate("49.31386319,-123.076908")
print(places)