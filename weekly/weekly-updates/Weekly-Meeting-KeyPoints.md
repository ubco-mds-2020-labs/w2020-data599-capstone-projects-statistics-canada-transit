# Weekly Meeting Content

This page provides the key points of each weekly meeting
* [May 4, 2021 Meeting](#May-4-2021–Meeting)
* [May 6, 2021  Draft Proposal Meeting](#May-6-2021-Draft-proposal-meeting)
* [May 10, 2021 - Proposal Follow-up Meeting](#May-10-2021-Proposal-follow-up-meeting)
#### May 4, 2021 – Meeting

**Dashboard**
- Linked list or graphs dataframe
- Bus stops are nodes and busses are the vectors
- Include all facilities connected to a transit network within 20 min walk from the network or that require no more than 3 transfers.

**Calculating the rank**

-Should the rank of a community be weighted based on the number of amenities within an allotted time in transit.
-Calculate score: SUM[(time)*LocationWeight]/N * NumberOfLocationsWeight

 

**Calculate time between two points**
- Walking time: distance from center of block and average walking speed
- Transit time: time between departure and arrival of bus between two stops
- Total time = time, time in transit, walk/wait between bus, time to walk to amenity

- Calculate average of the time it takes to get to each amenity.
- Suggests putting weight on the average time based on popularity/revenue/etc

 

**Calculating walking time**
- Walking time to/from bus stop based on lat and long of the centroid in each block and bus stop OR use existing "access to transit" as bias.

 

**Outcome:**
- Look over data and read paper for Wednesday
- Meet Wednesday to go over the project
- Identify what data we need to complete the project
- Create a short one page draft proposal


#### May 6, 2021 - Draft proposal meeting

**Questions for Joseph:**

##### How will we account for distances between the centroid of dissemination blocks (DBs) and bus stops/transit access?
Assume starting position is the center of the dissemination block Software should account for this Including walking time

##### Since the travel time also depends on the bus schedules, are we going to ignore the waiting time? 
- Starting from small then scale-up gradually
- Take the average over a period of time
- Possibly just do it for a few neighborhood
- Take the average over a period of time
##### And if we ignore the waiting time for the initial bus, do we account for waiting time at intermediate stops in case there’s a transfer?
##### When transferring is required, what will be the buffer (the maximum number of transfers) before reaching the destination?
- <= 3 transfers
##### Do we need to consider the size (or the popularity) of art facilities? If so any recommendations for the measure of mass (Revenue, capacity..)
- Depends on the amenity
- Suggest to try the nearest first 

##### When considering weights for multiple art facilities, do we apply these weights to the time it takes to get to each point of interest? (ie. low accessibility to popular facilities gets weighed more than high accessibility to unpopular facilities)
##### The goal 
- Able to scale-up, the report will clear explanations 
- Spark- graph 

#### May 10, 2021 - Proposal follow-up meeting

**Possible ways to reduce computational complexity:**
- Justifying the size of dissemination block base on the population
- Picking representation point-- higher geographic representation 

**Joseph link (may be useful for recreating the DBs):**

* [Census Subdivision Boundary Files](https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-eng.cfm)
* [2016 Census Boundary Files](https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2016-eng.cfm)

**Suggestions on measuring the weights on the point of interest.**
- Using API to generate the rate or view from google place, yelp, trip advisor 
- In the example of measuring the popularity of a library, we could use revenue/number of books/number of library membership cards as a proxy of the popularity. 

**Main focus is to aggregate the data,do the analysis and make a cool dashboard.**
