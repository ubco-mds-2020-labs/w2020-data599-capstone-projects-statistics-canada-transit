# All Project Logs by Week

The team will document (summarize) their overall project progress on a weekly basis, linking it to key project deliverables. 

Place your weekly status updates in this folder before your weekly meeting.

***

**Structure:** 

- We can use as single project log file where all updates are logged. 
- Updates are name tagged which can be aggregated to create indivudual logs.
- This way we can see all the work being done in 1 file, not 4.

***
### Week 8 (June 20 - June 22)
- Report writing/presentation/rehearsal (Graham) - 12+ hours
- Adhoc dashboard task (Graham) - 8 hours
- Add data explor tab (Yuxuan) -4 hours
- Add Clustering tabs (Yuxuan) -3 hours
### Week 7 (June 12 - June 19)
- Changed effiency model and maps (Graham) - 8 hours
- Report writing/presentation (Graham) - 12 + hours
- Report writing/presentation (Yuxuan) - 12 + hours
- PCA analysis (Yuxuan) - 10 hour

### Week 6 (June 7 - June 11)
- Developed effiency model and maps (Graham) - 16 hours
- Experimented efficiency models via the difference in population quantiles / score quantiles (Luka) - 3 hours
- Added geometry for locations of amenities on the Kepler.gl maps (Rain) - 6 hours
- Wrote background, methodology for UrbanAccess with Pandana, and Kepler.gl (Rain) - 8 hours
- Wrote method-dashboard section of the report (Graham) - 8 hours
- Wrote introduction, and methods (data, computation, scoring) - 10 hours
- Report formatting on Overleaf using latex (Yuxuan) - 1.5 hours
- Wrote amenity methodology part (Yuxuan) - 12 hours
- Recomputed travel time matrix, scores, and fresh HTML maps using the merged code with the most recent scoring function (check to see if all code is still functional) (All) - 6 hours
- Organized code (Yuxuan) - 3 hours

### Week 5 (May 31 - June 4)
- Worked on publishing dashboard (Graham) - 8 hours
- Added Kepler to dashboard (Graham) - 1 hour
- Researched methods to calculate transit efficiency scores (Graham + Luka) - 8 hours
-  Converted dashboard to use HTMLs instead of Leaflet maps  // Dashboard performance upgrade: added feature to call on pre-rendered HTML maps as opposed to performing computations on the fly (Luka + Graham) - 20 hours
- Developed functions to re-add NA values in the long score and isochrone frames. That way NA blocks can also be visualized (Luka) - 6 hours
- Created time windows for weekdays and weekends as well as hourly change of average travel time for the nearest amenity (Rain) - 5 hours
- Merged geometry data with DB data with time windows and developed Kepler.gl maps filtering by type of amensities (Rain) - 5 hours
- Generated isochrone data from the travel time matrix (Yuxuan) - 8 hours
- Cleaned repository of all unused data/files and moved to archive (Luka) - 2 hours
- Code merge: combined scripts for manipulating the travel time matrix (cleaning, scoring, etc.) and the scripts for visualization and output to have 1 main script for creating the HTML map renders. (Luka, Yuxuan) - 15 hours
- Prepared presentation slides (All) - 3 hours
- Updated the scoring function (Rain, Luka) - 6 hour
- Added the isochrone map on dashboard (Yuxuan) - 5 hours

### Week 4 (May 24 - May 28)
- Created an outline for the final report (All)
- Published dashboard draft (Graham) - 25 hours
- Explored downtown public library outlier point. Found that slight modifications of the coordinates fixes the problem which still has an unknown underlying cause (Luka) - 2 huors
- Created HTML map renders for all score sets using a special score import function (Luka) - 2 hours
- Team meeting and presentation preparation (All) - 3 hours
- Created a general amenity index function which will return the amenity weights for any type of interest (Yuxuan) -28 hours
- Collected and merged library information - total area, number of annual visitors, volume of physical copies (Rain) - 8 hours

### Week 3 (May 17 - May 21)
- Created visualization of scores in Vancouver using python (Graham) - 6 hours
- Researched R Shiny and Dash for dahsboard visualizations (Graham) - 3 hours
- Created visualization of scores for different DBs in GVA using local Kepler (Rain) - 16 hours
- Tried adding on the capcity of libraries to scores (Rain) - 8 hours
- Research on getting gallery and museum objective weights for mass of amenities (Rain) - 10 hours
- Used scripting in gitbash (via jq library) to extract 36 megabytes of Vancouver polygon data from 1.6 gigabytes of Canada wide geoJson polygon data (Luka) - 3 hours
- Fixed small errors in score set exports and finished interactive map score visualization in leaflet (Luka) - 2 hours
- Started ggplot visualizations in ggplot, tmap, and leaflet libraries (Luka) - 3 hours
- Wrote custom range normalization functions, scoring functions, improved score script comments, and created a accessibility score set folder (Luka) - 18 huors
- Tested Facebook and Twitter API for review data (Yuxuan) - 15 hours
- Generated opening days and hours data for point of interests from google place (Yuxuan) - 8 hours
- Built POI (Museum) Index weight using the google place data (Yuxuan) - 25 hours

### Week 2 (May 10 - May 14)
- Removed duplicate travel time matrix csvs (Luka) - 10 minutes
- Wrote an initial DB score computation script and ran it using 3k of 15k origins at 36 departure times (generated a 36 billion row dataframe) (Luka) - 10 hours
- Computed travel time matrix and created visualization using UrbanAccess with Pandana (Rain) - 16 hours
- Updated the CDB data wrangling file to output a CSV with standardized column names (Graham) - 2 hours
- Discussed and planned out the high level data manipulation and score computing (All) - 6 hours
- Built web scraper to collect cultural POIT review counts (Yuxuan) - 8 hours
- Set up and filled the Github project log up until May 5 (Luka) - 10 minutes
- Software research and exploration. See Wiki links in Taiga.io (Yuxuan + Luka) - 15 hours
- CDB and ODACF data filtering (Graham + Rain) - 8 hours
- Develop time matrix using R5 (All) - 8 hours
- Discussed method to develop score computations (All) - 8 hours


### Week 1 (May 3 - May 7)
- Setting up Vancouver OSM and GTFS network on Open Trip Planner (Luka) - 5 hours (mostly software research)
- Setting up Weekly Task file (Rain) - 1 hour
- Proposal and Statement of Work (All) - 6 hours
- Data Collection (minus DB polygons) (All) - 8 hours
- General Transit Feed System (GTFS) - 10 hours
  - Vancouver Open Street Maps (Van-OSM)
  - Open Database of Cultural and Art Facilities (ODACF)
  - Census Dissemination Blocks (CDB) 
- City Selected: Vancouver (All) - 1 hour
- Setting up our project page on Taiga.io (All) 2 hours
- First client meeting and project overview - 1 hour
