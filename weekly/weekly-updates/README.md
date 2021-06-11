# All Project Logs by Week

The team will document (summarize) their overall project progress on a weekly basis, linking it to key project deliverables. 

Place your weekly status updates in this folder before your weekly meeting.

***

**Structure:** 

- We can use as single project log file where all updates are logged. 
- Updates are name tagged which can be aggregated to create indivudual logs.
- This way we can see all the work being done in 1 file, not 4.

***

### Week 8 (June 21 - June 25)

### Week 7 (June 14 - June 18)

### Week 6 (June 7 - June 11)
- Developed effiency model and maps (Graham) - 10 hours
- Wrote method-dashboard section of the report (Graham) - 8 hours
- Report formatting on Overleaf using latex (Yuxuan) - 1 hour
- Wrote amenity methodology part (Yuxuan) - 5 hour
- Organized code (Yuxuan) - 3 hours

### Week 5 (May 31 - June 4)
- Worked on publishing dashboard (Graham) - 8 hours
- Added Kepler to dashboard (Graham) - 1 hour
- Converted dashboard to use HTMLs instead of Leaflet maps (Graham) - 15 hours
- Researched method to calculate transit efficiency scores (Graham) - 4 hours
- Dashboard performance upgrade: added feature to call on pre-rendered HTML maps as opposed to performing computations on the fly. (Luka)
- Developed functions to re-add NA values in the long score and isochrone frames. That way NA blocks can also be visualized (Luka)
- Created time windows for weekdays and weekends as well as hourly change of average travel time for the nearest amenity (Rain)
- Merged geometry data with DB data with time windows and developed Kepler.gl maps filtering by type of amensities (Rain)
- Generated isochrone data from the travel time matrix (Yuxuan)
- Cleaned repository of all unused data/files and moved to archive (Luka)
- Code merge: combined scripts for manipulating the travel time matrix (cleaning, scoring, etc.) and the scripts for visualization and output to have 1 main script for creating the HTML map renders. (Luka, Yuxuan)
- Prepared presentation slides (All)
- Updated the scoring function (Rain, Luka)
- Added the isochrone map on dashboard (Yuxuan)

### Week 4 (May 24 - May 28)
- Created an outline for the final report (All)
- Published dashboard draft (Graham) - 25 hours
- Explored downtown public library outlier point. Found that slight modifications of the coordinates fixes the problem which still has an unknown underlying cause (Luka)
- Created HTML map renders for all score sets using a special score import function (Luka)
- Team meeting and presentation preparation (All) - 3 hours
- Created a general amenity index function which will return the amenity weights for any type of interest (Yuxuan)
- Collected and merged library information - total area, number of annual visitors, volume of physical copies (Rain)

### Week 3 (May 17 - May 21)
- Created visualization of scores in Vancouver using python (Graham) - 6 hours
- Researched R Shiny and Dash for dahsboard visualizations (Graham) - 3 hours
- Created visualization of scores for different DBs in GVA using local Kepler (Rain)
- Tried adding on the capcity of libraries to scores (Rain)
- Research on getting gallery and museum objective weights for mass of amenities (Rain)
- Used scripting in gitbash (via jq library) to extract 36 megabytes of Vancouver polygon data from 1.6 gigabytes of Canada wide geoJson polygon data (Luka)
- Fixed small errors in score set exports and finished interactive map score visualization in leaflet (Luka)
- Started ggplot visualizations in ggplot, tmap, and leaflet libraries (Luka)
- Wrote custom range normalization functions, scoring functions, improved score script comments, and created a accessibility score set folder (Luka)
- Tested Facebook and Twitter API for review data (Yuxuan)
- Generated opening days and hours data for point of interests from google place (Yuxuan)
- Built POI (Museum) Index weight using the google place data (Yuxuan)

### Week 2 (May 10 - May 14)
- Removed duplicate travel time matrix csvs (Luka)
- Wrote an initial DB score computation script and ran it using 3k of 15k origins at 36 departure times (generated a 36 billion row dataframe) (Luka)
- Computed travel time matrix and created visualization using UrbanAccess with Pandana (Rain)
- Updated the CDB data wrangling file to output a CSV with standardized column names (Graham) - 2 hours
- Discussed and planned out the high level data manipulation and score computing (All) - 6 hours
- Built web scraper to collect cultural POIT review counts (Yuxuan)
- Set up and filled the Github project log up until May 5 (Luka)
- Software research and exploration. See Wiki links in Taiga.io (Yuxuan + Luka)
- CDB and ODACF data filtering (Graham + Rain) - 3 hours
- Develop time matrix using R5 (All) - 8 hours
- Discussed method to develop score computations (All) - 8 hours


### Week 1 (May 3 - May 7)
- Setting up Vancouver OSM and GTFS network on Open Trip Planner (Luka)
- Setting up Weekly Task file (Rain)
- Proposal and Statement of Work (All) - 6 hours
- Data Collection (minus DB polygons) (All) - 8 hours
- General Transit Feed System (GTFS) - 10 hours
  - Vancouver Open Street Maps (Van-OSM)
  - Open Database of Cultural and Art Facilities (ODACF)
  - Census Dissemination Blocks (CDB) 
- City Selected: Vancouver (All) - 1 hour
- Setting up our project page on Taiga.io (All) 2 hours
- First client meeting and project overview - 1 hour
