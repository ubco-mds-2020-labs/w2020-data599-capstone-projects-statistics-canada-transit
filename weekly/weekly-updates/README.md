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

### Week 5 (May 31 - June 4)
- Worked oon publishing dashboard (Graham)
- Added Kepler to dashboard (Graham)
- Converted dashboard to use HTMLs instead of Leaflet maps (Graham)
- Researched method to calculate transit efficiency scores (Graham)
- Dashboard performance upgrade: added feature to call on pre-rendered HTML maps as opposed to performing computations on the fly. (Luka)
- Developed functions to re-add NA values in the long score and isochrone frames. That way NA blocks can also be visualized (Luka)
- Generated isochrone data from the travel time matrix (Yuxuan)
- Cleaned repository of all unused data/files and moved to archive (Luka)
- Code merge: combined scripts for manipulating the travel time matrix (cleaning, scoring, etc.) and the scripts for visualization and output to have 1 main script for creating the HTML map renders.
- Prepared presentation slides (All)
- Updated the scoring function (Rain, Luka)
- Added the isochrone map on dashboard (Yuxuan)

### Week 4 (May 24 - May 28)
- Created an outline for the final report (All)
- Published dashboard draft (Graham)
- Explored downtown public library outlier point. Found that slight modifications of the coordinates fixes the problem which still has an unknown underlying cause (Luka)
- Created HTML map renders for all score sets using a special score import function (Luka)
- Team meeting and presentation preparation (All)
- Created a general amenity index function which will return the amenity weights for any type of interest(Yuxuan)
- Collected and merged library information - total area, number of annual visitors, volume of physical copies (Rain)

### Week 3 (May 17 - May 21)
- Created visualization of scores in Vancouver using python (Graham)
- Researched R Shiny and Dash for dahsboard visualizations (Graham)
- Created visualization of scores for different DBs in GVA using local Kepler (Rain)
- Tried adding on the capcity of libraries to scores (Rain)
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
- Updated the CDB data wrangling file to output a CSV with standardized column names (Graham)
- Discussed and planned out the high level data manipulation and score computing (All)
- Built web scraper to collect cultural POIT review counts (Yuxuan)
- Set up and filled the Github project log up until May 5 (Luka)
- Software research and exploration. See Wiki links in Taiga.io (Yuxuan + Luka)
- CDB and ODACF data filtering (Graham + Rain)

### Week 1 (May 3 - May 7)
- Setting up Vancouver OSM and GTFS network on Open Trip Planner (Luka)
- Setting up Weekly Task file (Rain)
- Proposal and Statement of Work (All)
- Data Collection (minus DB polygons) (All)
  - General Transit Feed System (GTFS)
  - Vancouver Open Street Maps (Van-OSM)
  - Open Database of Cultural and Art Facilities (ODACF)
  - Census Dissemination Blocks (CDB)
- City Selected: Vancouver (All)
- Setting up our project page on Taiga.io (All)
- First client meeting and project overview.
