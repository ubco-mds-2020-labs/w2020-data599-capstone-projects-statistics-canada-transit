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
- Code documentation and Readmes (All) - 5 hours
- Changed efficiency model and maps (Graham) - 8 hours
- Report writing (All) - 20 hours each
- Presentation (All) - 15 hours
- Fixed dashboard layout/display (Graham) - 2 hours
- Deployed a cut down version of the dashboard via heroku (Luka) - 7 hours
- Worked on deploying the full dashboard on heroku (Luka) - 12 hours
- Added draggable titles and page descriptions to the dashboard (All) - 4 hours
- Updated on k-mean clustering analysis (Rain) - 2 hours
- Rearranged the data sets for Kepler.gl time window maps (Rain) - 8 hours
- Dashboard formatting (Yuxuan)- 5 hours

### Week 7 (June 12 - June 19)
- Worked on report writing overall and subdivision analysis (Luka) - 4 hours
- Created the subdivision accessibility measure summary csv (Luka) - 15 minutes
- Created ggplot2 violin plot visualizations (Luka + Graham) - 2 hours
- Deployed preliminary dashboard on heroku (Luka) - 1 hour
- Fixed small bugs after recomputing the travel time matrix and scores (Luka) - 5 hours
- Worked on reorganizing some aesthetics of the dashboard about and data explorer page (Luka) - 4 hours
- Re-rendering html maps for leaflet after each new update (Luka) - 6 hours
- Analyzed the accessibility measure distributions (Luka) - 3 hours
- Updated maps popup and created figures folder for report figures (Luka) - 2 hours
- Directory reorganizing for documentation (Luka) - 4 hours
- Code merging of all files with similar code (Luka) - 10 hours
- Code review to complete the efficiency model and visualizations (Graham + Luka) - 4 hours
- Build data explore tab (Yuxuan) - 8 hours
- Documenting weight data function(Yuxuan) - 3 hours 
- Obtain bus frequency data and data cleaning (Yuxuan + Rain)- 5 hours
- PCA analysis on newly merged transit dataset(Yuxuan) - 8 hours
- Build unsupervised learning tab (Yuxua)- 3 hours
- Build About the Project tab and formatting(Yuxuan) - 5 hours
- Report writing on Isochrone maps/score maps(+ graph making) (Yuxuan)-8 hours
- Correlation analysis(Yuxuan) - 5 hours
- Conducted K-means clustering analysis on variables (Rain) - 3 hours
- Stored the large-scale data on cloud storage to reduce the data load for Kpeler.gl (Rain) - 10 hours
- Re-arranged data sets for generating Kepler.gl maps and compressed data size (Rain) - 10 hours
- Data visualization for transit times along with the time of the day, day of the week (Rain) - 3 hours
- Weekly presentations (All) - 3 hours
- Changed efficiency model and maps (Graham) - 15 hours
- Report writing/presentation (Graham & Rain) - 15 hours


### Week 6 (June 7 - June 11)
- Report writing (All) - 20 hours each
- Developed first iteration of the efficiency model and maps (Graham) 16 hours
- Experimented efficiency models via the difference in population quantiles / score quantiles (Luka) - 3 hours
- Added geometry for locations of amenities on the Kepler.gl maps (Rain) - 6 hours
- Wrote background, methodology for UrbanAccess with Pandana, and Kepler.gl (Rain) - 10 hours
- Wrote method-dashboard section of the report (Graham) - 8 hours
- Wrote introduction, and methods (data, computation, scoring) (Luka) - 10 hours
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
