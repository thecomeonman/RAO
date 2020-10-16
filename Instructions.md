## Instructions

These are three sets of instructions below:
- Instructions to set up RAO
- Instructions to process a new region
- Instructions to run and use the dashboard

### Instructions to set up RAO

You may use the city RAOExample, and use the default inputs to try this
tool out. Once comfortable, you can change the location you're optimistin, etc.

#### Installing R
Follow the instructions here - https://cran.r-project.org/

#### Installing RStudio
Follow the instructions here - https://www.rstudio.com/products/RStudio/#Desktop

#### Installing libraries that RAO needs
Open Rstudio and run the following command in the console to install some 
libraries that this tool uses to function - 
`install.packages(
   c(
      'shiny',
      'shinydashboard',
      'ggplot2',
      'gridExtra',
      'ggmap',
      'data.table',
      'plyr',
      'bit64',
      'DT',
      'GA',
      'snow',
      'snowfall',
      'rlecuyer',
      'igraph',
      'geosphere'
   )
)`

#### Installing the RAO library:

Lots of functions needed for setting the app up and running it. You have two
options to have this set up -
- Option 1: Open a terminal, navigate to the root directory of the repo, which
   is where you've placed the RAO folder you just downloaded at, ( this 
   location will be referred to as AtherGitRepo for the rest of the 
   readme ) and run this 
   command from there - `./RAO/Build.sh`
- Option 2: You can build it in Rstudio by opening the RAO.Rproj file in 
   Rstudio and clicking on the install and restart option under the build tab 
   in the top right pane. You may need to install the devtools package for this 
   option, which you can do by running the following command in the console -
   `install.packages('devtools')`

#### Running scripts in R

You can open an R script in Rstudio. Pressing ctrl+shift+s executes the script. 

### Instructions to process a new region:

The scripts have been kept separate, which is a little inconvenient to run but easier to track and debug.

#### Setting up the parameters
Look at the `AtherGitRepo`/RAO/DataExtraction/OSM/Parameters.R file and set the parameters accordingly. Description for each parameter listed in the script.

`AtherGitRepo` itself is the path that points to the parent folder for where you've downloaded the RAO repository. You will need to update the value of this variable in each of the scripts below. Just overwrite the declaration in the script with your own path, eg. `cAtherGitRepo = 'C:/Documents/'`.

#### Getting raw OSM data:
Hits an API, gets the data.
- Place the respective city's KML, a file called ServiceArea.kml, in a folder 
   pointed to by the path `AtherDataLocation`/Raw/RAO/`City`/. 
   If you don't have the kml, open 
   `AtherGitRepo`/RAO/DataExtraction/OSM/Parameters.R and override the need for
   a kml file with the `vnBox` variable as instructed in the comments in the 
   script.
- In getXML.R, point `cAtherGitRepo` to the correct path and run.
- The filepath pointed to by `cMapXMLFilePath` should have an XML created which
   looks something like -
   ```
   <?xml version="1.0" encoding="UTF-8"?>
   <osm version="0.6" generator="Overpass API 0.7.55.7 8b86ff77">
   <note>The data included in this document is from www.openstreetmap.org. The data is made available under ODbL.</note>
   <meta osm_base="2019-07-17T05:26:02Z"/>

   <node id="245640356" lat="17.3349295" lon="78.3012810">
      <tag k="source" v="AND"/>
   </node>
   <node id="245640508" lat="17.3961915" lon="78.4261977">
      <tag k="source" v="AND"/>
   </node>
   <node id="245640524" lat="17.3951469" lon="78.4338348">
      <tag k="source" v="AND"/>
   </node>
   <node id="245640535" lat="17.3949513" lon="78.4404526">
      <tag k="source" v="AND"/>
   </node>
   <node id="245640546" lat="17.3821322" lon="78.4933460">
      <tag k="source" v="AND"/>
   </node>
   ```

#### Extracting nodes and ways from raw OSM data:
Processes the data from previous step and splits it into nodes and ways / links.
- In parseXML.R, point `cAtherGitRepo` to the correct path and run.
- You should now have
   nodedetails.txt and ways.txt files under the 
   `AtherDataLocation`/Raw/RAO/`City`/OSMData path.

#### Cleaning OSM data
Removes map data outside serviceable area, isolated ways, etc.
- In cleanOSM.R, point `cAtherGitRepo` to the correct path and run.
- The file `cAtherDataLocation`/Processed/RAO/`cCityName`/OSMServiceable.Rdata should get created

#### Getting map tiles
- In getTiles.R, point `cAtherGitRepo` to the correct path and run.
- The file `cAtherDataLocation`/Processed/RAO/`cCityName`/ggMapTiles.Rdata should get created

#### Get locations data
This data captures information about the locations which will be considered as
the charging stations, warehouses, bus stops, or whatever else it is that you 
are optimising for.

Place the respective city's set of locations, a file called Locations.csv, in
a folder pointed to by the path `AtherDataLocation`/Raw/RAO/`City`/. It
should look like this -

```
"LocationID","LocationName","Longitude_deg","Latitude_deg","Status","Score"
1926,"Jim's Gym",77.134, 12.345,"?",0
1930,"Thor's Store",77.123,12.134,"?",0
1927,"Mala's Mall",77.252,12.123,"?",0
1929,"Cafu's Cafe",77.345,12.252,"?",0
```

LocationID is a unique number assigned to each location. LocationID must not
repeat across different locations.

Status can be one of three values - 'T', 'F', or '?'. T is when the location
is already selected, analogous to already having a charging station there, 
F is when that location has been rejected and shouldn't be considered for
optimisation, and ? is when the location is under consideration and needs
to be considered for the optimisation.

The score value can be fed as 0. It doesn't affect the optimisation.

If you don't have a locations file, you can randomly generate locations across
the map area by editing the input parms in 
`AtherGitRepo`/RAO/DataExtraction/GenerateRandomLocations.R and running it.

#### Initialising Coverage Data
Calculates the distance between particular locations specified in the locations
file, and the road stretches across the serviceable area
- In InitialisingCoverageData.R, point `cAtherGitRepo` to the correct path and run.
- You should have a file at `cAtherDataLocation`/Processed/RAO/`cCityName`/dtPolygonCoveredEdgesServiceable.Rdata

### Instructions to run and use the dashboard:
- Open `AtherGitRepo`/RAO/Dashboard/Global.R in Rstudio and click on the
   Run App button in the editor pane.
- The initialisation with all the data being loaded is quite slow. It is even 
   slower if you load a previous optimisation result. My advice - Be patient.
   Wait for the status pop ups at the bottom to go. The table with the 
   locations, etc. also takes time so don't get frustrated. If you have ideas
   on how to make it faster, please feel free to submit changes.

In the App Config tab:
- Select a city from the drop down, and click load to
   load the coverage data, tiles, etc. for it. This will take a while and you
   will be notified from the pop up in the bottom right.
- Upload the Locations.csv file you used to initialise 
   the coverage data in the upload scenario box and click on the Process button. This will take a while and you
   will be notified from the pop up in the bottom right.
- After you've uploaded a scenario and loaded data from
   the city, if the app detects any new locations for which the coverage hasn't 
   already been calculated, they will show up in the new locations detected box.
   You can choose to process them from the tool. This will back up the previous 
   coverage dataset and create a new dataset with these new locations processed with the coverage data logic you ran previously.
   You can choose to restore these back ups to be the one used by the app by 
   changing the name of the files in `AtherDataLocation`/Raw/RAO/`City`/
- Upload previous result lets you upload the results of a previous optimisation
   that you would have saved from the download result button in the Analysis 
   tab.

In the Analysis tab:
- The optimisation block in the analysis tab has many configurations for
   the optimisation you want to do.
   - Optimisation type:
      - Minimise stations: You can specify the proportion of overall area you
         want covered in the minimum coverage expected box, and the 
         optimisation will try to find the lowest number of stations that it
         can cover that amount of area with.
      - Maximise coverage: You can set the maximum number of stations you are
         willing to put in the maximum stations possible box, and the 
         optimisation will try to find a set of stations that cover the largest
         area.
   - Coverage radius: The distance you're considering as an accessible distance
      from a station. Any point within this distance from a location will be 
      considered to be covered by that station.
   - Maximum iterations and population are parameters that tune the workings
      of the optimisation algorithm itself. Setting both to a very high value
      will make the run time very long but will get you a great solution.
      Setting both to a very low value will lead to short run times but the
      solution might be sub-optimal. If you aren't sure or are using it for the
      first time, we recommend setting the population to a moderate number, 
      like 20, setting the iterations to a low number like 2, and then running.
      The tool allows you to resume from the previous optimisation so you can 
      keep running 2 iterations at a time and monitor progress.
   - The Optimise! button initiates the optimisation.
   - After running an optimisation, a checkbox appears to let you resume from
      latest run. You must check this if you're running a small number of 
      iterations and monitoring progress. This helps the optimisation start 
      from a partially optimised situation rather than completely from scratch.
      You can change the coverage radius, minimum coverage expected, etc. 
      before resuming if you like.
   - After completing a round of optimsation which resumed from the previous
      optimisation result, if you'd like to go back to the earlier optimistaion
      result then you have the option to do so from the Load prev result button.
      This will discard the last round of optimistaion which you just performed
      and return the app to the earlier result.
   - The results from the optimisation are displayed in the text and the chart
      below, eg. "Results of latest optimisation / calculation: Objective value 
      of 0.9618 with 414 stations." with a line chart showing how the number
      of stations reduced or coverage increased with each iteration. You can
      stop running incremental optimisations after this line has plateaued.

- The Results block:
   - Lets you plot a variety of charts to help you understand the input data, 
      the output of the optimisation, etc. Adjust the plot width slider to
      change the maximum width the plot is allowed to occupy. This number
      changes from display to display. You can zoom in, hover on stations to
      identify them, etc.
   - The table prints the details of the locations with some controls to let
      you override the results of the optimisation, or the information you
      uploaded. Once overriden, you should save changes, and then optimise or 
      recalculate to see the new results.
      - Status is T, F, or ? as per the locations data you uploaded and
         respectively indicates that a station has been selected, has been
         rejected, or it is undecided. You can change this entry for a location
         and it will be treated accordingly in an optimisation. This doesn't
         affect recalculation.
      - Chosen is the output of an optimisation and is either T or F to 
         indicate if it has been included in the solution or not. You can 
         override this and recalculate the coverage details. This doesn't 
         affect an optimisation and gets overriden with every optimisation.
   - Some of the columns are updated on optimisation or recalculation:
      - Chosen is updated with every optimisation.
      - C/02, C/04, and C/06 are the amount of road length covered within 
         2, 4, and 6 kms for the station.
      - C is road length covered as per the coverage radius set in the last
         optimisation or recalculation.
      - C/1 is the amount of road length covered only by this station which
         isn't covered by any other station amongst the chosen stations as per
         the last optimistaion or recalculation.
   - Download CSV lets you download the table in the same format as the 
      Locations.csv file you've uploaded. stats chosen??????? 
   - Download Result lets you download an object which contains the optimisation
      results in a format that can be uploaded back into the tool from the 
      Upload Previous Result box in the App Config tab. This file isn't human
      readable.
   - Coverage radius for Recalc lets you adjust the distance within which a 
      point is considered accessible from a location. On recalculation, you
      will see the updated coverage numbers.
   - Save changes allows you to save the overrides to the status or the chosen
      column. You need to save changes for your overrides to reflect in the
      results from the recalculation.
   - Recalculate - Is not optimisation! If you have edited the chosen column or
      the coverage radius for recalc slider and want to see what the coverage
      would be with those settings then use this. The status column entries
      don't affect this.

In the Map Corrections tab:

This is a crude screen for people to check if there are errors in the
OSM data. You can draw routes between various nodes to check if one ways, 
new roads, etc. are updated in the data or not. These errors can be rectified
by revisiting the cleaning OSM data step, editing the vcMapEdits variable
to incorporate your changes, and finishing the whole process from there.
This step doesn't need coverage data calculated for each location.
- Clicking on a node shows the ID of that node. Putting the nodes you're 
   interested in in the from and to boxes, and clicking on the draw route 
   button shows the route. You can zoom in to better select a point, better see
   the route, etc.
   
### To do

- Some elements are really sluggish, esp. initialisation and the datatable. 
- Automatically size plot elements based on display size.
- The dashboard works fine for happy flows but there may be other flows that
   break it.