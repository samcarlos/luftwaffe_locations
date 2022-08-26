Code for blog posts [here](https://scweiss.blogspot.com/2022/02/a-high-altitude-overview-of-european.html) and [here](https://scweiss.blogspot.com/2022/07/blog-post.html)

Data:

The Germans in WW2 kept records every 10 days on Luftwaffe strength. Online I found locations and monthly aggregates at ww2.dk. I scraped the data and included them under the `data` folder: `luftwaffe_sizes_losses.csv` and `luftwaffe_locations.csv` (`luftwaffe_sizes_locations.rdata` has data for both in R readable format). 

Geolocating the data proved to be difficult and inexact. In part this is due to the nature of recording the dataset itself. Germans would land in fields and generally describe a village or hamlet and not generally care about the spelling. Moreover the location names also highlight how different cities were named over time by different peoples and underscores that different ethnicities coexisted throughout Europe for hundreds of years. Where possible I made guesstimates and tried to get within the generally area. If a particular location is not found I use the previous known location.

In addition the dates are sometimes available at the monthly level eg a particular squadron landed at a place sometime in May 1940 and not 25 May 1940. This has the effect that some groups appear in a location before or after they should be there. I trust the reader understands the limitations of using this data and can understand how this error will be displayed in the visualization. 

Both csv files share group information and can be joined using the following colums:

    group_type: Type of Aircraft Designation such as -  Jagdgeschwader (Fighters), Nachtjagdgeschwader (Night Fighters), Zerstörergeschwader (Twin Engine Fighters), Schlachtgeschwader / Sturzkampfgeschwader (Ground Attack) and Kampfgeschwader (Bombers)
    squadron_number: Staffel or Squadron Number 
    group_number: Gruppe Number
    subgroup: Roman Numerial Designation which subgruppe they are in
    date: Date (middle of month location)

`luftwaffe_locations.csv` has the following additional columns:

    lat: Latitude
    lon: Longitude
    locations: Text of Location (used to geolocate and find the latitude and longitude) 

`luftwaffe_sizes_losses.csv` has the following additional columns (note that these exists for only the middle of the war 1942-1944):

    total: Total Number aircraft at the beginning of the month
    model: Model of aircraft and subvariant (eg Bf 110G-3) 
    add: Number of total aircraft added in the month
    add_new: Number of new aircraft added in the month
    add_maintenance: Number of aircraft added in the month from maintenance 
    add_other_units: Number of aircraft added in the month from other units
    lost: Number of total aircraft lost in the month
    lost_enemy: Number of total aircraft lost in combat
    lost_accident: Number of total aircraft lost in accidents
    lost_maintenance: Number of total aircraft lost in maintenance
    lost_other_units:Number of total aircraft lost to other units
    total_eom: Total aircaft available at the end of the month

The map boundries can be found [here](https://web.archive.org/web/20210304022330/https://web.stanford.edu/group/spatialhistory/cgi-bin/site/pub.php?id=51) 

Code:

For static image see: create_plots.R

![Alt text](https://github.com/samcarlos/luftwaffe_locations/blob/main/plots/main_plot.png "Optional title")

and for [GIF](https://www.youtube.com/watch?v=wnMIx-DsD6g&t=179s) see: movement_graphs_part_2.R


To create the GIF I cut (arbitarily) the map into 2 latitude by 2 longitude squares and counted the number of aircraft within that square. I then color coded it by different type of aircraft. In the lower left of each square I have fighters (these include single engine, twine engine, and night fighters) and in the upper right of each square I have the number of bombers (either ground attack or general bombers). 

Before February 1942 I estimate the sizes of groups as the data for number of planes are not available before than. Each vertical line heigh shows the number of planes maxing out at 100 before flowing over into subsequent lines. There can be 10 lines each representing a max of 1000 planes.

Starting February 1942 through December 1944 we not only have accurate data on the monthly number of planes at each location but we also see the monthly losses during that time period as well. In order to visualize the movements and losses I based the graphic as if the data came from a turned-based strategy game. Here I visualize two phases; a movement phase followed by a ‘battle phase’ which consists of two additions to the original visualization showed. 1) the hot spots of the war and 2) the planes destroyed fall off into place in this time series plot at the bottom.



