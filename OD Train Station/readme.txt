
Train station data: https://data.gov.uk/dataset/naptan

MSOAs etc - http://www.ons.gov.uk/ons/guide-method/geography/products/census/spatial/centroids/index.html

Travel to work mode split - https://www.nomisweb.co.uk/census/2011/qs701ew - Method of travel to work, Table population, All usual residents aged 16 to 74

Output data in GeoJSON: MSOA_Nearest_TrainStation_Polygon.geojson




R implimentation:


pkgs <- c("stplanr", "rgdal", "leaflet", "geojsonio")
install.packages(pkgs)
lapply(pkgs, require, character.only = TRUE)

#Download NaPTAN Data
    download.file("http://www.dft.gov.uk/NaPTAN/snapshot/NaPTANcsv.zip", ""NaPTAN/NaPTANcsv.zip","auto")
    unzip("NaPTAN/NaPTANcsv.zip",files="RailReferences.csv", exdir="NaPTAN")

#Download population weighted centroids (assumes OA,MSOA and LSOA directories exist)
# Potential bug found in download.file if a 302 response is received (example http://www.ons.gov.uk/ons/external-links/social-media/g-m/2011-oa-population-weighted-centroids.html vs https://geoportal.statistics.gov.uk/Docs/Boundaries/Output_areas_(E+W)_2011_Population_Weighted_Centroids_V2.zip)

#OA
    download.file("https://geoportal.statistics.gov.uk/Docs/Boundaries/Output_areas_(E+W)_2011_Population_Weighted_Centroids_V2.zip","OA/Output_areas_(E+W)_2011_Population_Weighted_Centroids_V2.zip","auto")
    unzip("OA/Output_areas_(E+W)_2011_Population_Weighted_Centroids_V2.zip",exdir="OA")

#MSOA
    download.file("https://geoportal.statistics.gov.uk/Docs/Boundaries/Middle_layer_super_output_areas_(E+W)_2011_Population_Weighted_Centroids_V2.zip","MSOA/Middle_layer_super_output_areas_(E+W)_2011_Population_Weighted_Centroids_V2.zip","auto")
    unzip("MSOA/Middle_layer_super_output_areas_(E+W)_2011_Population_Weighted_Centroids_V2.zip",exdir="MSOA")

#LSOA
    download.file("https://geoportal.statistics.gov.uk/Docs/Boundaries/Lower_layer_super_output_areas_(E+W)_2011_Population_Weighted_Centroids_V2.zip","LSOA/Lower_layer_super_output_areas_(E+W)_2011_Population_Weighted_Centroids_V2.zip","auto")
    unzip("LSOA/Lower_layer_super_output_areas_(E+W)_2011_Population_Weighted_Centroids_V2.zip",exdir="LSOA")

#Read in Sheffield Boundary shapefile
#Sheffield_Boundary <- readOGR("C:/Users/mattt_000/Documents/Sheffield Strategic Cycle Network - GIS/Boundary Line", "Sheffield Boundary")

#Read in the location of each MSOA/OA/LSOA etc.
    OA <- readOGR("OA", "OA_2011_EW_PWC")
    MSOA <- readOGR("MSOA", "MSOA_2011_EW_PWC")
    LSOA <- readOGR("LSOA", "LSOA_2011_EW_PWC")

#A bit of a hack, the code uses MSOA everywhere, change the second parameter to use different geography
    MSOA <- OA

#Read in the NaPTAN Public transport railway station data
    NaPTAN_Rail = read.csv("NaPTAN/RailReferences.csv")
    coordinates(NaPTAN_Rail)=~Easting+Northing
    proj4string(NaPTAN_Rail) <- proj4string(MSOA) #Set projection of NaPTAN_Rail to same as MSOA (they *should* already be the same but it complained when I used generic)

#plot some data
#plot(NaPTAN_Rail)
#points(MSOA, col='green')
plot(MSOA[c(1,100,500),])
points(NaPTAN_Rail[1700,],col='red')

#Calculate a distance matrix of every MSOA to every NaPTAN Station
    install.packages("SpatialTools") #needed for the dist2 distance calculation
    require(SpatialTools)

    dist_matrix <- dist2(MSOA@coords,NaPTAN_Rail@coords) #calculate euclidean distance matrix - should be fast - rows are MSOA, cols are NaPTAN_Rail
    #each row is an MSOA, each column is a train station, each value is the distance
    #7201 rows (MSOAs), 2619 columns (stations)

dist_matrix[1,] # distance to each 2619 stations for MSOA #1
min(dist_matrix[1,]) # value of the minimum distance for MSOA#1 to station
which(dist_matrix[1,] == min(dist_matrix[1,])) # which station is the closest to MSOA #1, (it's #250)

    closest <- apply(dist_matrix, MARGIN=1, function(x) which(x == min(x))) #returns 7201 MSOAs and the nearest NaPTAN station in vector
    dist <- apply(dist_matrix, MARGIN=1, function(x) min(x)) #distance from each MSOA to the nearest NaPTAN station. I think it is in meters

closest[1] # EG MSOA 1, nearest station is 250. Each row(?) represents an MSOA, each value represents a NaPTAN station
plot(NaPTAN_Rail[closest,]) #plot each of these stations (7201 of them)

#Create a dataset of routes from each MSOA to each NaPTAN station
#Create a dataframe with source, dest codes in first two columns
    MSOA_CD <- MSOA@data[,1] #the codes of each of the MSOAs (origin)
    NaPTAN_AtcoCD <- NaPTAN_Rail[closest,]@data[,1] #The codes of each of the NaPTAN stations (destination)
    flows <- data.frame(MSOA_CD, NaPTAN_AtcoCD,dist) #Join them up into a flows dataframe

#Function edited (poorly) to support a second zones dataset of destinatations (NaPTAN)
    od2line_custom <- function(flow, zones, zonesto){ 
      l <- vector("list", nrow(flow))
      for(i in 1:nrow(flow)){
        from <- zones@data[,1] %in% flow[i, 1]
        to <- zonesto@data[,1] %in% flow[i, 2]
        x <- sp::coordinates(zones[from, ])
        y <- sp::coordinates(zonesto[to, ])
        l[[i]] <- sp::Lines(list(sp::Line(rbind(x, y))), as.character(i))
      }
      l <- sp::SpatialLines(l)
      l <- sp::SpatialLinesDataFrame(l, data = flow, match.ID = F)
      sp::proj4string(l) <- sp::proj4string(zones)
      l
    }

travel_network <- od2line_custom(flow = flows, zones = MSOA, zonesto = NaPTAN_Rail) #use an edited version of od2line to convert origin dest data to lines, the network

#Plot the data
    plot(travel_network)
    points(NaPTAN_Rail,col='red')
    points(MSOA,col='green

sum(travel_network@data[,3])/1000 #km length (approx) - Used to check CycleStreets API limit!

#Generate route network
    travel_network = travel_network[travel_network$NaPTAN_AtcoCD =='9100SHEFFLD', ] #Filter on Sheffield Station Only

    travel_network <- spTransform(travel_network, CRS("+init=epsg:4326")) #convert to line2route compatable CRS
    cyclestreet_pat() #enter cyclestreets api key
    routes <- line2route(travel_network)

geojson_write(routes, file= "OA_Sheffield_Routes.geojson")
geojson_write(travel_network, file= "OA_Sheffield_Network.geojson")

#This didn't seem to work as expected
    install.packages('tmap')
    require(tmap)

    t_routes <- routes
    t_routes$All <- travel_network$All
    rnet <- overline(sldf = t_routes, attrib = "All", fun = sum)

    geojson_write(rnet, file= "OA_Sheffield_rnet.geojson")

#All done