#Authors: Arnaud Legrand <arnaud.legrand@imag.fr>
#         Damien Dosimont <damien.dosimont@imag.fr>

#Configure and load libraries

options( width = 200 )
library(plyr)
library(ggplot2)

#Read Paje csv trace file, obtained with paje_dump

read_paje_trace <- function(file) {
  df <- read.csv(file, header=FALSE, strip.white=TRUE)
  names(df) <- c("Nature","ResourceId","Type","Start","End","Duration", "Depth", "Value")
  df = df[!(names(df) %in% c("Nature","Type", "Depth"))]
  df$Origin=file
  df <- df[!(df$Value %in% c("Initializing","Nothing","Sleeping","execute_on_all_wrapper","Blocked")),]
  m <- min(df$Start)
  df$Start <- df$Start - m
  df$End <- df$Start+df$Duration
  df
}

df_native <- read_paje_trace('/home/dosimont/Documents/codesprint/traces_StarPU_SG/dataOld/paul/paje.irl.70.csv')

#Cut trace initialization

df_native <- df_native[df_native$End>42000,]
m <- min(df_native$Start)
df_native$Start <- df_native$Start - m
df_native$End <- df_native$Start+df_native$Duration
df_native <- df_native[with(df_native, order(Start,ResourceId,Value)),]

#Put manually timestamps from lp aggregation result
timestamps <-c(42000000000000, 42773619826755, 44320859480265, 45094479307020, 49736198267550, 50509818094305, 54377917228080, 55151537054835, 55925156881590, 57472396535100, 59019636188610, 59793256015365, 60566875842120, 62887735322385, 63661355149140, 64434974975895, 65982214629405, 66755834456160, 67529454282915, 68303074109670, 69850313763180, 70623933589935, 71397553416690, 144891436958415, 146438676611925, 147212296438680, 147985916265435, 148759536092190, 149533155918945, 150306775745700, 151080395572455, 151854015399210, 152627635225965, 153401255052720, 154174874879475, 154948494706230, 155722114532985, 156495734359740, 157269354186495, 158042974013250, 158816593840005, 159590213666760, 160363833493515, 161911073147025, 162684692973780, 163458312800535, 164231932627290, 165779172280800, 166552792107555, 167326411934310, 170420891241330, 171194511068085, 171968130894840, 172741750721595, 173515370548350, 174288990375105, 175062610201860, 175836230028615, 176609849855370, 177383469682125, 178157089508880, 178930709335635, 179704329162390, 181251568815900, 182025188642655, 182798808469410, 183572428296165, 184346048122920, 185119667949675, 185893287776430, 186666907603185, 187440527429940, 188214147256695, 188987767083450, 189761386910205, 190535006736960, 191308626563715, 192082246390470, 192855866217225, 193629486043980, 194403105870735, 195176725697490, 196723965351000)

#Divide timestamps by 10^9 to match with initial Paje trace (long->float) and shift
divide_timestamps <- function(shift, timestamps) {
  ts <- timestamps/1000000000
  ts <- ts - shift
  ts
}

ts_native <- divide_timestamps(42000, timestamps)

#Chose one event producer (=Paje container)
ep_df_native <- df_native[(df_native$ResourceId %in% c("13420")),]

#Create a data frame that contains part numbers, and timestamps provided by lp aggregation
df_ts_native = data.frame(part = 1:(length(ts_native)-1), start = ts_native[1:length(ts_native)-1], end = ts_native[2:length(ts_native)])

#Get possible state values from df_native (after filtering)
states <- as.character(unique(df_native$Value))

#Create a matrix that will contains activity time per part for each state
df_agg <-data.frame(matrix(0, ncol = length(states), nrow = (nrow(df_ts_native))))

#Name the columns
names(df_agg) <-c(states)

#Add part column in order to merge with df_ts_native df
df_agg$part=df_ts_native$part

#Merge both df_ts_native and df_agg by part number
df_ts_agg <-merge(df_ts_native, df_agg, by="part")

#Just a test
#test <- df_agg[,states[1]]

#Print ggplot of df_ts_native = aggregation parts 
ggplot(df_ts_native,aes(xmin=start,xmax=end,fill=factor(part)))+theme_bw()+ 
  geom_rect(aes(ymin=1.0,ymax=2.0),linetype=1,color="white")