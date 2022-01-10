#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Title:: *)
(*Hana Hawaii Broadband Measurements*)


(* ::Subtitle:: *)
(*Public measurements of OTWC broadband performance using atlas.ripe.net network measurement probes*)


(* ::Author:: *)
(*by Chase@stumpy.com*)


(* ::Subsubtitle:: *)
(*For discussion purposes only.  Use of this report requires specific permission of Chase@stumpy.com*)


(* ::Section:: *)
(*Terminology*)


(* ::Section:: *)
(*atlas.ripe.net - Documentation*)


(* ::Text:: *)
(*RIPE Atlas buit-in measurements : https://atlas.ripe.net/docs/built-in*)
(*RIPE Atlas : https://atlas.ripe.net/docs*)


(* ::Subsection:: *)
(*Conversion of Unix Machine Timestamp to HST*)


ClearAll[unixTimeGMTtoHST, unixTimeGMTtoHST];
(* Hawaiian Timezone is -10 from GMT *)
(* FIXME : Old conversion strategy was DateList[AbsoluteTime[{1970,1,1,0,0,0}]+x]].  
	Now it is best to use FromUnixTime, except it does not yield a correct timzone conversion.  *)
(* NOTE: I have to over-ride the global symbol $TimeZone to suggest I'm currently in the HST *)
unixTimeGMTtoHST[x_Integer] :=Block[{$TimeZone=-10}, DateList[FromUnixTime[x]]];
(* OR I can ensure DataList is called with the correct timezone *)
unixTimeGMTtoHST[x_Integer] :=DateList[FromUnixTime[x],TimeZone->-10];
(* TEST *)
Block[
{ testDateGMTAsUnixTime=UnixTime[DateObject[{2015,1,1,0,0,0.0},TimeZone->0]],
	testDateHST = {2014,12,31,14,0,0.0} ,
	testConversion,
	testResult },
	testConversion=unixTimeGMTtoHST[testDateGMTAsUnixTime];
	(* The expected output for HST is {2014,12,31,14,0,0.0} *)
	testResult =If[testConversion===testDateHST," == "," != "];
	"convertedHST["<>ToString[testConversion]<> testResult <> "expected HST value:"<> ToString[testDateHST]]


(* ::Section:: *)
(*atlas.ripe.net - BuiltIn measurements *)


(* ::Subsection:: *)
(*Where to find local data*)


builtInPathRoot=FileNameJoin[{NotebookDirectory[],"Data","builtIn"}];


(* ::Subsection:: *)
(*Utilities to fetch BuiltIn measurements*)


(* ::Subsubsection:: *)
(*builtInQuery - convienience function to construct the URL to pull data*)


Clear[builtInQuery];
builtInQuery[probeID_,(*AKA Hop*) measurementID_ ,resolution_,startTime_,endTime_]:= URLBuild[
	{"https://stat.ripe.net","/data/atlas-ping-measurements/data.json"},
	{"probe_id"->probeID, 
	"measurement_id"->measurementID,
	"starttime"->DateString[startTime,"ISODateTime"],
	"endtime"->DateString[endTime,"ISODateTime"],
	"resolution"->resolution (* 0, 1h, 1d, 1w *),
	"format"->"json",
	"display_mode"->"condensed"}];
(* TEST *)
If[True, builtInQuery[1178,2,0,{2012,1,1},DateList[]]]


(* ::Subsubsection:: *)
(*Future API to invoke rather than the above*)


(* ::Text:: *)
(*The following alternate API call is desirable as there is no need to specify the start and stop times.  But calls to this function result in a 404 error.  Manually copying and pasting to a web browser already logged into atlas does work.*)
(*Next step is to consult with Atlas to ask them to allow me to generate a key to fetch this information.  NOTE: I did experiment with a variety of private API keys to no avail.*)


URLBuild[{"https://atlas.ripe.net","/api/v1/measurement/1/result/"},
	{"start"->"1330387200",
	"stop"->"1441411199",
	"probe_id"->"1178",
	"key"->"xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx", 
	"format"->"json",
	"filename"-> "RIPE-Atlas-measurement-1-probe-1178.json"}];


(* ::Subsubsection:: *)
(*builtInFilename - convienience function to follow the ATLAS naming convention for measurements*)


Clear[dateAsFileName];
dateAsFileName[aDate_]:=DateString[aDate,{"Year","-","Month","-","Day"}];
(* Test *)
If[True, dateAsFileName[DateList[]]]


Clear[builtInFilename];
builtInFilename[probeID_,measurementID_,resolution_,startTime_,endTime_]:=
	"RIPE-Atlas-measurement-"<>ToString[measurementID]<>"-probe-"<>ToString[probeID]<>"-resolution-"<>ToString[resolution]<>"-starting-"<>dateAsFileName[startTime] <> "-ending-"<>dateAsFileName[endTime]<>".json";
(* TEST*)
If[True, builtInFilename[1178,2,0,{2012,1,1},DateList[]]]


(* ::Subsubsection:: *)
(*fetchBuiltin - convenience function to fetch and store ATLAS datums.  Returns the file path to the cached data file*)


Clear[builtInFilePath];
builtInFilePath[probeID_,measurementID_,resolution_,startTime_,endTime_]:=
Block[
	{fileName = builtInFilename[probeID,measurementID,resolution,startTime, endTime], filePath},

	(* Path to find or place the data pulled *)
	filePath=FileNameJoin[{builtInPathRoot, fileName}];

	(* Return local file or pull and return *)
	If[FileExistsQ[filePath], filePath, Export[filePath,Import[builtInQuery[probeID,measurementID,resolution,startTime,endTime],"JSON"],"JSON"];filePath]];


(* ::Subsubsection:: *)
(*rttDatum - convenience function to fetch a slice of rttDatum of interest.  *)


(* ::Text:: *)
(*NOTE : rttDatum is cached and so, if you want to ensure it is re-set, issue the Clear[rttDatum] to force the reload.*)
(*NOTE: ALSO that I am caching based upon YYYY, MM, DD only.*)


Clear[rttDatum];
rttDatum[probeID_,hop_,resolution_,startTime_, endTime_,rttMeasurementType_String] :=
	rttDatum[probeID,hop,resolution,startTime[[1;;3]], endTime[[1;;3]],rttMeasurementType] = Block[
	{
	rttMeasurements =( "measurements"/.( "data"/.Import[builtInFilePath[probeID,hop, resolution,startTime,endTime]] )),
	rttSlice
	},
	(* Traverse the JASON structure to reach the slices of interest *)
	rttSlice={unixTimeGMTtoHST["timestamp"],rttMeasurementType}/.rttMeasurements;
	rttSlice];


(* ::Subsubsection:: *)
(*rttDatumForHours - specialization to slice out datums that match a certain hour of the day.  *)


(* ::Text:: *)
(*NOTE : rttDatumForHours is cached and so, if you want to ensure it is re-set, issue the Clear[rttDatumForHours] to force the reload.*)
(*NOTE: ALSO that I am caching based upon YYYY, MM, DD only.*)


Clear[rttDatumForHours];
rttDatumForHours[probeID_,hop_,resolution_,startTime_,endTime_,rttMeasurementType_String, hours_]:=
	rttDatumForHours[probeID,hop,resolution,startTime[[1;;3]],endTime[[1;;3]],rttMeasurementType, hours]=Block[
	{
		rttDatums = rttDatum[probeID,hop,resolution,startTime,endTime, rttMeasurementType],
		hhPattern
	},

	(* construct DateList patterns to match *)
	hhPattern=If[Length[hours]==1, hours, Alternatives[hours]];

	(* RETURNING *)
	Cases[rttDatums,{{yy_,mm_,dd_,hhPattern,min_,sec_},__}]];


(* ::Section:: *)
(*Atlas.ripe.net probes in Hawaii (of interest)*)


Clear[probes];
probes::usage = "List of probes in Hawaii";
(* Pull 1st and 2nd hop builtIn metrics *)
probes = {1178, 1165, 16065, 15111, 22773, 14720, 02289, 12735, 10546, 18657};
(* FIXME - why is Mike's HawaiiTel 16090 returning odd errors? *) 
If[False, AppendTo[{16090}, probes], probes];
(* Set to true to add in probes from mainland *)
If[False, AppendTo[{14606}, probes], probes];
(* OVERRIDE TO LOAD MAINLAND PROBE DATA *)
If[False, probes = {22199, 14606, 14754}, probes];
(* Pull in BuiltIn measurements *)


(* ::Subsection:: *)
(*Display and Legends *)


(* ::Subsubsection:: *)
(*City where probe is located*)


ClearAll[cProbe];
Block[{},
cProbe::usage="City of the probe";
cProbe[01165]="Maui - Hana";
cProbe[01178]="Maui - Hana";
cProbe[16065]="Maui - Hana";
cProbe[15111]="Maui - Kahului";
cProbe[16090]="Maui - Wailuku";
cProbe[02289]="Big Island - Hawi";
cProbe[12735]="Big Island - Kailua-Kona";
cProbe[10546]="Big Island - Hilo";
cProbe[14720]="Ohau - Kaneohe";
cProbe[18657]="Kauai - Kapaa";
cProbe[22773]="Maui - Kahua";
cProbe[probeID_]:=If[False,MessageDialog["FIXME - please cProbe name for probe " <> ToString[probeID]], "p"<>ToString[probeID]];

];
(* TEST *)
cProbe/@ probes


(* ::Subsubsection:: *)
(*Whose Probe*)


ClearAll[wProbe];
Block[{},
wProbe::usage="City of the probe";
wProbe[01165]="Maui - Hana - Gaylord";
wProbe[01178]="Maui - Hana - Chase";
wProbe[16065]="Maui - Hana - BillS";
wProbe[15111]="Maui - Kahului - Harry";
wProbe[16090]="Maui - Kahului - MikeS-HawaiiTel";
wProbe[22773]="Maui - Wailuku - MikeS";
wProbe[probeID_]:= cProbe[probeID];
];
(* TEST *)
wProbe/@ probes


(* ::Subsubsection:: *)
(*Color Scheme for probes*)


ClearAll[colorOf];
Block[{},
	colorOf[1165]=Blue;
	colorOf[1178]=Red;
	colorOf[16065]=Yellow;
	colorOf[16090]=Purple;
	colorOf[15111]=LightPurple;
	colorOf[2289]=Gray;
	colorOf[12735]=Orange;
	colorOf[10546]=Pink;
	colorOf[14720]=LightBrown;
	colorOf[18657]=Brown;
	colorOf[22773]=LightBrown;
	colorOf[probeID_] := If[False,MessageDialog["FIXME - please specify color of probe " <> ToString[probeID]],Blue]];


(* ::Subsubsection:: *)
(*PlotStyle for probe *)


ClearAll[plotStyleOf];
plotStyleOf[aProbe_Integer, pSize_, pOpacity_]:=colorOf[aProbe];
(* TEST *)
plotStyleOf[#,.001,0.5]&/@ probes


(* ::Subsubsection:: *)
(*Pretty Print for Probe*)


ClearAll[pPrint];
pPrint[probe_Integer]:= Style[" p"<>ToString[probe] <> "_"<>cProbe[probe]<>" ",FontColor->colorOf[probe], Plain,"Text"];

pPrint[probes_List]:= Row[pPrint/@ probes];
(* TEST *)
pPrint/@ probes


(* ::Section:: *)
(*Visualization of Round Trip Time (RTT) of the "Second outbound Hop" to OTWC equipment on Oahu*)


(* ::Text:: *)
(*The "Second outbound Hop" is a measurement of OTWC network infrastructure response times.  Each measurement is an average of 3 tests.   In one year, each atlas.ripe.net probe collects 630,720 "Second outbound Hop" RTT measurements.*)


(* ::Subsubsection:: *)
(*General parameters*)


Clear[gStartDate,gEndDate,gResolution,gHop,gRTTRange];
gStartDate={2019,12,1};
gEndDate={2022,1,9};
gResolution="1h" (* 0, 1h, 1d, 1w *);
gHop = 2;
gRTTRange={-5,60};


(* Pull in data *)
If[False,
	Parallelize[builtInFilePath[#,gHop,gResolution,gStartDate,gEndDate]& /@probes];"Prefetch completed", 
	"Prefetch of data not exeucted"]


(* ::Subsubsection:: *)
(*Grouping for probes*)


gProbeGroupings={{1165}, {14720},{2289,12735,10546,18657}};


(* ::Subsection:: *)
(*Aggregated Visualization*)


(* ::Subsubsection:: *)
(*visualizeRTT.  NOTE: Dropped Packet rates are not displayed*)


Clear[visualizeRTT];


visualizeRTT[probeList_,hop_,resolution_,startDate_,endDate_,rttMeasures_List]:=
	Parallelize[
		Table[
			DateListPlot[
				{rttDatum[probeID,hop,resolution,startDate,endDate,#]&/@rttMeasures} , 
				Joined->False,
				DateTicksFormat->{"MonthNameShort"," ","Year"},
				ImageSize -> Medium,
				PlotRange->{{startDate,endDate},gRTTRange},
				PlotLegends->Placed[rttMeasures, {Scaled[{0,0.8}], {0, 0.4}}],
				FrameLabel->{
					{"RSS (ms) ", ""},
					{"probe # "<>ToString[probeID]<>" - Measurement: "<> ToString[hop]<> " Resolution: "<> ToString[resolution] ,"Location: "<>wProbe[probeID]}
					}
				],
	{probeID ,probeList}]];


(* ::Subsubsection:: *)
(*1 hour averaged values (typically 43 RTT tests per hour)*)


(visualizeRTT[#,gHop, gResolution,gStartDate,gEndDate,{"rtt_5pct", "rtt_med", "rtt_95pct"}]&/@ gProbeGroupings)//TableForm


(* ::Subsubsection:: *)
(*1 day averaged values (typically 1023 RTT tests per day)*)


(visualizeRTT[#,gHop, "1d",gStartDate,gEndDate,{"rtt_5pct", "rtt_med", "rtt_95pct"}]&/@ gProbeGroupings)//TableForm


(* ::Subsubsection:: *)
(*1 week averaged values (typically 7161 RTT tests per week)*)


(visualizeRTT[#,gHop, "1w",gStartDate,gEndDate,{"rtt_5pct", "rtt_med", "rtt_95pct"}]&/@ gProbeGroupings)//TableForm


(* ::Subsection:: *)
(*Animated by the hour*)


(* ::Subsubsection:: *)
(*visualizeRTTbyTheHour (typically 43 RTT tests per hour) .  Does not reflect packet drops*)


Clear[visualizeRTTbyTheHour];


visualizeRTTbyTheHour[probeList_,hop_,resolution_/; MemberQ[ {0,"0","1h"},resolution], startDate_,endDate_,rttMeasures_List]:=
ListAnimate[
Parallelize[
Table[
DateListPlot[
{rttDatumForHours[probeID,hop,resolution,startDate,endDate,#, hour]&/@rttMeasures} , 
DateTicksFormat->{"MonthNameShort"," ","Year"},
Joined->False,
ImageSize -> Large,
PlotRange->{{startDate,endDate},gRTTRange},
(* PlotLegends\[Rule]Reverse[rttMeasures], *)
FrameLabel->{
{"RTT (ms)", ""},
{"probe: "<> ToString[probeID]<>" Measurement: "<> ToString[hop]<> " Resolution: "<> ToString[resolution] ," Hour: " <>IntegerString[hour,10,2]<> " (HST)  Location: "<>wProbe[probeID]}
}
],
{hour, Range[0,23]},{probeID ,probeList}]]];


(* ::Subsubsection:: *)
(*RTT packet delay by the hour*)


If[True,
(visualizeRTTbyTheHour[{ 1178,1165,2289,12735}, gHop, gResolution, gStartDate,gEndDate,{"rtt_5pct", "rtt_med","rtt_95pct"}] )//TableForm,
"Skipped Animated Visualization"]


(* ::Subsection:: *)
(*SCRATCH*)


(* ::Subsubsection:: *)
(*gHop + gResolution*)


{gHop, gResolution}


gProbeGroupings


(* ::Subsubsection:: *)
(*data sources*)


builtInQuery[#,gHop ,gResolution,gStartDate,gEndDate]&/@Flatten[gProbeGroupings]//TableForm
