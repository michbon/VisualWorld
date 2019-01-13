import pandas
import numpy
import os
import os.path
import re


inputDir = r"C:\Users\pplsuser\Documents\I_Experiment\SardVW\AnalysisVW\Ita-Eng\proL1"

inputTiers = r"C:\Users\pplsuser\Documents\I_Experiment\SardVW\AnalysisVW\Ita-Eng\proL1\L1_tiers_pro.xlsx"

precisionre = r"precision \(RMS noise in pixels\): X=(\d+\.\d+), Y=(\d+\.\d+)"

# read the OpenSesame log and saves the experimental items data into variable "data"
def readSesame(inputFileSesame):
	data = pandas.read_csv(inputFileSesame)
	data = data.loc[data["type"]=="exp",:]
	return data

# read the eye-tracker log and cleans it, saving it into the outputfile
def cleanTsv(inputFileName, outputFileName, outputFileQuality):
	inputFile = open(inputFileName)
	outputFile = open(outputFileName, "w")
	qualityFile = open(outputFileQuality, "w")

	# define type of trial: read the MSG lines and takes beginning and end of exp trials, also avoids empty lines
	currentTrial = None 
	timestamp = None
	valid = 0
	invalid = 0

	precisionx = None
	precisiony = None

	for line in inputFile:
		line = line.strip()
		row = line.split("\t")
		row = [x.strip('"') for x in row]
		if row[0]=="MSG":
			if len(row) < 4:
				continue
			if row[3].startswith("exp"):
				currentTrial = row[3]
				timestamp = row[2]
			elif row[3].startswith("end"):
				currentTrial = None
				timestamp = None
			elif row[3] == "stop_trial":
				currentTrial = None
				timestamp = None
			elif row[3].startswith("precision"):
				m = re.match(precisionre, row[3])
				precisionx = m.group(1)
				precisiony = m.group(2)


		else:
			if currentTrial is not None and line != "":

				qualityline = ",".join([row[0], row[1], row[3], precisionx, precisiony])+"\n"
				qualityFile.write(qualityline)

				# accepts only records when quality of recording is = 7
				if row[3] == "7": 
					idlist = currentTrial.replace(".wav","").split("_")
					valid += 1
					outputline = ",".join([row[0],row[1],row[11], row[12], row[18], row[19], idlist[1], "_".join(idlist[2:5]), idlist[5], timestamp]) + "\n"
					outputFile.write(outputline)
				else:
					invalid += 1
	# tells percentage of excludedtrials			
	print("for file {} there were {} % invalid records".format(inputFileName, 100.0*invalid/(valid+invalid)))				

# define ROIs
liberal = 0
screenminx, screenmaxx, screenmny, screenmaxy = 0, 1366, 0, 768
minx,maxx, miny, maxy = 0, 330, 0, 330
#positions = {
#	"p1":[minx-liberal, maxx+liberal, miny-liberal, maxy+liberal], 
#	"p2":[minx+1050-liberal, maxx+1050+liberal, miny-liberal, maxy+liberal],
#	"p3":[minx, maxx, miny+500, maxy+500], 
#	"p4":[minx+1050, maxx+1050, miny+500, maxy+500], 
#} 

positions = {
	"p1":[minx-liberal, maxx+liberal, miny-liberal, maxy+liberal], 
	"p2":[screenmaxx-maxx-liberal, screenmaxx+liberal, miny-liberal, maxy+liberal],
	"p3":[minx-liberal, maxx+liberal, screenmaxy-maxy-liberal, screenmaxy+liberal], 
	"p4":[screenmaxx-maxx-liberal, screenmaxx+liberal, screenmaxy-maxy-liberal, screenmaxy+liberal], 
}
# creates a column with positions: if both left and right eye are included in ROI, write position ID
def tablePos(tracker):
	tracker["position"] = ""

	for positionId, limits in positions.items():
		inXleft = (tracker["lx"] > limits[0]) & (tracker["lx"]<limits[1])
		inYleft = (tracker["ly"] > limits[2]) & (tracker["ly"]<limits[3])
		inXright = (tracker["rx"] > limits[0]) & (tracker["rx"]<limits[1])
		inYright = (tracker["ry"] > limits[2]) & (tracker["ry"]<limits[3])
		tracker.loc[inXleft & inXright & inYleft & inYright, "position"] = positionId

	return tracker

# # creates a column with category of picture per position
def tablePicture(tracker,data):

	tracker["category"] = ""
	data["sentence"] = data["sentence"].str.replace(".wav","")
	data = data.set_index(["sentence"])
	# print(data.index)

	for i, row in tracker.iterrows():
		if row["position"] == "":
			continue

		category = data.loc[row["label"],row["position"]]
		tracker.ix[i, "category"] = category
		
	# print(data)
	return tracker



# reads stimuli time tiers
def readTiers(inputFileTiers):
	tiers = pandas.read_excel(inputFileTiers)
	return tiers

# adds multipletime lines to tracker data to follow probe sentence in time accordng to tiers
def addTiers(tracker,tiers):
	tracker["probe"] = numpy.nan
	tracker["verb"] = numpy.nan
	tracker["a1"] = numpy.nan
	tracker["a2"] = numpy.nan
	tracker["finish"] = numpy.nan


	for _,row in tiers.iterrows():
		time = tracker.loc[tracker["label"]==row["Sentence"],"time"]
		timestart = tracker.loc[tracker["label"]==row["Sentence"],"timestart"]
		
		#offset = timestart.iloc[0]+1200+row[5]

		probe = timestart.iloc[0]+1200+row[6]
		tracker.loc[tracker["label"]==row["Sentence"],"probe"] = time - probe

		verb = timestart.iloc[0]+1200+row[7] 
		tracker.loc[tracker["label"]==row["Sentence"],"verb"] = time - verb

		a1 = timestart.iloc[0]+1200+row[8]
		tracker.loc[tracker["label"]==row["Sentence"],"a1"] = time - a1

		a2 = timestart.iloc[0]+1200+row[9]
		tracker.loc[tracker["label"]==row["Sentence"],"a2"] = time - a2

		finish = timestart.iloc[0]+1200+row[10]
		tracker.loc[tracker["label"]==row["Sentence"],"finish"] = time - finish

	return tracker


def process(inputstem,inputTiers):

	# reads time tiers for stimuli
	tiers = readTiers(inputTiers)

	# reads OpenSesame log
	sesame = readSesame(inputstem+".csv")

	# cleans eye-tracker log
	cleanTsv(inputstem+".tsv",inputstem+"_clean.csv", inputstem+"_quality.csv")
	# reads off relevant tracker data from clean log into a table (two steps for versatility)
	tracker = pandas.read_csv(inputstem+"_clean.csv", header=None, names = 
		["clock", "time", "lx", "ly", "rx", "ry", "type", "label", "ntrial", "timestart", "probe" , "verb", "a1", "a2", "finish"])
	# adds to tracker data a column that identifies position of the gaze wrt to ROIS
	tracker = tablePos(tracker)
	# adds to tracker data the time tiers info
	tracker = addTiers(tracker, tiers)
	# adds to tracker data pictures and categories
	tracker = tablePicture(tracker,sesame)
	# prints all this to a file
	tracker.to_csv(inputstem+"_output.csv", index = False)



for filename in os.listdir(inputDir):
	if filename.endswith(".tsv"):
		process (os.path.join(inputDir,filename[:-4]), inputTiers)

