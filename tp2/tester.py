import subprocess as sp  # call, Popen, PIPE
from os import listdir, remove, mkdir
import os
from os.path import isfile, join
from multiprocessing import Manager, Pool, cpu_count
import time
import ntpath
from sys import argv, exit

from prologer import *
from generator import *

#important variables:
tmpFolder = "src/"#folder to use for the temporary usage of files, should not be inner folder, due to include referencing
dataPrefix = "data_"#append to generated data prolog files
mainPrefix = "main_"#append to generated data prolog files
dataFolder = "data/"#source folder to look for json files

def createPrologDataFromJson(jsonFile, filesToRemove):
	outputFileName = jsonFile[:-4] + "pl" # mieic_a3_s1.pl
	completeOutputFileName = tmpFolder + dataPrefix + outputFileName # src/mieic_a3_s1.pl
	completeJsonFile = dataFolder + jsonFile
	generatePrologForFile(completeJsonFile, output=completeOutputFileName) #generate the .pl file from the json
	filesToRemove.append(completeOutputFileName)#add to the list of files to remove

def createNewMainFile(jsonFile, filesToRemove, edit="src/main.pl"):
	contents = ""
	jsonToPl = jsonFile[:-4]+"pl"
	with open(edit, 'r', encoding="utf-8") as mainFile:#read default main file
		contents = mainFile.read()
	contents = contents.replace("data.pl", dataPrefix + jsonToPl)#replace included data file
	newMainFilename = getValidFileName(tmpFolder + mainPrefix + jsonToPl)
	writeFileWarnDuplicate(newMainFilename, contents)# write to file
	filesToRemove.append(newMainFilename)#add to the list of files to remove
	return newMainFilename

def executeMainFile(newMain):
	start = time.time()#time measurement start

	cmd = "echo init(Subjects, Teachers). | sicstus --nologo --noinfo -l %s" % newMain
	process = sp.Popen(cmd, shell=True, stdout=sp.PIPE, stderr=sp.PIPE)
	processOutput = processToStdout(process)

	diff = (time.time() - start) * 1000000 # microseconds, time measurement end
	return (processOutput, diff) # return tuple with output, time in microseconds

def executeMainFileToShell(newMain):
	cmd = "echo init(Subjects, Teachers). | sicstus --nologo --noinfo -l %s" % newMain
	sp.call(cmd, shell=True)

#read the output of a subprocess, print and return it
# it is done this way so that later we can parse the stdout in python
def processToStdout(process):
	out, err = process.communicate()
	output = out.decode("utf-8")
	error = err.decode("utf-8", errors='ignore')
	return output + error

def getValidFileName(original):
	return "".join([c for c in original if c.isalpha() or c.isdigit() or c in ['.', '/', '_', '-']]).rstrip()

#delete the files in filesToRemove
def removeTmpFiles(filesToRemove):
	for f in filesToRemove:
		try:
			remove(f)
		except OSError:
			print("----[WARNING]: Unable to remove temporay file: %s" % f)

#create the tmp folder if it does not exist
if not os.path.exists(tmpFolder):
	os.makedirs(tmpFolder)

def worker(jsonFile):
	filesToRemove = []
	createPrologDataFromJson(jsonFile, filesToRemove)
	newMain = createNewMainFile(jsonFile, filesToRemove)
	result = executeMainFile(newMain)
	removeTmpFiles(filesToRemove)
	return (jsonFile, result)

def outputResults(results):
	for (f, (output, time)) in results:
		print("-" * 50)
		print("%s lasted for %10f microseconds" % (f, time))
		result = {}
		#get the output variables from prolog
		outputVariables = ["Resumptions: ", "Entailments: ", "Prunings: ", "Backtracks: ", "Constraints created: ", "Runtime: ", "Walltime: "]
		for outVar in outputVariables:
			result[outVar] = [line.strip("\r").strip(outVar) for line in output.split('\n') if outVar in line]
		print(json.dumps(result))

if __name__ == '__main__':
	if len(argv) >= 2:
		if argv[1] == "gen":#run with a generator
			rounds = int(argv[2]) if len(argv) >= 3 else 1
			config = Config(rounds=rounds, maxDiff=0, nFields=1, randomizeEfficient=False)
			subjects, teachers = generate(config)
			filename = "data/auto_gen_%d.json" % rounds
			dataToPrologJson(config, subjects, teachers, filename)
			generatePrologForFile(filename, tabled = len(argv) >= 4 and argv[3] == "-t")
			executeMainFileToShell("src/main.pl")
		elif argv[1] == "default":#run the default file
			executeMainFileToShell("src/main.pl")
		else: #assume it is a datafile
			worker(ntpath.basename(argv[1]))

	else:#read all the files in the data folder
		jsonFiles = [f for f in listdir(dataFolder) if isfile(join(dataFolder, f)) and f[-5:] == ".json"]
		print("Found %d file(s): %s" % (len(jsonFiles), jsonFiles))

		nCores = cpu_count()#number of cpu cores
		with Pool(nCores) as pool:
			work_results = pool.map(worker, jsonFiles)
		outputResults(work_results)
	print("-" * 50)

	print("FINISHED")
