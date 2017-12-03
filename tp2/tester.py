import subprocess as sp  # call, Popen, PIPE
from os import listdir, remove, mkdir
import os
from os.path import isfile, join
from multiprocessing import Manager, Pool, cpu_count

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
    generatePrologForFile(completeJsonFile, ouput=completeOutputFileName) #generate the .pl file from the json
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
    cmd = "echo init. | sicstus --nologo --noinfo -l %s" % newMain
    process = sp.Popen(cmd, shell=True, stdout=sp.PIPE)
    return processToStdout(process) # it is done this way so that later we can parse the stdout in python

#read the output of a subprocess, print and return it
def processToStdout(process):
    out, err = process.communicate()
    ouput = out.decode("utf-8")
    print(ouput)
    return ouput

def getValidFileName(original):
    return "".join([c for c in original if c.isalpha() or c.isdigit() or c in ['.', '/', '_', '-']]).rstrip()

#delete the files in filesToRemove
def removeTmpFiles(filesToRemove):
    for f in filesToRemove:
        try:
            remove(f)
        except OSError:
            print("----[WARNING]: Unable to remove temporay file: %s" % f)
    print("DONE")

#create the tmp folder if it does not exist
if not os.path.exists(tmpFolder):
    os.makedirs(tmpFolder)

def worker(jsonFile):
    filesToRemove = []
    createPrologDataFromJson(jsonFile, filesToRemove)
    newMain = createNewMainFile(jsonFile, filesToRemove)
    result =  executeMainFile(newMain)
    removeTmpFiles(filesToRemove)
    return result

if __name__ == '__main__':
    #read all the files in the data folder
    jsonFiles = [f for f in listdir(dataFolder) if isfile(join(dataFolder, f)) and f[-5:] == ".json"]
    print("Found %d file(s): %s" % (len(jsonFiles), jsonFiles))

    nCores = cpu_count()#number of cpu cores
    with Pool(nCores) as pool:
        work_results = pool.map(worker, jsonFiles)

    print("------------------------------------------------")

    print("FINISHED")
