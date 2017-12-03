import subprocess as sp  # call, Popen, PIPE
from os import listdir, remove
from os.path import isfile, join

from generator import *

def createPrologDataFromJson(jsonFile):
    outputFileName = jsonFile[:-4] + "pl" # mieic_a3_s1.pl
    completeOutputFileName = "src/tmp/data_" + outputFileName # src/mieic_a3_s1.pl
    generatePrologForFile(completeJsonFile, ouput=completeOutputFileName) #generate the .pl file from the json

def createNewMainFile(jsonFile, edit="src/main.pl"):
    contents = ""
    print(edit)
    with open(edit, 'r', encoding="utf-8") as mainFile:
        contents = mainFile.read()
        print("HEER")
        print( mainFile.read())
    print(contents)


#read the output of a subprocess, print and return it
def processToStdout(process):
    out, err = process.communicate()
    ouput = out.decode("utf-8")
    print(ouput)
    return ouput

#edit main.pl file so that the correct datafile (.pl) is included
''' def editMainFile(dataFileName, edit="src/main.pl"):
    contents = ""
    with open(edit, 'r', encoding="utf-8") as mainFile:
        contents = mainFile.read()
    contents.replace("data.pl", dataFileName)#edit the main.pl file
    with open(edit, 'w', encoding="utf-8") as mainFile:
        contents = mainFile.read() '''

dataFolder = "data/"
jsonFiles = [f for f in listdir(dataFolder) if isfile(join(dataFolder, f)) and f[-5:] == ".json"]

print("Found %d file(s): %s" % (len(jsonFiles), jsonFiles))
print("Starting tests:")
for jsonFile in jsonFiles:
    completeJsonFile = dataFolder + jsonFile
    print("------------------------------------------------")
    print("TESTING %s:\n" % completeJsonFile)
    createPrologDataFromJson(jsonFile)
    createNewMainFile(jsonFile)
    # executeMainFile()
    # change extension from .json to .pl
    # editMainFile(outputFileName)
    # cmd = "echo run. | sicstus --nologo --noinfo -l src/main.pl"
    # process = sp.Popen(cmd, shell=True, stdout=sp.PIPE)
    # ouput = processToStdout(process) # it is done this way so that later we can parse the stdout in python
    # remove(completeOutputFileName) # delete created file

print("------------------------------------------------")
print("DONE")
