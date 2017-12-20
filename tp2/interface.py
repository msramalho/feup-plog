import argparse
import os, sys

from prologer import *
from generator import *
from tester import *


# parse the command line arguments
parser = argparse.ArgumentParser(description="Python generator, parser, executer and analyser for problems concerning the teacher hours assignment")

# actions
groupActions = parser.add_argument_group('actions')
groupActions.add_argument('-g','--generate', help='Generate a JSON file (-jf)', default=False, action='store_true')
groupActions.add_argument('-p','--parse', help='Parse from a JSON file into a Prolog file(-jf to -pf)', default=False, action='store_true')
groupActions.add_argument('-e','--execute', help='Execute the main.pl file with a custom Prolog data file (-pf, -cf)', default=False, action='store_true')
groupActions.add_argument('-tt','--test', help='Run all the json files inside data/', default=False, action='store_true')
groupActions.add_argument('-ht','--hardcoded-test', help='Run the hardcoded tests', default=False, action='store_true')

# generation arguments
groupGenArgs = parser.add_argument_group('generation arguments')
groupGenArgs.add_argument('-nr','--number-rounds', metavar="", help='Number of rounds for the generator', default=1, type=int)
groupGenArgs.add_argument('-nf','--number-fields', metavar="", help='Number of fields for the generator', default=1, type=int)
groupGenArgs.add_argument('-md','--max-diff', metavar="", help='Max Diff value for the generator', default=0, type=int)

# custom filenames
groupFiles = parser.add_argument_group('custom filenames')
groupFiles.add_argument('-jf','--json-file', metavar="", help='The name of the json file to be used (generation, parsing)', default="data/auto_gen.json")
groupFiles.add_argument('-pf','--prolog-file', metavar="", help='The name of the prolog file to be used (parsing, execution)', default="src/data.pl")
groupFiles.add_argument('-cf','--csv-file', metavar="", help='The name of the csv file to save the results (not specified = no output)', default=False)

# behaviour customization
groupCustom = parser.add_argument_group('behaviour customization')
groupCustom.add_argument('-r','--remove', help='Remove the generated files', default=False, action='store_true')
groupCustom.add_argument('-d','--debug', help='Run the prolog code in debug mode (only for executing)', default=False, action='store_true')
groupCustom.add_argument('-t','--tabled', help='Makes the output form JSON to Prolog be tabled for easy reading', default=False, action='store_true')
groupCustom.add_argument('-re','--randomize', help='randomize the number of effective hours, instead of using all available (for the generator)', default=False, action='store_true')

# parse the arguments
args = vars(parser.parse_args())

def disableStdOut():
	sys.stdout = open('stdout_file', 'a')
def enableStdout():
	sys.stdout.close
	sys.stdout = sys.__stdout__

#---------------------------------------Logic
subjects, teachers = [], []
generatedFilesToRemove = []
# generate
if args["generate"]:
	config = Config(rounds=args["number_rounds"], maxDiff=args["max_diff"], nFields=args["number_fields"], randomizeEfficient=args["randomize"])
	subjects, teachers = generate(config)
	dataToPrologJson(config, subjects, teachers, args["json_file"])
	generatedFilesToRemove.append(args["json_file"])

# parse
if args["parse"]:
	generatePrologForFile(args["json_file"], tabled=args["tabled"], output=args["prolog_file"])
	generatedFilesToRemove.append(args["prolog_file"])

# execute
if args["execute"]:
	filesToRemove = []
	newMain = createNewMainFile(args["prolog_file"], filesToRemove)
	output, diff = executeMainFile(newMain, args["debug"])
	removeTmpFiles(filesToRemove)
	if args["csv_file"]:
		csvHeader(args["csv_file"])
		outputToCsv(args["csv_file"], (output, None))

if __name__ == "__main__": # required for the multiprocessing
	# all tests
	if args["test"]:
		runAll(args["csv_file"])

	# hardcoded tests # todo if needed: use pool instead of single process
	if args["hardcoded_test"]:
		args["remove"] = True
		if not args["csv_file"]:
			print("hardcoded tests require csv_file")
			exit()
		filesToRemove = []
		# repetitions, nrounds, maxDiff, nFields
		tests = [
			[10, 1, 0, 1], # simplest
			[10, 1, 4, 1], # simplest with maxDiff
			[10, 1, 0, 4], # simplest with nFields
		]
		csvHeader(args["csv_file"]) # create csv file
		for countTest, test in enumerate(tests): # for each test
			print("Test %d/%d (%d repetitions)" % (countTest, len(tests), test[0]), end='', flush=True)
			for i in range(test[0]): # for each repetition in test
				print(".",end='', flush=True)
				# generate problem
				c = Config(rounds=test[1], maxDiff=test[2], nFields=test[3])
				disableStdOut()
				s, t = generate(c)
				#create unique filenames
				jsonFile = "data/hardcoded_test%d_%d_%d_%d.json" % tuple(test)
				plFile = "src/hardcoded_test%d_%d_%d_%d.pl" % tuple(test)
				# generation to json
				dataToPrologJson(c, s, t, jsonFile)
				# json to prolog
				generatePrologForFile(jsonFile, output=plFile)
				# generate main file
				newMain = createNewMainFile(plFile, filesToRemove)
				output, _ = executeMainFile(newMain, debug=True)
				outputToCsv(args["csv_file"], (output, None))
				generatedFilesToRemove.extend([jsonFile, plFile, newMain])
				enableStdout()
			print("done")
		pass


# remove generated files
if args["remove"]:
	removeTmpFiles(generatedFilesToRemove)