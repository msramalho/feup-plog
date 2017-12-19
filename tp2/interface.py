import argparse

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
groupActions.add_argument('-tt','--test', help='Run the hardcoded tests', default=False, action='store_true')

# generation arguments
groupGenArgs = parser.add_argument_group('generation arguments')
groupGenArgs.add_argument('-nr','--number-rounds', metavar="", help='Number of rounds for the generator', default=1, type=int)
groupGenArgs.add_argument('-nf','--number-fields', metavar="", help='Number of fields for the generator', default=1, type=int)
groupGenArgs.add_argument('-md','--max-diff', metavar="", help='Max Diff value for the generator', default=0, type=int)

# custom filenames
groupFiles = parser.add_argument_group('custom filenames')
groupFiles.add_argument('-jf','--json-file', metavar="", help='The name of the json file to be used (generation, parsing)', default="auto_gen.json")
groupFiles.add_argument('-pf','--prolog-file', metavar="", help='The name of the prolog file to be used (parsing, execution)', default="data.pl")
groupFiles.add_argument('-cf','--csv-file', metavar="", help='The name of the csv file to save the results (not specified = no output)', default=False)

# behaviour customization
groupCustom = parser.add_argument_group('behaviour customization')
groupCustom.add_argument('-t','--tabled', help='Makes the output form JSON to Prolog be tabled for easy reading', default=True, action='store_true')
groupCustom.add_argument('-r','--randomize', help='randomize the number of effective hours, instead of using all available (for the generator)', default=False, action='store_true')
groupCustom.add_argument('-d','--debug', help='Run the prolog code in debug mode (only for executing)', default=False, action='store_true')

# parse the arguments
args = vars(parser.parse_args())


#---------------------------------------Logic
subjects, teachers = [], []
# generate
if args["generate"]:
	config = Config(rounds=args["number_rounds"], maxDiff=args["max_diff"], nFields=args["number_fields"], randomizeEfficient=args["randomize"])
	subjects, teachers = generate(config)
	dataToPrologJson(config, subjects, teachers, args["json_file"])

# parse
if args["parse"]:
	generatePrologForFile(args["json_file"], tabled=args["tabled"], output=args["prolog_file"])

# execute
if args["execute"]:
	processOutput, diff = createNewMainFile(args["json_file"], filesToRemove, "src/%s_main.pl" % args["json_file"])
	removeTmpFiles(filesToRemove)
