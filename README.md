# feup-plog
Logic Programmin in PROLOG
## TP1 - LYNGK Board Game - [Report](https://github.com/msramalho/feup-plog/blob/master/tp1/report.pdf)
<p align="center"><img src="https://github.com/msramalho/feup-plog/blob/master/tp1/lyngk.png" width="40%" ></p>

<hr/>

## TP2- Teacher Hour Allocation - Constraints (Finite Domain) [paper](https://github.com/msramalho/feup-plog/blob/master/tp2/paper.pdf)
We developed a PROLOG file ([main.pl](https://github.com/msramalho/feup-plog/blob/master/tp2/src/main.pl)) that can be run to find a solution for the problem specified in [data.pl](https://github.com/msramalho/feup-plog/blob/master/tp2/src/data.pl) using sicstus constraint engine for Finite Domains (`:- use_module(library(clpfd)).`). 

## interface.py
This file essentially does everything else. It generates json versions of problems with solution, converts them to Prolog to feed our open-mouthed slow-eater solver, and executes them. It is also able to output the results to a .csv file for later interpretation.
To see everything it can do, call `python interface.py -h`:
```
usage: interface.py [-h] [-g] [-p] [-e] [-tt] [-ht] [-nr] [-nf] [-md] [-jf] [-pf] [-cf] [-r] [-d] [-t] [-ra]

Python generator, parser, executer and analyser for problems concerning the teacher hours assignment

optional arguments:
  -h, --help            show this help message and exit

actions:
  -g, --generate        Generate a JSON file (-jf) with a solvale problem
  -p, --parse           Parse from a JSON file into a Prolog file(-jf to -pf)
  -e, --execute         Execute the main.pl file with a custom Prolog data file (-pf, -cf)
  -tt, --test           Run all the json files inside data/
  -ht, --hardcode-test  Run the hardcoded tests

generation arguments:
  -nr , --number-rounds Number of rounds for the generator (default is 1)
  -nf , --number-fields Number of fields for the generator (default is 1)
  -md , --max-diff      Max Diff value for the generator (default is 0)

custom filenames:
  -jf , --json-file     The name of the json file to be used (generation, parsing)
  -pf , --prolog-file   The name of the prolog file to be used (parsing, execution)
  -cf , --csv-file      The name of the csv file to save the results (not specified = no output)

behaviour customization:
  -r, --remove          Remove the generated files
  -d, --debug           Run the prolog code in debug mode (only for executing)
  -t, --tabled          Makes the output form JSON to Prolog be tabled for easy reading
  -ra, --randomize      randomize the number of effective hours, instead of using all available (for the generator)
```

Some example of cool commands are:

 - `python interface.py -g -jf data/example.json` (generates a problem with the default values in JSON)
 - `python interface.py -g -jf data/example.json -nr 10` (same but using 10 rounds instead of 1, much harder for the solver)
 - `python interface.py -p -jf data/example.json -pf src/data_example.pl` (converts JSON to the predicates used by the solver) if -pf is not specified, src/data.pl is used
 - `python interface.py -e -pf src/data_exemple.pl -d -cf temp.csv` (executs the solver with the given data file, in debuf mode(-d) and writes the results (backtracks, heurisric, walltime, ...) to temp.csv and also on the console)
 - `python interface.py -pge -jf data/everything.json -pf src/data_exemple.pl -cf temp.csv -d -nr 10 -nf 5 -md 4` (generates, parses and executes a new problem with 10 rounds, 5 fields of expertise and a maxDiff of 4)
