# feup-plog
Logic Programmin in PROLOG
## TP1 - LYNGK Board Game


<hr/>

## TP2- Teacher's Working Hours Distribution - Restrictions over a Finite Domain
We developed a PROLOG file ([main.pl](https://github.com/msramalho/feup-plog/blob/master/tp2/src/main.pl)) that can be run to find a solution for the problem specified in [data.pl](https://github.com/msramalho/feup-plog/blob/master/tp2/src/data.pl). 

## generator.py
**However**, we went a step further and created a Python script ([generator.py](https://github.com/msramalho/feup-plog/blob/master/tp2/generator.py)) that is able to convert a given `dataFile.json` ([example](https://github.com/msramalho/feup-plog/blob/master/tp2/data/mieic_a3_s1.json)) into the format used in the `data.pl` file, which gives us flexibility when we want to test different problem specifications agains our restrictions algorithm.

To convert a `.json` file into a `data.pl`, we simply run:
```
python generator.py data/mieic_a3_s1.json
```
And this will replace the current `data.pl` file with the new data.
#### Notes on generator.py
 * It implements a function `generatePrologForFile(filename, print = False, ouput = "src/data.pl")`, that can be called and that can print an easy-to-ready tabular output from the `.json` file, if the parameter `print` is set to `True`;
 * A custom path to output the PROLOG data file can also be passed in the `ouput parameter`;
 * It runs some basic tests on the `.json` data so as to ascertain wether the configuration can be excluded due to immediate problems, such as a universe where there are more hours to teach than the available teachers can teach.
 
 ---
 
 ## tester.py
 
**Aditionally**, we decided one step was not enough, so we took another. We created yet another Python script ([tester.py](https://github.com/msramalho/feup-plog/blob/master/tp2/tester.py)) that is able to use [multiprocessing](https://docs.python.org/3.6/library/multiprocessing.html) so as to read all the `.json` files in a folder, namely `data/`, and foreach one of those, to use the `generator.py` tools to create multiple `data_customName.pl` data files **and then** run them by invoking the `sicstus` command. Thus giving us a tailored way of testing our program in a speedy way.

To test all the `.json` files in the `data/` folder, we simply run:
```
python tester.py
```
And watch the magic happening.

#### Notes on tester.py
 * This script performs an output, after running all the data files, and displays the duration that process took. Since we also wanted time measurement form inside the PROLOG code, `tester.py` is able to catch sicstus prints in the format `\nprologTime%` (where `\n` is a newline and `%` is anything after `prologTime`) and display them. They can also be used for further statistical analysis done on the PROLOG code performance.
