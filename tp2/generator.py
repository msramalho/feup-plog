from classes import Config, Subject, Teacher
import json
import glob, os

# receive config for generation and return tuple (subjects, teachers) with valid problem data
def generate(config):
	subjects = []
	teachers = []

	for round in range(config.rounds):
		semester = 1 + round % 2  # alterante between 1 and 2
		print("Entering round %d/%d (Semester: %d, maxDiff: %d)" % (round + 1, config.rounds, semester, config.maxDiff))

		# create the main subject that defines how this round will occur
		roundSubject = Subject.generateSubject(config, semester)
		# create the main teacher for (at least) the theoretical of roundSubject
		currentTeacher = Teacher.generateTeacher(config, roundSubject.field)
		# add the theoretical hours to this teacher (clears the roundSubject.tHours)
		currentTeacher.addTHours(roundSubject)

		# create teachers until the roundSubject's practical hours are filled
		roundTeachers = []
		# until there are no hours to fill in the roundSubject
		while True: # do-while
			roundTeachers.append(currentTeacher)
			# print("Old: %d" % roundSubject.pHours)
			currentTeacher.addPHoursMax(config, roundSubject) #add as many hours as possible
			# print("New: %d" % roundSubject.pHours)
			if roundSubject.pHours == 0: # enough teachers found to fill the practical hours
				break
			currentTeacher = Teacher.generateTeacher(config) # generate as many extra teachers (any field) as necessary

		# calculate debt of this round (hours each teacher needs to respect the maxDiff) and generate a subject to fill them
		debtSubject = Subject.generateDebtSubject(config, roundTeachers, semester)

		# add the subjects created in this round to the final results lists
		subjects.append(roundSubject)
		if debtSubject: # if debt == 0 then this is None
			subjects.append(debtSubject)

		# add the teachers created in this round to the final results lists
		teachers.extend(roundTeachers)

	return subjects, teachers

# convert a list of subjects, teachers and configurations into a json file for prologer to read and use
def dataToPrologJson(config, subjects, teachers, filename):
	output = {
		"department" : config.department,
		"config" : {
			"rounds" : config.rounds,
			"maxDiff" : config.maxDiff,
			"nFields" : config.nFields,
			"randomizeEfficiency" : config.randomizeEfficiency
		},
		"fields" : [],
		"subjects" : [],
		"teachers" : [],
		"teacherTypes" : []
	}
	for field in range(1, config.nFields + 1):
		output["fields"].append("Field_%d" % field)

	for i, s in enumerate(subjects):
		output["subjects"].append(s.toJsonData(i))

	for i, t in enumerate(teachers):
		output["teachers"].append(t.toJsonData(i))

	for i, tt in enumerate(Teacher.types):
		output["teacherTypes"].append({"id" : i + 1, "averageWeekHours" : tt/2})

	output = json.dumps(output)
	with open(filename, 'w', encoding="utf-8") as f:  # write the new main
		f.write(output)

def removeAutoJson():
	for f in glob.glob("data/auto*.json"):
		os.remove(f)

removeAutoJson()
for i in range(1, 2):
	config = Config(rounds=1, maxDiff=3, nFields=3, randomizeEfficiency=False)
	subjects, teachers = generate(config)
	dataToPrologJson(config, subjects, teachers, "data/auto_%d.json" % i)

	# for s in subjects:
	# 	print(s.toJson())
	# for t in teachers:
	# 	print(t.toJson())
# config = Config()
# s = Subject.generateSubject(config, 1)
# print(s.toJson())

