from classes import Config, Subject, Teacher

def generate(config):
	''' receiver config for generation and return tuple (subjects, teachers) with valid problem data '''
	subjects = []
	teachers = []

	for round in range(config.rounds):
		semester = 1 + round % 2  # alterante between 1 and 2
		print("Entering round %d/%d (Semester: %d)" % (round + 1, config.rounds, semester))

		# create the main subject that defines how this round will occur
		roundSubject = Subject.generateSubject(config, semester)
		print(roundSubject.toJson())
		# create the main teacher for (at least) the theoretical of roundSubject
		currentTeacher = Teacher.generateTeacher(config, roundSubject.field)
		# add the theoretical hours to this teacher (clears the roundSubject.tHours)
		currentTeacher.addTHours(roundSubject)

		# create teachers until the roundSubject's practical hours are filled
		roundTeachers = []
		# until there are no hours to fill in the roundSubject
		while True: # do-while
			roundTeachers.append(currentTeacher)
			print("Old: %d" % roundSubject.pHours)
			currentTeacher.addPHoursMax(config, roundSubject) #add as many hours as possible
			print("New: %d" % roundSubject.pHours)
			if roundSubject.pHours < 0:
				raise Exception('roundSubject.pHours < 0')
			if roundSubject.pHours == 0: # enough teachers found to fill the practical hours
				break
			currentTeacher = Teacher.generateTeacher(config) # generate as many extra teachers (any field) as necessary

		# calculate debt of this round (hours each teacher needs to respect the maxDiff)
		# and generate a subject to fill them
		debtSubject = Subject.generateDebtSubject(config, roundTeachers, semester)

		# add the subjects created in this round to the final results lists
		subjects.append(roundSubject)
		if debtSubject: # if debt == 0 then this is None
			subjects.append(debtSubject)

		# add the teachers created in this round to the final results lists
		teachers.extend(roundTeachers)

	return subjects, teachers

for i in range(1000):
	config = Config(rounds=i, maxDiff=3, nFields=2, randomizeEfficiency=False)
	subjects, teachers = generate(config)
	for s in subjects:
		print(s.toJson())
	for t in teachers:
		print(t.toJson())
# config = Config()
# s = Subject.generateSubject(config, 1)
# print(s.toJson())

