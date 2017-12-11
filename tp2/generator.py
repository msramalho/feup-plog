

def generate(rounds, maxDiff):
	''' receiver number of rounds and max div and return tuple (subjects, teachers) with valid problem data '''
    subjects = []
    teachers = []

    for round in range(rounds):
        semester = 1 + round % 2  # alterante between 1 and 2
        print("Entering round %d/%d (Semester: %d)" %
              (round + 1, rounds, semester))

        # create the main subject that defines how this round will occur
        roundSubject = generateSubject(semester=semester)
		# create the main teacher for (at least) the theoretical of roundSubject
		currentTeacher = generateTeacher(field = roundSubject.field)
		# add the theoretical hours to this teacher
		currentTeacher.addHours(roundSubject.tHours)

		# create teachers until the roundSubject's practical hours are filled
		roundTeachers = []
		# until there are no hours to fill in the roundSubject
		while True: # do-while
			roundTeachers.append(currentTeacher)
			attributeTHours(roundSubject, currentTeacher, maxDiff) #add as many hours as possible
			if roundSubject.pHours == 0: # enough teachers found to fill the pratical hours
				break
			currentTeacher = generateTeacher() # generate as many extra teachers (any field) as necessary

		# calculate debt of this round (hours each teacher needs to respect the maxDiff)
		# and generate a subject to fill them
		debtSubject = generateDebtSubject(roundTeachers)


		# add the subjects created in this round to the final results lists
		subjects.append(roundSubject)
		if debtSubject: # if debt == 0 then this is None
			subjects.append(debtSubject)

		# add the teachers created in this round to the final results lists
		teachers.extend(roundTeachers)

    return subjects, teachers


subjects, teachers = generate(10, 2)
print(subjects)
print(teachers)
