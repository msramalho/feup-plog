import random
import json

class Parent():
	def toJson(self):
		return json.dumps(self, default=lambda o: o.__dict__, sort_keys=True, indent=4)


class Subject(Parent):
	def __init__(self, semester=0, field=0, tHours=-1, pHours=-1, tDuration=-1, pDuration=-1):
		self.semester = semester
		self.field = field
		self.tHours = tHours
		self.pHours = pHours
		self.pDuration = pDuration
		self.tDuration = tDuration
		# this values will not be changed (tHours and pHours, may)
		self.tHoursOriginal = tHours
		self.pHoursOriginal = pHours

	# We can also add our own functions. When our ball bounces,
	def generateSubject(config, semester):
		# list of tuples of possible hours-duration for theoretical classes
		tuplesT = [(2, 1), (2, 2), (3, 1), (3, 3), (4, 1),
				   (4, 2), (4, 4)]  # (total, unitDuration)

		# list of tuples of possible hours-duration for practical classes
		# tuplesT = [(3,1), (3, 3), (4,1), (4,2), (5,1), (6,1), (6,2), (6,3), (7,1), (8,1), (8,2), (9,1), (9,3), (10,1), (10,2), (11,1), (12,1), (12,2), (12,3), (13,1), (14,1), (14,2), (15,1), (15,3), (16,1), (16,2), (17,1), (18,1), (18,2), (18,3), (19,1), (20,1), (20,2)] # (total, unitDuration) [COMPLETE]
		tuplesP = [(4, 1), (4, 2), (5, 1), (6, 1), (6, 2), (8, 2), (9, 3), (10, 2), (12, 2), (12, 3),
				   (14, 2), (15, 3), (16, 2), (18, 2), (18, 3)]  # (total, unitDuration) [MORE REALISTIC]

		# create the new subject
		s = Subject(semester)
		s.field = random.randint(1, config.nFields)
		s.tHours, s.tDuration = random.choice(tuplesT)
		s.pHours, s.pDuration = random.choice(tuplesP)

		return s

	# if the current durations are unable to produce tHours or pHours
	def adjustDurations(self):
		self.tDuration = self.tDuration if self.tHours % self.tDuration == 0 else self.tHours
		self.pDuration = self.pDuration if self.pHours % self.pDuration == 0 else self.pHours

	# receives a list of teachers, calculates their time debts to the maxDiff and returns a Subject that clears the debts
	def generateDebtSubject(config, teachers, semester):
		semester = 1 if semester == 2 else 2 #the new subject is in the next semester
		debts = []
		fields = []
		for teacher in teachers:
			maxDebt = int(abs(teacher.hs1 - teacher.hs2) - config.maxDiff) # formula
			if maxDebt > 0: # there is a debt
				# the 1 is never 0, otherwise there would be no debt
				rangeLeftSet = set(range(1, maxDebt + 1))
				rightMin = int(min(teacher.hs1, teacher.hs2) - config.maxDiff/2)
				rightMax = int(max(teacher.hs1, teacher.hs2) + config.maxDiff/2)
				rangeRightSet = set(range(rightMin, rightMax + 1))

				#intersect both ranges and choose the outcome, always respecting maxDiff
				debt = random.choice(list(rangeLeftSet.intersection(rangeRightSet)))
				teacher.addPHours(debt, semester)
				fields.append(teacher.field) #only append if the debt exists
				debts.append(debt)

		debt = sum(debts) # think of the scope as REST
		if debt == 0: # no debt so there is no need for this subject
			return None
		print("Debt is: %d" % debt)
		minDebt = min(debts) # the minimum debt (for any field in fields)

		s = Subject.generateSubject(config, semester) # generate the new subject for the next semester
		s.field = random.choice(fields) # this means the theoretical hours are guaranteed to have minDebt hours
		if debt < 3: # normally HP > HT
			s.tHours = 0
			s.pHours = debt
		elif debt == 3:
			s.tHours = 1
			s.pHours = 2
		else:
			s.tHours = minDebt
			s.pHours = debt - minDebt
		s.adjustDurations()
		return s


class Teacher(Parent):
	def __init__(self, field = 0, maxHours = -1, hs1 = 0, hs2 = 0, diff = 0):
		self.field = field
		self.maxHours = maxHours # maxHours is not changed
		self.hs1 = hs1
		self.hs2 = hs2
		self.diff = diff

	# receives a subject and adds its theoretical hours into the correct semester - always expected to be possible
	def addTHours(self, subject):
		if subject.semester == 1: # semester 1
			self.hs1 += subject.tHours
		else: # semester 2
			self.hs2 += subject.pHours
		# update the current values in the subject theoretical hours
		subject.tHours = 0

	# receives a semester and adds its practical hours into the correct semester
	def addPHours(self, pHours, semester):
		if semester == 1: # semester 1
			self.hs1 += pHours
		else: # semester 2
			self.hs2 += pHours

	# receives a subject and adds as many of its practical hours into the correct semester as possible
	def addPHoursMax(self, config, subject):
		# get the current semester's value
		current = self.hs1 if subject.semester == 1 else self.hs2
		# use the formula to calculate the maximum amount of class blocks this teacher can give
		# module of a block time by the maximum hours available that respect maxDiff
		blockTime = subject.pDuration
		maxBlocks = blockTime % (self.maxHours/2 + config.maxDiff/2 - current)
		maxBlocks = max(abs(subject.pDuration % subject.pHours), maxBlocks)
		print("pDuration: %d, pHours: %d, module: %d" % (subject.pDuration, subject.pHours, subject.pDuration % subject.pHours))
		print("maxBlocks is %d of %d" % (maxBlocks, subject.pDuration))

		# randomize used blocks (may lead to greater number of teachers)
		effectiveHours = blockTime * maxBlocks # maximum amount
		if config.randomizeEfficiency: # produce random choice instead
			effectiveHours = random.randrange(blockTime, effectiveHours + 1, blockTime)

		# update the subject practical hours with the hours this teacher is teaching
		subject.pHours -=  effectiveHours
		# update the teacher hours based on the semester
		if subject.semester == 1: # semester 1
			self.hs1 += effectiveHours
		else: # semester 2
			self.hs2 += effectiveHours

	def generateTeacher(config, field = None):
		if field is None:
			field = random.randint(1, config.nFields)
		t = Teacher(field) # create new teacher
		t.maxHours = random.choice([14, 16, 18]) # teacher maxHours is a random from the possible seniority steps
		return t


class Config(Parent):
	def __init__(self, rounds=3, maxDiff=4, nFields=5, randomizeEfficiency=False):
		self.rounds = rounds
		self.maxDiff = maxDiff
		self.nFields = nFields
		self.semesterCounter = 0
		self.randomizeEfficiency = randomizeEfficiency # if True this can lead to more teachers being spawned, since their time is not allocated by the maximum amount possible, but randomly between block time and max, @see addPHoursMax
