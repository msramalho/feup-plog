import json
import names
import random
from datetime import datetime

random.seed(datetime.now()) # set random seed

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
		tuplesP = [(5, 1), (6, 1), (6, 2), (8, 2), (9, 3), (10, 2), (12, 2), (12, 3),
				   (14, 2), (15, 3), (16, 2), (18, 2), (18, 3)]  # (total, unitDuration) [MORE REALISTIC]

		# create the new subject
		field = random.randint(1, config.nFields)
		tHours, tDuration = random.choice(tuplesT)
		pHours, pDuration = random.choice(tuplesP)
		return Subject(semester, field, tHours, pHours, tDuration, pDuration)

	# if the current durations are unable to produce tHours or pHours
	def adjustDurations(self):
		self.tDuration = self.tDuration if self.tHours % self.tDuration == 0 else self.tHours
		self.pDuration = self.pDuration if self.pHours % self.pDuration == 0 else self.pHours

	# receives a list of teachers, calculates their time debts to the maxDiff and returns a Subject that clears the debts
	def generateDebtSubject(config, teachers, semester):
		print("%sCalculating debts" % ("-"*20))
		semester = 1 if semester == 2 else 2 #the new subject is in the next semester
		debts = []
		fields = []
		for teacher in teachers:
			print("-"*20)
			print(teacher.toJson())
			debt = 0
			maxDebt = int(abs(teacher.hs1 - teacher.hs2) - config.maxDiff) # formula
			if maxDebt > 0: # there is a debt
				# the 1 is never 0, otherwise there would be no debt
				rangeLeftSet = set(range(1, teacher.maxHours - teacher.hs1 - teacher.hs2 + 1))
				current = teacher.hs1 if semester == 2 else teacher.hs2
				rightMin = int(current - config.maxDiff)
				rightMax = int(current + config.maxDiff)
				rangeRightSet = set(range(rightMin, rightMax + 1))
				intersection = list(rangeLeftSet.intersection(rangeRightSet))
				#intersect both ranges and choose the outcome, always respecting maxDiff
				print("Left: ", rangeLeftSet)
				print("Right: ", rangeRightSet)
				print("Intersection: ", intersection)
				debt = random.choice(intersection)

			''' if teacher.hs1 - teacher.hs2 > config.maxDiff: # debt is from hs2 to hs1
				choice = random.choice(range(int(teacher.hs1 - config.maxDiff), int(teacher.hs1 + config.maxDiff)))
				debt = choice - teacher.hs2 # calculate the debt
			elif teacher.hs2 - teacher.hs1 > config.maxDiff: # debt is from hs1 to hs2
				choice = random.choice(range(int(teacher.hs2 - config.maxDiff), int(teacher.hs2 + config.maxDiff)))
				debt = choice - teacher.hs1 # calculate the debt '''

			if debt > 0:
				print("debt is %d" % (debt))
				teacher.addPHours(debt, semester)
				fields.append(teacher.field) #only append if the debt exists
				debts.append(debt)
			print("-"*20)

		debt = sum(debts) # think of the scope as REST
		if debt == 0: # no debt so there is no need for this subject
			return None
		print("Debt is: %d" % debt)

		s = Subject.generateSubject(config, semester) # generate the new subject for the next semester
		s.field = random.choice(fields) # this means the theoretical hours are guaranteed to have minDebt hours
		''' if debt < 3: # normally HP > HT
			s.tHours = 0
			s.pHours = debt
		elif debt == 3:
			s.tHours = 1
			s.pHours = 2
		else:
			minDebt = min(debts) # the minimum debt (for any field in fields)
			s.tHours = minDebt if debt > minDebt * 2 else 0 # make sure HT < HP
			s.pHours = debt - s.tHours '''
		s.tHours = 0
		s.pHours = debt
		s.tHoursOriginal = s.tHours
		s.pHoursOriginal = s.pHours
		s.adjustDurations()
		print("new subject to fill debts is:\n", s.toJson())
		return s

	def toJsonData(self, index):
		return {
			"name" : ("Subject_%d" % index),
			"semester" : self.semester,
			"HT" : self.tHoursOriginal,
			"HP" : self.pHoursOriginal,
			"DT" : self.tDuration,
			"DP" : self.pDuration,
			"field" : self.field
		}

class Teacher(Parent):
	types = [14, 16, 18]
	def __init__(self, field = 0, maxHours = -1, hs1 = 0, hs2 = 0):
		self.field = field
		self.maxHours = maxHours # maxHours is not changed
		self.hs1 = hs1
		self.hs2 = hs2

	# receives a subject and adds its theoretical hours into the correct semester - always expected to be possible
	def addTHours(self, subject):
		if subject.semester == 1: # semester 1
			self.hs1 += subject.tHours
		else: # semester 2
			self.hs2 += subject.tHours
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
		print("-----Filling Subject\n", subject.toJson())
		# use the formula to calculate the maximum amount of class blocks this teacher can give
		# module of a block time by the maximum hours available that respect maxDiff
		# calculate the maximum ammount of blocks the teacher can give, the if is to avoid X % 0 (exception)
		availableHours = (self.maxHours - self.hs1 - self.hs2)
		maxAvailableTime = 0
		print("available hours: %d" % availableHours)
		if subject.semester == 1: # semester 1
			maxAvailableTime = int((config.maxDiff - self.hs1 + availableHours + self.hs2)/2)
		else: # semester 2
			maxAvailableTime = int((config.maxDiff - self.hs2 + availableHours + self.hs1)/2)
		print("maxAvailableTime: %d" % maxAvailableTime)

		print(self.toJson())
		maxBlocks = maxAvailableTime // subject.pDuration
		# print("Available time: %d -> maxBlocks = %d" % (maxAvailableTime, maxBlocks))
		# print("pDuration: %d, pHours: %d, modulo: %d" % (subject.pDuration, subject.pHours, subject.pHours // subject.pDuration))
		# print("maxBlocks is %d of %d (%d/%d hours)" % (maxBlocks, subject.pHours // subject.pDuration, maxBlocks * subject.pDuration, subject.pHours))

		# randomize used blocks (may lead to greater number of teachers)
		effectiveHours = subject.pDuration * maxBlocks # maximum amount
		if config.randomizeEfficiency: # produce random choice instead
			effectiveHours = random.randrange(subject.pDuration, effectiveHours + 1, subject.pDuration)

		# update the subject practical hours with the hours this teacher is teaching
		if subject.pHours - effectiveHours < 0:
			effectiveHours = subject.pHours
		print("   effectiveHours after  %d" % effectiveHours)
		subject.pHours -= effectiveHours
		# update the teacher hours based on the semester
		if subject.semester == 1: # semester 1
			self.hs1 += effectiveHours
		else: # semester 2
			self.hs2 += effectiveHours
		print(self.toJson())

	def generateTeacher(config, field = None):
		if field is None:
			field = random.randint(1, config.nFields)
		t = Teacher(field) # create new teacher
		t.maxHours = random.choice(Teacher.types) # teacher maxHours is a random from the possible seniority steps
		return t

	def toJsonData(self, index):
		return {
			"name" : ("Teacher_%d - %s" % (index, names.get_full_name())),
			"type" : Teacher.types.index(self.maxHours) + 1,
			"diff" : self.hs1 - self.hs2,
			"field" : self.field
		}

class Config(Parent):
	def __init__(self, rounds=3, maxDiff=4, nFields=5, randomizeEfficiency=False, department="DEI"):
		self.rounds = rounds
		self.maxDiff = maxDiff
		self.nFields = nFields
		self.randomizeEfficiency = randomizeEfficiency # if True this can lead to more teachers being spawned, since their time is not allocated by the maximum amount possible, but randomly between block time and max, @see addPHoursMax
		self.department = department
