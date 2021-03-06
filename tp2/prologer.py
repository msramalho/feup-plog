import json
from sys import argv
import os

# try to import pandas
try:
	import pandas as pd
except ImportError:
	pd = False

# print the json data to the console and perform minor validation tests


def printConfiguration(data):
	if not pd:
		print("Cannot print configuration without installing pandas")
		return
	print("Department: %s" % data["department"])

	# print teacher types in a table
	pDTeacherTypes = pd.DataFrame(data["teacherTypes"], columns=["averageWeekHours"])
	pDTeacherTypes.index += 1
	print("\nTeacher types (%d):" % len(pDTeacherTypes))
	print(pDTeacherTypes)

	# print scientific types in a table
	pdFields = pd.DataFrame(data["fields"])
	pdFields.index += 1
	print("\nScientific fields (%d):" % len(pdFields))
	print(pdFields)

	# print teacher information in a table
	pdTeachers = pd.DataFrame(data["teachers"], columns=["name", "type", "field", "diff"])
	pdTeachers.index += 1
	print("\nTeachers (%d):" % len(pdTeachers))
	print(pdTeachers)

	# print subject information in a table
	pdSubjects = pd.DataFrame(data["subjects"], columns=["name", "semester", "HT", "HP", "DT", "DP", "field"])
	pdSubjects.index += 1
	print("\nSubjects (%d):" % len(data["subjects"]))
	print(pdSubjects)

	# statistics
	print("\n%sStatistics%s" % ("-" * 40, "-" * 40))

	# print hours sum summary
	ht = pdSubjects["HT"].sum()
	hp = pdSubjects["HP"].sum()
	print("\n%30s: %4dh" % ("Theoretical", ht))
	print("%30s: %4dh" % ("Practical", hp))
	print("%30s: %4dh" % ("Total", ht + hp))
	# semester 1
	s1 = pdSubjects.query("semester == 1")
	ht1 = s1["HT"].sum()
	hp1 = s1["HP"].sum()
	print("\n%30s: %4dh" % ("Theoretical Semester 1", ht1))
	print("%30s: %4dh" % ("Practical Semester 1", hp1))
	print("%30s: %4dh" % ("Semester 1", ht1 + hp1))
	# semester 2
	s2 = pdSubjects.query("semester == 2")
	ht2 = s2["HT"].sum()
	hp2 = s2["HP"].sum()
	print("\n%30s: %4dh" % ("Theoretical Semester 2", ht2))
	print("%30s: %4dh" % ("Practical Semester 2", hp2))
	print("%30s: %4dh" % ("Semester 2", ht2 + hp2))

	# teacher stats
	mergedT = pd.merge(pdTeachers, pDTeacherTypes, how='left',
					   left_on="type", right_index=True)
	avgDiff = mergedT["diff"] / 2
	mergedT["HS1"] = mergedT["averageWeekHours"] + avgDiff
	mergedT["HS2"] = mergedT["averageWeekHours"] - avgDiff
	# print(mergedT)
	hs1 = sum(mergedT["HS1"])
	hs2 = sum(mergedT["HS2"])
	print("\n%30s: %4dh" % ("Teacher", hs1 + hs2))
	print("%30s: %4dh" % ("Teacher Semester 1", hs1))
	print("%30s: %4dh" % ("Teacher Semester 2", hs2))

	print("\n%30s: %4dh" % ("Unused Teacher", (hs1 + hs2) - (hp + ht)))
	print("%30s: %4dh" % ("Unused Teacher Semester 1", hs1 - (hp1 + ht1)))
	if hs1 - (hp1 + ht1) < 0:  # check if semester 1 has enought teacher hours
		print("--[ERROR] Semester 1 needs more teacher hours")
	print("%30s: %4dh" % ("Unused Teacher Semester 2", hs2 - (hp2 + ht2)))
	if hs2 - (hp2 + ht2) < 0:  # check if semester 2 has enought teacher hours
		print("--[ERROR] Semester 2 needs more teacher hours")

	# check that the hours of teachers, in each semester, are enough for each field
	# teachers table merged by field, HS1 and HS2 are the sum foreach field
	mergedT = mergedT.drop(['averageWeekHours', 'diff', 'type'], 1)
	mergedT = mergedT.groupby(["field"]).sum().reset_index()
	# subjects table has new column that is total number of hours for a sibject
	pdSubjects["Total"] = pdSubjects["HT"] + pdSubjects["HP"]

	for i in range(1, 3):  # test hours for semester i
		print("\n%sSEMESTER %d" % ("-"*40, i))
		mergedSSX = pdSubjects[pdSubjects.semester == i].groupby(
			["field"]).sum().reset_index().drop(['semester'], 1)
		# print(mergedSSX)
		otherSemester = ("HS%d" %  (1 if i == 2 else 2))
		mergedTSX = mergedT.drop([otherSemester], 1)
		# print(mergedTSX)
		merged = pd.merge(mergedSSX, mergedTSX, on="field")
		# print(merged)
		# for index, row in merged.iterrows():
		# 	if row["Total"] > row["HS%d"%i]:
		# 		print("--[WARNING] SEMESTER %d - FIELD %d has more hours than teachers, which means that the solution will never be able to give only teachers of the field (Required - Available = %.2f)" % (i, row["field"], row["Total"] - row["HS%d"%i]))


# convert the json data into prolog predicates, with proper comments and format
def convertToProlog(data, filename, tabled = False):
	content = ("%% Autogenerated data file from JSON file: '%s'\n" % filename)

	# print field
	''' content += "\n% field(Field).\n"
	for field in data["fields"]:
		content += ("%%field(%d). %% %s\n" % (i, field["name"])) '''
	content += "\nfields(%d). %% count fields\n" % len(data["fields"])

	# print subject
	content += "\n% subject(Semester,  HT,  HP,  DT,  DP,  Field).\n"
	if tabled:
		subjectPrint = "subject(%10d, %3d, %3d, %3d, %3d, %6s). %% %s\n"
	else:
		subjectPrint = "subject(%d, %2d, %2d, %d, %d, %2s). %% %s\n"
	for s in data["subjects"]:
		content += (subjectPrint % (s["semester"], s["HT"], s["HP"], s["DT"], s["DP"], s["field"], s["name"]))
	# content += "\nsubjects(%d). %% count subjects\n" % len(data["subjects"])

	# print teacher types
	content += "\n% teacherType(Type, AverageWeekHours).\n"
	i = 0
	for t in data["teacherTypes"]:
		i += 1
		content += ("teacherType(%d, %2d).\n" % (i, t["averageWeekHours"]))
	# content += "\nteacherTypes(%d). %% count teacher types\n" % len(data["teacherTypes"])

	# print teacher information
	content += "\n% teacher(Type, diff, Field).\n"
	if tabled:
		teacherPrint = "teacher(%6d, %4d, %5s). %% %2s - %2s %s\n"
	else:
		teacherPrint = "teacher(%d, %2d, %s). %% %2s - %2s %s\n"
	for t in data["teachers"]:
		hs1 = t["hs1"] if "hs1" in t else "_"
		hs2 = t["hs2"] if "hs2" in t else "_"
		content += (teacherPrint % (t["type"], t["diff"], t["field"], hs1, hs2, t["name"]))
	# content += "\nteachers(%d). %% count teachers" % len(data["teachers"])
	return content

# write contents to a file, warns if it already exists


def writeFileWarnDuplicate(filename, contents):
	if os.path.isfile(filename):
		print("----[WARNING]: %s already exists, either it was not deleted or our name clear rules had a collison" % filename)
	with open(filename, 'w', encoding="utf-8") as f:  # write the new main
		f.write(contents)


def generatePrologForFile(filename, print=False, output="src/data.pl", tabled = False):
	with open(filename, 'r', encoding="utf-8") as jsonFile:
		data = json.loads(jsonFile.read())
		if print:
			printConfiguration(data)
		dataFile = open(output, 'w', encoding="utf-8")
		prologCode = convertToProlog(data, filename, tabled)
		dataFile.write(prologCode)
		dataFile.close()


if __name__ == "__main__":
	if len(argv) < 2:
		print("Plog Generator usage is `python %s filename.json`\n" % argv[0])
		exit()
	generatePrologForFile(argv[1], True)
