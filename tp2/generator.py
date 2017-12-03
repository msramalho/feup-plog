import json
import pandas as pd
from sys import argv
import os

#print the json data to the console and perform minor validation tests
def printConfiguration(data):
    print("Department: %s" % data["department"])

    pdTeacherTypes = pd.DataFrame(data["teacherTypes"], columns=["id", "name", "averageWeekHours"])
    print("\nTeacher types (%d):" % len(data["teacherTypes"]))
    print(pdTeacherTypes.to_string(index=False))

    print("\nScientific fields (%d):" % len(data["scientificFields"]))
    print(pd.DataFrame(data["scientificFields"], columns=["id", "name"]).to_string(index=False))

    print("\nTeachers (%d):" % len(data["teachers"]))
    pdTeachers = pd.DataFrame(data["teachers"], columns=["id", "name", "type", "field", "HS1", "HS2"])
    print(pdTeachers.to_string(index=False))

    print("\nSubjects (%d):" % len(data["subjects"]))
    pdSubjects = pd.DataFrame(data["subjects"], columns=["id", "name", "semester", "HT", "HP", "durationT", "durationP" "field"])
    print(pdSubjects.to_string(index=False))

    ht = pdSubjects["HT"].sum()
    hp = pdSubjects["HP"].sum()
    print("\nTotal Theoretical: %dh" % ht)
    print("Total Practical  : %dh" % hp)

    s1 = pdSubjects.query("semester == 1")
    ht1 = s1["HT"].sum()
    hp1 = s1["HP"].sum()
    print("\nTotal Semester 1: %dh" % (ht1 + hp1))
    print("Total Theoretical Semester 1: %dh" % ht1)
    print("Total Practical   Semester 1: %dh" % hp1)

    s2 = pdSubjects.query("semester == 2")
    ht2 = s2["HT"].sum()
    hp2 = s2["HP"].sum()
    print("\nTotal Semester 2: %dh" % (ht2 + hp2))
    print("Total Theoretical Semester 2: %dh" % ht2)
    print("Total Practical   Semester 2: %dh" % hp2)

def testConfiguration(data):
    #create pandas DataFrame from json data
    pdTeacherTypes = pd.DataFrame(data["teacherTypes"], columns=["id", "name", "averageWeekHours"])
    pdTeachers = pd.DataFrame(data["teachers"], columns=["id", "name", "type", "field", "HS1", "HS2"])
    pdSubjects = pd.DataFrame(data["subjects"], columns=[ "id", "name", "semester", "HT", "HP", "field"])
    #merge teachers and subjects so as to extract joined data
    pdMergedTeachers = pd.merge(pdTeachers, pdTeacherTypes, left_on=['type'], right_on=['id'])

    #get total required hours for theoretical(HT) and pratical(HP) lessons
    ht = pdSubjects["HT"].sum()
    hp = pdSubjects["HP"].sum()
    #repeat, but for the 1st semester
    s1 = pdSubjects.query("semester == 1")
    ht1 = s1["HT"].sum()
    hp1 = s1["HP"].sum()
    #repeat, but for the 2nd semester
    s2 = pdSubjects.query("semester == 2")
    ht2 = s2["HT"].sum()
    hp2 = s2["HP"].sum()

    #check total hours semester 1
    maxHoursS1 = pdMergedTeachers["HS1"].sum()
    if maxHoursS1 < ht1 + hp1:
        print("Error: There are not enough teacher hours in the first semester (%dh) to cover the required hours (%dh), missing %dh" % (
            maxHoursS1, ht1 + hp1, ht1 + hp1 - maxHoursS1))
        return False

    #check total hours semester 2
    maxHoursS2 = pdMergedTeachers["HS2"].sum()
    if maxHoursS2 < ht2 + hp2:
        print("Error: There are not enough teacher hours in the second semester (%dh) to cover the required hours (%dh), missing %dh" % (
            maxHoursS2, ht2 + hp2, ht2 + hp2 - maxHoursS2))
        return False

    #falible but easy check on average validity, just tests if the sums match
    maxHours = pdMergedTeachers["averageWeekHours"].sum() * 2
    if maxHours != maxHoursS1 + maxHoursS2:
        print("Error: The total week hours sum (%dh) does not match the partial hours sum (%dh)" % (
            maxHours, maxHoursS1 + maxHoursS2))
        return False

    return True#if no test fails

#convert the json data into prolog predicates, with proper comments and format
def convertToProlog(data, filename):
    content = ("%% Autogenerated data file from JSON file: '%s'\n" % filename)
    content += "\n% scientificField(Field).\n"
    for field in data["scientificFields"]:
        content += ("scientificField(%d). %% %s\n" % (field["id"], field["name"]))

    content += "\n% subject(Subject, Semester, HT, HP, durationT, durationP, Fields).\n"
    for s in data["subjects"]:
        content += ("subject(%d, %d, %2d, %2d, %d, %d, %s). %% %s\n" % (s["id"], s["semester"], s["HT"], s["HP"], s["durationT"], s["durationP"], s["field"], s["name"]))

    content += "\n% teacherType(Type, AverageWeekHours).\n"
    for t in data["teacherTypes"]:
        content += ("teacherType(%d, %2d). %% %s\n" % (t["id"], t["averageWeekHours"], t["name"]))

    content += "\n% teacher(Teacher, Type, HS1, HS2, Fields)."
    for t in data["teachers"]:
        content += ("\nteacher(%d, %d, %2d, %2d, %s). %% %s" % (t["id"], t["type"], t["HS1"], t["HS2"], t["field"], t["name"]))
    return content

#write contents to a file, warns if it already exists
def writeFileWarnDuplicate(filename, contents):
    if os.path.isfile(filename):
        print("----[WARNING]: %s already exists, either it was not deleted or our name clear rules had a collison" % filename)
    with open(filename, 'w', encoding="utf-8") as f:#write the new main
        f.write(contents)

def generatePrologForFile(filename, print = False, ouput = "src/data.pl"):
    with open(filename, 'r', encoding="utf-8") as jsonFile:
        data = json.loads(jsonFile.read())
        if print:
            printConfiguration(data)
        if testConfiguration(data):
            dataFile = open(ouput, 'w', encoding="utf-8")
            prologCode = convertToProlog(data, filename)
            dataFile.write(prologCode)
            dataFile.close()

if __name__ == "__main__":
    if len(argv) < 2:
        print("Plog Generator usage is `python %s filename.json`\n" % argv[0])
        exit()
    generatePrologForFile(argv[1], True)
