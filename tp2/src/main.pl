:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(system)).

:- include('data.pl').
:- include('utils.pl').

%run congruence tests on the supplied data
test:-
    testMatcHTecherTypes.

%main function
init(Subjects, Teachers):-
% 1 - variable definition + 2 - domain specification
    defineTeachers(Teachers),
    defineSubjects(Subjects),

% 3 - restrictions application
    %Teachers preference on the diference
    restrictTeachers(Teachers),
    length(Teachers, NTeachers), %get the number of teachers
    restrictSubjects(Subjects, NTeachers, Teachers),

% 4 - search for solutions
    getTeachersVariablesToLabel(Teachers, TVars),
    labeling([], TVars).


% variable definition helpers
defineSubjects(Subjects):-
    findall([Semester-Field-HT-HP-DT-DP, _, _], subject(Semester, HT, HP, DT, DP, Field), Subjects).
defineTeachers(Teachers):-
    findall(Avg-Diff-Field-_HS1-_HS2, (teacher(Type, Diff, Field), teacherType(Type, Avg)), Teachers).

%restrictions on lists helpers
restrictTeachers([]).
restrictTeachers([Avg-Diff-_Field-HS1-HS2|T]):-
    MaxHours is Avg * 2, % the maximum amount of hours for a teacher in a semester = 2 * avg (extreme cases)
    domain([HS1, HS2], 0, MaxHours), % set the domain for the hours in each semester
    Diff #= HS1 - HS2, % Restriction-1
    Avg #=< (HS1 + HS2) / 2, % Restriction-2 (relaxed)
    %recursive call
    restrictTeachers(T).

restrictSubjects([], _, _).
restrictSubjects([[_Semester-_Field-HT-HP-DT-DP, TT, TP]|R], NTeachers, Teachers):-
    % number of teachers needed for Theoretical (Total Hours/Division)
    length(TT, NTeachers),
    sum(TT, #=, HT),
    generateFdset(0, HT, DT, TFdset),
    domainFdset(TT, TFdset),
    % number of teachers needed for Pratical (Total Hours/Division)
    length(TP, NTeachers),
    sum(TP, #=, HP),
    generateFdset(0, HT, DP, PFdset),
    domainFdset(TP, PFdset),
    %restrict the domain of the subjects to the total number of teachers
    domain(TT, 0, HT),
    domain(TP, 0, HP),

    %restrict the number of hours by the teachers
    % restrictHoursTeachers(HP-DT, HP-DP, Teachers),

    %field restrictions
    % restrictSubjectField(Field, TT, Teachers)

    %recursive call
    restrictSubjects(R, NTeachers, Teachers).

%generate an fdset with a start, end and step
generateFdset(Min, Max, Step, Fdset):-
    generateList(Min, Max, Step, List),
    list_to_fdset(List, Fdset).
%generate a list with a start, end and step
generateList(Max, Max, _Step, [Max]).
generateList(Min, Max, Step, NewList):-
    NewMin is Min + Step,
    generateList(NewMin, Max, Step, List),
    NewList = [Min|List].

%apply a domain restriction to every element of a list, namely in_set
domainFdset([], _).
domainFdset([N|R], Fdset):-
    N in_set Fdset,
    domainFdset(R, Fdset).

%labeling helpers
getTeachersVariablesToLabel([], []).
getTeachersVariablesToLabel([_Avg-_Diff-_Field-HS1-HS2|T], TVars):-
    getTeachersVariablesToLabel(T, TempTVars),
    append(TempTVars, [HS1, HS2], TVars).

/*
Everything is 1 indexed
Modeling:
    Subjects = [[Semester-Field-HT-HP-DT-DP, TT, TP], ...]
        Semester - The semester where this Subject occurs
        Field - The field of stufy for this subject
        HT - Hours in Theoretical classes
        HP - Hours in Pratical classes
        DT - Duration of Theoretical classes
        DP - Duration of Pratical classes
        TT - LIST Aligned with Teachers - hours that teach Teacher gives in theoretical lessons
        TP - LIST Aligned with Teachers - hours that teach Teacher gives in pratical lessons
        The id of the subject is its index in the Subjects list

    Teachers = [Avg-Diff-Field-HS1-HS2, ..]
        Avg - The average week hours a teacher from this type should teach
        Diff - the preference the teacher has shown towards having more hours on the 1st semester (positive value), on the 2nd semester (negative value) or a balanced schedule between semesters (zero)
        Field - The field of specialization for this teacher
        HS1 - Number of hours this teacher is giving during the 1st semester
        HS2 - Number of hours this teacher is giving during the 2nd semester
        The id of the subject is its index in the Subjects list

Restrictions:
    1. A teacher chooses the acceptable Diff value for the difference in hours from the 1st to the 2nd semesters;
    2. According to their type, teachers will need to teach an average week teaching time over the two semesters that is approximate to the stipulated values.

    . Theoretical Hours must be lectured by teachers that have the same field
    . Pratical lessons should be given by teachers of that area, prefearbly (minimize the number of teachers that teach something they "do not know")

    . maximize the number of teachers with their preferences respected


TODO:
    . Organize generator so that it only generates relevant information and in the same order as it is used in the prolog code

 */