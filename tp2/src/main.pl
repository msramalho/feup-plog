:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(between)).
:- use_module(library(timeout)).

:- include('data.pl').
:- include('utils.pl').

%run congruence tests on the supplied data
test(Res, TimeOutRes):-
    time_out(setof(FailedHours-Teachers-Subjects, init(Subjects, Teachers, FailedHours), Res), 5000, TimeOutRes).

%main function
init(Subjects, Teachers):-
    clear,
% 1 - variable definition + 2 - domain specification
    defineTeachers(Teachers),
    defineSubjects(Subjects),
    % PreferenceFailedCount in 2..20,
    % FailedHours in 0..100,
    write('1\n'),

% 3 - restrictions application
    %Teachers preference on the diference
    restrictTeachers(Teachers),
    write('2\n'),
    length(Teachers, NTeachers), %get the number of teachers
    write('3\n'),
    getMatrixOfComplementedFields(Teachers, CompFields),
    write('4\n'),
    restrictSubjects(Subjects, CompFields, NTeachers, PreferenceFailedCount),
    % write(PreferenceFailedCount), nl,
    write('5\n'),
    restrictSumBySemester(Subjects, Teachers),
    write('6\n'),
    getFailedHours(Teachers, FailedHours),
    write('7\n'),
    %restriction to force enough teacher hours to exist
    % write(FailedHours), nl,

    %generate heuristic to optimize

% 4 - search for solutions
    getTeachersVariablesToLabel(Teachers, TVars),
    write('8\n'),
    mergeSubjectTs(Subjects, SVars),
    append(TVars, SVars, Vars),
    write('\n...................................\n'),
    write(Vars),
    write('\n...................................\n'),

    % element(22, Vars, First),
    % indomain(First),
    % format('First is ~p\n', First).

    % labeling([], Vars).
    % labeling([minimize(FailedHours)], Vars).
    time_out(labeling([minimize(FailedHours)], Vars), 3000, Res), write('Res is: \n'), write(Res).
    % labeling([], Vars).
    % time_out(labeling([minimize(PreferenceFailedCount)], Vars), 6000, Res), write('Res is: \n'), write(Res).


%------------------------------------variable definition helpers
defineSubjects(Subjects):-
    findall([Semester-Field-HT-HP-DT-DP, _, _], subject(Semester, HT, HP, DT, DP, Field), Subjects).
defineTeachers(Teachers):-
    findall(Avg-Diff-Field-_HS1-_HS2, (teacher(Type, Diff, Field), teacherType(Type, Avg)), Teachers).

%get a list with the TT and TP for all Subjects (of a Semester) in a List
getSubjectsTimesBySemester([], _, [EmptyList], NTeachers):-
    emptyList(EmptyList, NTeachers, 0).
getSubjectsTimesBySemester([[Semester-_Field-_HT-_HP-_DT-_DP, TT, TP]|R], Semester, NewTimes, NTeachers):-
    getSubjectsTimesBySemester(R, Semester, Times, NTeachers),
    append([TT, TP], Times, NewTimes).
%skip if the semester does not match
getSubjectsTimesBySemester([[OtherSemester-_Field-_HT-_HP-_DT-_DP, _TT, _TP]|R], Semester, Times, NTeachers):-
    OtherSemester #\= Semester,
    getSubjectsTimesBySemester(R, Semester, Times, NTeachers).

%get a list with all the HS1. findall would not work
getTeachersHoursSemester1([], []).
getTeachersHoursSemester1([_Avg-_Diff-_Field-HS1-_HS2|R], [HS1|LHS]):-
    getTeachersHoursSemester1(R, LHS).
%get a list with all the HS2. findall would not work
getTeachersHoursSemester2([], []).
getTeachersHoursSemester2([_Avg-_Diff-_Field-_HS1-HS2|R], [HS2|LHS]):-
    getTeachersHoursSemester2(R, LHS).


%get a matrix with NFields lines, where each line has NTeachers elements, where each cell is 1 or 0 (1 if the corresponding teacher DOES NOT teach this field) - this worked on the first attempt - self-five
getMatrixOfComplementedFields(Teachers, CompFields):-
    length(Teachers, NTeachers),
    fields(NFields), %the number of fields
    numlist(1, NFields, Fields),%list with all the numbers from 1 to NFields
    findall(L, (
        length(L, NTeachers), %set the length for this line
        member(Field, Fields), %get the field matching this line
        findall(Complement, (
            member(_Avg-_Diff-TField-_HS1-_HS2, Teachers), %get the teacher Field
            Field #\= TField #<=> Complement % get the complement
        ), L)
    ), CompFields).



getFailedHours([], 0).
getFailedHours([Avg-_Diff-_Field-HS1-HS2|T], FailedHours):-
    getFailedHours(T, TempFailedHours),
    FailedHours #= TempFailedHours + (Avg * 2) - (HS1 + HS2).

/*
getFailedHours(Teachers, FailedHours):-
    findall(D, (
        member(Avg-_Diff-_Field-HS1-HS2, Teachers),
        D #= (Avg * 2) - (HS1 + HS2)
    ), FailedHoursList),
    % write(FailedHoursList),
    maximum(FailedHours, FailedHoursList).
    % sum(FailedHoursList, #=, FailedHours). */

%get a list of all the variables in TT and TP so that we can label them
mergeSubjectTs([], []).
mergeSubjectTs([[_, TT, TP]|T], Merged):-
    mergeSubjectTs(T, TempMerged),
    append([TT, TP, TempMerged], Merged).



%------------------------------------restrictions on lists helpers
restrictTeachers([]).
restrictTeachers([Avg-Diff-_Field-HS1-HS2|T]):-
    MaxHoursS1 is round(((2 * Avg)+Diff)/2), % the maximum amount of hours for the teacher in the 1st semester
    MaxHoursS2 is round(((2 * Avg)-Diff)/2), % - Diff, % the maximum amount of hours for the teacher in the 2nd semester
	MaxHours is Avg * 2, %in case diff exceeds the number of hours
    domain([HS1, HS2], 0, MaxHours), % set the domain for the hours in each semester
    HS1 in 0..MaxHoursS1,
    HS2 in 0..MaxHoursS2,
	% HS1 #> 0 #\/ HS2 #> 0, # at least one must be greater than 0
    Diff #= HS1 - HS2, % Restriction-1
    2 * Avg #>= HS1 + HS2, % Restriction-2 (relaxed)
    %recursive call
    restrictTeachers(T).

restrictSubjects([], _, _, 0).
restrictSubjects([[_Semester-Field-HT-HP-DT-DP, TT, TP]|R], CompFields, NTeachers, PreferenceFailedCount):-
    %recursive call
    restrictSubjects(R, CompFields, NTeachers, TempCount),
    %assert typical standards HP > HT
    HP #> HT,
    % number of teachers needed for Theoretical (Total Hours/Division)
    length(TT, NTeachers),
    sum(TT, #=, HT),
    generateFdset(0, HT, DT, TFdset),
    domainFdset(TT, TFdset),
    % number of teachers needed for Practical (Total Hours/Division)
    length(TP, NTeachers),
    sum(TP, #=, HP),
    generateFdset(0, HP, DP, PFdset),
    domainFdset(TP, PFdset),
    %restrict the domain of the subjects to the total number of teachers
    domain(TT, 0, HT),
    domain(TP, 0, HP),
    %restrict to only allow teachers of the field in the Theoretical lessons
    nth1(Field, CompFields, FieldComplements),
    scalar_product(FieldComplements, TT, #= , 0), %Restriction-3,
    scalar_product(FieldComplements, TP, #= , CurrentCount), % minimize this, Restriction-4
    PreferenceFailedCount #= TempCount + CurrentCount.


%make sure the sum of the times for each subject, in both semesters, match that of the teachers
restrictSumBySemester(Subjects, Teachers):-
    length(Teachers, NTeachers),
    %semester 1
    getSubjectsTimesBySemester(Subjects, 1, MatrixTimesS1, NTeachers),%matrix like [TT1,TP1,TT2,TP2,TT3,...]
    scalarSumMatrix(MatrixTimesS1, TimesS1),%sum every line in the matrix into TimesS
    getTeachersHoursSemester1(Teachers, LHS1), %!,
    restrictEqualLists(TimesS1, LHS1),

    %semester 2
    getSubjectsTimesBySemester(Subjects, 2, MatrixTimesS2, NTeachers),%matrix like [TT1,TP1,TT2,TP2,TT3,...]
    scalarSumMatrix(MatrixTimesS2, TimesS2),%sum every line in the matrix into TimesS
    getTeachersHoursSemester2(Teachers, LHS2), %!,
    restrictEqualLists(TimesS2, LHS2).

    % restrictTeacherSemester2(Teachers, TimesS2).%match the teacher's time with the corresponding cell


/* %make sure the sum of hours of a teacher in the first semester matches his/her HS1
restrictTeacherSemester1(_, []):-!.
restrictTeacherSemester1([], []).
restrictTeacherSemester1([_Avg-_Diff-_Field-Sum-_HS2|T1], [Sum|T2]):-restrictTeacherSemester1(T1, T2).
%make sure the sum of hours of a teacher in the first semester matches his/her HS2
restrictTeacherSemester2(_, []):-!.
restrictTeacherSemester2([], []).
restrictTeacherSemester2([_Avg-_Diff-_Field-_HS1-Sum|T1], [Sum|T2]):-restrictTeacherSemester2(T1, T2). */


%------------------------------------labeling helpers
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
        HP - Hours in Practical classes
        DT - Duration of Theoretical classes
        DP - Duration of Practical classes
        TT - LIST Aligned with Teachers - hours that teach Teacher gives in theoretical lessons
        TP - LIST Aligned with Teachers - hours that teach Teacher gives in practical lessons
        The id of the subject is its index in the Subjects list

    Teachers = [Avg-Diff-Field-HS1-HS2, ..]
        Avg - The average week hours a teacher from this type should teach
        Diff - the preference the teacher has shown towards having more hours on the 1st semester (positive value), on the 2nd semester (negative value) or a balanced schedule between semesters (zero), Diff = HS1 - HS2
        Field - The field of specialization for this teacher
        HS1 - Number of hours this teacher is giving during the 1st semester
        HS2 - Number of hours this teacher is giving during the 2nd semester
        The id of the subject is its index in the Subjects list

Restrictions:
    1. A teacher chooses the acceptable Diff value for the difference in hours from the 1st to the 2nd semesters;
    2. According to their type, teachers will need to teach an average week teaching time over the two semesters that is approximate to the stipulated values.
    3. Theoretical Hours must be lectured by teachers that have the same field
    4. Practical lessons should be given by teachers of that area, prefearbly (minimize the number of teachers that teach something they "do not know")

    . maximize the number of teachers with their preferences respected


TODO:
    . Organize generator so that it only generates relevant information and in the same order as it is used in the prolog code
    . Tabela de opções de labeling usadas para o relatório
    . ver fd_statistics, (numero de backtracks, ...)
    . ver timeout
    . gráfico de evolução do tempo com o aumento da complexidade
 */