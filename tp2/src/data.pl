% Autogenerated data file from JSON file: 'data/mieic_a3_s1.json'

% scientificArea(Area).
scientificArea(1). % Artificial Intelligence
scientificArea(2). % Computer Networks
scientificArea(3). % Algorithms
scientificArea(4). % Computer Graphics
scientificArea(5). % Web
scientificArea(6). % Software Engineering

% subject(Subject, Semester, HT, HP, Areas).
subject(1, 1,  2,  8, [1, 3]). % Plog
subject(2, 1,  2, 12, [2, 3]). % Rcom
subject(3, 1,  2,  8, [5, 6]). % LTW
subject(4, 1,  2,  8, [4]). % LAIG
subject(4, 1,  2,  8, [6]). % ESOF

% teacherType(Type, AverageWeekHours).
teacherType(1,  7). % Gownsman
teacherType(2,  8). % Associate
teacherType(3,  9). % Auxiliar

% teacher(Teacher, Type, HS1, HS2, Areas).
teacher(1, 1,  8,  6, [1, 2, 3]). % Álvaro Alves
teacher(2, 2,  8,  8, [1, 3, 6]). % Bernardo Belo
teacher(3, 2, 10,  6, [4, 5, 6]). % Carlos Celestino
teacher(4, 3, 10,  8, [1, 2, 4]). % Daniel Damásio
teacher(5, 3,  9,  9, [2, 4, 5, 6]). % Eduardo Ernesto
teacher(6, 3,  9,  9, [1, 3, 4, 6]). % Francisco Fanhoso