uses
	sysutils, strutils; // Додата библиотека sysutils

var
	s, t, line: string;
	n, m: int32;
	sa: TStringArray; // Специјалан тип низа за стрингове

begin
	readln(line); // Учиташ цео ред одједном

	// Дели ред на делове свуда где је размак
	sa := SplitString(line, ' '); 

	s := sa[0]; // Први стринг
	t := sa[1]; // Други стринг

	n := length(s);
	m := length(t);
end.
