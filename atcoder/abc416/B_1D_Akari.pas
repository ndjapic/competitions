program B_1D_Akari;
{$MODE DELPHI}
var
	n, i: int8;
	s: string;

begin
	readln(s);
	n := length(s);

	for i := 1 to n do
		if (s[i] = '.') and ((i = 1) or (s[i-1] = '#')) then s[i] := 'o';

	writeln(s);
end.
