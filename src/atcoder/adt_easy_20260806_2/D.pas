program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	n, m, i, j, i1, j1: int8;
	ans: boolean;
	s: array [1 .. NN] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n do readln(s[i]);

	for i1 := 9 to n do
		for j1 := 9 to m do begin

			ans := true;

			for i := i1-8 to i1-5 do
				for j := j1-8 to j1-5 do
					if (i = i1-5) or (j = j1-5) then
						ans := ans and (s[i][j] = '.')
					else
						ans := ans and (s[i][j] = '#');

			for i := i1-3 to i1 do
				for j := j1-3 to j1 do
					if (i = i1-3) or (j = j1-3) then
						ans := ans and (s[i][j] = '.')
					else
						ans := ans and (s[i][j] = '#');

			if ans then writeln(i1-8, ' ', j1-8);

		end;
end.
