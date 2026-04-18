program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 100;
var
	n, m, i, j, x, y: int8;
	tak: boolean;
	s: array [1 .. nn] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n do readln(s[i]);

	for x := 9 to n do
		for y := 9 to m do begin
			tak := true;

			if tak then
				for i := x-8 to x-6 do
					if tak then
						for j := y-8 to y-6 do
							tak := tak and (s[i][j] = '#');

			if tak then
				for i := x-8 to x-6 do
					tak := tak and (s[i][y-5] = '.');

			if tak then
				for j := y-8 to y-6 do
					tak := tak and (s[x-5][j] = '.');

			if tak then
				for i := x-2 to x-0 do
					if tak then
						for j := y-2 to y-0 do
							tak := tak and (s[i][j] = '#');

			if tak then
				for i := x-2 to x-0 do
					tak := tak and (s[i][y-3] = '.');

			if tak then
				for j := y-2 to y-0 do
					tak := tak and (s[x-3][j] = '.');

			if tak then writeln(x-8, ' ', y-8);
		end;
end.
