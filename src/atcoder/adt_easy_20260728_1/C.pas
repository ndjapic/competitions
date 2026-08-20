program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 99;
var
	n, m, i, j, x, y, mx: int8;
	space: boolean;
	s: array [1 .. NN] of string;
	p: array [1 .. NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(n, m);

	for i := 1 to n do begin
		readln(s[i]);
		p[i] := 0;
	end;

	for j := 1 to m do begin
		x := 0;
		y := 0;

		for i := 1 to n do
			case s[i][j] of
				'0': inc(x);
				'1': inc(y);
			end;

		if (x = 0) or (y = 0) then
			for i := 1 to n do inc(p[i])
		else if x < y then begin
			for i := 1 to n do
				if s[i][j] = '0' then inc(p[i]);
		end else {if x > y then} begin
			for i := 1 to n do
				if s[i][j] = '1' then inc(p[i]);
		end;
	end;

	mx := 0;
	for i := 1 to n do mx := max(mx, p[i]);

	space := false;
	for i := 1 to n do
		if p[i] = mx then begin
			if space then
				write(' ')
			else
				space := true;
			write(i);
		end;
	writeln;
end.
