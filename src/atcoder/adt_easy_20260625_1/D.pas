program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	MM = 99;
var
	n, m, i, j, x, y, mx: int8;
	space: boolean;
	s: array [1 .. MM] of string;
	c: array [1 .. MM, '0' .. '1'] of int8;
	score: array [1 .. MM] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for j := 1 to m do begin
		c[j, '0'] := 0;
		c[j, '1'] := 0;
	end;

	for i := 1 to n do begin
		readln(s[i]);
		score[i] := 0;
		for j := 1 to m do
			inc(c[j, s[i][j]]);
	end;

	for j := 1 to m do begin
		x := c[j, '0'];
		y := c[j, '1'];
		for i := 1 to n do
			case s[i][j] of
				'0': if (y = 0) or (x < y) then inc(score[i]);
				'1': if (x = 0) or (x > y) then inc(score[i]);
			end;
	end;

	mx := 0;
	for i := 1 to n do mx := max(mx, score[i]);

	space := false;
	for i := 1 to n do
		if score[i] = mx then begin
			if space then
				write(' ')
			else
				space := true;
			write(i);
		end;
end.
