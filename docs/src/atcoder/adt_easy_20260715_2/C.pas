program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	N = 8;
var
	i, j, x, y: int8;
	s: string;
	row, col: array [1 .. N] of char;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	for i := 1 to N do begin
		row[i] := '.';
		col[i] := '.';
	end;

	for i := 1 to N do begin
		readln(s);
		for j := 1 to N do
			if s[j] = '#' then begin
				row[i] := '#';
				col[j] := '#';
			end;
	end;

	x := 0;
	y := 0;
	for i := 1 to N do begin
		if row[i] = '.' then inc(x);
		if col[i] = '.' then inc(y);
	end;

	writeln(x * y);
end.
