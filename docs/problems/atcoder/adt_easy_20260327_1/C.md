# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	n = 8;
var
	i, j, ans: int8;
	row, col: array [1 .. n] of boolean;
	s: array [1 .. n] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	for i := 1 to n do begin
		row[i] := false;
		col[i] := false;
	end;

	for i := 1 to n do begin
		readln(s[i]);
		for j := 1 to n do
			if s[i][j] = '#' then begin
				row[i] := true;
				col[j] := true;
			end;
	end;

	ans := 0;
	for i := 1 to n do
		for j := 1 to n do
			if not (row[i] or col[j]) then inc(ans);

	writeln(ans);
end.

```
