# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 1000;
var
	n, i, j: int32;
	correct: boolean;
	a: array [1 .. nn] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	correct := true;
	for i := 1 to n do begin
		readln(a[i]);
		for j := 1 to i-1 do
			if correct then begin
				if a[i][j] = 'W' then
					correct := a[j][i] = 'L'
				else if a[i][j] = 'L' then
					correct := a[j][i] = 'W'
				else if a[i][j] = 'D' then
					correct := a[j][i] = 'D';
			end;
	end;

	if correct then
		writeln('correct')
	else
		writeln('incorrect');
end.

```
