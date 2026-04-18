# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 100;
var
	n, i, m, j: int8;
	s, t: array [1 .. nn] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	m := 0;
	for i := 1 to n do begin
		readln(s[i]);
		m := max(m, length(s[i]));
	end;

	for j := 1 to m do begin
		i := 1;
		while length(s[i]) < j do inc(i);
		setlength(t[j], n-i+1);
		while i <= n do begin
			if j <= length(s[i]) then
				t[j][n-i+1] := s[i][j]
			else
				t[j][n-i+1] := '*';
			inc(i);
		end;
		writeln(t[j]);
	end;
end.

```
