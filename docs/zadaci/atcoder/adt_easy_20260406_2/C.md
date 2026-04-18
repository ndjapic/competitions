# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 50;
var
	n, m, i, j, a, b: int8;
	found: boolean;
	s, t:  array [1 .. nn] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n do readln(s[i]);
	for i := 1 to m do readln(t[i]);

	for a := 1 to n-m+1 do
		for b := 1 to n-m+1 do begin
			found := true;
			for i := 1 to m do
				for j := 1 to m do
					found := found and (s[a+i-1][b+j-1] = t[i][j]);
			if found then writeln(a, ' ', b);
		end;
end.

```
