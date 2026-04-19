# Problem: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 100;
var
	n, m, i, j, k: int8;
	s: array [1 .. nn] of string;
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

	for i := 1 to n do begin
		k := (m - length(s[i])) div 2;
		for j := 1 to k do write('.');
		write(s[i]);
		for j := 1 to k do write('.');
		writeln;
	end;
end.

```
