# Задатак: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, m, q, i, j, k: int32;
	s, t: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, q);
	setlength(s, n);

	for i := 1 to n do s[i] := '?';

	for j := 1 to m do begin
		read(i);
		readln(s[i], s[i]);
	end;

	for k := 1 to q do begin
		readln(t);

		i := 1;
		while (i <= n) and ((s[i] = '?') or (s[i] = t[i])) do inc(i);

		if i > n then
			writeln('Yes')
		else
			writeln('No');
	end;
end.

```
