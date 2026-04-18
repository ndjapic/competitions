# Задатак: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 1000;
var
	n, m, i, j, t, ans: int32;
	s: array [1 .. nn] of int32;
	seen: array [0 .. nn] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n do readln(s[i]);

	for t := 0 to nn do seen[t] := false;

	for j := 1 to m do begin
		readln(t);
		seen[t] := true;
	end;

	ans := 0;
	for i := 1 to n do
		if seen[s[i] mod nn] then inc(ans);

	writeln(ans);
end.

```
