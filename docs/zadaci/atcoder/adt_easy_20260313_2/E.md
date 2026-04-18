# Задатак: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Math;
const
	nn = 1000 * 1000;
var
	n, i, j, k: int32;
	ans: int64;
	s: string;
	link: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	link[1] := 0;
	for i := 2 to n do
		if s[i-1] = s[i] then
			link[i] := link[i-1]
		else
			link[i] := i-1;

	ans := 0;
	i := n;
	j := link[i];
	while j > 0 do begin
		k := link[j];
		if ord(s[i]) - ord(s[j]) = 1 then
			inc(ans, min(i-j, j-k));
		i := j;
		j := k;
	end;

	writeln(ans);
end.

```
