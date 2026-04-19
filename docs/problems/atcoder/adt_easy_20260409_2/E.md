# Problem: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	aa = 1000 * 1000;
var
	n, i, a, ans: int32;
	seen: array [1 .. aa] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for a := 1 to aa do seen[a] := 0;

	ans := n+1;
	for i := 1 to n do begin
		read(a);
		if seen[a] > 0 then ans := min(ans, i - seen[a] + 1);
		seen[a] := i;
	end;
	readln;

	if ans > n then ans := -1;
	writeln(ans);
end.

```
