# Problem: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, m, k, i, a, b, ans: int32;
	know: array [1 .. nn] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, k);

	for i := 1 to k do know[i] := true;
	for i := k+1 to n do know[i] := false;

	for i := 1 to m do begin
		readln(a, b);
		know[a] := know[a] or know[b];
		know[b] := know[a];
	end;

	ans := 0;
	for i := 1 to n do
		if know[i] then inc(ans);

	writeln(ans);
end.

```
