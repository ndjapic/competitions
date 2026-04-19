# Problem: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100 * 1000;
var
	n, m, t, i, j, s, ans: int32;
	ca, sa: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, t);

	for i := 1 to n do begin
		ca[i] := 0;
		sa[i] := 0;
	end;

	for j := 1 to m do begin
		read(i);
		readln(s);
		inc(ca[i]);
		inc(sa[i], s);
	end;

	ans := 0;
	for i := 1 to n do
		if (ca[i] > 0) and (sa[i] < t * ca[i]) then inc(ans);

	writeln(ans);
end.

```
