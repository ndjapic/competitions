# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 200 * 1000;
var
	n, i, l, r, a, b: int32;
	ans, k: int64;
	sa, sb: array [0 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	sa[0] := 0;
	for i := 1 to n do begin
		read(a);
		sa[i] := sa[i-1] + a;
	end;
	readln;

	sb[0] := 0;
	for i := 1 to n do begin
		read(b);
		sb[i] := sb[i-1] + b;
	end;
	readln;

	l := 0;
	ans := 0;
	for r := 1 to n do begin
		while sb[r] - sb[l] > k do inc(l);
		ans := max(ans, sa[r] - sa[l]);
	end;

	writeln(ans);
end.

```
