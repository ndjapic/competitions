# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, ans: int32;
	k, a, x: uint64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);
	x := 0;
	ans := 0;

	for i := 1 to n do begin
		read(a);
		if a and k = a then begin
			x := x or a;
			inc(ans);
		end;
	end;
	readln;

	if x < k then ans := -1;
	writeln(ans);
end.

```
