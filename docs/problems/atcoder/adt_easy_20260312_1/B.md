# Problem: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	n, c, l, r, ans: int32;
	t: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, c);
	read(t[1]);

	ans := 1;
	l := 1;
	for r := 2 to n do begin
		read(t[r]);
		if t[r] - t[l] >= c then begin
			inc(ans);
			l := r;
		end;
	end;
	readln;
	writeln(ans);
end.

```
