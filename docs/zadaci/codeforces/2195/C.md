# Задатак: C.pas

```pascal
program _C;
{$OPTIMIZATION LEVEL3,ON}
const
	nn = 300 * 1000;
var
	notc, tci, n, i, ans: int32;
	a: array [1 .. nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do read(a[i]); readln;

		i := 2;
		ans := 0;
		while i <= n do
			if (a[i] = a[i-1]) or (a[i] + a[i-1] = 7) then begin
				inc(ans);
				inc(i, 2);
			end else
				inc(i);

		writeln(ans);

	end;
end.

```
