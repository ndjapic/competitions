# Задатак: E.pas

```pascal
program _E;
{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 3105;
var
	notc, tci, n, i, j, ans: int32;
	a: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		ans := 0;
		for i := 1 to n do begin
			read(a[i]);
			{while a[i] > 0 do begin
				write(a[i] mod 2);
				a[i] := a[i] div 2;
			end;
			writeln;}
			for j := 1 to i-1 do ans := max(ans, a[i] xor a[j]);
		end;
		readln;

		writeln(ans);

	end;
end.

```
