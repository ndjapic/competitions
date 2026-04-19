# Problem: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, i: int32;
	total, avg, dif, ans: int64;
	a: array [1 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	total := 0;
	for i := 1 to n do begin
		read(a[i]);
		inc(total, a[i]);
	end;
	readln;

	if total mod n > 0 then
		ans := -1
	else begin

		ans := 0;
		avg := total div n;
		for i := 1 to n-1 do begin
			dif := a[i] - avg;
			inc(ans, abs(dif));
			dec(a[i], dif);
			inc(a[i+1], dif);
		end;

	end;

	writeln(ans);
end.

```
