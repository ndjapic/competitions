# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 200 * 1000 + 1;
var
	n, k, t, c, i: int32;
	ans, d: int64;
	a: array [1 .. nn] of int32;
	op: array [1 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k, t, c);

	for i := 1 to n do begin
		read(a[i]);
		op[i] := 0;
	end;
	readln;

	ans := 0;
	for i := 1 to n do begin
		inc(a[i], op[i]);
		d := t - a[i];
		if d > 0 then begin
			inc(op[i], d);
			inc(ans, d);
			dec(op[min(i+k, n+1)], d);
			if i < n then inc(op[i+1], op[i]);
		end;
	end;

	writeln(ans * c);
end.

```
