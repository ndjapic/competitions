program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, i, k, x: int32;
	a: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	k := 0;
	for i := 1 to n do begin
		read(x);
		if (k >= 3) and (a[k-2] = x) and (a[k-1] = x) and (a[k] = x) then
			dec(k, 3)
		else begin
			inc(k);
			a[k] := x;
		end;
	end;
	readln;

	writeln(k);
end.
