program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	xx = 1000 * 1000;
var
	n, x, x3: int64;
	l, r: int8;
	found: boolean;
	d: array [0 .. 18] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	x := xx + 1;
	found := false;
	while not found do begin
		dec(x);
		x3 := x*x*x;

		if x3 <= n then begin
			l := 1;
			r := 0;

			while x3 > 0 do begin
				inc(r);
				d[r] := x3 mod 10;
				x3 := x3 div 10;
			end;

			while (l < r) and (d[l] = d[r]) do begin
				inc(l);
				dec(r);
			end;

			found := l >= r;
		end;
	end;

	writeln(x*x*x);
end.
