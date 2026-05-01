program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 200 * 1000;
var
	notc, tci, n, i, j: int32;
	a, b: array [1 .. nn] of int32;
	c: array [0 .. 1, 0 .. 1] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		c[0, 0] := 0;
		c[0, 1] := 0;
		c[1, 0] := 0;
		c[1, 1] := 0;

		for i := 1 to n do begin
			read(a[i]);
			if a[i] mod 6 = 0 then
				inc(c[0, 0])
			else if a[i] mod 2 = 0 then
				inc(c[0, 1])
			else if a[i] mod 3 = 0 then
				inc(c[1, 0])
			else
				inc(c[1, 1]);
		end;
		readln;

		j := 0;

		if c[0, 1] <= c[1, 0] then
			for i := 1 to n do
				if a[i] mod 6 = 0 then begin
					inc(j);
					b[j] := a[i];
				end;

		for i := 1 to n do
			if (a[i] mod 2 = 0) and (a[i] mod 3 > 0) then begin
				inc(j);
				b[j] := a[i];
			end;

		for i := 1 to n do
			if (a[i] mod 2 > 0) and (a[i] mod 3 > 0) then begin
				inc(j);
				b[j] := a[i];
			end;

		for i := 1 to n do
			if (a[i] mod 2 > 0) and (a[i] mod 3 = 0) then begin
				inc(j);
				b[j] := a[i];
			end;

		if c[0, 1] > c[1, 0] then
			for i := 1 to n do
				if a[i] mod 6 = 0 then begin
					inc(j);
					b[j] := a[i];
				end;

		for j := 1 to n-1 do write(b[j], ' ');
		writeln(b[n]);

	end;
end.
