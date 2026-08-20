program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Math;
const
	NN = 200 * 1000;
var
	notc, tci, n, i: int32;
	h, d, e: int64;
	a: array [1 .. NN] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		h := 1 shl 30;
		e := 0;
		for i := 1 to n do begin
			read(a[i]);
			inc(a[i], e);

			if a[i] >= h then
				e := a[i] - h
			else begin
				d := max(h - a[i] +i-1, 0) div i;
				inc(a[i], d * (i-1));
				dec(h, d);
				e := a[i] - h;
			end;

			a[i] := h;
			write(h);
			if i < n then write(' ');
		end;
		readln;
		writeln;

	end;
end.
