program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 100;
var
	n, i, k, r, x: int8;
	p, rank: array [1 .. NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do begin
		rank[i] := 0;
		read(p[i]);
	end;
	readln;

	r := 1;
	repeat

		x := 0;
		for i := 1 to n do
			if rank[i] = 0 then begin
				x := max(x, p[i]);
			end;

		k := 0;
		for i := 1 to n do
			if p[i] = x then begin
				rank[i] := r;
				inc(k);
			end;

		inc(r, k);

	until k = 0;

	for i := 1 to n do writeln(rank[i]);
end.
