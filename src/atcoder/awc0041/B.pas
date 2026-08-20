program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 200 * 1000;
var
	n, m, i, j, b, l, r, p: int32;
	w: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	w[1] := 0;
	for i := 2 to m do begin
		read(w[i]);
		w[i] := max(w[i-1], w[i]);
	end;
	readln;

	for j := 1 to n do begin
		read(b);
		l := 1;
		r := m+1;

		while r-l > 1 do begin
			p := (l+r) div 2;
			if b >= w[p] then
				l := p
			else
				r := p;
		end;

		writeln(l);
	end;
	readln;
end.
