program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #bisect
uses
	math;
var
	ntc, tci, na, nb, nc: int32;
	l, r, m: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(ntc);
	for tci := 1 to ntc do begin
		readln(na, nb, nc);

		l := 0;
		r := min(na, nc) + 1;
		while r-l > 1 do begin
			m := (l+r) div 2;
			if (m <= na) and (m <= nc) and (m <= na-m + nb + nc-m) then
				l := m
			else
				r := m;
		end;

		writeln(l);
	end;
end.
