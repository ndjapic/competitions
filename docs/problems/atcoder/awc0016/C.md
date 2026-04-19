# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, l, r, t, p, s, i0, p0, s0: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, l, r, t);
	i0 := -1;

	for i := 1 to n do begin
		readln(p, s);
		if (l <= p) and (p <= r) and (s >= t) then begin
			if (i0 = -1) or (
				(p < p0) or (p = p0) and (s > s0)
			) then begin
				i0 := i;
				p0 := p;
				s0 := s;
			end;
		end;
	end;

	writeln(i0);
end.

```
