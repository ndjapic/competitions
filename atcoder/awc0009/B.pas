program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, c, h, p: int32;
	s, k: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, s, c);

	k := 0;
	for i := 1 to n do begin
		readln(h, p);
		if s >= h then begin
			dec(s, h);
			inc(s, p);
		end else
			inc(k, c);
	end;

	writeln(k);
end.
