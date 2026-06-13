program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, y, m, a, b, p, q, c, thism, nextm: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, y, m);
	inc(m, y*12);

	thism := 0;
	nextm := 0;

	for i := 1 to n do begin
		readln(a, b, p, q, c);
		inc(q, p*12);
		if q = m then
			inc(thism, c)
		else if q = m+1 then
			inc(nextm, c);
	end;

	writeln(thism, ' ', nextm);
end.
