program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, m, i, j, a, b: int32;
	s, d: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, s);

	d := 0;
	for i := 1 to n do begin
		read(a);
		inc(d, a);
	end;
	readln;

	for j := 1 to m do begin
		read(b);
		dec(d, b);
	end;
	readln;

	if d >= 0 then
		writeln(-1)
	else
		writeln(s div (-d));
end.
