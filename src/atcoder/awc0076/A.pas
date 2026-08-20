program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #query
var
	m, i, v, p: int32;
	s, r: int64;
	e: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s, p, r);
	readln(m);

	for i := 1 to m do begin
		readln(e, v);
		case e of
			1: inc(s, v);
			2: dec(s, v * p);
		end;
	end;

	writeln(s - r);
end.
