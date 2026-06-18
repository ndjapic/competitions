program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, v: int32;
	w: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, w);

	for i := 1 to n do begin
		read(v);
		if v <= w then inc(w, v);
	end;
	readln;

	writeln(w);
end.
