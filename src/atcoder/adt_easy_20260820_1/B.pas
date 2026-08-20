program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int8;
	s, t: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	s := 0;
	for i := 1 to n do begin
		t := i * i * i;
		if odd(i) then t := -t;
		inc(s, t);
	end;

	writeln(s);
end.
