program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, t: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	t := 0;
	for i := 1 to n do begin
		readln(s);
		if s[1] = 'T' then inc(t);
	end;

	writeln(t);
end.
