program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int8;
	a, b, c: boolean;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(s);

	a := false;
	b := false;
	c := false;
	i := 0;
	while not (a and b and c) do begin
		inc(i);
		case s[i] of
			'A': a := true;
			'B': b := true;
			'C': c := true;
		end;
	end;

	writeln(i);
end.
