program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, i, depth, mx: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	depth := 0;
	mx := 0;

	for i := 1 to n do begin
		mx := max(mx, depth);
		case s[i] of
			'(': inc(depth);
			')': dec(depth);
		end;
	end;

	writeln(mx);
end.
