program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	notc, tci, n, l, r, d: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		readln(s);

		l := 0;
		d := 0;
		for r := 1 to n do
			if s[r] = '*' then
				l := r
			else
				d := max(d, r-l);

		writeln((d+1) div 2);

	end;
end.
