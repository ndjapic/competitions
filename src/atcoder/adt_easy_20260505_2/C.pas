program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, k, l, r, strawberries: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);
	readln(s);

	strawberries := 0;
	l := 0;
	for r := 1 to n do
		if s[r] = 'X' then
			l := r
		else if r-l = k then begin
			inc(strawberries);
			l := r;
		end;

	writeln(strawberries);
end.
