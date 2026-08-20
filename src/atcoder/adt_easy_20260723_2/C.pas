program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, d, i, j, l, r, ans: int32;
	s, t: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, d);
	readln(t);

	for i := 2 to n do begin
		readln(s);
		for j := 1 to d do
			if s[j] = 'x' then t[j] := 'x';
	end;

	l := 0;
	ans := 0;
	for r := 1 to d do
		if t[r] = 'x' then
			l := r
		else
			ans := max(ans, r-l);

	writeln(ans);
end.
