program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, t, a: int8;
	s: string;
	ans: char;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(s);

	t := 0;
	a := 0;

	for i := 1 to n do begin

		case s[i] of
			'T': inc(t);
			'A': inc(a);
		end;

		if t > a then
			ans := 'T'
		else if t < a then
			ans := 'A';

	end;

	writeln(ans);
end.
