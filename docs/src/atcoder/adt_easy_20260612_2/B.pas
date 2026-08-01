program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, agrees, disagrees: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	agrees := 0;
	disagrees := 0;

	for i := 1 to n do begin
		readln(s);

		case s[1] of
			'F': inc(agrees);
			'A': inc(disagrees);
		end;
	end;

	if agrees > disagrees then
		writeln('Yes')
	else
		writeln('No');
end.
