program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, upper, lower: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	n := length(s);
	upper := 0;
	lower := 0;

	for i := 1 to n do
		if s[i] < 'a' then
			inc(upper)
		else
			inc(lower);

	if upper > lower then begin
		for i := 1 to n do
			if s[i] >= 'a' then
				s[i] := chr( ord(s[i]) - ord('a') + ord('A') );
	end else begin
		for i := 1 to n do
			if s[i] < 'a' then
				s[i] := chr( ord(s[i]) - ord('A') + ord('a') );
	end;

	writeln(s);
end.
