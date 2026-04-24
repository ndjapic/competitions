program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	notc, tci, n, i, c: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(s);
		n := length(s);

		c := 0;
		for i := 2 to n do
			if s[i-1] = s[i] then inc(c);

		if c <= 2 then
			writeln('YES')
		else
			writeln('NO');

	end;
end.
