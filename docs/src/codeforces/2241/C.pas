program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	notc, tci, n, i, c: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		readln(s);

		c := 0;
		for i := 2 to n do
			if s[i-1] <> s[i] then inc(c);

		if c = 1 then
			writeln('2')
		else
			writeln('1');

	end;
end.
