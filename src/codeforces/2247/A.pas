program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	notc, tci, n, i, a, s: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		s := 0;
		for i := 1 to n do begin
			read(a);
			inc(s, a);
		end;
		readln;

		if abs(s) mod 4 > 0 then
			writeln('NO')
		else
			writeln('YES');

	end;
end.
