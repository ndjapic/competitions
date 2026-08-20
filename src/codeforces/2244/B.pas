program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	notc, tci, n, i, a: int32;
	s: int64;
	neat: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		s := 0;
		neat := true;
		for i := 1 to n do begin
			read(a);

			if neat then begin
				inc(s, a - i);
				neat := s >= 0;
			end;
		end;
		readln;

		if neat then
			writeln('YES')
		else
			writeln('NO');

	end;
end.
