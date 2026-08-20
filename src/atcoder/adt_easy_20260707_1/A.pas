program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	N = 8;
var
	i: int8;
	cond1, cond2, cond3: boolean;
	s: array [1 .. N] of int16;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	cond1 := true;
	cond3 := true;
	for i := 1 to N do begin
		read(s[i]);
		if cond1 and cond3 then begin
			if n > 1 then cond1 := s[i-1] <= s[i];
			cond3 := s[i] mod 25 = 0;
		end;
	end;
	readln;

	cond2 := (100 <= s[1]) and (s[N] <= 675);

	if cond1 and cond2 and cond3 then
		writeln('Yes')
	else
		writeln('No');
end.
