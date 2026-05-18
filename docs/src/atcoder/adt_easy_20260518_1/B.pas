program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	if s[1] = s[2] then begin

		if s[2] = s[3] then
			writeln(1)
		else
			writeln(3);

	end else begin

		if (s[1] = s[3]) or (s[2] = s[3]) then
			writeln(3)
		else
			writeln(6);

	end;
end.
