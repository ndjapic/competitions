program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 1000;
var
	n, i, j: int32;
	found: boolean;
	s: array [1 .. NN] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	found := false;
	for j := 1 to n do begin
		readln(s[j]);
		if not found then
			for i := 1 to j-1 do
				if not found then
					found := s[i] = s[j];
	end;

	if found then
		writeln('Yes')
	else
		writeln('No');
end.
