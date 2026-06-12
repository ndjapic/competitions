program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	notc, tci, n, k, i: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n, k);
		readln(s);

		for i := n downto k+1 do
			if s[i] = s[i-k] then
				s[i-k] := '0'
			else
				s[i-k] := '1';

		i := k;
		while (i > 0) and (s[i] = '0') do dec(i);

		if i > 0 then
			writeln('NO')
		else
			writeln('YES');

	end;
end.
