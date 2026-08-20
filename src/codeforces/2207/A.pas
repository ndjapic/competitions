program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	notc, tci, n, i, mn, mx: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		readln(s);

		for i := 2 to n-1 do
			if (s[i-1] = '1') and (s[i+1] = '1') then s[i] := '1';

		mx := 0;
		for i := 1 to n do
			if s[i] = '1' then inc(mx);

		for i := 2 to n-1 do
			if (s[i-1] = '1') and (s[i+1] = '1') then s[i] := '0';

		mn := 0;
		for i := 1 to n do
			if s[i] = '1' then inc(mn);

		writeln(mn, ' ', mx);

	end;
end.
