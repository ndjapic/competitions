program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	notc, tci, n, k, i, ans: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n, k);
		readln(s);

		if 2*k > n then
			ans := -1
		else begin

			ans := 0;

			for i := 1 to k do
				if s[i] = 'L' then
					inc(ans);

			for i := n-k+1 to n do
				if s[i] = 'R' then
					inc(ans);

		end;

		writeln(ans);

	end;
end.
