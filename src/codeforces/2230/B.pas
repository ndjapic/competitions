program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 300 * 1000;
var
	notc, tci, n, i, ans: int32;
	ch: char;
	s: string;
	c: array [0 .. NN] of array ['1' .. '4'] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(s);
		n := length(s);

		for ch := '1' to '4' do c[0][ch] := 0;

		for i := 1 to n do begin
			c[i] := c[i-1];
			if s[i] = '3' then s[i] := '1';
			inc(c[i][s[i]]);
		end;

		ans := n;
		for i := 0 to n do
			ans := min(ans, c[i]['1'] + c[n]['2'] - c[i]['2']);

		writeln(ans + c[n]['4']);

	end;
end.
