program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #joke
uses
	math;
const
	NN = 200 * 1000;
	PRIME = 998244353;
var
	notc, tci, n, i: int32;
	x, ans: int8;
	s, t: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		readln(s);

		setlength(t, max(n, 4));

		ans := 0;
		for x := 0 to 3 do begin

			t[1] := chr(ord('0') + x mod 2);
			t[2] := chr(ord('0') + x div 2);

			t[3] := chr(ord('1') - x mod 2);
			t[4] := chr(ord('1') - x div 2);

			for i := 5 to n do t[i] := t[i-4];

			i := 1;
			while (i <= n) and ((s[i] = '?') or (s[i] = t[i])) do inc(i);

			if i > n then inc(ans);

		end;

		writeln(ans);

	end;
end.
