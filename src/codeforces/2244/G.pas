program _F;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #unsolved
uses
	math;
const
	NN = 200 * 1000 + 1;
var
	notc, tci, n, i, j: int32;
	ans: int64;
	a: array [1 .. NN] of int32;
	pre, suf: array [0 .. NN] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		ans := 0;

		pre[0] := 0;
		for i := 1 to n do begin
			read(a[i]);
			j := i - a[i] - 1;

			if j < 1 then
				pre[i] := a[i]
			else if j + a[j] < i then
				pre[i] := a[i] + pre[i-a[i]-1];

			pre[i] := max(pre[i-1], pre[i]);
			ans := max(ans, pre[i]);
		end;
		readln;

		{suf[n+1] := 0;
		for i := n downto 1 do begin
			if i + a[i] + 1 > n+1 then
				suf[i] := a[i]
			else
				suf[i] := a[i] + suf[i+a[i]+1];

			suf[i] := max(suf[i+1], suf[i]);
			ans := max(ans, min(pre[i], suf[i]));
		end;}

		writeln(ans);

	end;
end.
