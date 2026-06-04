program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #variations #repetition
const
	NN = 60;
var
	k: int64;
	n, e: int8;
	d: array [0 .. NN] of int8;
	ans: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(k);

	n := 0;
	while k > 0 do begin
		d[n] := k mod 2 * 2;
		inc(n);
		k := k div 2;
	end;

	setlength(ans, n);
	for e := 0 to n-1 do
		ans[n-e] := chr(ord('0') + d[e]);

	writeln(ans);
end.
