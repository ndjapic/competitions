program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 30;
var
	n, m, i, j, target, pairs: int32;
	s: string;
	ch: char;
	mask: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);
	target := (1 shl m) - 1;

	pairs := 0;
	for i := 1 to n do begin
		readln(s);

		mask[i] := 0;
		for ch in s do begin
			mask[i] := mask[i] * 2;
			if ch = 'o' then inc(mask[i]);
		end;

		for j := 1 to i-1 do
			if mask[j] or mask[i] = target then inc(pairs);
	end;

	writeln(pairs);
end.
