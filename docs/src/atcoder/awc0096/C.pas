program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 300 * 1000;
var
	n, k, i: int32;
	ans, op: int64;
	a, b, d: array [1 .. NN] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	for i := 1 to n do read(a[i]); readln;
	for i := 1 to n do read(b[i]); readln;
	for i := 1 to n do d[i] := 0;

	ans := 0;
	for i := n downto 1 do
		if ans > -1 then begin
			op := b[i] - (a[i] + d[i]);

			if (i >= k) and (op > 0) then begin
				inc(ans, op);
				inc(d[i], op);
				if i > k then dec(d[i-k], op);
			end else if op <> 0 then
				ans := -1;

			inc(d[i-1], d[i]);
		end;

	writeln(ans);
end.
