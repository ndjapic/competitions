program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 1500;
var
	n, m, q, i, j, k, r1, c1, r2, c2: int32;
	ans: int64;
	b: array [1 .. nn, 1 .. nn] of int32;
	enhanced: array [0 .. nn, 0 .. nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, q);

	for i := 1 to n do begin
		for j := 1 to m do read(b[i, j]);
		readln;
	end;

	for i := 0 to n do
		for j := 0 to m do enhanced[i, j] := 0;

	for k := 1 to q do begin
		readln(r1, c1, r2, c2);
		dec(r1);
		dec(c1);
		enhanced[r1, c1] := enhanced[r1, c1] xor 1;
		enhanced[r1, c2] := enhanced[r1, c2] xor 1;
		enhanced[r2, c1] := enhanced[r2, c1] xor 1;
		enhanced[r2, c2] := enhanced[r2, c2] xor 1;
	end;

	ans := 0;
	for i := n downto 1 do
		for j := m downto 1 do begin
			inc(ans, b[i, j] * (enhanced[i, j] + 1));
			enhanced[i, j-1] := enhanced[i, j-1] xor enhanced[i, j];
			enhanced[i-1, j] := enhanced[i-1, j] xor enhanced[i, j];
			enhanced[i-1, j-1] := enhanced[i-1, j-1] xor enhanced[i, j];
		end;

	writeln(ans);
end.
