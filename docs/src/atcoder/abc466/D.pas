program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #backward #counting
const
	NN = 300 * 1000;
var
	n, m, i, j, ans: int32;
	r, c: array [1 .. NN] of int32;
	row, col: array [1 .. NN] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for j := 1 to m do readln(r[j], c[j]);

	for i := 1 to n do begin
		row[i] := false;
		col[i] := false;
	end;

	ans := 0;
	for j := m downto 1 do begin
		if not row[r[j]] and not col[c[j]] then inc(ans);
		row[r[j]] := true;
		col[c[j]] := true;
	end;

	writeln(ans);
end.
