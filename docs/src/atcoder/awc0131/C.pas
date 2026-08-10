program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 1000 * 1000;
var
	n, i, mx, ans: int32;
	h: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do read(h[i]);
	readln;

	ans := 1;
	mx := h[n];
	for i := n-1 downto 1 do
		if mx <= h[i] then begin
			mx := h[i];
			inc(ans);
		end;

	writeln(ans);
end.
